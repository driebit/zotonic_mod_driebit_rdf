%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(rdf_utils).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    resolve_triple/4,
    value_triple/4,
    nested_triple/4,

    resolve_iri/2,

    to_literal/2,
    to_literal/3,
    to_literal/4
]).


-spec resolve_triple(m_rsc:resource() | undefined, iri(), m_rsc:resource() | undefined, z:context() ) -> #rdf_triple{}.
resolve_triple(SubjectRsc, Predicate, ObjectRsc, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = resolve_iri(ObjectRsc, Context)
    }.

-spec value_triple(m_rsc:resource() | undefined, iri(), #rdf_literal{} | term(), z:context() ) -> #rdf_triple{}.
value_triple(SubjectRsc, Predicate, #rdf_literal{} = Object, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = Object
    };
value_triple(SubjectRsc, Predicate, Object, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = to_literal(Object, Context)
    }.

-spec nested_triple(m_rsc:resource() | undefined, list(iri()), #rdf_literal{} | term(), z:context() ) -> #rdf_triple{}.
nested_triple(SubjectRsc, [Predicate], Object, Context) when is_binary(Predicate) ->
    value_triple(SubjectRsc, Predicate, Object, Context);
nested_triple(SubjectRsc, [Predicate | Predicates], Object, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = nested_triple(undefined, Predicates, Object, Context)
    }.

-spec resolve_iri( m_rsc:resource(), z:context() ) -> iri() | blank.
resolve_iri(Rsc, Context) ->
    case m_rsc:rid(Rsc, Context) of
        undefined ->
            blank;
        RscId ->
            case m_rsc:uri(RscId, Context) of
                undefined -> blank;
                URI when is_binary(URI) -> URI
            end
    end.

% https://www.w3.org/TR/rdf12-concepts/#xsd-datatypes
-spec to_literal( term(), z:context() ) -> #rdf_literal{}.
to_literal(Value, _Context) when is_binary(Value) ->
    case z_string:sanitize_utf8(Value) of
        Value ->
            % if the binary is all valid utf8, treat it as a string:
            to_literal(Value, rdf_xsd:namespace_iri(string), undefined);
        _Other ->
            % otherwise, encode it as a hex binary:
            to_literal(z_utils:hex_encode(Value), rdf_xsd:namespace_iri(hexBinary), undefined)
    end;
to_literal(Value, Context) when is_atom(Value) ->
    to_literal(z_convert:to_binary(Value), Context);
to_literal(Value, Context) when is_list(Value) ->
    to_literal(z_convert:to_binary(Value), Context);

to_literal(Value, _Context) when is_boolean(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespace_iri(boolean), undefined);
to_literal(Value, _Context) when is_float(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespace_iri(decimal), undefined);
to_literal(Value, _Context) when is_integer(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespace_iri(integer), undefined);

to_literal({{_, _, _}, {_, _, _}} = DTValue, _Context) ->
    Value = z_dateformat:format(DTValue, "c", []),
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespace_iri(dateTime), undefined);
to_literal(#trans{tr = Tr} = TransRec, Context) ->
    case z_trans:lookup_fallback(TransRec, Context) of
        undefined ->
            to_literal(<<>>, Context);
        Value when is_binary(Value) ->
            {TrLangs, _} = lists:unzip(Tr),
            LanguageCode = z_trans:lookup_fallback_languages(TrLangs, Context),
            LanguageTag = z_convert:to_binary(LanguageCode),
            BaseDirection = case z_language:is_rtl(LanguageCode) of
                true -> rtl;
                false -> ltr
            end,
            to_literal(Value, rdf_xsd:namespace_iri(string), LanguageTag, BaseDirection)
    end;

to_literal(Value, Context) ->
    to_literal(z_convert:to_binary(Value), Context).

-spec to_literal( term(), iri(), undefined | binary() ) -> #rdf_literal{}.
to_literal(Value, DatatypeIRI, LanguageTag) ->
    to_literal(Value, DatatypeIRI, LanguageTag, undefined).

-spec to_literal( term(), iri(), undefined | binary(), undefined | ltr | rtl) -> #rdf_literal{}.
to_literal(Value, DatatypeIRI, LanguageTag, BaseDirection) ->
    #rdf_literal{
        lexical_form = Value,
        datatype_iri = DatatypeIRI,
        language_tag = LanguageTag,
        base_direction = BaseDirection
    }.
