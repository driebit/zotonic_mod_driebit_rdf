%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Utilities for the creation/manipulation of RDF types.
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
    prefixed_iri/2,

    to_literal/2,
    to_literal/3,
    to_literal/4
]).


% Resolve the given resource IDs to a triple using the given predicate.
-spec resolve_triple(m_rsc:resource() | undefined, iri(), m_rsc:resource() | undefined, z:context() ) -> #rdf_triple{}.
resolve_triple(SubjectRsc, Predicate, ObjectRsc, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = resolve_iri(ObjectRsc, Context)
    }.

% Resolve the given resource ID and value to a triple using the given predicate.
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

% Like 'value_triple', but for values linked by a chain of predicates.
% Note: the intermediate triples are created with 'blank' subjects.
-spec nested_triple(m_rsc:resource() | undefined, list(iri()), #rdf_literal{} | term(), z:context() ) -> #rdf_triple{}.
nested_triple(SubjectRsc, [Predicate], Object, Context) when is_binary(Predicate) ->
    value_triple(SubjectRsc, Predicate, Object, Context);
nested_triple(SubjectRsc, [Predicate | Predicates], Object, Context) ->
    #rdf_triple{
        subject = resolve_iri(SubjectRsc, Context),
        predicate = Predicate,
        object = nested_triple(undefined, Predicates, Object, Context)
    }.

% Resolve the IRI of a resource from its ID.
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

-spec prefixed_iri(iri(), map()| list({binary() | undefined, binary()})) -> iri() | undefined.
prefixed_iri(IRI, NamespaceMap) when is_map(NamespaceMap) ->
    prefixed_iri(IRI, maps:to_list(NamespaceMap));
prefixed_iri(IRI, [{NamespacePrefix, NamespaceIRI} | Namespaces]) ->
    NamespaceSize = size(NamespaceIRI),
    case IRI of
        <<NamespaceIRI:NamespaceSize/binary, Rest/binary>> ->
            if
                NamespacePrefix =:= undefined -> Rest;
                is_binary(NamespacePrefix) -> <<NamespacePrefix/binary, ":", Rest/binary>>
            end;
        _ ->
            prefixed_iri(IRI, Namespaces)
    end;
prefixed_iri(_, _) ->
    undefined.

% Convert any value to an RDF literal, using the standard 'xsd' namespace.
% See also: https://www.w3.org/TR/rdf12-concepts/#xsd-datatypes
-spec to_literal( term(), z:context() ) -> #rdf_literal{}.
to_literal(Value, _Context) when is_binary(Value) ->
    case z_string:sanitize_utf8(Value) of
        Value ->
            % if the binary is all valid utf8, treat it as a string:
            to_literal(Value, rdf_xsd:namespaced_iri(string), undefined);
        _Other ->
            % otherwise, encode it as a hex binary:
            to_literal(z_utils:hex_encode(Value), rdf_xsd:namespaced_iri(hexBinary), undefined)
    end;
to_literal(Value, _Context) when is_boolean(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespaced_iri(boolean), undefined);
to_literal(Value, Context) when is_atom(Value) ->
    to_literal(z_convert:to_binary(Value), Context);
to_literal(Value, Context) when is_list(Value) ->
    to_literal(z_convert:to_binary(Value), Context);
to_literal(Value, _Context) when is_float(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespaced_iri(decimal), undefined);
to_literal(Value, _Context) when is_integer(Value) ->
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespaced_iri(integer), undefined);

to_literal({{_, _, _}, {_, _, _}} = DTValue, _Context) ->
    Value = z_dateformat:format(DTValue, "c", []),
    to_literal(z_convert:to_binary(Value), rdf_xsd:namespaced_iri(dateTime), undefined);
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
            to_literal(Value, rdf_xsd:namespaced_iri(string), LanguageTag, BaseDirection)
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
