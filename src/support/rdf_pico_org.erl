%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for personsincontext.org ontology.
%% See also: https://personsincontext.org/model/
%% @end

-module(rdf_pico_org).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    triple_to_rdf/2,

    property_to_rdf/5,

    namespace_iri/0,
    namespaced_iri/1,
    terms_namespace_iri/0,
    terms_namespaced_iri/1,
    type_triple/3
]).


-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
triple_to_rdf(#triple_to_rdf{link_type = property, ontology = pico_org} = TripleToRdf, Context) ->
    property_to_rdf(
        TripleToRdf#triple_to_rdf.rsc_id,
        TripleToRdf#triple_to_rdf.category,
        TripleToRdf#triple_to_rdf.link_name,
        TripleToRdf#triple_to_rdf.value,
        Context
    );
triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

-spec property_to_rdf(m_rsc:resource_id(), atom(), binary(), term() | m_rsc:resource_id(), z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
property_to_rdf(_RscId, _Category, _PropName, undefined, _Context) ->
    % Ignore empty values
    undefined;

% The ID property doesn't produce any triple, but is always there,
% so we take this opportunity to determine the resource type instead:
property_to_rdf(RscId, person, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"PersonObservation">>, Context)};

property_to_rdf(RscId, person, <<"date_start">>, Value, Context) ->
    case m_rsc:p(RscId, <<"date_end">>, Context) of
        undefined ->
            Now = calendar:universal_time(),
            {{Years, _, _}, _} = z_datetime:diff(Value, Now),
            {ok, rdf_utils:value_triple(
                RscId,
                namespaced_iri(hasAge),
                z_convert:to_binary(Years),
                Context
            )};
        _ ->
            undefined
    end;
property_to_rdf(RscId, person, <<"date_end">>, Value, Context) ->
    Now = z_datetime:timestamp(),
    case z_datetime:datetime_to_timestamp(Value) of
        Timestamp when is_integer(Timestamp) andalso Timestamp < Now ->
            {ok, rdf_utils:value_triple(
                RscId,
                namespaced_iri(deceased),
                true,
                Context
            )};
        _ ->
            undefined
    end;

property_to_rdf(RscId, Category, PropName, Value, Context) ->
    % If there was no match, try with parent categories (when possible):
    case lists:delete(Category, m_category:is_a(Category, Context)) of
        [] ->
            undefined;
        ParentCatList ->
            ParentCat = lists:last(ParentCatList),
            property_to_rdf(RscId, ParentCat, PropName, Value, Context)
    end.

-spec namespace_iri() -> iri().
namespace_iri() ->
    <<"https://personsincontext.org/model#">>.

-spec namespaced_iri(atom() | binary()) -> iri().
namespaced_iri(TermName) when is_atom(TermName) ->
    namespaced_iri(z_convert:to_binary(TermName));
namespaced_iri(TermName) when is_binary(TermName) ->
    <<(namespace_iri())/binary, TermName/binary>>.

-spec type_triple(m_rsc:resource(), iri(), z:context()) -> #rdf_triple{}.
type_triple(RscId, PicoType, Context) ->
    #rdf_triple{
        subject = rdf_utils:resolve_iri(RscId, Context),
        predicate = rdf_xsd:rdf_namespaced_iri(type),
        object = namespaced_iri(PicoType)
    }.

-spec terms_namespace_iri() -> iri().
terms_namespace_iri() ->
    <<"https://terms.personsincontext.org/">>.

-spec terms_namespaced_iri(atom() | binary()) -> iri().
terms_namespaced_iri(TermName) when is_atom(TermName) ->
    terms_namespaced_iri(z_convert:to_binary(TermName));
terms_namespaced_iri(TermName) when is_binary(TermName) ->
    <<(terms_namespace_iri())/binary, TermName/binary>>.

