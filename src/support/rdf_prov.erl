%% @author Driebit
%% @copyright 2025 Driebit
%% @doc Default implementation for the PROV Provenance ontology:
%% https://www.w3.org/TR/prov-o/
%% (see also: https://www.w3.org/TR/prov-overview/)
%% @end

-module(rdf_prov).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    triple_to_rdf/2,

    property_to_rdf/5,

    namespace_iri/0,
    namespaced_iri/1,
    type_triple/3
]).

-spec triple_to_rdf(#triple_to_rdf{}, z:context()) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
triple_to_rdf(#triple_to_rdf{link_type = property, ontology = prov} = TripleToRdf, Context) ->
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
% so we take this opportunity to determine the resource type instead.
% In PROV-O anything with an id is a valid entity, so we only do that by default
property_to_rdf(RscId, _, <<"id">>, RscId, Context) ->
    {ok, type_triple(RscId, <<"Entity">>, Context)};

property_to_rdf(_RscId, _Category, _PropName, _Value, _Context) ->
    undefined.

-spec namespace_iri() -> iri().
namespace_iri() ->
    <<"http://www.w3.org/ns/prov#">>.

-spec namespaced_iri(atom() | binary()) -> iri().
namespaced_iri(TermName) when is_atom(TermName) ->
    namespaced_iri(z_convert:to_binary(TermName));
namespaced_iri(TermName) when is_binary(TermName) ->
    <<(namespace_iri())/binary, TermName/binary>>.

-spec type_triple(m_rsc:resource(), iri(), z:context()) -> #rdf_triple{}.
type_triple(RscId, PvnType, Context) ->
    #rdf_triple{
        subject = rdf_utils:resolve_iri(RscId, Context),
        predicate = rdf_xsd:rdf_namespaced_iri(type),
        object = namespaced_iri(PvnType)
    }.
