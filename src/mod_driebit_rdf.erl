%% @author Driebit
%% @copyright 2025 Driebit
%% @doc RDF support for Zotonic sites

-module(mod_driebit_rdf).
-author("Driebit <tech@driebit.nl>").

-mod_title("Driebit RDF").
-mod_description("RDF support for Zotonic sites").

-mod_prio(500).
-mod_schema(1).
-mod_provides([mod_driebit_rdf, rdf]).
-mod_depends([admin]).

-behaviour(zotonic_observer).
-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    manage_schema/2,
    observe_content_types_dispatch/3,
    observe_serialization_content_type/2,

    rsc_to_rdf/4,
    rsc_to_rdf/5,
    serialize_rdf/4,

    observe_rsc_to_rdf_graph/2,
    observe_triple_to_rdf/2,
    observe_expand_namespace/2,
    observe_serialize_rdf/2
]).

manage_schema(_, _) ->
    ok.

-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), #context{}) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [
        {{<<"text">>, <<"turtle">>, []}, rdf_schema_org_turtle},
        {{<<"application">>, <<"ld+json">>, []}, rdf_schema_org_json_ld}
    | Acc].

-spec observe_serialization_content_type(#serialization_content_type{}, #context{}) ->
    {binary(), binary(), [{binary(), binary()}]} | undefined.
observe_serialization_content_type(#serialization_content_type{serialization = turtle}, _Context) ->
    {<<"text">>, <<"turtle">>, []};
observe_serialization_content_type(#serialization_content_type{serialization = json_ld}, _Context) ->
    {<<"application">>, <<"ld+json">>, []};
observe_serialization_content_type(#serialization_content_type{}, _Context) ->
    undefined.

% RDF representation of a resource using the given ontologies and serialization.
-spec rsc_to_rdf(m_rsc:resource_id(), list(atom()) | atom(), atom(), #context{}) -> {ok, binary()} | {error, term()}.
rsc_to_rdf(RscId, Ontology, Serialization, Context) ->
    rsc_to_rdf(RscId, Ontology, Serialization, [], Context).

% RDF representation of a resource using the given ontologies, serialization and namespaces.
-spec rsc_to_rdf(m_rsc:resource_id(), list(atom()) | atom(), atom(), list(atom()), #context{}) -> {ok, binary()} | {error, term()}.
rsc_to_rdf(RscId, Ontology, Serialization, Namespaces, Context) when is_atom(Ontology) ->
    rsc_to_rdf(RscId, [Ontology], Serialization, Namespaces, Context);
rsc_to_rdf(RscId, Ontologies, Serialization, Namespaces, Context) when is_list(Ontologies) ->
    % First build an RDF graph with a 'rsc_to_rdf_graph' notification per ontology
    RdfGraphResult = lists:foldl(
        fun
            (_Ontology, {error, _Reason} = Error) ->
                Error;
            (Ontology, {ok, AccRdfGraph}) ->
                Rsc_to_rdf_graph = #rsc_to_rdf_graph{
                    rsc_id = RscId,
                    category = lists:last(m_rsc:is_a(RscId, Context)),
                    ontology = Ontology
                },
                case z_notifier:first(Rsc_to_rdf_graph, Context) of
                    undefined ->
                        {ok, AccRdfGraph};
                    {ok, NewRdfGraph} ->
                        {ok, sets:union(AccRdfGraph, NewRdfGraph)};
                    Error ->
                        Error
                end
        end,
        {ok, sets:new()},
        Ontologies
    ),
    % then, if you have a graph, serialize it:
    case RdfGraphResult of
        {error, Reason} ->
            {error, Reason};
        {ok, RdfGraph} ->
            serialize_rdf(RdfGraph, Serialization, Namespaces, Context)
    end.

% Representation of an RDF graph using the given serialization and namespaces.
-spec serialize_rdf(rdf_graph(), atom(), map() | list(), #context{}) -> {ok, binary()} | {error, term()}.
serialize_rdf(RdfGraph, Serialization, NamespaceMap, Context) when is_map(NamespaceMap) ->
    % if the graph is not empty, serialize it with a 'serialize_rdf' notification
    case sets:is_empty(RdfGraph) of
        true ->
            {error, no_rdf_graph};
        false ->
            Serialize_rdf = #serialize_rdf{
                rdf_graph = RdfGraph,
                serialization = Serialization,
                namespace_map = NamespaceMap
            },
            case z_notifier:first(Serialize_rdf, Context) of
                undefined ->
                    {error, no_rdf_serialization};
                Result ->
                    Result
            end
    end;
serialize_rdf(RdfGraph, Serialization, Namespaces, Context) when is_list(Namespaces) ->
    % find the namespaces prefixes and IRIs with 'expand_namespace' notification
    NamespaceMap = lists:foldl(
        fun (Namespace, Acc) ->
                ExpandNamespace = #expand_namespace{name = Namespace},
                case z_notifier:first(ExpandNamespace, Context) of
                    {NamespacePrefix, NamespaceIRI} ->
                        maps:put(NamespacePrefix, NamespaceIRI, Acc);
                    _ ->
                        Acc
                end
        end,
        maps:new(),
        Namespaces
    ),
    serialize_rdf(RdfGraph, Serialization, NamespaceMap, Context).

% Default implementation for 'rsc_to_rdf_graph', using all fields and edges to
% compute a graph via multiple 'triple_to_rdf' notifications.
-spec observe_rsc_to_rdf_graph(#rsc_to_rdf_graph{}, #context{}) -> {ok, rdf_graph()} | {error, term()}.
observe_rsc_to_rdf_graph(#rsc_to_rdf_graph{rsc_id = RscId, category = Category, ontology = Ontology}, Context) ->
    case m_rsc:get(RscId, Context) of
        undefined -> undefined;
        PropsMap ->
            PropsTriples = lists:map(
                fun({PropName, PropValue}) ->
                    #triple_to_rdf{
                        rsc_id = RscId,
                        category = Category,
                        link_type = property,
                        link_name = z_convert:to_binary(PropName),
                        value = PropValue,
                        ontology = Ontology
                    }
                end,
                maps:to_list(PropsMap)
            ),
            OutEdgeTriples = lists:flatmap(
                fun(PredicateName) ->
                    lists:map(
                        fun(ObjectId) ->
                            #triple_to_rdf{
                                rsc_id = RscId,
                                category = Category,
                                link_type = outgoing_edge,
                                link_name = z_convert:to_binary(PredicateName),
                                value = ObjectId,
                                ontology = Ontology
                            }
                        end,
                        m_edge:objects(RscId, PredicateName, Context)
                    )
                end,
                m_edge:object_predicates(RscId, Context)
            ),
            InEdgeTriples = lists:flatmap(
                fun(PredicateName) ->
                    lists:map(
                        fun(SubjectId) ->
                            #triple_to_rdf{
                                rsc_id = RscId,
                                category = Category,
                                link_type = incoming_edge,
                                link_name = z_convert:to_binary(PredicateName),
                                value = SubjectId,
                                ontology = Ontology
                            }
                        end,
                        m_edge:subjects(RscId, PredicateName, Context)
                    )
                end,
                m_edge:subject_predicates(RscId, Context)
            ),
            lists:foldl(
                fun
                    (_, {error, _} = Error) ->
                        Error;
                    (TripleNotification, {ok, RdfGraph}) ->
                        case z_notifier:first(TripleNotification, Context) of
                            undefined ->
                                {ok, RdfGraph};
                            {ok, RdfTriples} when is_list(RdfTriples) ->
                                {ok, sets:union(sets:from_list(RdfTriples), RdfGraph)};
                            {ok, RdfTriple} ->
                                {ok, sets:add_element(RdfTriple, RdfGraph)};
                            Error ->
                                Error
                        end
                end,
                {ok, sets:new()},
                PropsTriples ++ OutEdgeTriples ++ InEdgeTriples
            )
    end.

-spec observe_triple_to_rdf(#triple_to_rdf{}, #context{}) ->
    {ok, #rdf_triple{}} | {ok, list(#rdf_triple{})} | {error, term()} | undefined.
observe_triple_to_rdf(#triple_to_rdf{ontology = schema_org} = TripleToRdf, Context) ->
    rdf_schema_org:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(#triple_to_rdf{ontology = pico_org} = TripleToRdf, Context) ->
    rdf_pico_org:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(#triple_to_rdf{ontology = pnv} = TripleToRdf, Context) ->
    rdf_pnv:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(#triple_to_rdf{ontology = prov} = TripleToRdf, Context) ->
    rdf_prov:triple_to_rdf(TripleToRdf, Context);
observe_triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

% "Expansion" of a namespace name to a namespace prefix/base IRI.
-spec observe_expand_namespace(#expand_namespace{}, #context{}) ->
    {binary() | undefined, iri()} | undefined.
observe_expand_namespace(#expand_namespace{name = site}, Context) ->
    {<<"site">>, z_context:site_url(undefined, Context)};
observe_expand_namespace(#expand_namespace{name = xsd}, _Context) ->
    {<<"xsd">>, rdf_xsd:namespace_iri()};
observe_expand_namespace(#expand_namespace{name = rdf}, _Context) ->
    {<<"rdf">>, rdf_xsd:rdf_namespace_iri()};
observe_expand_namespace(#expand_namespace{name = rdfs}, _Context) ->
    {<<"rdfs">>, rdf_xsd:rdfs_namespace_iri()};
observe_expand_namespace(#expand_namespace{name = schema_org}, _Context) ->
    {undefined, rdf_schema_org:namespace_iri()};
observe_expand_namespace(#expand_namespace{name = sdo}, _Context) ->
    {<<"sdo">>, rdf_schema_org:namespace_iri()};
observe_expand_namespace(#expand_namespace{name = pico_org}, _Context) ->
    {<<"pico">>, rdf_pico_org:namespace_iri()};
observe_expand_namespace(#expand_namespace{name = picot_org}, _Context) ->
    {<<"picot">>, rdf_pico_org:terms_namespace_iri()};
observe_expand_namespace(#expand_namespace{name = pnv}, _Context) ->
    {<<"pnv">>, rdf_pnv:namespace_iri()};
observe_expand_namespace(#expand_namespace{name = prov}, _Context) ->
    {<<"prov">>, rdf_prov:namespace_iri()};
observe_expand_namespace(#expand_namespace{}, _Context) ->
    undefined.

-spec observe_serialize_rdf(#serialize_rdf{}, #context{}) ->
    {ok, binary()} | {error, term()} | undefined.
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = turtle, namespace_map = NSMap}, Context) ->
    rdf_turtle:serialize(RdfGraph, NSMap, Context);
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = json_ld, namespace_map = NSMap}, Context) ->
    rdf_json_ld:serialize(RdfGraph, NSMap, Context);
observe_serialize_rdf(#serialize_rdf{}, _Context) ->
    undefined.
