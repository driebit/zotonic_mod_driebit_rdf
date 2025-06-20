%% @author Driebit
%% @copyright 2025 Driebit
%% @doc RDF support for Zotonic sites

-module(mod_driebit_rdf).
-author("Driebit <tech@driebit.nl>").

-mod_title("Driebit RDF").
-mod_description("RDF support for Zotonic sites").

-mod_prio(500).
-mod_schema(1).
-mod_provides([rdf]).
-mod_depends([admin]).

-behaviour(zotonic_observer).
-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    manage_schema/2,
    observe_content_types_dispatch/3,

    rsc_to_rdf/4,
    observe_rsc_to_rdf_graph/2,
    observe_triple_to_rdf/2,
    observe_serialize_rdf/2
]).

manage_schema(_, _) ->
    ok.

-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), #context{}) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [
        {"application/ld+json", rdf_schema_org_json_ld},
        {"text/turtle", rdf_schema_org_turtle}
    | Acc].


% RDF representation of a resource using the given ontologies and serialization.
-spec rsc_to_rdf(m_rsc:resource_id(), list(atom()) | atom(), atom(), #context{}) -> {ok, binary()} | {error, term()}.
rsc_to_rdf(RscId, Ontology, Serialization, Context) when is_atom(Ontology) ->
    rsc_to_rdf(RscId, [Ontology], Serialization, Context);
rsc_to_rdf(RscId, Ontologies, Serialization, Context) when is_list(Ontologies) ->
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
    % Then, if you have a graph, serialize it with a 'serialize_rdf' notification
    case RdfGraphResult of
        {error, Reason} ->
            {error, Reason};
        {ok, RdfGraph} ->
            case sets:is_empty(RdfGraph) of
                true ->
                    {error, no_rdf_graph};
                false ->
                    Serialize_rdf = #serialize_rdf{
                        rdf_graph = RdfGraph,
                        serialization = Serialization
                    },
                    case z_notifier:first(Serialize_rdf, Context) of
                        undefined ->
                            {error, no_rdf_serialization};
                        Result ->
                            Result
                    end
            end
    end.

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
observe_triple_to_rdf(_TripleToRdf, _Context) ->
    undefined.

-spec observe_serialize_rdf(#serialize_rdf{}, #context{}) ->
    {ok, binary()} | {error, term()} | undefined.
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = turtle}, Context) ->
    rdf_turtle:serialize(RdfGraph, Context);
observe_serialize_rdf(#serialize_rdf{rdf_graph = RdfGraph, serialization = json_ld}, Context) ->
    rdf_json_ld:serialize(RdfGraph, Context);
observe_serialize_rdf(#serialize_rdf{}, _Context) ->
    undefined.
