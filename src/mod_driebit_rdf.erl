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
    observe_rsc_to_rdf_graph/2
]).

manage_schema(_, _) ->
    ok.

-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), #context{}) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [
        {"application/ld+json", rdf_schema_org_json_ld},
        {"text/turtle", rdf_schema_org_turtle}
    | Acc].


rsc_to_rdf(RscId, Ontology, Serialization, Context) ->
    Rsc_to_rdf_graph = #rsc_to_rdf_graph{
        rsc_id = RscId,
        category = lists:last(m_rsc:is_a(RscId, Context)),
        ontology = Ontology
    },
    case z_notifier:first(Rsc_to_rdf_graph, Context) of
        undefined ->
            {error, no_rdf_graph};
        {ok, RdfGraph} ->
            Serialize_rdf = #serialize_rdf{
                rdf_graph = RdfGraph,
                serialization = Serialization
            },
            case z_notifier:first(Serialize_rdf, Context) of
                undefined ->
                    {error, no_rdf_serialization};
                Result ->
                    Result
            end;
        Error ->
            Error
    end.

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
            % TODO also do incoming edges?
            EdgeTriples = lists:flatmap(
                fun({PredicateName, Edges}) ->
                    lists:map(
                        fun(EdgeProps) ->
                            #triple_to_rdf{
                                rsc_id = RscId,
                                category = Category,
                                link_type = outgoing_edge,
                                link_name = z_convert:to_binary(PredicateName),
                                value = proplists:get_value(object_id, EdgeProps),
                                ontology = Ontology
                            }
                        end,
                        Edges
                    )
                end,
                m_edge:get_edges(RscId, Context)
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
                PropsTriples ++ EdgeTriples
            )
    end.
