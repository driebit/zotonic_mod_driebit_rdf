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

-export([
    manage_schema/2,
    observe_content_types_dispatch/3,

    rsc_to_rdf/4
]).

manage_schema(_, _) ->
    ok.

-spec observe_content_types_dispatch(#content_types_dispatch{}, list(), #context{}) -> list().
observe_content_types_dispatch(#content_types_dispatch{}, Acc, _Context) ->
    [
        {"application/ld+json", rdf_schema_org_json_ld},
        {"text/turtle", rdf_schema_org_turtle}
    | Acc].


% TODO:
rsc_to_rdf(RscId, Ontology, Serialization, _Context) ->
    BinRscId = z_convert:to_binary(RscId),
    BinOntology = z_convert:to_binary(Ontology),
    BinSerialization = z_convert:to_binary(Serialization),

    Result = <<
        "TODO rsc_to_rdf (id #",
        BinRscId/binary,
        "):\n- ontology: ",
        BinOntology/binary,
        "\n- serialization: ",
        BinSerialization/binary
    >>,
    {ok, Result}.
