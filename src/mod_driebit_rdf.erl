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

-export([
    manage_schema/2
]).

manage_schema(_, _) ->
  ok.
