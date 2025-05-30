%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(rdf_json_ld).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-export([
    serialize/2
]).

-spec serialize(rdf_graph(), z:context()) -> {ok, binary()} | {error, term()}.
serialize(_, _) ->
    undefined.

