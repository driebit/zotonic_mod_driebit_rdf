%% @author Driebit
%% @copyright 2025 Driebit
%% @end

-module(m_driebit_rdf).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").
-include("driebit_rdf.hrl").

-behaviour(zotonic_model).

-export([
    m_get/3
]).

-spec m_get( list(), zotonic_model:opt_msg(), z:context() ) -> zotonic_model:return().
m_get([ RscId | Rest ], _Msg, Context) when is_integer(RscId) ->
    case mod_driebit_rdf:rsc_to_rdf(RscId, schema_org, json_ld, Context) of
        {ok, JsonLd} ->
            {ok, {JsonLd, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.
