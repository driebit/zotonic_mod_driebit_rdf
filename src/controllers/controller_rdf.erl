%% @author Driebit
%% @copyright 2025 Driebit

-module(controller_rdf).
-author("Driebit <tech@driebit.nl>").

-export([
    resource_exists/1,
    forbidden/1,
    service_available/1,
    allowed_methods/1,
    content_types_provided/1,

    process/4
]).

-include_lib("zotonic_core/include/zotonic.hrl").

resource_exists(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    Id = get_id(ContextQs),
    {m_rsc:exists(Id, ContextQs), ContextQs}.

forbidden(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    Id = get_id(ContextQs),
    {not z_acl:rsc_visible(Id, ContextQs), ContextQs}.

service_available(Context) ->
    ContextQs = z_context:ensure_qs(Context),
    % unlike 'controller_api', this doesn't look at the configuration, but
    % instead it unconditionally exposes the context between domains.
    % This makes sharing content much easier.
    ContextCh = z_context:set_cors_headers([{<<"access-control-allow-origin">>, <<"*">>}], ContextQs),
    z_context:logger_md(ContextCh),
    % The 'id' and 'serialization' arguments are required:
    Id = get_id(ContextCh),
    Serialization = get_serialization(ContextCh),
    if
        Id =:= undefined ->
            ?LOG_WARNING(#{
                text => <<"missing necessary parameter: id">>,
                in => controller_rdf,
                path => z_context:get(zotonic_dispatch_path, ContextCh)
            }),
            {{halt, 400}, ContextCh};
        Serialization =:= undefined ->
            ?LOG_WARNING(#{
                text => <<"missing necessary parameter: serialization">>,
                in => controller_rdf,
                path => z_context:get(zotonic_dispatch_path, ContextCh)
            }),
            {{halt, 400}, ContextCh};
        true ->
            {true, ContextCh}
    end.

allowed_methods(Context) ->
    {[<<"GET">>], Context}.

content_types_provided(Context) ->
    {
        [
            {<<"application">>, <<"ld+json">>, []},
            {<<"text">>, <<"turtle">>, []}
        ],
        Context
    }.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    ContextQs = z_context:ensure_qs(Context),
    RscId = m_rsc:rid(get_id(ContextQs), ContextQs),
    Ontologies = get_ontologies(ContextQs),
    Serialization = get_serialization(ContextQs),
    case mod_driebit_rdf:rsc_to_rdf(RscId, Ontologies, Serialization, ContextQs) of
        {ok, Result} when is_binary(Result) ->
            {Result, ContextQs};
        Error ->
            ?LOG_ERROR(#{
                text => <<"unexpected conversion error">>,
                in => controller_rdf,
                rsc_id => RscId,
                ontology => Ontologies,
                serialization => Serialization,
                message => Error
            }),
            {{halt, 500}, ContextQs}
    end.

get_id(Context) ->
    z_convert:to_binary(get_argument(id, Context)).
get_serialization(Context) ->
    z_convert:to_atom(get_argument(serialization, Context)).

get_argument(ArgName, Context) ->
    case z_context:get(ArgName, Context) of
        undefined -> z_context:get_q(ArgName, Context);
        Value -> Value
    end.

get_ontologies(Context) ->
    OntologiesCtx = case z_context:get(ontology, Context) of
        undefined -> [];
        Value -> [z_convert:to_atom(Value)]
    end,
    OntologiesQs = lists:map(
        fun z_convert:to_atom/1,
        z_context:get_q_all(ontology, Context)
    ),
    % when none is specified, default to the 'schema_org' ontology
    case lists:uniq(OntologiesCtx ++ OntologiesQs) of
        [] -> [schema_org];
        List -> List
    end.
