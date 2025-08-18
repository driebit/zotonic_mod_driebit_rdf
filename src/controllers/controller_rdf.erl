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
-include("driebit_rdf.hrl").

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
    SerializationContentType = #serialization_content_type{
        serialization = get_serialization(z_context:ensure_qs(Context))
    },
    {
        case z_notifier:first(SerializationContentType, Context) of
            undefined -> [];
            ContentTypes when is_list(ContentTypes) -> ContentTypes;
            ContentType -> [ContentType]
        end,
        Context
    }.

process(_Method, _AcceptedCT, _ProvidedCT, Context) ->
    ContextQs = z_context:ensure_qs(Context),
    RscId = m_rsc:rid(get_id(ContextQs), ContextQs),
    Ontologies = get_ontologies(ContextQs),
    Serialization = get_serialization(ContextQs),
    Namespaces = get_namespaces(Context),
    case mod_driebit_rdf:rsc_to_rdf(RscId, Ontologies, Serialization, Namespaces, ContextQs) of
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
    to_known_atom(get_argument(serialization, Context)).

get_ontologies(Context) ->
    case lists:uniq(get_arguments(ontology, ontologies, Context)) of
        % when none is specified, default to the 'schema_org' ontology
        [] -> [schema_org];
        Ontologies -> Ontologies
    end.

get_namespaces(Context) ->
    get_arguments(namespace, namespaces, Context).

get_argument(ArgName, Context) ->
    case z_context:get(ArgName, Context) of
        undefined -> z_context:get_q(ArgName, Context);
        Value -> Value
    end.

get_arguments(ArgName, MultArgName, Context) ->
    ArgsCtx = case z_context:get(ArgName, Context) of
        undefined ->
            case z_context:get(MultArgName, Context) of
                Values when is_list(Values) -> lists:map(fun to_known_atom/1, Values);
                _ -> []
            end;
        Value ->
            [to_known_atom(Value)]
    end,
    ArgsQs = lists:map(
        fun to_known_atom/1,
        z_context:get_q_all(ArgName, Context)
    ),
    ArgsCtx ++ ArgsQs.

% Note: this has similar logic to 'z_convert:to_atom', but only using existing atoms.
% We do this to avoid potentially reaching Erlang's atoms' limit from converting the
% (arbitrary!) amount of parameters, which woudld result in a DOS.
% See also: https://www.erlang.org/doc/apps/erts/erlang.html#binary_to_existing_atom/2
to_known_atom(Bin) when is_binary(Bin) ->
    try binary_to_existing_atom(Bin) catch error:badarg -> undefined end;
to_known_atom(String) when is_list(String) ->
    to_known_atom(z_convert:to_binary(String));
to_known_atom(Atom) when is_atom(Atom) ->
    Atom;
to_known_atom(_) ->
    undefined.
