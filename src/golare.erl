%%%-------------------------------------------------------------------
%% @doc golare public API
%% @end
%%%-------------------------------------------------------------------

-module(golare).

-behaviour(application).

-export([start/2, stop/1]).

-export([capture_event/1]).

-export([test_logger/1]).

-include_lib("kernel/include/logger.hrl").

start(normal, _StartArgs) ->
    ok = set_context(),
    ok = set_global_scopes(),
    ok = set_process_scopes(),
    ok = add_logger(),
    golare_sup:start_link().

stop(_State) ->
    ok.

capture_event(Event) ->
    ScopeFuns = persistent_term:get({golare, process_scope}, #{}),
    ScopeValues = #{K => F() || K := F <- ScopeFuns, is_function(F, 0)},
    golare_transport:capture({event, maps:merge(ScopeValues, Event)}).

test_logger(Event) ->
    ?LOG_ERROR(Event).

set_global_scopes() ->
    DefaultScope = #{
        sdk => fun golare_scope:global_sdk/0,
        platform => fun golare_scope:global_platform/0,
        environment => fun golare_scope:global_environment/0,
        release => fun golare_scope:global_release/0,
        server_name => fun golare_scope:global_server_name/0,
        modules => fun golare_scope:global_modules/0,
        contexts => fun golare_scope:global_contexts/0
    },
    ScopeOverride = application:get_env(golare, global_scope, #{}),
    DefaultConfig = maps:merge(DefaultScope, ScopeOverride),
    DefaultFun = fun
        (Key, Fun) when is_atom(Key), is_function(Fun, 0) ->
            apply(Fun, []);
        (Key, Value) when is_atom(Key) ->
            Value
    end,
    persistent_term:put({golare, global_scope}, maps:map(DefaultFun, DefaultConfig)).

set_process_scopes() ->
    ScopeDefaults = #{
        user => fun golare_scope:process_user/0,
        transaction => fun golare_scope:process_transaction/0
    },
    ScopeOverrides = application:get_env(golare, process_scope, #{}),
    ProcessScope = maps:merge(ScopeDefaults, ScopeOverrides),
    persistent_term:put({golare, process_scope}, ProcessScope).

set_context() ->
    HostOs = golare_scope:context_os(),
    Runtime = golare_scope:context_runtime(),
    Context = #{
        os => HostOs,
        runtime => Runtime
    },
    persistent_term:put({golare, contexts}, Context).

add_logger() ->
    Level = golare_config:logger_level(),
    Config = #{
        config => #{},
        level => Level,
        filter_default => log,
        filters => [{golare, {fun logger_filters:domain/2, {stop, sub, [golare]}}}]
    },
    ok = logger:add_handler(golare, golare_logger_h, Config).
