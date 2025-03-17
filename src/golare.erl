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
    ok = set_defaults(),
    ok = add_logger(),
    golare_sup:start_link().

stop(_State) ->
    ok.

capture_event(Event) ->
    Scope = #{
        user => scope_user(erlang:get({golare, user}))
    },
    golare_transport:capture({event, maps:merge(Scope, Event)}).

scope_user(undefined) -> null;
scope_user(#{id := Id}) when is_binary(Id) ->
    #{ id => Id };
scope_user(_) -> null.

test_logger(Event) ->
    ?LOG_ERROR(Event).

set_defaults() ->
    DefaultScope = #{
            sdk => fun golare_scope:global_sdk/0,
            platform => fun golare_scope:global_platform/0,
            environment => fun golare_scope:global_environment/0,
            release => fun golare_scope:global_release/0,
            server_name => fun golare_scope:global_server_name/0,
            modules => fun golare_scope:global_modules/0
    },
    ScopeOverride = application:get_env(golare, global_scope, #{}),
    DefaultConfig = maps:merge(DefaultScope, ScopeOverride),
    DefaultFun = fun
        (Key, Fun) when is_atom(Key), is_function(Fun, 0) ->
            Fun();
        (Key, Value) when is_atom(Key) ->
            Value
    end,
    persistent_term:put({golare, defaults}, maps:map(DefaultFun, DefaultConfig)).

add_logger() ->
    Config = #{
        config => #{},
        level => warning,
        filter_default => log,
        filters => [{golare, {fun logger_filters:domain/2, {stop, sub, [golare]}}}]
    },
    ok = logger:add_handler(golare, golare_logger_h, Config).
