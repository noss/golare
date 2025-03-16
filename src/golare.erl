%%%-------------------------------------------------------------------
%% @doc golare public API
%% @end
%%%-------------------------------------------------------------------

-module(golare).

-behaviour(application).

-export([start/2, stop/1]).

-export([capture_event/1]).

start(normal, _StartArgs) ->
    set_defaults(),
    golare_sup:start_link().

stop(_State) ->
    ok.

capture_event(Event) ->
    golare_transport:capture({event, Event}).

set_defaults() ->
    DefaultScope = #{
            sdk => fun golare_scope:global_sdk/0,
            platform => fun golare_scope:global_platform/0,
            environment => fun golare_scope:global_environment/0,
            release => fun golare_scope:global_release/0,
            server_name => fun golare_scope:global_server_name/0
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
