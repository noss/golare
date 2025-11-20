-module(golare_config).

-export([dsn/0]).
-export([environment/0]).
-export([logger_level/0]).

-spec dsn() -> binary().
dsn() ->
    lookup("SENTRY_DSN", dsn, <<>>).

-spec environment() -> atom().
environment() ->
    lookup("SENTRY_ENVIRONMENT", environment, production).

-spec logger_level() -> logger:level().
logger_level() ->
    lookup("SENTRY_LOGGER_LEVEL", logger_level, notice).

%% Internal

lookup(EnvName, ConfigName, Default) ->
    case {os:getenv(EnvName), application:get_env(golare, ConfigName, false)} of
        {false, false} -> Default;
        {false, Config} -> type_match(Config, Default);
        {Env, _} -> type_match(Env, Default)
    end.

type_match(Value, Default) when is_atom(Value), is_atom(Default) ->
    Value;
type_match(Value, Default) when is_atom(Default) ->
    list_to_atom(Value);
type_match(Value, Default) when is_binary(Value), is_binary(Default) ->
    Value;
type_match(Value, Default) when is_binary(Default) ->
    list_to_binary(Value).
