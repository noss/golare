-module(golare_config).

-export([dsn/0]).
-export([environment/0]).

-spec dsn() -> binary().
dsn() ->
    lookup("SENTRY_DSN", dsn, <<>>).

-spec environment() -> atom().
environment() ->
    lookup("SENTRY_ENVIRONMENT", environment, production).

%% Internal

lookup(EnvName, ConfigName, Default)  ->
    case {os:getenv(EnvName), application:get_env(golare, ConfigName, false)} of
        {false, false} -> Default;
        {false, Config} -> type_match(Config, Default);
        {Env, _} -> type_match(Env, Default)
    end.

type_match(Value, Default) when is_atom(Default) ->
    list_to_atom(Value);
type_match(Value, Default) when is_binary(Default) ->
    list_to_binary(Value).
