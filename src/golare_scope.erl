-module(golare_scope).

-export([global_sdk/0]).
-export([global_platform/0]).
-export([global_environment/0]).
-export([global_release/0]).
-export([global_server_name/0]).

global_sdk() ->
    #{
        name => 'erlang-golare',
        version => <<"0.1.0">>
    }.

global_platform() ->
    other.

global_environment() ->
    production.

global_release() ->
    'unknown-release'.

global_server_name() ->
    'unknown-server'.

