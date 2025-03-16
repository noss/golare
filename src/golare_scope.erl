-module(golare_scope).

-export([global_sdk/0]).
-export([global_platform/0]).
-export([global_environment/0]).
-export([global_release/0]).
-export([global_server_name/0]).
-export([global_modules/0]).

global_sdk() ->
    {ok, Vsn} = application:get_key(golare, vsn),
    #{
        name => 'kivra.erlang.golare',
        version => binary:list_to_bin(Vsn)
    }.

global_platform() ->
    other.

global_environment() ->
    production.

global_release() ->
    'unknown-release'.

global_server_name() ->
    'unknown-server'.

global_modules() ->
    ModVsnList = [{N, binary:list_to_bin(Vsn)} || {N, _, Vsn} <- application:loaded_applications()],
    maps:from_list(ModVsnList). 
