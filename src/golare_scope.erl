-module(golare_scope).

-export([global_sdk/0]).
-export([global_platform/0]).
-export([global_environment/0]).
-export([global_release/0]).
-export([global_server_name/0]).
-export([global_modules/0]).
-export([global_contexts/0]).
-export([context_os/0]).
-export([context_runtime/0]).
-export([process_user/0]).
-export([process_transaction/0]).
-export([process_request/0]).

global_sdk() ->
    {ok, Vsn} = application:get_key(golare, vsn),
    #{
        name => 'kivra.erlang.golare',
        version => iolist_to_binary(Vsn)
    }.

global_platform() ->
    other.

global_environment() ->
    golare_config:environment().

global_release() ->
    %% These are set by rebar3 releases
    case {os:getenv("RELEASE_NAME"), os:getenv("RELEASE_VSN")} of
        {false, _} -> null;
        {_, false} -> null;
        {Name, Vsn} -> iolist_to_binary([Name, $@, Vsn])
    end.

global_server_name() ->
    case os:getenv("HOSTNAME") of
        false -> unknown;
        Hostname -> unicode:characters_to_binary(Hostname)
    end.

global_modules() ->
    ModVsnList = [{N, binary:list_to_bin(Vsn)} || {N, _, Vsn} <- application:loaded_applications()],
    maps:from_list(ModVsnList).

global_contexts() ->
    persistent_term:get({golare, contexts}).

context_os() ->
    context_os(os:type()).

context_os({unix, linux}) ->
    try
        {ok, ReleaseInfo} = file:read_file("/etc/os-release"),
        {Mj, Mi, Pa} = os:version(),
        OsVersion = iolist_to_binary(io_lib:format("~b.~b.~b", [Mj, Mi, Pa])),
        Context = #{
            type => os,
            name => linux,
            version => OsVersion
        },
        Items = binary:split(ReleaseInfo, <<"\n">>, [global, trim_all]),
        ExtractFun = fun
            (<<"ID=", Id/binary>>) ->
                {true, {distribution_name, Id}};
            (<<"VERSION_ID=", Vsn/binary>>) ->
                {true, {distribution_version, string:trim(Vsn, both, "\"")}};
            (<<"PRETTY_NAME=", Pretty/binary>>) ->
                {true, {distribution_pretty_name, string:trim(Pretty, both, "\"")}};
            (_) ->
                false
        end,
        Distribution = lists:filtermap(ExtractFun, Items),
        maps:merge(Context, maps:from_list(Distribution))
    catch
        _ -> null
    end;
context_os(_) ->
    null.

context_runtime() ->
    Vsn = list_to_binary(erlang:system_info(otp_release)),
    Raw = string:trim(list_to_binary(erlang:system_info(system_version))),
    #{
        type => runtime,
        name => <<"erlang">>,
        version => Vsn,
        raw_description => Raw
    }.

process_user() ->
    maybe
        undefined ?= erlang:get({golare, user}),
        null
    end.

process_transaction() ->
    maybe
        undefined ?= erlang:get({golare, transaction}),
        null
    end.

process_request() ->
    maybe
        undefined ?= erlang:get({golare, request}),
        null
    end.
