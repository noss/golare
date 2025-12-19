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
-export([process_tags/0]).
-export([process_user/0]).
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
        Context = #{
            type => os,
            name => linux,
            version => os_version()
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
        _:_ -> #{type => os, name => linux, version => unknown}
    end;
context_os({unix, darwin}) ->
    try
        SWVers = maps:from_list([
            {string:trim(K), string:trim(V)}
         || Line <- string:split(iolist_to_binary(os:cmd("sw_vers")), "\n", all),
            [K, V] <- [string:split(Line, ":")]
        ]),
        #{
            <<"BuildVersion">> := BuildVersion,
            <<"ProductName">> := ProductName,
            <<"ProductVersion">> := ProductVersion
        } = SWVers,
        #{
            type => os,
            name => ProductName,
            version => ProductVersion,
            build => BuildVersion,
            kernel_version => os_version()
        }
    catch
        _:_ -> #{type => os, name => macOS}
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

process_tags() ->
    case sentry:get_tags() of
        undefined -> null;
        T -> T
    end.

process_user() ->
    case sentry:get_user() of
        undefined -> null;
        U -> U
    end.

process_request() ->
    case sentry:get_request() of
        undefined -> null;
        R -> R
    end.

os_version() ->
    {Mj, Mi, Pa} = os:version(),
    iolist_to_binary(io_lib:format("~b.~b.~b", [Mj, Mi, Pa])).
