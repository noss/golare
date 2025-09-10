-module(golare_sentry_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("kernel/include/logger.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    ct_helper:make_certs_in_ets(),
    application:set_env(
        golare,
        tls_opts,
        ct_helper:get_certs_from_ets()
    ),
    {ok, Apps} = application:ensure_all_started([golare, cowboy]),
    {ok, _Pid} = sentry_mock_server:start(),
    ok = wait_connected(20),
    [{apps, Apps} | Config].

wait_connected(N) when N > 0 ->
    case sys:get_state(golare_transport) of
        {available, _} ->
            ok;
        {connecting, _} ->
            timer:sleep(500),
            wait_connected(N - 1)
    end;
wait_connected(0) ->
    exit(not_connecting).

end_per_suite(Config) ->
    sentry_mock_server:stop(),
    [application:stop(App) || App <- ?config(apps, Config)],
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TestName, Config) ->
    ok = sentry_mock_server:hear(self()),
    Config.

end_per_testcase(_TestName, _Config) ->
    ok.

%%%% Internal

%%%% Tests

groups() ->
    [
        {scope, [shuffle], [
            user_scope,
            transaction_scope
        ]},
        {log, [shuffle], [
            string_log,
            string_log_mfa,
            format_log
        ]}
    ].

all() ->
    [
        basic,
        {group, scope},
        {group, log}
    ].

basic(_Config) ->
    {ok, EventId} = golare:capture_event(#{message => basic}),
    ?assertMatch(<<_Data:16/binary>>, EventId),
    {Header, Item} = wait_for(EventId),
    ct:pal(info, "This is the header:~n~p~nand this is the item:~n~p", [Header, Item]),
    ?assertMatch(
        #{
            <<"contexts">> := #{
                <<"os">> := #{
                    <<"name">> := _,
                    <<"version">> := _
                },
                <<"runtime">> := #{
                    <<"name">> := _,
                    <<"version">> := _
                }
            },
            <<"modules">> := _,
            <<"sdk">> := #{
                <<"name">> := _,
                <<"version">> := _
            },
            <<"server_name">> := _,
            <<"user">> := _,
            <<"environment">> := _,
            <<"message">> := <<"basic">>
        },
        Item
    ),
    ok.

user_scope(_Config) ->
    erlang:put({golare, user}, <<"testuser">>),
    {ok, EventId} = golare:capture_event(#{message => basic}),
    {_Header, Item} = wait_for(EventId),
    ?assertMatch(#{<<"user">> := <<"testuser">>}, Item),
    erlang:erase({golare, user}),
    ok.

transaction_scope(_Config) ->
    erlang:put({golare, transaction}, <<"testtransaction">>),
    {ok, EventId} = golare:capture_event(#{message => basic}),
    {_Header, Item} = wait_for(EventId),
    ?assertMatch(#{<<"transaction">> := <<"testtransaction">>}, Item),
    erlang:erase({golare, transaction}),
    ok.

string_log(_Config) ->
    LogItem = #{level => warning, meta => #{time => 0}, msg => {string, <<"hello world">>}},
    {ok, EventId} = golare_logger_h:log(LogItem, #{}),
    {_, Item} = wait_for(EventId),
    ct:pal(default, "Captured: ~p", [Item]),
    ?assertMatch(
        #{
            <<"level">> := <<"warning">>,
            <<"timestamp">> := <<"1970-01-01T00:00:00", _/binary>>,
            <<"logentry">> := #{<<"formatted">> := <<"hello world">>}
        },
        Item
    ),
    ok.

string_log_mfa(_Config) ->
    LogItem = #{
        level => warning,
        meta => #{time => 0, mfa => {foo, bar, 0}},
        msg => {string, <<"hello world">>}
    },
    {ok, EventId} = golare_logger_h:log(LogItem, #{}),
    {_, Item} = wait_for(EventId),
    ct:pal(default, "Captured: ~p", [Item]),
    ?assertMatch(
        #{
            <<"level">> := <<"warning">>,
            <<"logger">> := <<"foo:bar/0">>,
            <<"timestamp">> := <<"1970-01-01T00:00:00", _/binary>>,
            <<"logentry">> := #{<<"formatted">> := <<"hello world">>}
        },
        Item
    ),
    ok.

format_log(_Config) ->
    LogItem = #{level => warning, meta => #{time => 0}, msg => {"format ~b", [42]}},
    {ok, EventId} = golare_logger_h:log(LogItem, #{}),
    {_, Item} = wait_for(EventId),
    ct:pal(default, "Captured: ~p", [Item]),
    ?assertMatch(
        #{
            <<"level">> := <<"warning">>,
            <<"timestamp">> := <<"1970-01-01T00:00:00", _/binary>>,
            <<"logentry">> := #{
                <<"message">> := <<"format ~b">>,
                <<"formatted">> := <<"format 42">>,
                <<"params">> := [_]
            }
        },
        Item
    ),
    ok.

wait_for(EventId) ->
    receive
        {capture, EventId, Payload} ->
            Payload
    end.
