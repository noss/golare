-module(golare_event_SUITE).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(assertJSON(Term),
    ?assert(is_binary(iolist_to_binary(json:encode(Term))))
).

all() ->
    [
        base,
        timestamp_valid,
        timestamp_invalid,
        exception,
        exception_handled,
        message,
        request,
        thread,
        thread_pid,
        extra,
        user,
        context_response_cowboy
    ].

% Tests

base(_Config) ->
    Result = golare:event(#{}, []),
    ?assertMatch(#{timestamp := TS} when is_integer(TS), Result),
    ?assertJSON(Result).

timestamp_valid(_Config) ->
    Result = golare:event(#{timestamp => 1}, []),
    ?assertMatch(#{timestamp := 1}, Result),
    ?assertJSON(Result).

timestamp_invalid(_Config) ->
    ?assertError({invalid_attr, timestamp, foo}, golare:event(#{timestamp => foo}, [])).

exception(_Config) ->
    {Class, Reason, Stacktrace} =
        try
            error({my_exception, data})
        catch
            C:R:ST -> {C, R, ST}
        end,
    Result = golare:event(#{}, [
        golare_interface:exception(Class, Reason, Stacktrace)
    ]),
    ?assertMatch(
        #{
            exception := [
                #{
                    type := error,
                    value := ~"{my_exception,data}",
                    mechanism := #{type := auto, handled := false},
                    stacktrace := #{
                        frames := [
                            #{},
                            #{},
                            #{},
                            #{}
                        ]
                    }
                }
            ]
        },
        Result
    ),
    ?assertJSON(Result).

exception_handled(_Config) ->
    Result = golare:event(#{}, [
        golare_interface:exception(error, undef, [], #{handled => true})
    ]),
    ?assertMatch(
        #{
            exception := [
                #{
                    type := error,
                    value := ~"undef",
                    mechanism := #{type := auto, handled := true},
                    stacktrace := #{frames := []}
                }
            ]
        },
        Result
    ),
    ?assertJSON(Result).

message(_Config) ->
    Result = golare:event(#{}, [
        golare_interface:message(~"My message with params: ~p, ~p", [1.0, {foo, bar}])
    ]),
    ?assertMatch(
        #{
            logentry := #{
                message := ~"My message with params: ~p, ~p",
                params := [~"1.0", ~"{foo,bar}"]
            }
        },
        Result
    ),
    ?assertJSON(Result).

request(_Config) ->
    Result = golare:event(#{}, [golare_interface:request_cowboy(cowboy_req())]),
    ?assertMatch(
        #{
            request := #{
                method := ~"POST",
                query_string := ~"foo=bar&baz=qux",
                url := ~"https://example.com:9124/path/to/endpoint/123",
                headers := #{
                    ~"content-type" := ~"application/json",
                    ~"content-length" := ~"1098",
                    ~"x-Custom" := "MyValue"
                },
                cookies := #{
                    ~"PHPSESSID" := ~"298zf09hf012fh2",
                    ~"csrftoken" := ~"u32t4o3tb3gg43",
                    ~"_gat" := ~"1"
                }
            }
        },
        Result
    ),
    ?assertJSON(Result).

thread(_Config) ->
    PidStr = iolist_to_binary(io_lib:format("~p", [self()])),
    Name = my_name,
    register(Name, self()),
    Result = golare:event(#{}, [golare_interface:thread()]),
    ?assertMatch(
        #{threads := [#{id := PidStr, current := true, name := Name}]},
        Result
    ),
    ?assertJSON(Result),
    unregister(Name).

thread_pid(_Config) ->
    Pid = spawn(fun() -> ok end),
    PidStr = iolist_to_binary(io_lib:format("~p", [Pid])),
    Result = golare:event(#{}, [golare_interface:thread(#{pid => Pid})]),
    ?assertMatch(
        #{threads := [#{id := PidStr, current := true}]},
        Result
    ),
    ?assertJSON(Result).

extra(_Config) ->
    Ref = make_ref(),
    RefStr = iolist_to_binary(io_lib:format("~p", [Ref])),
    Result = golare:event(#{}, [
        golare_interface:extra(#{
            foo => bar,
            baz => {hello, "wor\"ld"},
            qux => #{ref => Ref}
        })
    ]),
    ?assertMatch(
        #{
            extra := #{
                ~"foo" := ~"bar",
                ~"baz" := ~"{hello,\"wor\\\"ld\"}",
                ~"qux" := #{~"ref" := RefStr}
            }
        },
        Result
    ),
    ?assertJSON(Result).

user(_Config) ->
    Result = golare:event(#{}, [
        golare_interface:user(#{
            % Sentry user interaface keys
            id => 123,
            email => ~"user@example.com",
            username => ~"testuser",
            % Custom keys
            subscription => ~"premium",
            account_age => {365, days}
        })
    ]),
    ?assertMatch(
        #{
            user := #{
                ~"id" := 123,
                ~"email" := ~"user@example.com",
                ~"username" := ~"testuser",
                ~"subscription" := ~"premium",
                ~"account_age" := ~"{365,days}"
            }
        },
        Result
    ),
    ?assertJSON(Result).

context_response_cowboy(_Config) ->
    Req = cowboy_req_with_resp_headers(),
    Result = golare:event(#{}, [
        golare_interface:context(response_cowboy, {Req, #{status_code => 200}})
    ]),
    ?assertMatch(
        #{
            contexts := #{
                response := #{
                    status_code := 200,
                    headers := #{
                        ~"content-type" := ~"application/json"
                    }
                }
            }
        },
        Result
    ),
    ?assertJSON(Result).

% Internal

cowboy_req() ->
    #{
        pid => self(),
        port => 9124,
        scheme => ~"https",
        version => 'HTTP/2',
        path => ~"/path/to/endpoint/123",
        host => ~"example.com",
        peer => {{127, 0, 0, 1}, 51653},
        bindings => #{id => <<"2">>},
        cert => ~"fake_cert",
        headers => #{
            ~"content-type" => ~"application/json",
            ~"content-length" => ~"1098",
            ~"x-Custom" => "MyValue",
            ~"cookie" => ~"PHPSESSID=298zf09hf012fh2; csrftoken=u32t4o3tb3gg43; _gat=1;"
        },
        ref => sentry_mock_server,
        method => ~"POST",
        host_info => undefined,
        path_info => undefined,
        body_length => 1098,
        has_body => true,
        has_read_body => true,
        qs => ~"foo=bar&baz=qux",
        sock => {{127, 0, 0, 1}, 9123},
        streamid => 15
    }.

cowboy_req_with_resp_headers() ->
    maps:merge(cowboy_req(), #{
        resp_headers => #{
            ~"content-type" => ~"application/json"
        }
    }).
