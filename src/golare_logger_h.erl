-module(golare_logger_h).

-behavior(logger_handler).

-export([add_logger/0]).
-export([adding_handler/1]).
-export([removing_handler/1]).
-export([changing_config/3]).
-export([filter_config/1]).
-export([log/2]).

%%% Install logger

add_logger() ->
    case logger:get_handler_config(golare_logger_h) of
        {ok, _Config} ->
            ok;
        {error, {not_found, _}} ->
            Level = golare_config:logger_level(),
            Config = #{
                config => #{},
                level => Level,
                filter_default => log,
                filters => [{golare, {fun logger_filters:domain/2, {stop, sub, [golare]}}}]
            },
            ok = logger:add_handler(golare, golare_logger_h, Config)
    end.

%%% logger_handler callbacks

-spec adding_handler(Config1) -> {ok, Config2} | {error, Reason} when
    Config1 :: logger_handler:config(), Config2 :: logger_handler:config(), Reason :: term().
adding_handler(Config) ->
    {ok, Config}.

-spec removing_handler(Config) -> ok when Config :: logger_handler:config().
removing_handler(_Config) ->
    ok.

-spec changing_config(SetOrUpdate, OldConfig, NewConfig) -> {ok, Config} | {error, Reason} when
    SetOrUpdate :: set | update,
    OldConfig :: logger_handler:config(),
    NewConfig :: logger_handler:config(),
    Config :: logger_handler:config(),
    Reason :: term().
changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

-spec filter_config(Config) -> FilteredConfig when
    Config :: logger_handler:config(), FilteredConfig :: logger_handler:config().
filter_config(Config) ->
    Config.

-spec log(logger:log_event(), logger_handler:config()) -> term().
log(LogEvent, _Config) ->
    try
        Event0 = #{
            level => sentry_level(LogEvent),
            timestamp => event_timestamp(LogEvent)
        },
        Event1 = logger_name(Event0, LogEvent),
        Event = describe(Event1, LogEvent),
        {ok, _EventId} = golare:capture_event(Event)
    catch
        exit:{noproc, _} ->
            ok;
        Type:Rsn:Trace ->
            ExceptionValue0 =
                #{
                    type => <<"golare sdk crash">>,
                    value => format("~s:~tp", [Type, Rsn])
                },
            case [frame(T) || T <- Trace] of
                [] ->
                    ExceptionValue = ExceptionValue0;
                Frames ->
                    ExceptionValue = ExceptionValue0#{
                        stacktrace => #{frames => lists:reverse(Frames)}
                    }
            end,
            Crash = #{
                exception => #{
                    values => [ExceptionValue]
                }
            },
            golare:capture_event(Crash)
    end.

%% Internal

sentry_level(#{level := Level}) ->
    sentry_level(Level);
sentry_level(emergency) ->
    fatal;
sentry_level(alert) ->
    fatal;
sentry_level(critical) ->
    fatal;
sentry_level(error) ->
    error;
sentry_level(warning) ->
    warning;
sentry_level(notice) ->
    info;
sentry_level(info) ->
    info;
sentry_level(debug) ->
    debug;
sentry_level(_) ->
    info.

event_timestamp(#{meta := #{time := MicrosecondEpoch}}) ->
    Opts = [{unit, microsecond}, {offset, "Z"}],
    Rfc3339 = calendar:system_time_to_rfc3339(MicrosecondEpoch, Opts),
    iolist_to_binary(Rfc3339).

logger_name(Event, #{msg := {report, #{label := Label}}}) ->
    Event#{logger => format("~p", [Label])};
logger_name(Event, #{meta := #{mfa := {M, F, A}}}) ->
    Event#{logger => format("~s:~s/~b", [M, F, A])};
logger_name(Event, _) ->
    Event.

describe(Event0, #{msg := {report, TopReport}, meta := #{report_cb := ReportFun}}) when
    is_map(TopReport)
->
    case ReportFun of
        Fun when is_function(Fun, 1) ->
            {FormatString, Params} = Fun(TopReport),
            LogEntry = #{
                formatted => format(FormatString, Params),
                message => unicode:characters_to_binary(FormatString),
                params => [format("~tp", [P]) || P <- Params]
            };
        Fun when is_function(Fun, 2) ->
            Config = #{
                depth => unlimited,
                chars_limit => unlimited,
                single_line => false
            },
            Formatted = Fun(TopReport, Config),
            LogEntry = #{
                formatted => unicode:characters_to_binary(Formatted)
            }
    end,
    Event1 = Event0#{
        logentry => LogEntry
    },
    case TopReport of
        #{label := {proc_lib, crash} = _Label, report := [Info, LinkedInfos]} ->
            CrashThread0 = #{
                id => print(self()),
                name => print_list([
                    proplists:get_value(registered_name, Info), lists:keyfind(initial_call, 1, Info)
                ])
            },
            case lists:keyfind(error_info, 1, Info) of
                false ->
                    CrashThread = CrashThread0;
                {error_info, {_ErrorClass, _Reason, Trace}} ->
                    CrashThread = CrashThread0#{
                        crashed => true,
                        current => true,
                        stacktrace => #{
                            frames => lists:reverse([frame(T) || T <- Trace])
                        }
                    }
            end,
            LinkedThreads = [
                #{
                    id => print(proplists:get_value(pid, LI)),
                    name => print(proplists:get_value(registered_name, LI)),
                    current => false,
                    stacktrace => #{
                        frames => lists:reverse([
                            frame(T)
                         || T <- proplists:get_value(current_stacktrace, LI, [])
                        ])
                    }
                }
             || {neighbour, LI} <- LinkedInfos
            ],
            Event1#{
                threads => #{
                    values => [CrashThread] ++ LinkedThreads
                }
            };
        #{label := {supervisor, _} = Label, report := Info} ->
            Event1#{
                exception => #{
                    values => [
                        #{
                            type => print_list([Label, lists:keyfind(supervisor, 1, Info)]),
                            value => print(lists:keyfind(reason, 1, Info))
                        }
                    ]
                }
            };
        #{
            label := {gen_server, _} = Label,
            name := Name,
            reason := Reason0,
            client_info := ClientInfo
        } ->
            erlang:display(TopReport),
            ServerThread0 = #{
                id => print(self()),
                name => format("server ~tp", [Name]),
                state => print(Label),
                current => true,
                crashed => true
            },
            case ClientInfo of
                undefined ->
                    ClientThreads = [];
                {ClientPid, {ClientName, ClientTrace}} ->
                    ClientThreads = [
                        #{
                            id => print(ClientPid),
                            name => format("client ~tp", [ClientName]),
                            crashed => true,
                            stacktrace => #{
                                frames => lists:reverse([frame(F) || F <- ClientTrace])
                            }
                        }
                    ]
            end,
            case Reason0 of
                {_Reason, [{_Mod, _Fun, _A, _Opts} | _] = ServerTrace} when
                    is_atom(_Mod), is_atom(_Fun), is_list(_Opts)
                ->
                    ServerThread =
                        ServerThread0#{
                            stacktrace => #{
                                frames => lists:reverse([frame(F) || F <- ServerTrace])
                            }
                        };
                _ ->
                    ServerThread = ServerThread0
            end,
            Event1#{
                threads => #{
                    values => [ServerThread | ClientThreads]
                }
            };
        _ ->
            Event1
    end;
describe(E0, #{msg := {report, Report}, meta := Meta}) when is_map(Report) ->
    Fields = [message, msg, reason],
    case maps:with(Fields, Report) of
        Map when map_size(Map) > 0 ->
            Values = [{F, maps:get(F, Map)} || F <- Fields, is_map_key(F, Map)],
            {_, Message} = hd(Values);
        _ ->
            Message = Report
    end,
    E1 = E0#{
        logentry =>
            #{formatted => format("~tkp", [Message])}
    },
    describe_log(E1, Message, Report, Meta);
describe(E0, #{msg := {report, Report}, meta := Meta}) when is_list(Report) ->
    Fields = [message, msg, reason],
    case [lists:keyfind(F, 1, Report) || F <- Fields, lists:keymember(F, 1, Report)] of
        [] ->
            Message = Report;
        Values ->
            {_, Message} = hd(Values)
    end,
    E1 = E0#{
        logentry =>
            #{formatted => format("~tkp", [Message])}
    },
    describe_log(E1, Message, Report, Meta);
describe(E0, #{msg := {string, Raw}, meta := Meta}) ->
    E1 = E0#{
        logentry =>
            #{formatted => unicode:characters_to_binary(Raw)}
    },
    maybe_mfa(E1, Raw, Meta);
describe(E0, #{msg := {FormatString, Params}, meta := Meta}) when is_list(Params) ->
    E1 = E0#{
        logentry =>
            #{
                formatted => format(FormatString, Params),
                message => unicode:characters_to_binary(FormatString),
                params => [format("~tp", [P]) || P <- Params]
            }
    },
    maybe_mfa(E1, FormatString, Meta);
describe(Event0, #{msg := Fallback}) ->
    Event0#{
        logentry =>
            #{formatted => print(Fallback)}
    }.

describe_log(E0, Message, Report, Meta) when is_list(Report) ->
    E1 = maybe_mfa(E0, Message, Meta),
    E1#{
        extra => maps:from_list([{K, print(V)} || {K, V} <- Report, is_atom(K)])
    };
describe_log(E0, Message, Report, Meta) when is_map(Report) ->
    E1 = maybe_mfa(E0, Message, Meta),
    E1#{
        extra => #{K => print(V) || K := V <- Report, is_atom(K)}
    }.

maybe_mfa(E0, Message, #{mfa := {Mod, Fun, A}, file := Filename, line := Line}) ->
    Value = #{
        type => print(Message),
        stacktrace => #{
            frames => [
                frame({Mod, Fun, A, [{filename, Filename}, {line, Line}]})
            ]
        }
    },
    Exception = #{values => [Value]},
    E0#{
        exception => Exception
    };
maybe_mfa(E0, _Message, _Meta) ->
    E0.

frame({M, F, A, Opts}) ->
    case A of
        _ when is_integer(A) -> ArgNum = A;
        _ when is_list(A) -> ArgNum = length(A)
    end,
    F0 = #{function => format("~tp:~tp/~b", [M, F, ArgNum])},
    F1 = frame_extra(F0, lists:keyfind(file, 1, Opts)),
    F2 = frame_extra(F1, lists:keyfind(line, 1, Opts)),
    F2.

frame_extra(F, {file, String}) ->
    F#{filename => unicode:characters_to_binary(String)};
frame_extra(F, {line, Line}) ->
    F#{lineno => Line};
frame_extra(F, _) ->
    F.

format(Format, Args) ->
    try
        Msg = io_lib:format(Format, Args),
        unicode:characters_to_binary(Msg)
    catch
        error:badarg ->
            print([format_error, Format, Args])
    end.

print(Term) -> print_list([Term]).
print_list(Terms) ->
    Printed = [io_lib:print(T) || T <- Terms],
    unicode:characters_to_binary(lists:join(" ", Printed)).
