-module(golare_logger_h).

-behavior(logger_handler).

-export([adding_handler/1]).
-export([removing_handler/1]).
-export([changing_config/3]).
-export([filter_config/1]).
-export([log/2]).

%%% logger_handler callbacks

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

filter_config(Config) ->
    Config.

log(#{level := Level} = LogEvent, _Config) ->
    try
        Event0 = #{
            level => sentry_level(Level),
            timestamp => event_timestamp(LogEvent),
            logger => logger_name(LogEvent),
            logentry => null,
            fingerprint => null
        },
        Event = maps:merge(Event0, describe(LogEvent)),
        {ok, _EventId} = golare:capture_event(Event)
    catch
        exit:{noproc, _} ->
            ok;
        Type:Rsn:Trace ->
            Crash = #{
                logger => ?MODULE,
                logentry => #{
                    formatted => print({Type, Rsn, Trace}),
                    message => <<"Crash in golare_logger_h">>
                },
                exception => #{
                    values => [
                        #{
                            type => <<"sdk crash">>,
                            value => print({Type, Rsn, Trace}),
                            stacktrace =>
                                case [frame(T) || T <- Trace] of
                                    [] -> null;
                                    Frames -> #{frames => lists:reverse(Frames)}
                                end
                        }
                    ]
                }
            },
            golare:capture_event(Crash)
    end.

%% Internal

sentry_level(emergency) -> fatal;
sentry_level(alert) -> fatal;
sentry_level(critical) -> fatal;
sentry_level(error) -> error;
sentry_level(warning) -> warning;
sentry_level(notice) -> info;
sentry_level(info) -> info;
sentry_level(debug) -> debug;
sentry_level(_) -> info.

event_timestamp(#{meta := #{time := MicrosecondEpoch}}) ->
    Opts = [{unit, microsecond}, {offset, "Z"}],
    Rfc3339 = calendar:system_time_to_rfc3339(MicrosecondEpoch, Opts),
    iolist_to_binary(Rfc3339).

logger_name(#{meta := #{mfa := {M, F, A}}}) ->
    format("~s:~s/~b", [M, F, A]);
logger_name(_) ->
    null.

describe(#{msg := {report, Report}, meta := Meta}) ->
    describe_report(Report, Meta);
describe(#{msg := {string, Raw}, meta := _Meta}) ->
    #{
        logentry =>
            #{formatted => unicode:characters_to_binary(Raw)}
    };
describe(#{msg := {FormatString, Params}, meta := Meta}) when is_list(Params) ->
    #{
        logentry =>
            #{
                formatted => format(FormatString, Params),
                message => unicode:characters_to_binary(FormatString),
                params => [format("~tp", [P]) || P <- Params]
            },
        extra =>
            #{K => print(V) || K := V <- maps:without([time], Meta)}
    };
describe(#{msg := Fallback, meta := #{mfa := _MFA, line := _Line}}) ->
    #{
        logentry => #{
            formatted => print(Fallback)
        }
    };
describe(#{msg := Fallback}) ->
    #{
        logentry =>
            #{formatted => print(Fallback)}
    }.

describe_report(#{label := Label, format := Format, args := Args}, Meta) ->
    #{
        logentry =>
            #{
                message => unicode:characters_to_binary(Format),
                params => [format("~tp", [A]) || A <- Args],
                formatted => format(Format, Args)
            },
        extra =>
            #{K => print(V) || K := V <- maps:without([time], Meta)},
        logger => print(Label)
    };
describe_report(#{label := Label, report := _} = Report, Meta) ->
    #{
        logentry =>
            #{
                message => print(Label),
                formatted => print(Report)
            },
        logger => print(Label),
        exception => describe_error_info(Report),
        extra =>
            #{K => print(V) || K := V <- maps:without([time, report_cb], Meta)}
        %%fingerprint => fingerprint_report(Report)
    };
describe_report(Report, _Meta) ->
    #{
        logentry =>
            #{formatted => write(Report)}
    }.

describe_error_info(#{report := [[{_, _} | _] = KVs | _]}) ->
    case proplists:get_value(error_info, KVs) of
        {Type, Rsn, Trace} ->
            #{
                values => [
                    #{
                        type => print(Type),
                        value => print(Rsn),
                        stacktrace =>
                            case [frame(T) || T <- Trace] of
                                [] -> null;
                                Frames -> #{frames => lists:reverse(Frames)}
                            end
                    }
                ]
            };
        _ ->
            null
    end;
describe_error_info(_) ->
    null.

frame({M, F, A, Opts}) ->
    ArgNum =
        if
            is_integer(A) -> A;
            is_list(A) -> length(A)
        end,
    MFA = format("~p:~p/~b", [M, F, ArgNum]),
    Filename =
        case proplists:get_value(file, Opts) of
            undefined -> null;
            String -> unicode:characters_to_binary(String)
        end,
    #{
        function => MFA,
        lineno => proplists:get_value(line, Opts, null),
        filename => Filename
    }.

format(Format, Args) ->
    Msg = io_lib:format(Format, Args),
    unicode:characters_to_binary(Msg).

print(Term) ->
    Printed = io_lib:print(Term),
    unicode:characters_to_binary(Printed).

write(Term) ->
    Written = io_lib:write(Term),
    unicode:characters_to_binary(Written).
