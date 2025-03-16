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

log(#{level := Level}=LogEvent, _Config) ->
    try
        Event = #{
            level => sentry_level(Level),
            timestamp => event_timestamp(LogEvent),
            logger => logger_name(LogEvent),
            logentry => logentry(LogEvent)
        },
        {ok, _EventId} = golare:capture_event(Event)
    catch 
        Type:Rsn:Trace ->
            Crash = #{logentry => #{formatted => iolist_to_binary(io_lib:print({Type, Rsn, Trace}))}},
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

logger_name(#{meta := #{mfa := MFA}}) ->
    iolist_to_binary(io_lib:print(MFA));
logger_name(_) ->
    null.

logentry(#{msg := Msg, meta := _Meta}) ->
    case Msg of
        {report, Report} ->
            #{formatted => iolist_to_unicode_binary(io_lib:print(Report, 1, 80, 4))};
        {string, Raw} ->
            #{formatted => iolist_to_unicode_binary(Raw)};
        {FormatString, Params} ->
            #{formatted => iolist_to_unicode_binary(io_lib:format(FormatString, Params)),
              message => iolist_to_unicode_binary(FormatString),
              params => [iolist_to_unicode_binary(io_lib:print(P, 1, 80, 3)) || P <- Params]
            }
    end. 

iolist_to_unicode_binary(In) ->
    unicode:characters_to_binary(iolist_to_binary(In), latin1, utf8).
