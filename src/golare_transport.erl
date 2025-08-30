%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(golare_transport).

-behaviour(gen_statem).

%% API
-export([start_link/0]).
-export([capture/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, format_status/1]).

%% States
-export([started/3]).
-export([connecting/3]).
-export([available/3]).
-export([sending/3]).
-export([rate_limited/3]).

-define(SERVER, ?MODULE).

-record(envelope, {
    event_id :: binary(),
    type = event,
    payload
}).
-record(data, {
    max_queued = 100 :: pos_integer(),
    max_pipeline = 4 :: pos_integer(),
    dsn :: binary(),
    conn :: pid() | undefined,
    conn_mref,
    q :: queue:queue() | undefined,
    posted = [] :: list(gun:stream_ref())
}).

name() -> ?MODULE.

%%%===================================================================
%%% Exposed API
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, name()}, ?MODULE, [], []).

-spec capture(term()) -> {ok, EventId :: uuid:uuid() | down}.
capture(Capture) ->
    try
        gen_statem:call(name(), {capture, Capture}, 5000)
    catch
        error:noproc ->
            {ok, down}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
    quickrand_cache:init(),
    State = #data{dsn = golare_config:dsn()},
    case State#data.dsn of
        <<>> ->
            Actions = [];
        _ ->
            Actions = [{next_event, internal, connect}]
    end,
    {ok, started, State, Actions}.

terminate(_Reason, _State, _Data) ->
    ok.

format_status(Status) ->
    Status.

callback_mode() ->
    state_functions.

%%%===================================================================
%%% States
%%%===================================================================

started(internal, connect, #data{dsn = DSN}=State) ->
    ParsedDSN = #{scheme := Scheme, host := Host} = uri_string:parse(DSN),
    Port = case maps:get(port, ParsedDSN, undefined) of
        undefined when Scheme == <<"http">> -> 80;
        undefined when Scheme == <<"https">> -> 443;
        P -> P
    end,
    Opts = #{http_opts => #{keepalive => 20}},
    {ok, Conn} = gun:open(binary_to_list(Host), Port, Opts),
    Mref = monitor(process, Conn),
    {next_state, connecting,
        State#data{conn = Conn, conn_mref = Mref, q = queue:new()}, []};
started({call, From}, {capture, _Event}, _Data) ->
    EventId = uuid:get_v4(cached),
    {keep_state_and_data, [{reply, From, {ok, EventId}}]};
started(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

connecting(info, {gun_up, Conn, _Protocol}, #data{conn = Conn} = Data) ->
    {next_state, available, Data};
connecting(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

available({call, From}, {capture, Event}, Data) ->
    {NextData, EventId} = enqueue(Data, Event),
    {next_state, sending, NextData, [
        {reply, From, {ok, EventId}},
        {next_event, internal, send}
    ]};
available(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

sending(internal, send, Data) ->
    {ok, StreamRef} = post_capture(Data, queue:last(Data#data.q)),
    Posted = [StreamRef | Data#data.posted],
    Q1 = queue:init(Data#data.q),
    Actions =
        case queue:is_empty(Q1) of
            false when length(Posted) < Data#data.max_pipeline ->
                [{next_event, internal, send}];
            _ ->
                []
        end,
    NextData = Data#data{q = Q1, posted = Posted},
    {keep_state, NextData, Actions};
sending(
    info, {gun_response, Conn, StreamRef, _IsFin, Status, Headers}, #data{conn = Conn} = Data
) ->
    case Status of
        200 ->
            keep_state_and_data;
        429 ->
            RetryAfter = proplists:get_value(<<"retry-after">>, Headers, <<"60">>),
            Seconds = binary_to_integer(RetryAfter),
            NextEvent = {next_event, internal, {retry_after, Seconds}},
            lists:foreach(
                fun(Ref) ->
                    ok = gun:cancel(Conn, Ref)
                end,
                lists:delete(StreamRef, Data#data.posted)
            ),
            NextData = Data#data{posted = []},
            {next_state, rate_limited, NextData, [NextEvent]}
    end;
sending(info, {gun_data, Conn, _StreamRef, nofin, _BodyChunk}, #data{conn = Conn}) ->
    keep_state_and_data;
sending(
    info, {gun_data, Conn, StreamRef, fin, _BodyChunk}, #data{conn = Conn, q = Q} = Data
) ->
    Posted = lists:delete(StreamRef, Data#data.posted),
    NextData = Data#data{posted = Posted},
    case queue:is_empty(Q) of
        true when Posted /= [] ->
            {keep_state, NextData};
        true ->
            {next_state, available, NextData};
        false when length(Posted) < Data#data.max_pipeline ->
            {keep_state, NextData, [{next_event, internal, send}]};
        false ->
            {keep_state, NextData}
    end;
sending(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

rate_limited(internal, {retry_after, Seconds}, _Data) ->
    RetryTimeoutAction = {state_timeout, Seconds * 1000, retry},
    {keep_state_and_data, [RetryTimeoutAction]};
rate_limited(state_timeout, retry, Data) ->
    case queue:is_empty(Data#data.q) of
        true ->
            {next_state, available, Data};
        false ->
            SendAction = {next_event, internal, send},
            {next_state, sending, Data, [SendAction]}
    end;
rate_limited(info, {gun_response, _Conn, _StreamRef, _IsFin, _Status, _Headers}, _Data) ->
    keep_state_and_data;
rate_limited(info, {gun_data, _Conn, _StreamRef, fin, _BodyChunk}, _Data) ->
    keep_state_and_data;
rate_limited(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_event({call, From}, {capture, Event}, Data) ->
    {NextData, EventId} = enqueue(Data, Event),
    {keep_state, NextData, [{reply, From, {ok, EventId}}]};
handle_event(info, {'DOWN', Mref, process, Conn, Reason}, #data{conn_mref = Mref, conn = Conn}) ->
    exit(Reason);
handle_event(info, {gun_down, Conn, _Protocol, _Reason, _Killed}, #data{conn = Conn} = Data) ->
    {next_state, connecting, Data#data{posted = []}};
handle_event(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

post_capture(#data{conn = Conn, dsn = DSN}, #envelope{
    event_id = RawEventId, type = Type, payload = Event
}) ->
    #{path := "/" ++ ProjectId} = uri_string:parse(binary_to_list(DSN)),
    StreamRef = gun:post(Conn, ["/api/", ProjectId, "/envelope/"], capture_http_headers()),
    EventId = list_to_binary(uuid:uuid_to_string(RawEventId, nodash)),
    Items = encode_items(Type, Event),
    SentAt = calendar:system_time_to_rfc3339(erlang:system_time(), [{unit, native}]),
    EnvelopeHeader = json:encode(#{event_id => EventId, dsn => DSN, sent_at => list_to_binary(SentAt)}),
    Body = iolist_to_binary([EnvelopeHeader, $\n, Items]),
    ok = gun:data(Conn, StreamRef, fin, Body),
    {ok, StreamRef}.

encode_items(ItemType, Event) ->
    try
        Item = json:encode(event_defaults(Event)),
        ItemHeader = json:encode(#{type => ItemType, length => iolist_size(Item)}),
        [ItemHeader, $\n, Item]
    catch
        Exception:Reason:Stacktrace ->
            Errors = #{
                type => Exception,
                value => iolist_to_binary(io_lib:print(Reason)),
                details => <<"failed to json encode event">>
            },
            LogEntry = #{
                formatted => <<"Failed to encode message">>
            },
            ErrEvent = json:encode(event_defaults(#{errors => [Errors], logentry => LogEntry})),
            ErrHeader = json:encode(#{type => event, length => iolist_size(ErrEvent)}),
            Attachment =
                [
                    io_lib:print(Reason),
                    $\n,
                    $\n,
                    <<"Raw event:\n">>,
                    io_lib:print(Event),
                    $\n,
                    <<"Raw stacktrace:\n">>,
                    io_lib:print(Stacktrace),
                    $\n
                ],
            AttachmentHeader = json:encode(#{
                type => attachment,
                filename => <<"failure.txt">>,
                content_type => <<"text/plain">>,
                length => iolist_size(Attachment)
            }),
            [
                [ErrHeader, $\n, ErrEvent],
                $\n,
                [AttachmentHeader, $\n, Attachment]
            ]
    end.

event_defaults(Event0) ->
    Defaults = persistent_term:get({golare, global_scope}, #{}),
    maps:merge(Defaults, Event0).

capture_http_headers() ->
    #{
        <<"content-type">> => <<"application/x-sentry-envelope">>
    }.

enqueue(Data, {event, Event}) ->
    EventId = uuid:get_v4(cached),
    Q1 = queue:cons(#envelope{event_id = EventId, type = event, payload = Event}, Data#data.q),
    {Data#data{q = overflow(Q1, Data#data.max_queued)}, EventId}.

%% Take one from the queue tail until it is not larger than MaxLen
overflow(Q, MaxLen) ->
    case queue:len(Q) > MaxLen of
        true ->
            overflow(queue:init(Q), MaxLen);
        _ ->
            Q
    end.
