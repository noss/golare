%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(golare_connection).

-behaviour(gen_statem).

%% API
-export([start_link/0]).
-export([capture/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, format_status/1]).

%% States
-export([connecting/3]).
-export([available/3]).
-export([sending/3]).

-define(SERVER, ?MODULE).

-record(data, {
        max_queued = 100 :: pos_integer(),
        max_pipeline = 4 :: pos_integer(),
        dsn :: binary(),
        conn :: pid() | undefined,
        conn_mref,
        q :: queue:queue(),
        posted = [] :: list(gun:stream_ref())
    }).

name() -> ?MODULE.

%%%===================================================================
%%% Exposed API
%%%===================================================================

start_link() ->
    gen_statem:start_link({local, name()}, ?MODULE, [], []).

capture(Event) ->
    try
        gen_statem:call(name(), {capture, Event}, 5000)
    catch
        error:noproc ->
            ok
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
    {ok, DSN} = application:get_env(golare, dsn),
    #{ host := Host, port := Port} = uri_string:parse(DSN),
    Opts = #{http_opts => #{keepalive => 20}},
    {ok, Conn} = gun:open(Host, Port, Opts),
    Mref = monitor(process, Conn),
    Q = queue:new(),
    {ok, connecting, #data{conn = Conn, conn_mref = Mref, dsn = list_to_binary(DSN), q = Q}, []}.

terminate(_Reason, _State, _Data) ->
    ok.

format_status(Status) ->
    Status.

callback_mode() ->
    state_functions.

connecting(info, {gun_up, Conn, _Protocol}=Up, #data{conn=Conn}=Data) ->
    erlang:display(Up),
    {next_state, available, Data};
connecting(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data). 

available({call, From}, {capture, Event}, #data{q = Q0} = Data) ->
    Q1 = max_len(queue:cons(Event, Q0), 100), 
    {next_state, sending, Data#data{q = Q1},
        [{reply, From, ok},
         {next_event, internal, send}
         ]};
available(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data). 

sending(internal, send, #data{q = Q0} = Data) ->
    {ok, StreamRef} = post_capture(Data, queue:last(Q0)),
    {keep_state, Data#data{q = queue:init(Q0), posted = [StreamRef | Data#data.posted]}};
sending(info, {gun_response, Conn, _StreamRef, _IsFin, Status, _Headers}=Resp, #data{conn = Conn}) ->
    case Status of
        200 ->
            erlang:display(Resp),
            keep_state_and_data;
        429 ->
            exit({retry_after, undefined})
    end;
sending(info, {gun_data, Conn, _StreamRef, nofin, _BodyChunk}=Resp, #data{conn = Conn}) ->
    erlang:display(Resp),
    keep_state_and_data;
sending(info, {gun_data, Conn, StreamRef, fin, _BodyChunk}=Resp, #data{conn = Conn, q = Q} = Data) ->
    erlang:display(Resp),
    
    Posted = lists:delete(StreamRef, Data#data.posted),
    NextData = Data#data{posted = Posted},
    case queue:is_empty(Q) of
        true when Posted /= [] ->
            {keep_state, NextData};
        true ->
            {next_state, available, NextData};
        false when length(Posted) > Data#data.max_pipeline ->
            {keep_state, NextData};
        false ->
            {keep_state, NextData, [{next_event, internal, send}]}
    end;
sending(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data). 

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_event({call, From}, {capture, Event}, #data{q = Q0} = Data) ->
    Q1 = max_len(queue:cons(Event, Q0), Data#data.max_queued), 
    {keep_state, Data#data{q = Q1}, [{reply, From, ok}]};
handle_event(info, {'DOWN', Mref, process, Conn, Reason}, #data{conn_mref = Mref, conn=Conn}) ->
    exit(Reason);
handle_event(info, {gun_down, Conn, _Protocol, closed, _Killed}, #data{conn=Conn} = Data) ->
    {next_state, connecting, Data};
handle_event(info, {gun_down, Conn, _Protocol, _Reason, _Killed}=Down, #data{conn=Conn} = Data) ->
    erlang:display(Down),
    {next_state, connecting, Data};
handle_event(EventType, EventContent, _Data) ->
    erlang:display({EventType, EventContent}),
    keep_state_and_data.

post_capture(#data{conn=Conn, dsn=DSN}, Event) ->
    EventId = list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)),
    #{path := "/" ++ ProjectId} = uri_string:parse(binary_to_list(DSN)),
    StreamRef = gun:post(Conn, ["/api/", ProjectId, "/envelope/"], capture_http_headers()),
    Headers = json:encode(#{event_id => EventId, dsn => DSN}),
    Payload = json:encode(Event),
    Item = [$\n, json:encode(#{type => event, length => iolist_size(Payload)}), $\n, Payload],
    Body = iolist_to_binary([Headers, Item]),
    ok = gun:data(Conn, StreamRef, fin, Body),
    {ok, StreamRef}.

capture_http_headers() ->
    #{
        %%<<"content-type">> => <<"application/x-sentry-envelope">>
    }.


%% Take one from the queue tail until it is not larger than MaxLen
max_len(Q, MaxLen) ->
    case queue:len(Q) > MaxLen of
        true ->
            erlang:display(overflow),
            max_len(queue:init(Q), MaxLen);
        _ ->
            Q
    end.

