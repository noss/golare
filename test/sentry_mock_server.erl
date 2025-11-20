%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Mock sentry - gen_server holds state
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(sentry_mock_server).
-behaviour(gen_server).

%%%_* Exports ==========================================================
-export([start/0]).
-export([stop/0]).
-export([hear/1]).
-export([gossip/2]).

-export([init/1]).
-export([terminate/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).

-record(s, {
    pid :: pid(),
    hear :: pid()
}).

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [_Routes = cowboy_routes()], []).
stop() ->
    gen_server:call(?MODULE, stop).

hear(Pid) ->
    gen_server:call(?MODULE, {hear, Pid}).

gossip(EventId, Item) ->
    gen_server:call(?MODULE, {gossip, EventId, Item}).

cowboy_routes() ->
    [
        {'_', [
            {"/api/:id/envelope", sentry_mock_callbacks, []}
        ]}
    ].

%%%_ * gen_server callbacks --------------------------------------------
init([Routes]) ->
    CertOpts = ct_helper:get_certs_from_ets(),
    Dispatch = cowboy_router:compile(Routes),
    {ok, Pid} = cowboy:start_tls(
        ?MODULE,
        [{verify, verify_none}, {fail_if_no_peer_cert, false}] ++
            CertOpts ++
            [
                {port, 9123},
                {ip, {127, 0, 0, 1}}
            ],
        #{
            env =>
                #{dispatch => Dispatch}
        }
    ),
    {ok, #s{pid = Pid}}.

terminate(_Rsn, _S) ->
    cowboy:stop_listener(?MODULE).

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};
handle_call({hear, Pid}, _From, S) ->
    {reply, ok, S#s{hear = Pid}};
handle_call({gossip, EventId, Item}, _From, S) ->
    case S#s.hear of
        Pid when is_pid(Pid) ->
            Pid ! {capture, EventId, Item};
        _ ->
            ok
    end,
    {reply, ok, S}.

handle_cast(Msg, S) ->
    ct:pal(warning, "unexpected cast ~p", [Msg]),
    {stop, {bad_cast, Msg}, S}.

handle_info(Msg, S) ->
    ct:pal(warning, "unexpected info ~p", [Msg]),
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
