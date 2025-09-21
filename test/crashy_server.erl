-module(crashy_server).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-export([start_link/0]).
-export([childspec/0]).

start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

childspec() ->
    #{
        id => crashy_server,
        start => {crashy_server, start_link, []},
        shutdown => 5_000
    }.

init(_Args) ->
    {ok, #{}}.

handle_call({exit, Reason}, _From, _State) ->
    exit(Reason);
handle_call({error, Reason}, _From, _State) ->
    error(Reason);
handle_call({throw, Reason}, _From, State) ->
    throw({reply, Reason, State});
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, State};
handle_call({spawn, Reason}, _From, State) ->
    _ = proc_lib:spawn(fun() -> proc_lib_crash(Reason) end),
    {reply, ok, State};
handle_call({spawn_link, Reason}, _From, State) ->
    _ = proc_lib:spawn_link(fun() -> proc_lib_crash(Reason) end),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

proc_lib_crash(Reason) ->
    proc_lib:set_label(crashy_proc_lib),
    exit(Reason).
