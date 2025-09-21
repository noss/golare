-module(crashy_server).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-export([start_link/0]).

start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

init(_Args) ->
    {ok, #{}}.

handle_call({exit, Reason}, _From, _State) ->
    exit(Reason);
handle_call({error, Reason}, _From, _State) ->
    error(Reason);
handle_call({throw, Reason}, _From, _State) ->
    throw(Reason);
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
