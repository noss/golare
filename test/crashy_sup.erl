%%%-------------------------------------------------------------------
%% @doc crashy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(crashy_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [connection_childspec()],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

connection_childspec() ->
    #{
        id => crashy_server,
        start => {crashy_server, start_link, []},
        shutdown => 5_000
    }.
