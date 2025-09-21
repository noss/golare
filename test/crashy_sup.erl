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
    ChildSpecs = [crashy_server:childspec()],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
