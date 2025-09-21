%%%-------------------------------------------------------------------
%% @doc crashy_app public API
%% @end
%%%-------------------------------------------------------------------

-module(crashy_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("kernel/include/logger.hrl").

start(normal, _StartArgs) ->
    crashy_sup:start_link().

stop(_State) ->
    ok.
