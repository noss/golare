%%%-------------------------------------------------------------------
%% @doc golare public API
%% @end
%%%-------------------------------------------------------------------

-module(golare).

-behaviour(application).

-export([start/2, stop/1]).

-export([capture_event/1]).

start(normal, _StartArgs) ->
    golare_sup:start_link().

stop(_State) ->
    ok.

capture_event(Event) ->
    golare_transport:capture({event, Event}).
