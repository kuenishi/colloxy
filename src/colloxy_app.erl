-module(colloxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    colloxy_sup:start_link().

stop(_State) ->
    ok.

-include_lib("eunit/include/eunit.hrl").

start_stop_test()->
    ok = application:start(colloxy),
    ok = application:stop(colloxy).

