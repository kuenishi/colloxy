%% @author Mochi Media <dev@mochimedia.com>
%% @copyright colloxy Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the colloxy application.

-module(colloxy_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for colloxy.
start(_Type, _StartArgs) ->
    colloxy_deps:ensure(),
    colloxy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for colloxy.
stop(_State) ->
    ok.
