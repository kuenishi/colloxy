%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc colloxy.

-module(colloxy).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the colloxy server.
start() ->
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    colloxy_deps:ensure(),
    application:start(colloxy).


%% @spec stop() -> ok
%% @doc Stop the colloxy server.
stop() ->
    application:stop(colloxy).
