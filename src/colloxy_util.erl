-module(colloxy_util).

-export([tcp_pipe/2, ssl_pipe/2]).

-include_lib("eunit/include/eunit.hrl").


tcp_pipe(SocketLeft, SocketRight)->
    receive
	{tcp, SocketLeft, Data}->
	    ok = gen_tcp:send(SocketRight, Data),
	    tcp_pipe(SocketLeft, SocketRight);
	{tcp, SocketRight, Data}->
	    ok = gen_tcp:send(SocketLeft, Data),
	    tcp_pipe(SocketLeft, SocketRight);
	{tcp_closed, _}->
	    ok = gen_tcp:close(SocketLeft),
	    ok = gen_tcp:close(SocketRight);
	{tcp_error, _, _}->
	    ok = gen_tcp:close(SocketLeft),
	    ok = gen_tcp:close(SocketRight)
    end.

% maybe not used
ssl_pipe(SslSocket, TcpSocket)->
    receive
	{tcp, TcpSocket, Data}->
	    ok = ssl:send(SslSocket, Data),
	    ssl_pipe(SslSocket, TcpSocket);
	{ssl, SslSocket, Data}->
	    ok = gen_tcp:send(TcpSocket, Data),
	    ssl_pipe(SslSocket, TcpSocket);
	{tcp_closed, _}->
    ?debugHere,
	    ok = ssl:close(SslSocket),
	    ok = gen_tcp:close(TcpSocket);
	{ssl_closed, _}->
    ?debugHere,
	    ok = ssl:close(SslSocket),
	    ok = gen_tcp:close(TcpSocket);
	{tcp_error, _, _}->
	    ok = ssl:close(SslSocket),
	    ok = gen_tcp:close(TcpSocket);
	{ssl_error, _, _}->
	    ok = ssl:close(SslSocket),
	    ok = gen_tcp:close(TcpSocket)
    end.
