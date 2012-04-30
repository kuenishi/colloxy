%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for colloxy.

-module(colloxy_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

%loop(Req, DocRoot) ->
loop(Req, _) ->
%    "/" ++ Path = Req:get(path),
    try
	do_req(Req)
    catch
	exit:normal ->
	    Req:cleanup();
        Type:What ->
            Report = ["web request failed",
                      %{path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
	    %inet:setopts(Req:get(socket), [{packet,http}]),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

do_req(Req)->
%    erlang:display(Req),
    %io:format("~p~n", [Req]),
    HostPort = Req:get_primary_header_value("host"),
    % Scheme = Req:get(scheme), % http | https
    erlang:display(HostPort),
    {Host,Port}=case string:tokens(HostPort, ":") of
		    [Host0] -> {Host0, 80};
		    [Host0,Port0,_] -> {Host0, list_to_integer(Port0)}
		end,
    %erlang:display(Port , Host),

    M = Req:get(method),
    P = Req:get(raw_path),
    % V = Req:get(version),

    HeaderList = mochiweb_headers:to_list(Req:get(headers)),
    F = fun({K,V}, Acc) ->
		<< (list_to_binary(mochiweb_util:make_io(K)))/binary, <<": ">>/binary,
		   (list_to_binary(V))/binary, <<"\r\n">>/binary, Acc/binary>>
	end,
    BinHeaders = lists:foldl(F, <<"\r\n">>, HeaderList),
    ReqBin = <<(list_to_binary(lists:flatten(io_lib:format("~s ~s HTTP/1.1\r\n", [M, P]))))/binary,
	       BinHeaders/binary>>,
    
    io:format("~s\n", [binary_to_list(ReqBin)]),
    
    io:format("connecting to ~s:~w ..\n", [Host,Port]),
    % TODO: copy options from Socket (inet6, ssl and so on)
    {ok,Socket} = gen_tcp:connect(Host, Port, [{packet,raw},binary,{active,false}]),

    case gen_tcp:send(Socket, ReqBin) of
	ok ->
	    ok = inet:setopts(Socket,[{active,true},binary,{packet,raw}]),
	    ok = inet:setopts(Req:get(socket),[{active,true},binary,{packet,raw}]),
	    run_forever(Socket,Req:get(socket),Req);
	_Other ->
	    error_logger:error_report(_Other),
	    inet:setopts(Req:get(socket), [{packet,http},list]),
	    Req:respond({500, [], io_lib:format("~p", [_Other])})
    end.

run_forever(SocketLeft, SocketRight,Req) ->
    receive
	{tcp,SocketLeft,Data}->
	    case gen_tcp:send(SocketRight,Data) of
		ok -> run_forever(SocketLeft,SocketRight,Req);
		_Other ->
		    error_logger:error_report(_Other),
		    inet:setopts(Req:get(socket), [{packet,http},list]),
		    Req:respond({500, [], io_lib:format("~p", [_Other])})
	    end;
	{tcp,SocketRight,Data}->
	    case gen_tcp:send(SocketLeft,Data) of
		ok -> run_forever(SocketLeft,SocketRight,Req);
		_Other ->
		    error_logger:error_report(_Other),
		    inet:setopts(Req:get(socket), [{packet,http},list]),
		    Req:respond({500, [], io_lib:format("~p", [_Other])})
	    end;
	%{tcp_closed,S} ->
	    %ok = gen_tcp:close(S);
	    %ok = gen_tcp:close(SocketRight);
	_Other ->
	    error_logger:error_report(_Other),
	    inet:setopts(Req:get(socket), [{packet,http},list]),
     	    Req:respond({500, [], io_lib:format("~p", [_Other])})
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
