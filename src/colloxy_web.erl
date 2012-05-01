%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for colloxy.

-module(colloxy_web).
-author("Mochi Media <dev@mochimedia.com>").


-include_lib("eunit/include/eunit.hrl").

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
    try
	error_logger:info_report("new connection!"),
	case Req:get(path) of

	    {scheme, Host, Port0} ->
		
		% TODO: do some filter logic here.

		Req:ok({200, <<"">>}),
		Port = list_to_integer(Port0),
		{ok, Socket} = gen_tcp:connect(Host, Port, [binary,{packet,raw},{active,true}]),
		ok = inet:setopts(Req:get(socket), [{packet,raw},binary,{active,true}]),
		colloxy_util:tcp_pipe(Socket, Req:get(socket));
		
	    _ ->
		
		% TODO: do some filter logic here.

		{ok, RequestBin} = make_request_binary(Req),
		ok = inet:setopts(Req:get(socket), [{packet,raw},binary,{active,false}]),
						% Scheme = Req:get(scheme), % http | https
		{ok, Socket} = make_socket(Req:get_primary_header_value("host")),
		case do_http_req(Socket, RequestBin, Req:get(socket)) of
		    keepalive ->
			my_loop(Req:get(socket), <<>>);
		    _ ->
			exit(normal)
		end
	end

    catch
	exit:normal ->
	    Req:cleanup();

        Type:What ->
            Report = ["web request failed",
                      {path, Req:get(path)},
		      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),

            Req:respond({500, [{"Content-Type", "text/plain"}], "request failed, sorry\n"})
    end.

%% Internal API

make_socket(HostPort) when is_binary(HostPort)->
    make_socket(binary_to_list(HostPort));

make_socket(HostPort)->

    {Host,Port}=case string:tokens(HostPort, ":") of
		    [Host0] -> {Host0, 80};
		    [Host0,Port0,_] -> {Host0, list_to_integer(Port0)}
		end,
    gen_tcp:connect(Host, Port, [{packet,raw},binary,{active,false}]).

make_request_binary(Req)->

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
    % io:format("sending ~w bytes ..\n", [ byte_size(ReqBin) ]),
    {ok, ReqBin}.
    


do_http_req(Socket, RequestBin, ReturnSocket)->
    
    io:format("new request!! ===============  (~p) ~n~s~n", [self(), binary_to_list(RequestBin)]),

    case gen_tcp:send(Socket, RequestBin) of
	ok ->
	    {ok, Rest0, Total0, _Headers0} = get_first_line(Socket, <<>>),
	    {ok, Rest1, Total1, Headers1} = get_header(Socket, Rest0, Total0, []),

	    % send back the header
	    HeaderSize = byte_size(Total1) - byte_size(Rest1),
	    <<HeaderBin:HeaderSize/binary, Rest1/binary>> = Total1,

	    %error_logger:info_report("got response header."),
	    io:format(">>> got response:~n~s", [binary_to_list(HeaderBin)]),
	    io:format("<<~w bytes>>~n", [byte_size(Rest1)]),

	    ok = gen_tcp:send(ReturnSocket, HeaderBin),

	    case get_content_length(Headers1) of
		{ok, ContentLength}->
		    transfer_all(Socket, ReturnSocket, ContentLength, Rest1);
		_ -> ok
	    end,

	    case get_chunked(Headers1) of
		chunked ->
		    process_chunked(Socket, ReturnSocket, Rest1);
		_ -> ok
	    end,

	    error_logger:info_report("===========================http req-res done (~p).",
				     [self()]),

	    case get_connection(Headers1) of
		keepalive -> keepalive;
		close ->     gen_tcp:close(Socket),
			     close
	    end;
	    
	_Other ->
	    error_logger:error_report(_Other)

    end.

process_chunked(Socket, ReturnSocket, Binary0)->
    %error_logger:info_report({"got response header for chunked data.", binary_to_list(Binary0)}),
    case get_size_from_binary(0,0, Binary0) of
	{error, short} ->
	    {ok, Binary} = gen_tcp:recv(Socket, 0),
	    process_chunked(Socket, ReturnSocket, <<Binary0/binary, Binary/binary>>);

	{_, 0} ->
	    error_logger:info_report("all chunks have been sent."),
	    ok = gen_tcp:send(ReturnSocket, Binary0);

	{StringSize, ZipSize} when byte_size(Binary0) < StringSize + ZipSize + 2->
	    {ok, Binary} = gen_tcp:recv(Socket, 0),
	    process_chunked(Socket, ReturnSocket, <<Binary0/binary, Binary/binary>>);

	{StringSize, ZipSize} ->
	    TrimSize = StringSize+ZipSize,
	    <<Binary:TrimSize/binary, 13, 10, Rest0/binary>> = Binary0,
	    ok = gen_tcp:send(ReturnSocket, Binary),

	    process_chunked(Socket, ReturnSocket, Rest0)
    end.


get_first_line(Socket, Binary)->
    ok = inet:setopts(Socket, [{active,false}]),

    case erlang:decode_packet(http_bin, Binary, []) of
	{ok, H, Rest} ->
	    {ok, Rest, Binary, [H]};
	{more, undefined} ->
	    case gen_tcp:recv(Socket, 0) of
		{ok, BinPacket} ->
		    get_first_line(Socket, <<Binary/binary, BinPacket/binary>>);
		Error ->
		    Error
	    end;
	{more, Length} ->
	    case gen_tcp:recv(Socket, Length) of
		{ok, BinPacket} ->
		    get_first_line(Socket, <<Binary/binary, BinPacket/binary>>);
		Error ->
		    Error
	    end;
	Error -> Error
    end.

get_header(Socket, Binary, Total, Headers)->

    case erlang:decode_packet(httph_bin, Binary, []) of
	{ok, http_eoh, Rest} -> {ok, Rest, Total, Headers};
	{ok, Elem, Rest} -> get_header(Socket, Rest, Total, [Elem|Headers]);
	{more, undefined} ->
	    {ok, Bin} = gen_tcp:recv(Socket, 0),
	    get_header(Socket,
		       <<Binary/binary, Bin/binary>>,
		       <<Total/binary, Bin/binary>>,
		       Headers);
	{more, Length} ->
	    {ok, Bin} = gen_tcp:recv(Socket, Length),
	    get_header(Socket,
		       <<Binary/binary, Bin/binary>>,
		       <<Total/binary, Bin/binary>>,
		       Headers);
	Error ->
	    error_logger:error_report(Error),
	    Error
    end.


transfer_all(_, _, 0, <<>>)->    ok;
transfer_all(_, _, Length, <<>>) when Length < 0 ->    ok;

transfer_all(Socket, ReturnSocket, Length, <<>>) when Length > 0 ->
    case gen_tcp:recv(Socket, Length) of
	{ok, BinPacket} ->
	    transfer_all(Socket, ReturnSocket, Length, BinPacket);
	{error, closed} -> ok;
	_Other ->
	    error_logger:error_report(_Other),
	    _Other
    end;
transfer_all(Socket, ReturnSocket, Length, Binary) ->
    ok = gen_tcp:send(ReturnSocket, Binary),
    transfer_all(Socket, ReturnSocket, Length - byte_size(Binary), <<>>).


my_loop(ReturnSocket, RestBin0)->

    case get_first_line(ReturnSocket, RestBin0) of
	{error, closed} -> exit(normal);
	{ok, Rest0, Total0, _Headers0} ->
	    {ok, _Rest1, Total1, Headers1} = get_header(ReturnSocket, Rest0, Total0, []),

            % TODO: do some filter logic here.

	    {ok, HostPort} = get_host(Headers1),
	    {ok, Socket} = make_socket(HostPort),
	    
	    case do_http_req(Socket, Total1, ReturnSocket) of
		close -> close;
		_ -> my_loop(ReturnSocket, <<>>)
	    end
    end.


% TODO: following functions are easy to write its tests

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

hex2int(H) when H < $a -> H - $0;
hex2int($a) -> 16#a;
hex2int($b) -> 16#b;
hex2int($c) -> 16#c;
hex2int($d) -> 16#d;
hex2int($e) -> 16#e;
hex2int($f) -> 16#f.

get_size_from_binary(_, _, <<>>) -> {error, short};
get_size_from_binary(D, I, <<13,10,_/binary>>)->  {D+2, I};
get_size_from_binary(D, I, <<J/unsigned-integer, Rest/binary>>)->
    get_size_from_binary(D+1, I*16+hex2int(J), Rest);
get_size_from_binary(_, _, _) -> error.

get_chunked([]) -> none;
get_chunked([{http_header,_,'Transfer-Encoding',_,<<"chunked">>}|_]) -> chunked;
get_chunked([_|L]) -> get_chunked(L).

get_connection([]) -> close;
get_connection([{http_header,_,'Connection',_,<<"close">>}|_]) -> close;
get_connection([{http_header,_,'Connection',_,<<"Keep-Alive">>}|_]) -> keepalive;
get_connection([_|L]) -> get_connection(L).

get_content_length([])-> {error, not_found};
get_content_length([{http_header,_,'Content-Length',_,Value}|_])->
    {ok, list_to_integer(binary_to_list(Value))};
get_content_length([_|Headers]) ->
    get_content_length(Headers).

get_host([])-> {error,not_found};
get_host([{http_header,_,'Host',_,Value}|_]) -> {ok,Value};
get_host([_|L]) ->  get_host(L).    

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(0, 0),
%       "No, but I will!",
%       "Have you written any tests?"),
    ok.

-endif.
