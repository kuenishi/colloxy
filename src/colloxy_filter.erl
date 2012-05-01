%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2012, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2012 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(colloxy_filter).
-author('@kuenishi').

-export([init/0, check/2, white/2, black/2, list/0]).

init()->
    % create local whitelist for IPs, domains, URLs
    whitelist = ets:new(whitelist, [named_table, public, set]),
    
    % create local blacklist for IPs, domains, URLs
    blacklist = ets:new(blacklist, [named_table, public, set]),

    ok.

in_list(Name, Host, Path)->
    case ets:lookup(Name, Host) of
	[]->
	    case ets:lookup(Name, {Host, Path}) of
		[] ->                false;
		L when is_list(L) -> true;
		_ -> error
	    end;
	L when is_list(L) -> true;
	_ -> error
    end.

delete(Name, Host, Path)->
    ets:delete(Name, Host),
    ets:delete(Name, {Host,Path}).

check(Host, Path)->
    case {in_list(whitelist, Host, Path),
	  in_list(blacklist, Host, Path)} of
	{true, false} -> true;
	{false, true} -> false;
	{true,  true} -> % sth wrong
	    delete(whitelist, Host, Path),
	    delete(blacklist, Host, Path);
	_ -> % ask to server
	    unknown
    end.

white(Host, Path)->
    ets:insert_new(whitelist, Host, none),
    ets:insert_new(whitelist, {Host, Path}, none).

black(Host, Path)->
    ets:insert_new(blacklist, Host, none),
    ets:insert_new(blacklist, {Host, Path}, none).

list()->
    PrintFun = fun(Elem, Acc)->
		       io:format("~p~n", [Elem]), Acc
	       end,
    io:format("whitelist:~n"),
    ets:foldl(PrintFun, none, whitelist),
    io:format("blacklist:~n"),
    ets:foldl(PrintFun, none, blacklist),
    ok.
