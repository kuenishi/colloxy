
% This file is auto-generated from /home/kuenishi/src/jubatus.github/src/server/recommender.idl
% *** DO NOT EDIT ***


-module(recommender_server).
-author('@msgpack-idl').

-include("recommender_types.hrl").
-behaviour(gen_msgpack_rpc_srv).

-record(state, {}).

-export([init/1, code_change/3, handle_call/3, terminate/2]). % fixed
-export([set_config/2, get_config/1, clear_row/2, update_row/3, clear/1, complete_row_from_id/2, complete_row_from_data/2, similar_row_from_id/3, similar_row_from_data/3, decode_row/2, get_all_rows/1, similarity/3, l2norm/2, save/2, load/2, get_status/1]).

init(_Argv)-> {ok, #state{}}.

code_change(_,_,State)-> % fixed
    {ok, State}.

handle_call(_,_,State)->
    {ok, State}.

terminate(_,_)->
    ok.
                            
% TODO enable recommender

-spec set_config(string(), config_data()) -> boolean().
set_config(Name, C) ->
  Reply = ok,  % write your code here
    Name, C,
  {reply, Reply}.

-spec get_config(string()) -> config_data().
get_config(Name) ->
  Reply = ok,  % write your code here
    Name,
  {reply, Reply}.

-spec clear_row(string(), string()) -> boolean().
clear_row(Name, Id) ->
  Reply = ok,  % write your code here
    Name,
  {reply, Reply}.

-spec update_row(string(), string(), datum()) -> boolean().
update_row(Name, Id, Arg2) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec clear(string()) -> boolean().
clear(Name) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec complete_row_from_id(string(), string()) -> datum().
complete_row_from_id(Name, Id) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec complete_row_from_data(string(), datum()) -> datum().
complete_row_from_data(Name, Arg1) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec similar_row_from_id(string(), string(), integer()) -> similar_result().
similar_row_from_id(Name, Id, Size) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec similar_row_from_data(string(), datum(), integer()) -> similar_result().
similar_row_from_data(Name, Data, Size) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec decode_row(string(), string()) -> datum().
decode_row(Name, Id) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec get_all_rows(string()) -> list(string()).
get_all_rows(Name) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec similarity(string(), datum(), datum()) -> float().
similarity(Name, Lhs, Rhs) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec l2norm(string(), datum()) -> float().
l2norm(Name, Arg1) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec save(string(), string()) -> boolean().
save(Name, Arg1) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec load(string(), string()) -> boolean().
load(Name, Arg1) ->
  Reply = ok,  % write your code here
  {reply, Reply}.

-spec get_status(string()) -> list({string(), list({string(), string()})}).
get_status(Name) ->
  Reply = ok,  % write your code here
  {reply, Reply}.





