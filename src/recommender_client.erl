

-module(recommender_client).
-author('@msgpack-idl').

-include("recommender_types.hrl").
-include_lib("erl_msgpack/include/msgpack_rpc.hrl"). % fixed

% TODO: enable services  recommender
-export([set_config/3, get_config/2, clear_row/3, update_row/4, clear/2, complete_row_from_id/3, complete_row_from_data/3, similar_row_from_id/4, similar_row_from_data/4, decode_row/3, get_all_rows/2, similarity/4, l2norm/3, save/3, load/3, get_status/2]).
-export([connect/2, close/1]). %fixed

-spec set_config(mprc:mprc(), string(), config_data()) -> {boolean(), mprc:mprc()}|{error, term()}.
set_config(MPRC, Name, C) ->
   mprc:call(MPRC, Name, C).

-spec get_config(mprc:mprc(), string()) -> {config_data(), mprc:mprc()}|{error, term()}.
get_config(MPRC, Name) ->
   mprc:call(MPRC, Name).

-spec clear_row(mprc:mprc(), string(), string()) -> {boolean(), mprc:mprc()}|{error, term()}.
clear_row(MPRC, Name, Id) ->
   mprc:call(MPRC, Name, Id).

-spec update_row(mprc:mprc(), string(), string(), datum()) -> {boolean(), mprc:mprc()}|{error, term()}.
update_row(MPRC, Name, Id, Arg2) ->
   mprc:call(MPRC, Name, Id, Arg2).

-spec clear(mprc:mprc(), string()) -> {boolean(), mprc:mprc()}|{error, term()}.
clear(MPRC, Name) ->
   mprc:call(MPRC, Name).

-spec complete_row_from_id(mprc:mprc(), string(), string()) -> {datum(), mprc:mprc()}|{error, term()}.
complete_row_from_id(MPRC, Name, Id) ->
   mprc:call(MPRC, Name, Id).

-spec complete_row_from_data(mprc:mprc(), string(), datum()) -> {datum(), mprc:mprc()}|{error, term()}.
complete_row_from_data(MPRC, Name, Arg1) ->
   mprc:call(MPRC, Name, Arg1).

-spec similar_row_from_id(mprc:mprc(), string(), string(), integer()) -> {similar_result(), mprc:mprc()}|{error, term()}.
similar_row_from_id(MPRC, Name, Id, Size) ->
   mprc:call(MPRC, Name, Id, Size).

-spec similar_row_from_data(mprc:mprc(), string(), datum(), integer()) -> {similar_result(), mprc:mprc()}|{error, term()}.
similar_row_from_data(MPRC, Name, Data, Size) ->
   mprc:call(MPRC, Name, Data, Size).

-spec decode_row(mprc:mprc(), string(), string()) -> {datum(), mprc:mprc()}|{error, term()}.
decode_row(MPRC, Name, Id) ->
   mprc:call(MPRC, Name, Id).

-spec get_all_rows(mprc:mprc(), string()) -> {list(string()), mprc:mprc()}|{error, term()}.
get_all_rows(MPRC, Name) ->
   mprc:call(MPRC, Name).

-spec similarity(mprc:mprc(), string(), datum(), datum()) -> {float(), mprc:mprc()}|{error, term()}.
similarity(MPRC, Name, Lhs, Rhs) ->
   mprc:call(MPRC, Name, Lhs, Rhs).

-spec l2norm(mprc:mprc(), string(), datum()) -> {float(), mprc:mprc()}|{error, term()}.
l2norm(MPRC, Name, Arg1) ->
   mprc:call(MPRC, Name, Arg1).

-spec save(mprc:mprc(), string(), string()) -> {boolean(), mprc:mprc()}|{error, term()}.
save(MPRC, Name, Arg1) ->
   mprc:call(MPRC, Name, Arg1).

-spec load(mprc:mprc(), string(), string()) -> {boolean(), mprc:mprc()}|{error, term()}.
load(MPRC, Name, Arg1) ->
   mprc:call(MPRC, Name, Arg1).

-spec get_status(mprc:mprc(), string()) -> {list({string(), list({string(), string()})}), mprc:mprc()}|{error, term()}.
get_status(MPRC, Name) ->
   mprc:call(MPRC, Name).


-spec connect(gen_tcp:ip_address(), inet:port_number())->{ok, mprc:mprc()}.
connect(Addr, Port)->
  mprc:start(),
  mprc:connect(Addr, Port, []).

-spec close(mprc:mprc())->ok.
close(MPRC)->
  mprc:close(MPRC).
