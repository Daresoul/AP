%%%-------------------------------------------------------------------
%%% @author tomato
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Oct 2022 19:46
%%%-------------------------------------------------------------------
-module(test).
-author("tomato").
-import(string, [len/1, concat/2]).

-export([start/0, incr/1, decr_with/2, get_value/1]).

start() -> spawn(fun () -> loop(100) end).
incr(Cid) -> request_reply(Cid, increment).
decr_with(Cid, N) -> request_reply(Cid, {decr, N}).
get_value(Cid) -> request_reply(Cid, get_value).

request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

loop(Count) ->
  receive
    {From, increment} ->
      {NewState, Res} = {Count + 1, ok}, From ! {self(), Res},
      loop(NewState);
    {From, {decr, N}} ->
      {NewState, Res} = {Count - N, ok}, From ! {self(), Res},
      loop(NewState);
    {From, get_value} ->
      {NewState, Res} = {Count, {ok, Count}}, From ! {self(), Res},
      loop(NewState)
  end.