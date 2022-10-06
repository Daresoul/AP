-module(async).

-export([new/2, wait/1, poll/1]).

-import(string, [len/1, concat/2]).

new(Fun, Arg) ->
  spawn(
    fun() ->
      Fun(Arg),
      loop(val, beginning)
    end).
wait(Aid) -> request_reply(Aid, {value,done}).
poll(Aid) -> nope.

request_reply(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {Pid, Response} -> Response
  end.

loop(Value, State) ->
  receive
    {From, {Fun, Arg}} ->
      Value = Fun(Arg)
  end.