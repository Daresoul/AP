-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1, init/1, handle_cast/2, handle_call/3]).

new(Fun, Arg) ->
  {_, Aid} = gen_server:start(?MODULE, {}, []),
  spawn_link(fun() -> worker(Aid, Fun, Arg) end),
  Aid.
wait(Aid) ->
  waiter(Aid).
poll(Aid) -> gen_server:call(Aid, poll).
wait_catch(Aid) -> nope.
wait_any(Aids) -> nope.

waiter(Aid) ->
  State = poll(Aid),
  case State of
    nothing -> waiter(Aid);
    {exception, Reason} ->
      throw(Reason);
    _ -> State
  end.

worker(Aid, Fun, Arg) ->
  try
    Res = Fun(Arg),
    gen_server:cast(Aid, {worker, {ok, Res}})
  catch
    _Class:Reason ->
      gen_server:cast(Aid, {worker, {exception, Reason}})
  end.

handle_call(Msg, Aid, State) ->
  case Msg of
    poll ->
      ReplyMsg = State,
      {reply, ReplyMsg, State}
  end.

handle_cast(Msg, _State) ->
  case Msg of
    {worker, {ok, Res}} ->
      {noreply, {ok, Res}};
    {worker, {exception, Reason}} ->
      {noreply, {exception, Reason}}
  end.

init(_Args) ->
  {ok, nothing}.

-ifdef(comment).
c(async).
Aid = async:new(fun(X) -> timer:sleep(5000) end,42).
async:wait(Aid).
-endif.