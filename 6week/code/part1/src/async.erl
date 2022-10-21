-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1, init/1, handle_cast/2, handle_call/3]).

new(Fun, Arg) ->
  {_, Aid} = gen_server:start(?MODULE, {}, []),
  spawn_link(fun() -> worker(Aid, Fun, Arg) end),
  Aid.
wait(Aid) ->
  waiter(Aid).
poll(Aid) -> gen_server:call(Aid, poll).
wait_catch(Aid) ->
  try
    waiter(Aid)
  catch
      _:Reason  -> {exception, Reason}
  end.

wait_any(Aids) ->
  Self = self(),
  recursive(Aids, Self),

  receive
    {ok, State} -> {ok, State};
    A -> A
  end.

recursive(Aids, _Self) when Aids == [] ->
  [];

recursive(Aids, Self) ->
  [Head|Tail] = Aids,
  gen_server:call(Head, {wait_any, Self}),
  recursive(Tail, Self).

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
      {reply, ReplyMsg, State};
    {wait_any, Aid2} ->
      spawn_link(fun() ->
        State3 = waiter(Aid2),
        gen_server:reply(Aid2, {ok,State, State3})
                 end),
      {reply, ok, State}
  end.

handle_cast(Msg, State) ->
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
Aid = async:new(fun(X) -> timer:sleep(X), 5 end,1000).
async:wait_any([Aid]).
async:wait(Aid).
async:poll(Aid).
-endif.