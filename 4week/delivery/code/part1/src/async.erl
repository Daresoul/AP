-module(async).
-export([new/2, wait/1, poll/1]).

loop(State) -> 
    receive
        {_From, {fx, Fun, Arg}} ->
            Self = self(),
            spawn(fun() ->
              run_fx(Self, Fun, Arg)
                  end),
            loop(State);
        {_From, {update_state, Type, Res}} ->
            loop({Type, Res});
        {From, poll} -> 
            From ! {self(), State},
            loop(State);
        {From, {wait, Aid}} ->
            spawn(fun() ->
              wait_for_executed(From, Aid)
                  end),
            loop(State)
    end .

run_fx(From, Fun, Arg) ->
  try
      Result = Fun (Arg),
      From ! {self(), {update_state, ok, Result}}
  catch
    _:Reason ->
      From ! {self(), {update_state, exception, Reason}}
  end.


wait_for_executed(From, Aid) ->
  case poll(Aid) of
    nothing ->
      wait_for_executed(From, Aid);
    {ok, Res} -> From ! {self(), {ok, Res}};
    {exception, Reason} -> From ! {self(), {exception, Reason}}
  end.

new(Fun, Arg) -> 
    Aid = spawn(fun() -> loop({ok, nothing}) end),
    Aid ! {self(), {fx, Fun, Arg}},
    Aid .

wait(Aid) -> 
    Aid ! {self(), {wait, Aid}},

    receive
      {_From, {ok, Res}} ->
        Res;
      {_From, {exception, Reason}} ->
        {exception, Reason}
    end.


poll(Aid) -> 
    Aid ! {self(), poll},
    receive
        {_From, State} ->
          case State of
            {ok, nothing} -> nothing;
            {ok, Res} -> {ok, Res};
            {exception, Reason} -> {exception, Reason}
          end
    end .