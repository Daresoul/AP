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
      From ! {self(), {update_state, ok,Result}}
  catch
    Ex:_ ->
      From ! {self(), {update_state, error, Ex}}
  end.
wait_for_executed(From, Aid) ->
  case poll(Aid) of
    {ok, nothing} ->
      io:fwrite('Rerunning\n'),
      wait_for_executed(From, Aid);
    {ok, Res} -> From ! {self(), {ok, Res}};
    {error, Reason} -> From ! {self(), {error, Reason}}
  end.

% c(async). N = 3. E = async:new(fun(X) -> throw(X+1) end, N). async:poll(E). async:wait(E).

new(Fun, Arg) -> 
    Aid = spawn(fun() -> loop({ok, nothing}) end),
    Aid ! {self(), {fx, Fun, Arg}},
    Aid .

wait(Aid) -> 
    Aid ! {self(), {wait, Aid}},

    receive
      {_From, {ok, Res}} ->
        Res;
      {_From, {error, Reason}} ->
        {error, Reason}
    end.


poll(Aid) -> 
    Aid ! {self(), poll},
    receive
        {_From, {Message, State}} ->
            {Message, State}
    end .