-module(async).
-export([new/2, wait/1, poll/1]).

loop(State) -> 
    receive
        {_From, {fx, Fun, Arg}} -> 
            Result = Fun (Arg),
            loop({ok, Result});
        {From, poll} -> 
            From ! {self(), State},
            loop(State);
        {From, wait} ->
            if 
                State =:= {} ->
                    From ! {notdone};
                true ->
                    From ! {succes, State}
            end,
            loop(State)
    end . 

new(Fun, Arg) -> 
    Aid = spawn(fun() -> loop(nothing) end),
    Aid ! {self(), {fx, Fun, Arg}},
    Aid .

wait(Aid) -> 
    Aid ! {self(), wait},
    receive
        {succes, State} ->
            State;
        {notdone} ->
            wait(Aid)
    end . 

poll(Aid) -> 
    Aid ! {self(), poll},
    receive
        {_From, {Message, State}} ->
            {Message, State}
    end .