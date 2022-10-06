-module(async).

-export([new/2, wait/1, poll/1, executor/3, waiter/2, loop/1]).

-type args() :: any().
-type result() :: any().

%%%%%%%%%
%% API %%
%%%%%%%%%
-spec new(fun(), args()) -> pid().
new(Fun, Args) ->
    Aid = spawn(async, loop, [{ok, nothing}]),
    Aid ! {new, Fun, Args},
    Aid.

-spec wait(pid()) -> result() | no_return().
wait(Aid) ->
    Aid ! {wait, self()},
    receive
        {exception, Reason} ->
            throw(Reason);
        {ok, Result} ->
            Result
    end.

-spec poll(pid()) -> nothing | {exception, _} | {ok, result()}.
poll(Aid) ->
    Aid ! {poll, self()},
    receive
        {ok, nothing} ->
            nothing;
        State ->
            State
    end.

%%%%%%%%%%%%
%% SERVER %%
%%%%%%%%%%%%
-spec executor(fun(), args(), pid()) -> pid().
executor(Fun, Args, Pid) ->
             State =
                 try
                     {ok, Fun(Args)}
                 catch
                     _Exception:Reason -> {exception, Reason}
                 end,
             Pid ! {setState, State}.

-spec waiter(pid(), pid()) -> {exception, _} | {ok, result()}.
waiter(Pid, From) ->
    Pid ! {poll, self()},
    receive
        {ok, nothing} ->
            waiter(Pid, From);
        State ->
            From ! State
    end.

-spec loop({exception, _} | {ok, result()} | {ok, nothing}) -> no_return().
loop(State) ->
    receive
        {new, Fun, Args} ->
            spawn(async, executor, [Fun, Args, self()]),
            loop(State);
        {wait, From} ->
            spawn(async, waiter, [self(), From]),
            loop(State);
        {poll, From} ->
            From ! State,
            loop(State);
        {setState, NewState} ->
            loop(NewState)
    end.
