-module(emoji).

-import(lists, [append/2]).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, get_state/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%%%%%%%%%
%% API %%
%%%%%%%%%

get_state(E) ->
  request_reply(E, get_state).

start(Initial) -> 
  try 
    E = spawn(fun() -> loop(Initial, []) end),
    {ok, E}
  catch 
    _:reason -> {error, reason}
  end .

new_shortcode(E, Short, Emo) ->
  try 
    case lookup(E, Short) of
      {ok, _} -> {error, 'Emoji with this short is already registered.'};
      no_emoji -> request_reply(E, {add_short_code, Short, Emo})
    end
  catch 
    error:reason -> {error, reason}
  end .


alias(E, Short1, Short2) ->
  case lookup(E, Short2) of
    no_emoji -> {error, 'No emoji with Short1.'};
    {ok, Bitcode} -> case lookup(E, Short1) of
                       no_emoji -> request_reply(E, {alias, Short1, Bitcode, Short2});
                       {ok, Bitcode} -> {error, 'Short1 already exists in the state.'}
                     end
  end.


delete(E, Short) ->
  request_reply(E, {delete_short, Short}).

lookup(E, Short) ->
  request_reply_different_server(E, {look_up_short, Short}).

analytics(E, Short, Fun, Label, Init) ->
  request_reply(E, {analytics_create, Short, Fun, Init, Label}).

get_analytics(E, Short) -> 
  request_reply(E, {analytics_return, Short}).

remove_analytics(_, _, _) -> not_implemented.

stop(E) -> 
    exit(E, kill).

%%%%%%%%%%%%
%% SERVER %%
%%%%%%%%%%%%

request_reply_different_server(Pid, Request) ->
  Pid ! {self(), Request},
  receive
    {_From, Response} ->
      Response
  end.

request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
      {Pid, Response} ->
        Response
    end .

loop(State, Analytics) ->
  receive
    {From, {add_short_code, Short, Emo}} -> % for new shortcode
      NewState = lists:append(State, [{Short, Emo}]),
      From ! {self(), ok},
      loop(NewState, Analytics);

    {From, {look_up_short, Short}} -> % for lookup
      Self = self(),
      spawn(fun() ->
        perform_lookup(Self, Analytics, From, State, Short)
      end),
      loop(State, Analytics);

    {From, {alias, NewShort, BitCode, OldShort}} -> % for alias
      NewState = lists:append(State, [{NewShort, BitCode, OldShort}]),
      From ! {self(), ok},
      loop(NewState, Analytics);

    {From, {delete_short, Short}} -> % for delete
      Self = self(),
      spawn(fun() ->
        perform_delete(Self, Short, State, [])
            end),
      From ! {self(), ok},
      loop(State, Analytics);

    {From, {analytics_create, Short, Fun, Init, Label}} ->
      NewAnalytics = lists:append(Analytics, [{Label, Fun, Init, Short}]),
      From ! {self(), ok},
      loop(State, NewAnalytics);

    {_From, {new_state, NewState}} -> % helper for delete
      loop(NewState, Analytics);

    {_From, {new_analytics_state, NewAnalytics}} ->
      loop(State, NewAnalytics);

    {From, get_state} ->
      From ! {self(), {State, Analytics}},
      loop(State, Analytics)
  end.

%%%%%%%%%%%%%
%% HELPERS %%
%%%%%%%%%%%%%

run_analytics(To, Analytics, LookForShort, NewAnalytics) ->
  try
    run_analytics_inner(To, Analytics, LookForShort, NewAnalytics)
  catch
    _:_ -> To ! {self(), {new_analytics_state, NewAnalytics}}
  end .

run_analytics_inner(To, Analytics, LookForShort, NewAnalytics) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Init, Short} = Head,
  if
    LookForShort == Short ->
      Result = Fun(Init),
      NewAnalytic = lists:append(NewAnalytics, [{Label, Fun, Result, Short}]),
      run_analytics(To, Tail, LookForShort, NewAnalytic);
    true ->
      NewAnalytic = lists:append(Analytics, [Head]),
      run_analytics(To, Tail, LookForShort, NewAnalytic)
  end.


perform_delete(From, Short, State, NewState) ->
  try
    perform_delete_inner(From, State, Short, NewState)
  catch
    _:_ -> From ! {self(), {new_state, NewState}}
  end .

perform_delete_inner(From, Short, State, NewState) ->
  [Head|Tail] = State,
  case Head of
    {Name, Bitcode, Alias} ->
      if
        Name == Short -> perform_delete(From, Short, Tail, NewState);
        Alias == Short -> perform_delete(From, Short, Tail, NewState);
        Name /= Short ->
          NewStates = lists:append(NewState, [{Name, Bitcode, Alias}]),
          perform_delete(From, Short, Tail, NewStates)
      end;
    {Name, Bitcode} ->
      if
        Name == Short ->
          perform_delete(From, Tail, Short, NewState);
        Name /= Short ->
          NewStates = lists:append(NewState, [{Name, Bitcode}]),
          perform_delete(From, Short, Tail, NewStates)
      end
  end.



%The outer function of the lookup stack. This and perform_lookup_inner will alterate
%creating Try/catch blocks on each level of the recursive calls, making sure
%That unwanted crashes does not occur, instead a controlled error message is produced.
perform_lookup(Server, Analytics, From, State, Short) ->
  try
    perform_lookup_inner(Server, Analytics, From, State, Short)
  catch
    _:_ -> From ! {self(), no_emoji}
  end .

%The inner function of the lookup stack. This and perform_lookup will alterate
%creating Try/catch blocks on each level of the recursive calls, making sure
%That unwanted crashes does not occur, instead a controlled error message is produced.
perform_lookup_inner(Server, Analytics, From, State, Short) ->
    [Head|Tail] = State,
    case Head of
      {Name, Bitcode, _Alias} ->
        if
          Name == Short ->
            run_analytics(Server, Analytics, Short, []),
            From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(Server, Analytics, From, Tail, Short)
        end;
      {Name, Bitcode} ->
        if
          Name == Short ->
            run_analytics(Server, Analytics, Short, []),
            From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(Server, Analytics, From, Tail, Short)
        end
    end.

% c(emoji). E = emoji:start([]).
% emoji:new_shortcode(E, "123", "321").
% emoji:lookup(E, "123").
% emoji:alias(E, "lol", "123").
% emoji:get_state(E).
% emoji:delete(E, "123").


% E, Short, Fun, Label, Init
% emoji:analytics(E, "123", (fun(X) -> X+1 end), "Str", 0).

% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:lookup(E, "123").
% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:alias(E, "lol", "123"). emoji:delete(E, "123").