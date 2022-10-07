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
    _:_ -> {error, "The server did not start (Start it with an empty list)."}
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


alias(E, Old, New) ->
  case lookup(E, Old) of
    no_emoji -> {error, 'No emoji with Short1.'};
    {ok, Bitcode} -> case lookup(E, New) of
                       no_emoji -> request_reply(E, {alias, New, Bitcode, Old});
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
  request_reply_different_server(E, {analytics_get, Short}).

remove_analytics(E, Short, Label) ->
  request_reply_different_server(E, {analytics_remove, Short, Label}).

stop(E) -> 
  exit(E, kill),
  ok.

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

    {From, {analytics_create, Short, Fun, Init, Label}} -> % create analytics
      case analytics_label_exists_with_short(Analytics, Label, Short) of
        nothing ->
          NewAnalytics = lists:append(Analytics, [{Label, Fun, Init, Short}]),
          From ! {self(), ok},
          loop(State, NewAnalytics);
        exists ->
          From ! {self(), {error, 'Combination of Label and short already exits'}},
          loop(State, Analytics)
      end;

    {From, {analytics_get, Short}} -> % get analytics
      spawn(fun() ->
        get_analytics(From, Analytics, Short, [])
            end),
      loop(State, Analytics);

    {From, {analytics_remove, Short, Label}} -> % remove analytics
      Self = self(),
      spawn(fun() ->
        remove_analytic(Self, From, Analytics, Short, Label, [])
            end),
      loop(State, Analytics);

    {_From, {new_state, NewState}} -> % helper for delete
      loop(NewState, Analytics);

    {_From, {new_analytics_state, NewAnalytics}} -> % helper for analytics
      loop(State, NewAnalytics);

    {From, get_state} ->
      From ! {self(), {State, Analytics}},
      loop(State, Analytics)
  end.

%%%%%%%%%%%%%
%% HELPERS %%
%%%%%%%%%%%%%
remove_analytic(Self, From, Analytics, _LookingForShort, _LookingForLabel, NewAnalytics) when Analytics == [] ->
  From ! {self(), ok},
  Self ! {self(), {new_analytics_state, NewAnalytics}};

remove_analytic(Self, From, Analytics, LookingForShort, LookingForLabel, NewAnalytics) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Val, Short} = Head,
  if
    ((Label == LookingForLabel) and (Short == LookingForShort)) -> remove_analytic(Self, From, Tail, LookingForShort, LookingForLabel, NewAnalytics);
    true ->
      NewAnalytic = lists:append(NewAnalytics, [Head]),
      remove_analytic(Self, From, Tail, LookingForShort, LookingForLabel, NewAnalytic)
  end.


analytics_label_exists_with_short(Analytics, _LookingForLabel, _LookingForShort) when Analytics == [] ->
  nothing;

analytics_label_exists_with_short(Analytics, LookingForLabel, LookingForShort) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Val, Short} = Head,
  if
    ((Label == LookingForLabel) and (Short == LookingForShort)) -> exists;
    true -> analytics_label_exists_with_short(Tail, LookingForLabel, LookingForShort)
  end.


get_analytics(From, Analytics, LookingForShort, Result) ->
  try
    get_analytics_inner(From, Analytics, LookingForShort, Result)
  catch
    _:_ -> From ! {self(), {ok, Result}}
  end .

get_analytics_inner(From, Analytics, LookingForShort, Result) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Val, Short} = Head,
  if
    LookingForShort == Short ->
      Results = lists:append(Result, [{Label, Val}]),
      get_analytics(From, Tail, LookingForShort, Results);
    true ->
      get_analytics(From, Tail, LookingForShort, Result)
  end.

run_analytics(To, Analytics, LookForShort, NewAnalytics) when Analytics == [] ->
  To ! {self(), {new_analytics_state, NewAnalytics}};

run_analytics(To, Analytics, LookForShort, NewAnalytics) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Init, Short} = Head,
  if
    LookForShort == Short ->
      try Result = Fun(Init),
      NewAnalytic = lists:append(NewAnalytics, [{Label, Fun, Result, Short}]),
      run_analytics(To, Tail, LookForShort, NewAnalytic)
      catch
        _:Reason -> {error, Reason}
      end;
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
            spawn(fun() ->
            run_analytics(Server, Analytics, Short, [])
                  end),
            From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(Server, Analytics, From, Tail, Short)
        end;
      {Name, Bitcode} ->
        if
          Name == Short ->
            spawn(fun() ->
              run_analytics(Server, Analytics, Short, [])
                  end),
            From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(Server, Analytics, From, Tail, Short)
        end
    end.

% c(emoji). {ok, E} = emoji:start([]).
% emoji:new_shortcode(E, "123", "321").
% emoji:lookup(E, "123").
% emoji:alias(E, "lol", "123").
% emoji:get_state(E).
% emoji:delete(E, "123").


% E, Short, Fun, Label, Init
% emoji:analytics(E, "123", (fun(X) -> X+1 end), "Str", 0).
% Hit = fun(_, N) -> N+1 end.
% emoji:analytics(E, "123", Hit, "Str", 0).
% Last = fun (S, _) -> S end.
% emoji:analytics(E, "123", Last, "Str", 0).
%
% emoji:get_analytics(E, "123").
% emoji:remove_analytics(E, "123", "Str").

% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:lookup(E, "123").
% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:alias(E, "lol", "123"). emoji:delete(E, "123").