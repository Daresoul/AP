-module(emoji).

-import(lists, [append/2]).
-import(maps, [map/2]).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, get_state/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%%%%%%%%%
%% API %%
%%%%%%%%%

%returns the current state of the process (server) with the pid: E, by requesting
%the state from the server loop.
get_state(E) ->
  request_reply(E, get_state).

% Checks if either Shortcode or Bitcode already exists in the list.
exists_in_list(List, SearchShortcode, SearchBitcode) ->
    lists:foreach(fun(X) ->
      {Shortcode, Bitcode} = X,
      if
        (SearchShortcode == Shortcode) or (Bitcode == SearchBitcode) -> throw('Bitcode or Shortcode already exits.');
        true -> ok
      end
                end, List).

% Checks a list for correct formatting, as well as, checks if it they contain themselves.
check_if_exists_in_initial_list(Initial, CurrentList) when Initial == [] ->
  ok;

check_if_exists_in_initial_list(Initial, CurrentList) ->
    [Head | Tail] = Initial,
    case Head of
      {Shortcode, Bitcode} ->
        exists_in_list(CurrentList, Shortcode, Bitcode),
        NewCurrentList = lists:append(CurrentList, [{Shortcode, Bitcode}]),
        check_if_exists_in_initial_list(Tail, NewCurrentList)
    end.


%starts a new process (server) with the state Initial. e.g. calling it with a list
%of pairs (2-tuples) or f.x. analytical functions will create an environment (state)
%where these specific emojies or analytical functions are accessable through other commands.
start(Initial) -> 
  try
    check_if_exists_in_initial_list(Initial, []),
    E = spawn(fun() -> loop(Initial, []) end),
    {ok, E}
  catch 
    _:Reason -> {error, Reason}
  end .

%Main function in the new shortcode function stack.
%Creates a new shortcode:Short for the emoji:Emo and stores the new
%projection in the E state (the current processs state) for later availability/use
new_shortcode(E, Short, Emo) ->
  try 
    case lookup(E, Short) of
      {ok, _} -> {error, 'Emoji with this short is already registered.'};
      no_emoji -> request_reply(E, {add_short_code, Short, Emo})
    end
  catch 
    error:reason -> {error, reason}
  end .

%Main function in the alias function stack
%Creates an alias:New which refers to the object:Old. Cannot be called on
%other aliases, and cannot be called on an object:Old which does not exist
alias(E, Old, New) ->
  case lookup(E, Old) of
    no_emoji -> {error, 'No emoji with Short1.'};
    {ok, Bitcode} -> case lookup(E, New) of
                       no_emoji -> request_reply(E, {alias, New, Bitcode, Old});
                       {ok, Bitcode} -> {error, 'Short1 already exists in the state.'}
                     end
  end.

%Main function in the delete function stack.
%Deletes a shortcode:Short from the state:E, will simply try to delete, and
%if no object is found for deletion, will simply pretend it deleted it anyway
%since it should not affect the result.
delete(E, Short) ->
  request_reply(E, {delete_short, Short}).

%Main function in the lookup function stack.
%Looks up a shortcode:Short in the state:E, will return a no_emoji if not found
%to enable graceful handling of errors within process.
lookup(E, Short) ->
  request_reply_different_server(E, {look_up_short, Short}).

%main function in the analytics function stack
%The user defines a function:Fun, gives it a name:Label, an
%initial accumulator:Init, and which emoji it will be working on:Short.
%And the current state:E. The function will then be created and ran
%each time an emoji is looked up (e.g. used).
analytics(E, Short, Fun, Label, Init) ->
  request_reply(E, {analytics_create, Short, Fun, Init, Label}).

%Returns the analytics data for a specific Shortcode:Short in the state:E
get_analytics(E, Short) ->
  request_reply_different_server(E, {analytics_get, Short}).

%Removes analytics function for shortcode:Short, with the function name:Label
%in the state:E. all data associated with the analytics function will also be removed
%one cannot get the data back after deletion.
remove_analytics(E, Short, Label) ->
  request_reply_different_server(E, {analytics_remove, Short, Label}).

%stops the current server, by using the exit/2 kill, it is somehow forceful
%and regardless of how the server is setup, termination should occur.
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

%Auxilliary function for requesting replies from the main server loop with
%Pid:Pid and with the request:Request
request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
      {Pid, Response} ->
        Response
    end .

%The main server loop with the state:State and with the argument analytics
%being an implementation chocie that holds the analytical functions of that
%specific state.
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

    {_From, {error, Reason}} ->
      io:fwrite("~w\n", [Reason]),
      loop(State, Analytics);

    {From, get_state} ->
      From ! {self(), {State, Analytics}},
      loop(State, Analytics)
  end.

%%%%%%%%%%%%%
%% HELPERS %%
%%%%%%%%%%%%%

%Auxilliary function for the remove analytic function stack
%Is a helper which checks for the list being empty as a
%recursive basecase match
remove_analytic(Self, From, Analytics, _LookingForShort, _LookingForLabel, NewAnalytics) when Analytics == [] ->
  From ! {self(), ok},
  Self ! {self(), {new_analytics_state, NewAnalytics}};

%The main remove_analytic function in the remove_analytic function stack.
%calls itself recursively while looking for the short and label to remove
%always taking the head of the list:Analytics and calling itself with the tail in
%search of the specific analytic to remove.
remove_analytic(Self, From, Analytics, LookingForShort, LookingForLabel, NewAnalytics) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Val, Short} = Head,
  if
    ((Label == LookingForLabel) and (Short == LookingForShort)) -> remove_analytic(Self, From, Tail, LookingForShort, LookingForLabel, NewAnalytics);
    true ->
      NewAnalytic = lists:append(NewAnalytics, [Head]),
      remove_analytic(Self, From, Tail, LookingForShort, LookingForLabel, NewAnalytic)
  end.

%Auxilliary function for the analytics_label_exists function stack.
%Will simply return nothing when the list:Analytics is empty, e.g the recursive
%basecase when the label:_LookingForLabel is not found for the short:_LookingForShort
analytics_label_exists_with_short(Analytics, _LookingForLabel, _LookingForShort) when Analytics == [] ->
  nothing;

%Checks if an analytical label:LookingForLabel already exists for a given shortcode:LookingForShort
%in the list:Analytics. Calls itself recursively with the [head|tail] structure to run through
%the list:Analytics. the "if true-> case" is simply a smart way to make an if-else statement.
analytics_label_exists_with_short(Analytics, LookingForLabel, LookingForShort) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Val, Short} = Head,
  if
    ((Label == LookingForLabel) and (Short == LookingForShort)) -> exists;
    true -> analytics_label_exists_with_short(Tail, LookingForLabel, LookingForShort)
  end.

%base recursive case of the get_analytics main function stack.
%simply checks for the empty list and then returns results.
get_analytics(From, Analytics, _LookingForShort, Result) when Analytics == [] ->
  From ! {self(), {ok, Result}};

%outer function for the get_analytics function stack main function.
%Alternates between get_analytic_inner and itself to get try catch structure
%on each level of the recursion. 
get_analytics(From, Analytics, LookingForShort, Result) ->
  try
    get_analytics_inner(From, Analytics, LookingForShort, Result)
  catch
    _:Reason -> From ! {self(), {error, Reason}}
  end .

%Inner function fore the get_analytics function stack main function.
%Alternates between get_analytic_inner and itself to get try catch structure
%uses recursive calls with the [head|tail] technique to pull out and match 
%shortcode:LookingForShort against the list:Analytics
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

%Auxilliary function which is the recursive basecase of run_analytics.
%is catching the case where the list is empty, and the recursion therefore
%has to stop.
run_analytics(To, Analytics, LookForShort, NewAnalytics) when Analytics == [] ->
  To ! {self(), {new_analytics_state, NewAnalytics}};

%The function that internally is applied to each lookup when a specific analytics 
%Function has been applied to a shortcode. Uses the [head|tail] structure
%to run through all the analytic functions currecntly active:Analytics
%and applies the given shortcode:LookForShort to them if found.
%returns the newAnalytics which holds updated accumulator
run_analytics(To, Analytics, LookForShort, NewAnalytics) ->
  [Head|Tail] = Analytics,
  {Label, Fun, Init, Short} = Head,
  if
    LookForShort == Short ->
      try
        Result = Fun(Init),
        NewAnalytic = lists:append(NewAnalytics, [{Label, Fun, Result, Short}]),
        run_analytics(To, Tail, LookForShort, NewAnalytic)
      catch
        _:Reason -> To ! {self(), {error, Reason}},
          {error, Reason}
      end;
    true ->
      NewAnalytic = lists:append(Analytics, [Head]),
      run_analytics(To, Tail, LookForShort, NewAnalytic)
  end.

%The outer function of the delete function stack. It deletes
%A short:Short from the state:State and returns a newstate:NewState
%If none is found, it should fail gracefully as if nothing happened
%as not deleting something that is not there does not change the outcome.
perform_delete(From, Short, State, NewState) ->
  try
    perform_delete_inner(From, State, Short, NewState)
  catch
    _:_ -> From ! {self(), {new_state, NewState}}
  end .

%Inner function being alternated between to apply the try catch on each level.
%We wish to apply the try catch to catch unforeseen consequences, regardless of
%deletes gracefully failing.
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