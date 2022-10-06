-module(emoji).

-import(lists, [append/2]).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, get_state/1, perform_lookup/3]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%%%%%%%%%
%% API %%
%%%%%%%%%

get_state(E) ->
  request_reply(E, get_state).

start(Initial) -> spawn(fun() ->
  loop(Initial)
                        end).

new_shortcode(E, Short, Emo) ->
  request_reply(E, {add_short_code, Short, Emo}).

alias(E, Short1, Short2) ->
  case lookup(E, Short2) of
    no_emoji -> {error, 'No emoji with Short1.'};
    {ok, Bitcode} -> case lookup(E, Short1) of
                       no_emoji -> request_reply(E, {alias, Short1, Bitcode, Short2});
                       {ok, Bitcode} -> {error, 'Short1 already exists in the state.'}
                     end
  end.
  %{LShort2, Bitcode2} = lookup(E, Short2), % should return an error


delete(E, Short) ->
  request_reply(E, {delete_short, Short}).

lookup(E, Short) ->
  request_reply_different_server(E, {look_up_short, Short}).

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.

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
  end.

loop(State) ->
  receive
    {From, {add_short_code, Short, Emo}} -> % for new shortcode
      NewState = lists:append(State, [{Short, Emo}]),
      From ! {self(), ok},
      loop(NewState);

    {From, {look_up_short, Short}} -> % for lookup
      Self = self(),
      spawn(fun() ->
        perform_lookup(From, State, Short)
      end),
      loop(State);

    {From, {alias, NewShort, BitCode, OldShort}} -> % for alias
      NewState = lists:append(State, [{NewShort, BitCode, OldShort}]),
      From ! {self(), ok},
      loop(NewState);

    {From, {delete_short, Short}} -> % for delete
      Self = self(),
      spawn(fun() ->
        perform_delete(Self, Short, State, [])
            end),
      From ! {self(), ok},
      loop(State);

    {From, {new_state, NewState}} ->
      loop(NewState);

    {From, get_state} ->
      From ! {self(), State},
      loop(State)
  end.

%%%%%%%%%%%%%
%% HELPERS %%
%%%%%%%%%%%%%
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
          NewStates = perform_delete(From, Tail, Short, NewState);
        Name /= Short ->
          NewStates = lists:append(NewState, [{Name, Bitcode}]),
          perform_delete(From, Short, Tail, NewStates)
      end
  end.


perform_lookup(From, State, Short) ->
  try
    perform_lookup_inner(From, State, Short)
  catch
    _:_ -> From ! {self(), no_emoji}
  end .

perform_lookup_inner(From, State, Short) ->
    [Head|Tail] = State,
    case Head of
      {Name, Bitcode, _Alias} ->
        if
          Name == Short -> From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(From, Tail, Short)
        end;
      {Name, Bitcode} ->
        if
          Name == Short -> From ! {self(), {ok, Bitcode}};
          Name /= Short -> perform_lookup(From, Tail, Short)
        end
    end.

% c(emoji). E = emoji:start([]).
% emoji:new_shortcode(E, "123", "321").
% emoji:lookup(E, "123").
% emoji:alias(E, "lol", "123").
% emoji:get_state(E).
% emoji:delete(E, "123").

% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:lookup(E, "123").
% c(emoji). E = emoji:start([]). emoji:new_shortcode(E, "123", "321"). emoji:alias(E, "lol", "123"). emoji:delete(E, "123").