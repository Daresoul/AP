-module(emoji).

-import(lists, [append/2]).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1, get_state/1, perform_lookup/3]).

%-type shortcode() :: string().
%-type emoji() :: binary().
%-type analytic_fun(State) :: fun((shortcode(), State) -> State).

%%%%%%%%%
%% API %%
%%%%%%%%%

get_state(E) ->
  request_reply(E, get_state).

start(Initial) -> spawn(fun() ->
  loop(Initial, [])
                        end).

new_shortcode(E, Short, Emo) ->
  request_reply(E, {add_short_code, Short, Emo}).

alias(E, Short1, Short2) ->
  {LShort1, Bitcode1} = lookup(E, Short1), % take care of there can be errors here (maybe case of?)
  %{LShort2, Bitcode2} = lookup(E, Short2), % should return an error
  request_reply(E, {alias, Short2, Bitcode1}). % how to implement alias?

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
    {From, Response} ->
      Response
  end.

request_reply(Pid, Request) ->
  Pid ! {self(), Request},

  receive
    {Pid, Response} ->
      Response
  end.

loop(State, Aliases) ->
  receive
    {From, {add_short_code, Short, Emo}} -> % for new shortcode
      NewState = lists:append(State, [{Short, Emo}]),
      From ! {self(), ok},
      loop(NewState, Aliases);
    {From, {look_up_short, Short}} -> % for lookup
      spawn(fun() ->
        perform_lookup(From, State, Short)
      end),
      loop(State, Aliases);
    {From, {delete_short, Short}} -> % for delete
      NewState = perform_delete(State, Short, []),
      From ! {self(), ok},
      loop(NewState, Aliases);
      % go through list
      % remove all isntances of the shortcode.
    {From, get_state} ->
      From ! {self(), State},
      loop(State, Aliases)
  end.

%%%%%%%%%%%%%
%% HELPERS %%
%%%%%%%%%%%%%
perform_delete(State, _, NewState) when State == [] ->
  NewState;

perform_delete(State, Short, NewState) ->
  [Head|Tail] = State,
  {Name, Bitcode} = Head,
  if
    Name =:= Short -> perform_delete(Tail, Short, NewState);
    Name /= Short -> perform_delete(Tail, Short, lists:append([NewState, Head]))
  end.


% perform_lookup(To, State, _) when State == [] ->
%   To ! {self(), {ok, error}}; % send error instead

perform_lookup(To, State, Short) ->
  try 
    perform_lookup_inner(To, State, Short)
  catch
    _:_ -> To ! {self(), {ok, "The given emoji could not be found."}}
  end .

perform_lookup_inner(To, State, Short) -> 
    [Head|Tail] = State,
    {Name, Bitcode} = Head,
    if
      Name == Short -> To ! {self(), {ok, Bitcode}};
      Name /= Short -> perform_lookup(To, Tail, Short)
    end .

% c(emoji). E = emoji:start([]).
% emoji:new_shortcode(E, "123", "321").
% emoji:lookup(E, "123").
% emoji:alias(E, "lol", "123").
% emoji:get_state(E).
% emoji:delete(E, "123").
%