-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).

%%% A non-symbolic generator for bst, parameterised by key and value generators
bst2(Key, Value) ->
  ?LET(KVS, eqc_gen:list({Key, Value}), % Binds a list pair to the variable kvs
    lists:foldl(fun({K,V}, T) -> insert(K, V, T) end,   % Call anonymous function with key value pair and T
                                                        % Then insert KV into T, with a starting tree empty
                                                        % and our ealier KVS list as each pair.
      empty(),
      KVS)).

bst(Key, Value) ->
  ?LAZY(
    oneof([
      ?LET(KVS, eqc_gen:list({Key, Value}), % Binds a list pair to the variable kvs
      {call, lists, foldl,
        [
          fun({K,V}, T) -> insert(K, V, T) end,
          empty(),
          KVS
        ]
      }
    )
    ])
  ).

% example key and value generators
int_key() -> eqc_gen:int().
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h]).

int_value() -> eqc_gen:int().


%%% invariant properties

% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
            valid(eqc_symbolic:eval(T))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            valid (insert(K, V, eqc_symbolic:eval(T)))).

% if we insert into a valid tree it stays valid
prop_empty_valid() ->
  ?FORALL(T, empty(), valid(eqc_symbolic:eval(T))).

prop_delete_valid() ->
  ?FORALL({K, V, T}, {atom_key(), int_value(), empty()},
    valid(delete(K, insert(K, V, eqc_symbolic:eval(T))))
  ).

prop_delete_valid2() ->
  ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
    valid(delete(K, insert(K, V, eqc_symbolic:eval(T))))
  ).

prop_union_valid() ->
  ?FORALL({G, T}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
    valid(union(eqc_symbolic:eval(G), eqc_symbolic:eval(T)))
  ).

% eqc:quickcheck(test_bst:prop_deletee_valid()).

%%% -- postcondition properties

prop_insert_post() ->
    ?FORALL({K1, K2, V, T},
            {atom_key(), atom_key(), int_value(), bst(atom_key(), int_value())},
            eqc:equals(find(K2, insert(K1, V, eqc_symbolic:eval(T))),
                       case K1 =:= K2 of
                           true ->  {found, V};
                           false -> find(K2, eqc_symbolic:eval(T))
                       end)).


prop_find_post_present() ->
  % ∀ k v t. find k (insert k v t) === {found, v}
  ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
    eqc:equals(find(K, insert(K, V, eqc_symbolic:eval(T))),
      {found, V})).


prop_find_post_absent() ->
  ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
    eqc:equals(find(K, delete(K, eqc_symbolic:eval(T))),
      nothing)).

% ∀ k t. find k (delete k t) === nothing
% c('src/bst'), c('src/test_bst').
% eqc:quickcheck(test_bst:prop_find_post_absent()).

prop_find_post_absent2() ->
  ?FORALL({K, T}, {atom_key(), empty()},
    eqc:equals(find(K, T),
      nothing)).

prop_delete_post_absent() ->
  ?FORALL({K, T}, {atom_key(), empty()},
    eqc:equals(delete(K, T),
      leaf
      )
  ).

prop_delete_post_present() ->
  ?FORALL({K, V, T}, {int_key(), int_key(), bst(atom_key(), int_value())}, % is it an alright test to use?
    eqc:equals(delete(K, insert(K, V, eqc_symbolic:eval(T))),
      eqc_symbolic:eval(T)
    )
  ).

prop_union_post_empty() ->
  ?LET({G, T}, {empty(), empty()}, % hvad er forskellen på LET og forall denne kører stadig 100 gange????
    eqc:equals(union(G, T),
      empty()
    )
  ).

prop_union_post() ->
  ?FORALL({T, G}, {bst(int_key(), int_value()), bst(atom_key(), int_value())}, % hvad er forskellen på LET og forall denne kører stadig 100 gange????
    helper(eqc_symbolic:eval(T), eqc_symbolic:eval(G))
  ).

prop_union_post2() -> % TODO: find out what the hell is going on
  ?FORALL({T, G}, {bst(int_key(), int_value()), bst(atom_key(), int_value())}, % hvad er forskellen på LET og forall denne kører stadig 100 gange????
    helper2(eqc_symbolic:eval(T), eqc_symbolic:eval(G))
  ).

helper2(T,G) ->
  GT = union(G, T),
  TG = union(T, G),

  GTL = to_sorted_list(GT),
  TGL = to_sorted_list(TG),

  GTL == TGL.


helper(T, G) ->
  GT = union(T, G),

  Keys1 = to_sorted_list(T),
  Keys2 = to_sorted_list(G),
  Keys3 = to_sorted_list(GT),

  TExistsInGT = lists:all(fun(X) ->
    lists:member(X, Keys3)
                          end, Keys1),

  GExistsInGT = lists:all(fun(X) ->
    lists:member(X, Keys3)
                          end, Keys2),

  TExistsInGT and GExistsInGT.
%%% -- metamorphic properties

%% the size is larger after an insert
prop_size_insert() ->
    % ∀ k v t. size (insert k v t) >= size t
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            bst:size(insert(K, V, eqc_symbolic:eval(T))) >= bst:size(eqc_symbolic:eval(T))).

prop_size_delete() ->
  % ∀ k v t. size (insert k v t) >= size t
  ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
    bst:size(delete(K, eqc_symbolic:eval(T))) =< bst:size(eqc_symbolic:eval(T))).

prop_size_union() ->
  % ∀ k v t. size (insert k v t) >= size t
  ?FORALL({G, T}, {bst(int_key(), int_value()), bst(atom_key(), int_value())},
    eqc:equals(bst:size(union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      case eqc_symbolic:eval(T) of
        leaf ->  bst:size(eqc_symbolic:eval(G));
        {branch, _, _, _, _} -> case eqc_symbolic:eval(G) of
                   leaf -> bst:size(eqc_symbolic:eval(T));
                   {branch, _, _, _, _} -> bst:size(eqc_symbolic:eval(G)) + bst:size(eqc_symbolic:eval(T))
                 end
      end)).



obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
            obs_equals(insert(K1, V1, insert(K2, V2, eqc_symbolic:eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eqc_symbolic:eval(T));
                           false -> insert(K2, V2, insert(K1, V1, eqc_symbolic:eval(T)))
                       end)).

prop_insert_delete() ->
  ?FORALL({K1, K2, V1, T},
    {atom_key(), atom_key(), int_value(),
      bst(atom_key(), int_value())},
    obs_equals(insert(K1, V1, delete(K2, eqc_symbolic:eval(T))),
      case K1 =:= K2 of
        true ->  insert(K1, V1, eqc_symbolic:eval(T));
        false -> delete(K2, insert(K1, V1, eqc_symbolic:eval(T)))
      end)).

prop_insert_union_left() ->
  ?FORALL({K, V, G, T},
    {
      int_key(), int_value(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(insert(K, V, eqc_symbolic:eval(G)), eqc_symbolic:eval(T)))).

prop_insert_union_right() ->
  ?FORALL({K, V, G, T},
    {
      int_key(), int_value(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(eqc_symbolic:eval(G), insert(K, V, eqc_symbolic:eval(T))))).

prop_insert_union() ->
  ?FORALL({K, V, G, T},
    {
      int_key(), int_value(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(insert(K, V, eqc_symbolic:eval(G)), insert(K, V, eqc_symbolic:eval(T))))).

prop_delete_union() ->
  ?FORALL({K, G, T},
    {
      atom_key(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    obs_equals(delete(K, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(delete(K, eqc_symbolic:eval(G)), delete(K, eqc_symbolic:eval(T))))).

%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
            equals(model(insert(K, V, eqc_symbolic:eval(T))),
                   sorted_insert(K, V, delete_key(K, model(eqc_symbolic:eval(T)))))).

prop_delete_model() ->
  ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
            equals(model(delete(K, eqc_symbolic:eval(T))),
              delete_key(K, model(eqc_symbolic:eval(T)))
              )
    ).

prop_empty_model() ->
  ?LET(T, empty(),
    equals(model(T),
      model(leaf)
    )
  ).

prop_union_model() ->
  ?FORALL({G,T}, {bst(int_key(), int_value()), bst(int_key(), int_value())},
    eqc:equals(model(union(eqc_symbolic:eval(G),eqc_symbolic:eval(T))),
      helper3(model(eqc_symbolic:eval(G)), model(eqc_symbolic:eval(T)))
    )
  ).

helper3(G, T) when T == [] ->
  G;

helper3(G, T) ->
  [Head|Tail] = T,
  {Key, Value} = Head,
  case lists:keyfind(Key, 1, G) of
    false -> helper3(sorted_insert(Key, Value, G), Tail);
    _ -> helper3(G, Tail)
  end.



%      lists:usort(fun(A, B) ->
%{Key1, Value1} = A,
%{Key2, Value2} = B,
%Key1 =< Key2
%end, model(G) ++ model(T))
%
%
%
%
%
prop_find_model() ->
  ?FORALL({K, T}, {int_key(), bst(int_key(), int_value())},
    equals(find(eqc_symbolic:eval(K), eqc_symbolic:eval(T)),
      case model(eqc_symbolic:eval(T)) of
        [] -> nothing;
        _ ->
          case lists:keyfind(K, 1, model(eqc_symbolic:eval(T))) of
            {_Key, Value} -> {found, Value};
            false -> nothing
          end
      end
    )
  ).

-spec delete_key(Key, [{Key, Value}]) -> [{Key, Value}].
delete_key(Key, KVS) -> [ {K, V} || {K, V} <- KVS, K =/= Key ].

-spec sorted_insert(Key, Value, [{Key, Value}]) -> nonempty_list({Key, Value}).
sorted_insert(Key, Value, [{K, V} | Rest]) when K < Key ->
    [{K, V} | sorted_insert(Key, Value, Rest)];
sorted_insert(Key, Value, KVS) -> [{Key, Value} | KVS].



%% -- Test all properties in the module: eqc:module(test_bst)
% c('src/bst'), c('src/test_bst').