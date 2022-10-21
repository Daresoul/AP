-module(test_bst).

-import(bst, [empty/0, insert/3, delete/2, find/2, union/2]).
-import(bst, [valid/1, to_sorted_list/1, keys/1]).

-include_lib("eqc/include/eqc.hrl").

%% The following two lines are super bad style, except during development
-compile(nowarn_export_all).
-compile(export_all).

bst(Key, Value) ->
  ?LAZY(
    oneof([ % oneof and the lazy could be excluded as there are only one option to get
      ?LET(KVS, eqc_gen:list(27,{Key, Value}), % Binds a list pair to the variable kvs
      {call, lists, foldl, [  % Symbolic representation of the foldl
        % Could in the future be representated as a symbolic call
        % But chose not to as i contains no data that cant be seen in the code.
          fun({K,V}, T) -> insert(K, V, T) end,
          empty(), KVS
        ]})])
  ).

% example key and value generators
% Since the generators seem to function correctly we havent changed them
int_key() -> eqc_gen:int().   % Since keys are evaluated as numeric value having an int key in a atom tree seems to be fine.
% Added a few more atoms to be able to create larger trees
atom_key() -> eqc_gen:elements([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).
int_value() -> eqc_gen:int(). % Havent tried any specific other values like strings or atoms as values.


% Short function to gain relevant data from a call to a function
getInfoTreeStruckture(T) ->
    {_, _, _, [_, _, F]} = T,
    F.

%%% TESTING OF GENERATORS
prop_measure() ->
  ?FORALL(T,bst(atom_key(), int_value()),
    collect(
      bst:size(
          eqc_symbolic:eval(T)
      ),
      eqc:equals(                   %To get a valid prop for the second argument of collect.
        eqc_symbolic:eval(T),
        eqc_symbolic:eval(T))
    )
  ).

%%% invariant properties
% all generated bst are valid
prop_arbitrary_valid() ->
    ?FORALL(T, bst(atom_key(), int_value()),
      ?WHENFAIL(io:fwrite('T: ~w\n', [eqc_symbolic:eval(T)]),
            valid(eqc_symbolic:eval(T)))).

% if we insert into a valid tree it stays valid
prop_insert_valid() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
      ?WHENFAIL(io:fwrite('K: ~w\nV: ~w\nT: ~w\n', [K, V, eqc_symbolic:eval(T)]),
            valid (insert(K, V, eqc_symbolic:eval(T))))).

% if we insert into a valid tree it stays valid
prop_empty_valid() ->
  ?FORALL(T, empty(), % Should be using the onceonly
    ?WHENFAIL(io:fwrite('T: ~w\n', [eqc_symbolic:eval(T)]),
    valid(eqc_symbolic:eval(T)))).

prop_delete_valid() ->
  ?FORALL({K, V, T}, {atom_key(), int_value(), empty()},
    ?WHENFAIL(io:fwrite('K: ~w\nV: ~w\nT: ~w\n', [K, V, eqc_symbolic:eval(T)]),
    valid(delete(K, insert(K, V, eqc_symbolic:eval(T))))
  )).

prop_delete_valid2() ->
  ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
    ?WHENFAIL(io:fwrite('K: ~w\nV: ~w\nT: ~w\n', [K, V, eqc_symbolic:eval(T)]),
    valid(delete(K, insert(K, V, eqc_symbolic:eval(T))))
  )).

prop_union_valid() ->
  ?FORALL({G, T}, {bst(atom_key(), int_value()), bst(atom_key(), int_value())},
    ?WHENFAIL(io:fwrite('T: ~w\nG: ~w\nGT: ~w\n', [eqc_symbolic:eval(T), eqc_symbolic:eval(G), union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))]),
    valid(union(eqc_symbolic:eval(G), eqc_symbolic:eval(T)))
  )).

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
    ?WHENFAIL(io:fwrite('atom1: ~w\nvalue1: ~w\nbst: ~w\n', [ K, V, getInfoTreeStruckture(T)]),
    eqc:equals(find(K, insert(K, V, eqc_symbolic:eval(T))),
      {found, V}))).


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
  ?FORALL({K, V, T}, {int_key(), int_key(), bst(atom_key(), int_value())},
    ?WHENFAIL(io:fwrite('atom: ~w\nvalue: ~w\nbst: ~w\n', [ K, V, getInfoTreeStruckture(T)]),
      eqc:equals(delete(K, insert(K, V, eqc_symbolic:eval(T))), % Using the insert to make sure the key exists in the tree
        eqc_symbolic:eval(T))
    )
  ).

prop_union_post_empty() ->
  ?FORALL({G, T}, {empty(), empty()}, % Should be a onceonly since it is gonna be the same each time
    eqc:equals(union(G, T),
      empty()
    )
  ).

prop_union_post() ->
  ?FORALL({T, G}, {bst(int_key(), int_value()), bst(atom_key(), int_value())},
    helper(eqc_symbolic:eval(T), eqc_symbolic:eval(G)) %Using serpate function to check if all keys are in the union
  ).

prop_union_post2() ->
  ?FORALL({T, G}, {bst(int_key(), int_value()), bst(atom_key(), int_value())},
    ?WHENFAIL(io:fwrite('T: ~w\nG: ~w\n', [ getInfoTreeStruckture(T), getInfoTreeStruckture(G)]),
      begin
        GT = union(eqc_symbolic:eval(G), eqc_symbolic:eval(T)),
        TG = union(eqc_symbolic:eval(T), eqc_symbolic:eval(G)),

        eqc:equals(to_sorted_list(GT),
        to_sorted_list(TG))
      end
  )).

% Takes 2 trees and checks if the list of all keys of each tree are in the union
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
    bst:size(delete(K, eqc_symbolic:eval(T))) =< bst:size(eqc_symbolic:eval(T))). % Just checking the size is smaller than the original size

prop_size_union() ->
  % ∀ k v t. size (insert k v t) >= size t
  ?FORALL({G, T}, {bst(int_key(), int_value()), bst(atom_key(), int_value())},
    eqc:equals(bst:size(union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      case eqc_symbolic:eval(T) of  % A bit complex: If T is a leave then the size should be the size of G
        leaf ->  bst:size(eqc_symbolic:eval(G));
        {branch, _, _, _, _} -> case eqc_symbolic:eval(G) of % If T is a tree then we should check if G is a leaf
                   leaf -> bst:size(eqc_symbolic:eval(T));
                   {branch, _, _, _, _} -> bst:size(eqc_symbolic:eval(G)) + bst:size(eqc_symbolic:eval(T))
                                % If G is not a leaf then it is the size of the 2 unions.
                 end
      end)).

obs_equals(T1, T2) ->
     eqc:equals(to_sorted_list(T1), to_sorted_list(T2)).

prop_insert_insert() ->
    ?FORALL({K1, K2, V1, V2, T},
            {atom_key(), atom_key(), int_value(), int_value(),
             bst(atom_key(), int_value())},
      ?WHENFAIL(io:fwrite('atom1: ~w\nvalue1: ~w\natom2: ~w\nvalue2: ~w\nbst: ~w\n', [ K1, V1, K2, V2, getInfoTreeStruckture(T)]),
            obs_equals(insert(K1, V1, insert(K2, V2, eqc_symbolic:eval(T))),
                       case K1 =:= K2 of
                           true ->  insert(K1, V1, eqc_symbolic:eval(T));
                           false -> insert(K2, V2, insert(K1, V1, eqc_symbolic:eval(T)))
                       end))).

prop_insert_delete() ->
  ?FORALL({K1, K2, V1, T},
    {atom_key(), atom_key(), int_value(),
      bst(atom_key(), int_value())},
    obs_equals(insert(K1, V1, delete(K2, eqc_symbolic:eval(T))),
      case K1 =:= K2 of % It matters if the key is already existing or not, and we therefore have 2 things that can happen
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
    ?WHENFAIL(io:fwrite('atom: ~w\nvalue: ~w\nT: ~w\nG: ~w\n',
      [ K, V, getInfoTreeStruckture(T), getInfoTreeStruckture(G)]),
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(insert(K, V, eqc_symbolic:eval(G)), eqc_symbolic:eval(T))))).
% Would like to check that the a union with an insert is the same as the insert of an already existing union.
% Here we only do it on the left side

prop_insert_union_right() ->
  ?FORALL({K, V, G, T},
    {
      int_key(), int_value(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    ?WHENFAIL(io:fwrite('atom: ~w\nvalue: ~w\nT: ~w\nG: ~w\n', [ K, V, getInfoTreeStruckture(T), getInfoTreeStruckture(G)]),
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(eqc_symbolic:eval(G), insert(K, V, eqc_symbolic:eval(T)))))).
% Would like to check that the a union with an insert is the same as the insert of an already existing union.
% Here we only do it on the right side

prop_insert_union() ->
  ?FORALL({K, V, G, T},
    {
      int_key(), int_value(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    ?WHENFAIL(io:fwrite('atom1: ~w\nvalue: ~w\nT: ~w\nG: ~w\n', [ K, V, getInfoTreeStruckture(T), getInfoTreeStruckture(G)]),
    obs_equals(insert(K, V, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(insert(K, V, eqc_symbolic:eval(G)), insert(K, V, eqc_symbolic:eval(T)))))).
% Would like to check that the a union with an insert is the same as the insert of an already existing union.
% This time if we insert the same element into both trees is should overlap, and the same tree made.

prop_delete_union() ->
  ?FORALL({K, G, T},
    {
      atom_key(),
      bst(atom_key(), int_value()),
      bst(atom_key(), int_value())
    },
    ?WHENFAIL(io:fwrite('atom: ~w\nG: ~w\nT: ~w\n', [ K, getInfoTreeStruckture(G), getInfoTreeStruckture(T)]),
    obs_equals(delete(K, union(eqc_symbolic:eval(G), eqc_symbolic:eval(T))),
      union(delete(K, eqc_symbolic:eval(G)), delete(K, eqc_symbolic:eval(T)))))).
% If we delete K from each of the trees and then union the K should be absent, and it should also if we delete it
% From the union that has already been made.

%%% -- Model based properties
model(T) -> to_sorted_list(T).


prop_insert_model() ->
    ?FORALL({K, V, T}, {atom_key(), int_value(), bst(atom_key(), int_value())},
      ?WHENFAIL(io:fwrite('atom: ~w\nvalue: ~w\nbst: ~w\n', [ K, V, getInfoTreeStruckture(T)]),
            equals(model(insert(K, V, eqc_symbolic:eval(T))),
                   sorted_insert(K, V, delete_key(K, model(eqc_symbolic:eval(T)))))
      )
    ).

prop_delete_model() ->
  ?FORALL({K, T}, {atom_key(), bst(atom_key(), int_value())},
    ?WHENFAIL(io:fwrite('atom: ~w\nbst: ~w\n', [ K, getInfoTreeStruckture(T)]),
            equals(model(delete(K, eqc_symbolic:eval(T))),
              delete_key(K, model(eqc_symbolic:eval(T)))
              )
    )).

prop_empty_model() ->
  ?FORALL(T, empty(), % Should have been a onceonly as no data changes.
    equals(model(T),
      model(leaf)
    )
  ).

prop_union_model() ->
  ?FORALL({G,T}, {bst(int_key(), int_value()), bst(int_key(), int_value())},
    ?WHENFAIL(io:fwrite('T: ~w\nG: ~w\n', [getInfoTreeStruckture(T), getInfoTreeStruckture(G)]),
    eqc:equals(model(union(eqc_symbolic:eval(G),eqc_symbolic:eval(T))),
      helper3(model(eqc_symbolic:eval(G)), model(eqc_symbolic:eval(T)))
      % using the model functions given to create a union
      % as a list
    ))
  ).

% Takes 2 lists and calls sorted insert on each element of the second unless it already exists
% in the list, and then return the first tree.
helper3(G, T) when T == [] ->
  G;

helper3(G, T) ->
  [Head|Tail] = T,
  {Key, Value} = Head,
  case lists:keyfind(Key, 1, G) of
    false -> helper3(sorted_insert(Key, Value, G), Tail);
    _ -> helper3(G, Tail)
  end.

prop_to_sorted_list_model() ->
  ?FORALL(T, bst(int_key(), int_value()),
    ?WHENFAIL(io:fwrite('T: ~w\n', [getInfoTreeStruckture(T)]),
      eqc:equals(model(eqc_symbolic:eval(T)),
        % The model of the implementation should give the
        % same as taking each element and putting it into a sorted insert
        helper3([], model(eqc_symbolic:eval(T))))
    )).

prop_find_model() ->
  ?FORALL({K, T}, {int_key(), bst(int_key(), int_value())},
    equals(find(eqc_symbolic:eval(K), eqc_symbolic:eval(T)),
      case model(eqc_symbolic:eval(T)) of % To check if the list is empty
        [] -> nothing;
        _ ->
          case lists:keyfind(K, 1, model(eqc_symbolic:eval(T))) of % Does K exist in the model?
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