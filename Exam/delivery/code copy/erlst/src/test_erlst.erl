-module(test_erlst).

-include_lib("eunit/include/eunit.hrl").
-export([test_all/0, test_everything/0]).
-export([]). % Remember to export the other functions from Q2.2


% You are allowed to split your testing code in as many files as you
% think is appropriate, just remember that they should all start with
% 'test_'.
% But you MUST have a module (this file) called test_erlst.
test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
  [ {"Basic behaviour", spawn,
    [ test_start_server(),
      test_add_account(),
      test_add_trader(),
      test_make_offer(),
      test_rescind_offer(),
      test_shutdown_server(),
      test_make_offer_rescind_with_trader()
    ]
  }
  ].

test_everything() ->
  test_all().

% c(erlst), c(test_erlst), test_erlst:test_all().

test_start_server() ->
  {"Starting server",
    fun () ->
      ?assertMatch({ok, _}, erlst:launch())
    end }.

test_shutdown_server() ->
  {"Starting server and shutting it down",
    fun () ->
      {ok, S} = erlst:launch(),
      ?assertMatch(true ,is_process_alive(S)),
      ?assertMatch(0, erlst:shutdown(S)),
      timer:sleep(500),
      ?assertMatch(false ,is_process_alive(S))
    end }.

test_add_account() ->
  {"Add account to stock exchange",
    fun () ->
      {ok, S} = erlst:launch(),
      Acc = erlst:open_account(S, {200, [{ll, 1}, {ff, 2}]}),
      ?assertMatch({200, [{ll, 1}, {ff, 2}]},erlst:account_balance(Acc))
    end }.

test_make_offer() ->
  {"Make an offer on the stock exchange",
    fun () ->
      {ok, S} = erlst:launch(),
      Acc = erlst:open_account(S, {200, [{ll, 1}, {ff, 2}]}),
      Acc2 = erlst:open_account(S, {1000, []}),
      Strat = fun(N) -> timer:sleep(5000), accept end,
      Strat2 = fun(N) -> accept end,
      TraderId = erlst:add_trader(Acc, Strat),
      TraderId2 = erlst:add_trader(Acc2, Strat2),
      ?assertMatch({ok, 1}, erlst:make_offer(Acc, {ll, 500})),
      timer:sleep(1000),
      ?assertMatch({500, [{ll, 1}]}, erlst:account_balance(Acc2))
    end }.

test_make_offer_rescind_with_trader() ->
  {"Make an offer and rescind it, before it can be bought",
    fun () ->
      {ok, S} = erlst:launch(),
      Acc = erlst:open_account(S, {200, [{ll, 1}, {ff, 2}]}),
      Acc2 = erlst:open_account(S, {1000, []}),
      Strat = fun(N) -> timer:sleep(1000), accept end,
      TraderId = erlst:add_trader(Acc, Strat),
      TraderId2 = erlst:add_trader(Acc2, Strat),
      {ok, OfferId} = erlst:make_offer(Acc, {ll, 500}),
      erlst:rescind_offer(Acc, OfferId),
      ?assertMatch(ok, erlst:rescind_offer(Acc, OfferId)),
      ?assertMatch(true ,is_process_alive(TraderId)),
      ?assertMatch(true ,is_process_alive(TraderId2)),
      timer:sleep(500),
      ?assertMatch({200, [{ll, 1}, {ff, 2}]}, erlst:account_balance(Acc)),
      ?assertMatch({1000, []}, erlst:account_balance(Acc2)),
      erlst:shutdown(S),
      timer:sleep(500),
      ?assertMatch(false ,is_process_alive(TraderId2)),
      ?assertMatch(false ,is_process_alive(TraderId))
    end }.

test_rescind_offer() ->
  {"Rescind offer from the stock exchange",
    fun () ->
      {ok, S} = erlst:launch(),
      Acc = erlst:open_account(S, {200, [{ll, 1}, {ff, 2}]}),
      Strat = fun(N) -> accept end,
      TraderId = erlst:add_trader(Acc, Strat),
      {ok, OfferId} = erlst:make_offer(Acc, {ll, 500}),
      ?assertMatch(ok, erlst:rescind_offer(Acc, OfferId))
    end }.

test_add_trader() ->
  {"Add trader to an account",
    fun () ->
      {ok, S} = erlst:launch(),
      Acc = erlst:open_account(S, {200, [{ll, 1}, {ff, 2}]}),
      Strat = fun(N) -> accept end,
      TraderId = erlst:add_trader(Acc, Strat),
      ?assertMatch(ok, erlst:remove_trader(Acc, TraderId))
    end }.
