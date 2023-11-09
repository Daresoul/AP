-module(erlst).

% You are allowed to split your Erlang code in as many files as you
% find appropriate.
% However, you MUST have a module (this file) called erlst.

% Export at least the API:
-export([launch/0,
         shutdown/1,
         open_account/2,
         account_balance/1,
         make_offer/2,
         rescind_offer/2,
         add_trader/2,
         remove_trader/2
        ]).

% You may have other exports as well
-export([get_state/1]).

-import(rand, [uniform/3]).

-type stock_exchange() :: term().
-type account_id() :: term().
-type offer_id() :: term().
-type trader_id() :: term().
-type stock() :: atom().
-type isk() :: non_neg_integer().
-type stock_amount() :: pos_integer().
-type holdings() :: {isk(), [{stock(), stock_amount()}]}.
-type offer() :: {stock(), isk()}.
-type decision() :: accept | reject.
-type trader_strategy() :: fun((offer()) -> decision()).

%Simple function to gain state of a server as described in the server
get_state(S) ->
  S ! {self(), {get_state}},
  ok.

-spec launch() -> {ok, stock_exchange()} | {error, term()}.
%returning {ok, E} unless it cant start the stock exchange
launch() ->
    try
        E = spawn(fun() -> loop(1, [], #{}) end),
        {ok, E}
    catch
        %Catching all errors, for security measures, never interested in throwing an exception that isnt handled
        _:Reason -> {error, Reason}
    end .

-spec shutdown(S :: stock_exchange()) -> non_neg_integer().
shutdown(S) ->
    request_reply(S, kill_process).

-spec open_account(S :: stock_exchange(), holdings()) -> account_id().
open_account(S, Holdings) ->
    request_reply(S, {open_acc, Holdings}).

-spec account_balance(Acct :: account_id()) -> holdings().
account_balance(Acc) ->
    request_reply(Acc, get_balance).

-spec make_offer(Acct :: account_id(), Terms :: offer()) -> {ok, offer_id()} | {error, term()}.
make_offer(Acc, Offer) ->
    request_reply(Acc, {make_offer, Offer}).

-spec rescind_offer(Acct :: account_id(), Offer :: offer_id()) -> ok.
rescind_offer(Acc, OfferId) ->
    Acc ! {self(), {rescind, OfferId}}, % non-blocking
    ok.

-spec add_trader(Acct :: account_id(), Strategy :: trader_strategy()) -> trader_id().
add_trader(Acc, Strategy) ->
  request_reply(Acc, {add_trader, Strategy}).

-spec remove_trader(Acct :: account_id(), Trader :: trader_id()) -> ok.
remove_trader(Acc, TraderId) ->
    Acc ! {self(), {remove_trader, TraderId}}, % non-blocking
    ok.

% Auxilliary function for requesting replies from the main server loop with
% Pid:Pid and with the request:Request
% Taken from course slides, and used in the same way as described there
request_reply(Pid, Request) ->
    Pid ! {self(), Request},
    receive
      {Pid, Response} ->
        Response
    end .

% Stock exchange server
loop(Counter, Offers, Accounts) ->
    receive
      {_, {get_state}} ->
        % Writes out the state to IO
        io:fwrite('Counter: ~w\nOffers: ~w\nAccounts: ~w\n', [Counter, Offers, Accounts]),
        loop(Counter, Offers, Accounts);

      {From, get_balance} -> %returns the balance from the Accounts mapping
        #{From := {Holdings}} = Accounts,
        From ! {self(), {holdings, Holdings}},
        loop(Counter, Offers, Accounts);

      {From, {open_acc, Holdings}} ->
        Self = self(),
        ID = spawn(fun() -> account_loop(Self, []) end), % Chosen to use each account as a different process
        NewAccounts = Accounts#{ID => {Holdings}}, % Adding the process to our map
        From ! {self(), ID},
        loop(Counter, Offers, NewAccounts);

      {From, {make_offer, To, Offer, Seller}} ->
        % check they have the balance
        {StockName, Price} = Offer,
        Stocks = id_has_stock(Accounts, Seller, StockName), % checking if the id has 1 of the given stock
        if
          Stocks == not_sufficient_amount -> % if not then send that back
            From ! {self(), {offer_made, To, {error, not_sufficient_amount}}},
            loop(Counter, Offers, Accounts);
          true -> % otherwise change account state to remove one of the given stock
            #{ Seller := {{Isk, _}}} = Accounts,
            NewAccounts = Accounts#{ Seller := {{Isk, Stocks}}},
            NewOffer = {Counter, Seller, Offer, undecided},
            NewOffers = lists:append(Offers, [NewOffer]),
            % After updating the accounts we are sending a signal to
            % All existing traders
            callWithOffer(maps:keys(Accounts), {broadcast_to_traders, NewOffer}),
            % sending to the account that the offer has been made
            From ! {self(), {offer_made, To, {ok, Counter}}},
            % Adding counter for new offer ID
            NewCounter = Counter + 1,
            loop(NewCounter, NewOffers, NewAccounts)
        end;

      {From, {accept, Id}} ->
        {_,Seller,{Stock,Price},Status} = lists:nth(Id, Offers),
        if
          % If status is undecided then buy
          Status == undecided ->
            NewOffers = update_array_at(Offers, Id, {Id, Seller, {Stock, Price}, accepted}),
            NewAccounts = update_holding(Accounts, From, -Price, Stock),
            NewAccounts2 = update_isk_holding(NewAccounts, Seller, Price),
            loop(Counter, NewOffers, NewAccounts2);
          true ->
            loop(Counter, Offers, Accounts)
        end;

      {From, {rescind, OfferId}} ->
        {_,Seller,{Stock,Price},Status} = lists:nth(OfferId, Offers),
        if
          Status == undecided ->
            % add offter to be rescinded
            NewOffers = update_array_at(Offers, OfferId, {OfferId, Seller, {Stock, Price}, rescinded}),
            #{Seller := {Holdings}} = Accounts,
            {ISK, Stocks} = Holdings,
            Index = get_stock_index(0, Stocks, Stock), % finding the index of the stock in your holdings
            if
              Index == nope ->
                % should be impossible to reach, as i never remove it from your holdings
                loop(Counter, Offers, Accounts);
              true -> % if we can find it in holding then we can add it back
                Index2 = Index + 1,
                {Stock, Amount} = lists:nth(Index2, Stocks),
                NewStocks = update_array_at(Stocks, Index2, {Stock, Amount + 1}),
                NewAccounts = Accounts#{ Seller => {{ISK, NewStocks}}},
                loop(Counter, NewOffers, NewAccounts)
            end;
          true ->
            % already bought then we cant rescind
            loop(Counter, Offers, Accounts)
        end;

      {From, kill_process} ->
        % counting for the response otherwise just sending to kill all processes, and itself.
        Count = calculate_transactions(0, Offers),
        From ! {self(), Count},
        callWithOffer(maps:keys(Accounts), kill_process),
        exit(self(), kill)
    end.


account_loop(Parent, Traders) ->
  receive
    {_, {get_state}} ->
      io:fwrite('Parent: ~w\nTraders: ~w\n', [Parent, Traders]),
      account_loop(Parent, Traders);

    {From, get_balance} ->
      % just sending to recieve the balance and send it back
      Parent ! {self(), get_balance},
      receive
        {Parent, {holdings, Holdings}} ->
          From ! {self(), Holdings}
      end,
      account_loop(Parent, Traders);

    {From, {add_trader, Strategy}} ->
      Self = self(),
      ID = spawn(fun() -> trader_loop(Self, Strategy, []) end), % creates a new trader process
      NewTraders = lists:append(Traders, [ID]), % appends to the list of traders
      From ! {self(), ID},
      account_loop(Parent, NewTraders);

    {From, {make_offer, Offer}} ->
      Parent ! {self(), {make_offer, From, Offer, self()}},
      account_loop(Parent, Traders);

    {From, {decision_made, Id, Type}} ->
      if
        %If trader accepts then send that if reject do nothing
        Type == accept ->
          Parent ! {self(), {accept, Id}};
        true -> nope
      end,
      account_loop(Parent, Traders);

    {Parent, {offer_made, To, Response}} ->
      To ! {self(), Response},
      account_loop(Parent, Traders);

    {From, {broadcast_to_traders, Obj}} ->
      {_, Seller, _, _} = Obj,
      if
        Seller == self() ->
          account_loop(Parent, Traders);
        true ->
          callWithOffer(Traders, {offer_check, Obj}),
          account_loop(Parent, Traders)
      end;

    {From, {rescind, OfferId}} ->
      Parent ! {self(), {rescind, OfferId}},
      account_loop(Parent, Traders);

    {From, {remove_trader, TraderId}} ->
      Index = get_index_from_content(1, Traders, TraderId), % gets index of the traders
      if
        Index == nope ->
          account_loop(Parent, Traders);
        true ->
          NewTraders = remove_elem_at(Traders, Index), % removes element at position
          TraderId ! {self(), kill_process},
          account_loop(Parent, NewTraders)
      end;

    {Parent, kill_process} ->
      callWithOffer(Traders, kill_process), % sends kill signal to all traders
      exit(self(), kill)

  end.

trader_loop(Parent, Strategy, OfferProcesses) ->
  receive
    {_, {get_state}} ->
      io:fwrite('Parent: ~w\nStrategy: ~w\nStrategy: ~w\n', [Parent, Strategy, OfferProcesses]),
      trader_loop(Parent, Strategy, OfferProcesses);

    {Parent, {offer_check, {Id, _, Offer, _}}} ->
      % spawns new process
      PId = spawn(fun() -> calculate_trade(Parent, Strategy, Offer, Id) end),
      NewOfferProcesses = lists:append(OfferProcesses, [{Id, PId, unfinished}]),
      trader_loop(Parent, Strategy, NewOfferProcesses);

    {Parent, kill_process} ->
      kill_all_processes(OfferProcesses)
  end.

% kills all processes in the array
kill_all_processes(OfferProcesses) when OfferProcesses == [] ->
  ok;

kill_all_processes(OfferProcesses) ->
  [Head|Tail] = OfferProcesses,
  {_, Pid, Status} = Head,
  if
    Status == unfinished ->
      exit(Pid, kill)
  end,
  kill_all_processes(Tail).

% the function in a process of a trader to calculate
calculate_trade(Account, Strat, Offer, Id) ->
  try
    Result = Strat(Offer),
    if
      Result == accept -> % accepted the offer
        Account ! {self(), {decision_made, Id, accept}};
      true -> % rejected the offer, send rejection
        Account ! {self(), {decision_made, Id, reject}}
    end
  catch % error send rejection
    _:_ -> Account ! {self(), {decision_made, Id, reject}}
  end.

calculate_transactions(Counter, Array) when Array == [] ->
  Counter;

calculate_transactions(Counter, Array) ->
  [Head|Tail] = Array,
  {_, _, {_, _}, Status} = Head,
  if
    Status == accepted ->
      NewCounter = Counter + 1,
      calculate_transactions(NewCounter, Tail);
    true -> calculate_transactions(Counter, Tail)
  end.

% sending object (tuple) to all in the array
callWithOffer(Accounts, _) when Accounts == [] ->
  ok;

callWithOffer(Accounts, Obj) ->
  [Head|Tail] = Accounts,
  Head ! {self(), Obj},
  callWithOffer(Tail, Obj).

% updating both ISK and stocks of a acc
update_holding(Accounts, Id, ISKDiff, Stock) ->
  #{Id := {Holdings}} = Accounts,
  {ISK, Stocks} = Holdings,
  NewISK = ISK + ISKDiff,
  NewStock = edit_stocks(Stocks, Stock, true),
  NewAccounts = Accounts#{Id => {{NewISK, NewStock}}},
  NewAccounts.

% only updates the ISK of an account
update_isk_holding(Accounts, Id, ISKDiff) ->
  #{Id := {Holdings}} = Accounts,
  {ISK, Stocks} = Holdings,
  NewISK = ISK + ISKDiff,
  NewAccounts = Accounts#{Id => {{NewISK, Stocks}}},
  NewAccounts.


% Checks if an id has a specific stock
id_has_stock(Accounts, Id, StockName) ->
  #{Id := {Holdings}} = Accounts,
  {ISK, Stocks} = Holdings,
  Index = stock_index_in_array(0, Stocks, StockName),
  if
    Index == not_found_or_not_sufficient_amount ->
      not_sufficient_amount;
    true ->
      Index2 = Index + 1,
      {Stock, Amount} = lists:nth(Index2, Stocks),
      update_array_at(Stocks, Index2, {Stock, Amount - 1})
  end.

% checking if u have more than zero of a stock
stock_index_in_array(_, Stocks, _) when Stocks == [] ->
  not_found_or_not_sufficient_amount;

stock_index_in_array(Counter, Stocks, Stock) ->
  [Head|Tail] = Stocks,
  {StockName, Amount} = Head,
  if
    StockName == Stock ->
      if
        Amount > 0 ->
          Counter;
          true -> not_found_or_not_sufficient_amount
      end;
    true -> stock_index_in_array(Counter+1, Tail, Stock)
  end.

% gets index of a stock
get_stock_index(_, Stocks, _) when Stocks == [] ->
  nope;

get_stock_index(Counter, Stocks, Stock) ->
  [Head|Tail] = Stocks,
  {StockName, _} = Head,
  if
    StockName == Stock -> Counter;
    true -> stock_index_in_array(Counter+1, Tail, Stock)
  end.

% gets an index from some content
get_index_from_content(Counter, Array, Content) when Array == [] ->
  nope;

get_index_from_content(Counter, Array, Content) ->
  [Head|Tail] = Array,
  if
    Content == Head -> Counter;
    true -> get_index_from_content(Counter+1, Tail, Content)
  end.

% removes an element at a position
remove_elem_at(Array, Index) ->
  lists:sublist(Array, Index - 1) ++ lists:nthtail(Index, Array).

% updates an element at nth position
update_array_at(Array, Index, Object) ->
  lists:sublist(Array, Index - 1) ++ [Object] ++ lists:nthtail(Index, Array).

% edit stocks to either contain the new or update it
% to contain it
edit_stocks(Stocks, Stock, Found) when Stocks == [] ->
  if
    Found == found ->
      [];
    true ->
     [{Stock, 1}]
  end;

edit_stocks(Stocks, Stock, Found) ->
  [Head|Tail] = Stocks,
  {StockName, Amount} = Head,
  if
    StockName == Stock ->
      lists:append([{StockName, Amount + 1}], edit_stocks(Tail, Stock, found));
    true ->
      lists:append([Head], edit_stocks(Tail, Stock, Found))
  end.

% c(erlst). {ok, S} = erlst:launch(). ACC = erlst:open_account(S, {100, [{abc, 100}]}). erlst:add_trader(ACC, fun(N) -> accept end). erlst:make_offer(ACC, {abc, 100}).
% c(erlst). {ok, S} = erlst:launch().
% erlst:shutdown(S).
% erlst:get_state(S).
% ACC = erlst:open_account(S, {100, [{abc, 100}]}).
% erlst:add_trader(ACC, fun(N) -> accept end).
% erlst:account_balance(ACC).
% erlst:make_offer(ACC, {abc, 100}).

% c(erlst). {ok, S} = erlst:launch(). ACC = erlst:open_account(S, {100, []}). erlst:add_trader(ACC, fun() -> accept end).