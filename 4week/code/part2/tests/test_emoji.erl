-module(test_emoji).
-export([test_all/0]).
% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server(),
         test_start_5_servers(),
         test_shortcode_smiley(),
         test_lookup()
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1, 5 times and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_start_5_servers() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([])),
       ?assertMatch({ok, _}, emoji:start([])),
       ?assertMatch({ok, _}, emoji:start([])),
       ?assertMatch({ok, _}, emoji:start([])),
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley", <<240,159,152,131>>))
     end }.

test_lookup() ->
    {"We can call start/1, 5 times and it does not crash",
     fun () ->
       {ok, S} = emoji:start([]),
       emoji:new_shortcode(S, "123", "321"),
       emoji:lookup(S, "123"),
       ?assertMatch({ok, _}, emoji:start([]))
     end }.


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