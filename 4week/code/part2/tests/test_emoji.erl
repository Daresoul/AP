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
         test_lookup(),
         test_lookup_fail(),
         test_multi_lookup(),
         test_alias(),
         test_get_state(),
         test_get_state2(),
         test_delete(),
         test_delete_multiple(),
         test_delete_ASCIIname(),
         test_alias_ASCII(),
         test_analytics_creating(),
         test_analytics_creating_multiple()
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
    {"We can call lookup/1, and find a specific emoji",
     fun () ->
       {ok, S} = emoji:start([]),
       emoji:new_shortcode(S, "123", "321"),
       ?assertMatch({ok, _}, emoji:lookup(S, "123"))
     end }.

test_lookup_fail() ->
    {"We can call lookup/1, and fail if the shortcode is not in the state",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(no_emoji, emoji:lookup(S, "5125125"))
     end }.


test_multi_lookup() ->
    {"We can call multiple lookups lookup/1, and find or fail to find specific emoji(s)",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "123", "321"),
        emoji:new_shortcode(S, "234", "333"),
        ?assertMatch({ok, _}, emoji:lookup(S, "123")),
        ?assertMatch({ok, _}, emoji:lookup(S, "234")),
        ?assertEqual(no_emoji, emoji:lookup(S, "321")),
        ?assertEqual(no_emoji, emoji:lookup(S, "333"))
      end }.

test_alias() ->
    {"We can create an alias for a shortcode and call it for an emoji",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        emoji:new_shortcode(S, "420", "6666"),
        ?assertMatch(ok, emoji:alias(S, "4242", "42")), 
        ?assertMatch({ok, _}, emoji:lookup(S, "4242")),
        ?assertMatch({ok, _}, emoji:lookup(S, "42"))
      end }.

test_alias_ASCII() ->
    {"We can create an alias for a shortcode with ASCII and call it for an emoji",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        emoji:new_shortcode(S, "420", "6666"),
        ?assertMatch(ok, emoji:alias(S, "Mikkel", "42")), 
        ?assertMatch({ok, _}, emoji:lookup(S, "Mikkel")),
        ?assertMatch({ok, _}, emoji:lookup(S, "42"))
      end }.


test_get_state() ->
    {"We can create a state and get it with get_state (an empty state)",
      fun () ->
        {ok, S} = emoji:start([]),
        ?assertMatch({[],[]}, emoji:get_state(S))
      end }.

test_get_state2() ->
    {"We can create a state and get it with get_state (a state of something)",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        ?assertMatch({[{"42","666"}],[]}, emoji:get_state(S))
      end }.

test_delete() ->
    {"We can create a short and delete it to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        ?assertMatch({[{"42","666"}],[]}, emoji:get_state(S)),
        emoji:delete(S, "42"),
        ?assertMatch({[],[]}, emoji:get_state(S))
      end }.


test_delete_multiple() ->
    {"We can create multiple short and delete it to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        ?assertMatch({[{"42","666"}],[]}, emoji:get_state(S)),
        emoji:new_shortcode(S, "84", "999"),
        emoji:delete(S, "42"),
        emoji:delete(S, "84"),
        ?assertMatch({[],[]}, emoji:get_state(S))
      end }.

test_delete_ASCIIname() ->
    {"We can create multiple shorts with ascii names and delete Them to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        ?assertMatch({[{"42","Mikkel"}],[]}, emoji:get_state(S)),
        emoji:new_shortcode(S, "84", "Nicolas"),
        emoji:delete(S, "42"),
        emoji:delete(S, "84"),
        ?assertMatch({[],[]}, emoji:get_state(S))
      end }.
  
test_analytics_creating() ->
    {"We can create analytics functions to monitor specific short codes/emojies in our server",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0))  
      end }.

test_analytics_creating_multiple() ->
    {"We can create multiple analytics functions to monitor specific short codes/emojies in our server",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        emoji:new_shortcode(S, "19", "Nicolaj"),  
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0)),
        ?assertMatch(ok, emoji:analytics(S, "19", (fun(X) -> X+1 end), "Str", 0)),
      end }.

test_analytics_creating_exists() ->
    {"We can create analytics functions and see that they exists in the server state",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0)),
         
      end }.




%c('src/emoji'), c('tests/test_emoji'), test_emoji:test_all().
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