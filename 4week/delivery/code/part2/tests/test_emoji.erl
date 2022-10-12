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
         test_delete(),
         test_delete_multiple(),
         test_delete_ASCIIname(),
         test_alias_ASCII(),
         test_analytics_creating(),
         test_analytics_creating_multiple(),
         test_analytics_creating_exists(),
         test_analytics_creating_multiple_different(),
         test_analytics_creating_and_deleting(),
         test_works_with_small_set(),
         test_works_with_medium_set(),
         test_works_with_small_set_more_commands(),
         test_works_with_medium_set_more_commands(),
         test_stop_server()
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1,and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_start_5_servers() ->
    {"We can call start/1 5 times and it does not crash",
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
        ?assertMatch(ok, emoji:alias(S, "42", "4242")), 
        ?assertMatch({ok, _}, emoji:lookup(S, "4242")),
        ?assertMatch({ok, _}, emoji:lookup(S, "42"))
      end }.

test_alias_ASCII() ->
    {"We can create an alias for a shortcode with ASCII and call it for an emoji",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        emoji:new_shortcode(S, "420", "6666"),
        ?assertMatch(ok, emoji:alias(S, "42", "Mikkel")), 
        ?assertMatch({ok, _}, emoji:lookup(S, "Mikkel")),
        ?assertMatch({ok, _}, emoji:lookup(S, "42"))
      end }.

test_delete() ->
    {"We can create a short and delete it to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        ?assertMatch({ok, "666"}, emoji:lookup(S, "42")),
        emoji:delete(S, "42"),
        ?assertMatch(no_emoji, emoji:lookup(S, "42"))
      end }.


test_delete_multiple() ->
    {"We can create multiple short and delete it to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "666"),
        ?assertMatch({ok, "666"}, emoji:lookup(S, "42")),
        emoji:new_shortcode(S, "84", "999"),
        emoji:delete(S, "42"),
        emoji:delete(S, "84"),
        ?assertMatch(no_emoji, emoji:lookup(S, "42")),
        ?assertMatch(no_emoji, emoji:lookup(S, "84"))
      end }.

test_delete_ASCIIname() ->
    {"We can create multiple shorts with ascii names and delete Them to return a clean state.",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        ?assertMatch({ok, _}, emoji:lookup(S, "42")),
        emoji:new_shortcode(S, "84", "Nicolas"),
        emoji:delete(S, "42"),
        emoji:delete(S, "84"),
        ?assertMatch(no_emoji, emoji:lookup(S, "42")),
        ?assertMatch(no_emoji, emoji:lookup(S, "84"))
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
        ?assertMatch(ok, emoji:analytics(S, "19", (fun(X) -> X+1 end), "Str", 0))
      end }.

test_analytics_creating_multiple_different() ->
    {"We can create multiple analytics functions to monitor specific short codes/emojies in our server",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        emoji:new_shortcode(S, "19", "Nicolaj"),  
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0)),
        ?assertMatch(ok, emoji:analytics(S, "19", (fun(X) -> X++"a" end), "Str", ""))
      end }.


test_analytics_creating_exists() ->
    {"We can create analytics functions and see that they exists in the server state",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "123", "42"),
        emoji:new_shortcode(S, "42", "123"),
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0)),
        ?assertMatch({ok, [{"Str",0}]}, emoji:get_analytics(S, "42"))
      end }.

test_analytics_creating_and_deleting() ->
    {"We can create multiple analytics functions to monitor specific short codes/emojies in our server",
      fun () ->
        {ok, S} = emoji:start([]),
        emoji:new_shortcode(S, "42", "Mikkel"),
        ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0)),
        ?assertMatch(ok, emoji:remove_analytics(S, "42", "Str"))
      end }.

test_works_with_small_set() ->
    {"We can call start/1 with small set of emojies ,and it does not crash",
     fun () ->
       ?assertMatch({ok, _S}, emoji:start(someemoji:small()))
     end }.

test_works_with_medium_set() ->
    {"We can call start/1 with small set of emojies ,and it does not crash",
     fun () ->
       ?assertMatch({ok, _S}, emoji:start(someemoji:medium()))
     end }.

test_works_with_small_set_more_commands() ->
    {"We can call start/1 with small set of emojies ,and it does not crash",
     fun () ->
       {ok, S} = emoji:start(someemoji:small()),
       emoji:new_shortcode(S, "123", "42"),
       emoji:new_shortcode(S, "42", "123"),
       ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0))
     end }.

test_works_with_medium_set_more_commands() ->
    {"We can call start/1 with small set of emojies ,and it does not crash",
     fun () ->
       {ok, S} = emoji:start(someemoji:medium()),
       emoji:new_shortcode(S, "123", "42"),
       emoji:new_shortcode(S, "42", "123"),
       ?assertMatch(ok, emoji:analytics(S, "42", (fun(X) -> X+1 end), "Str", 0))
     end }.

test_stop_server() ->
    {"We can call start/1,and then stop/0 and it ends the server process",
     fun () ->
       {ok, S} = emoji:start([]),
       emoji:stop(S),
       emoji:new_shortcode(S, "123", "42"),
       emoji:new_shortcode(S, "51125", "22")
     end }.


%c('src/emoji'), c('tests/test_emoji'), c('tests/someemoji'), test_emoji:test_all().