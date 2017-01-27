%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. ene 2017 17:48
%%%-------------------------------------------------------------------
-module('topic-utils_SUITE').
-author("dmoranj").

-include_lib("common_test/include/ct.hrl").
-export([match_static/1, match_inner_wildcard/1, match_substring/1]).
-export([all/0]).

all() -> [match_static, match_inner_wildcard, match_substring].

match_static(_) ->
  true = topic_utils:match("/root1/root2", "/root1/root2"),
  false = topic_utils:match("/root1/root2", "/root3/root4"),
  ok.

match_substring(_) ->
  false = topic_utils:match("/root1/root2", "/root1"),
  false = topic_utils:match("/root1/root2", "/root2"),
  false = topic_utils:match("/root1/root2/root3", "root2").

match_inner_wildcard(_) ->
  true = topic_utils:match("/root1/root2", "/+/root2"),
  true = topic_utils:match("/root1/root2", "/root1/+"),
  true = topic_utils:match("/root1/root2", "/+/+"),
  false = topic_utils:match("/root1/root2", "/+"),
  false = topic_utils:match("/root1/root2", "+/root2"),
  false = topic_utils:match("/root1/root2", "root1/.+"),
  ok.
