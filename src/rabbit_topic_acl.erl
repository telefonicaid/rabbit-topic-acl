%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 16:46
%%%-------------------------------------------------------------------
-module(rabbit_topic_acl).
-author("dmoranj").

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
%%  io:format("~n~nStarted ACL Topic Application ~n~n~n"),
%%  aclstore_worker:create_tables(node()),
%%  mnesia:wait_for_tables([aclstore_record], 5000),
  rabbit_topic_acl_sup:start_link().


stop(_State) ->
  ok.

