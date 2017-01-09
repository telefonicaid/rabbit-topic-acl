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
  rabbit_topic_acl_sup:start_link().

stop(_State) ->
  ok.

