%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 10:03
%%%-------------------------------------------------------------------
-module(topicaclplugin).
-author("dmoranj").

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
  io:format("~n~n~nStarted plugin~n~n~n"),
  lager:error("~n~n~nStarted plugin with lager~n~n~n"),
  topicaclplugin_sup:start_link().

stop(_State) ->
  ok.
