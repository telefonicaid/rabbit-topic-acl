%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 10:06
%%%-------------------------------------------------------------------
-module(topicaclplugin_sup).
-author("dmoranj").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
  io:format("~n~nStarted supervisor~n~n~n"),

  {ok, {{one_for_one, 3, 10},
    [{topicaclplugin_worker,
      {topicaclplugin_worker, start_link, []},
      permanent,
      10000,
      worker,
      [topicaclplugin_worker]}
    ]}}.

