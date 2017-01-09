%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 16:38
%%%-------------------------------------------------------------------
-module(rabbit_topic_acl_sup).
-author("dmoranj").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
  io:format("~n~nStarted global supervisor~n~n~n"),

  {ok, {{one_for_one, 3, 10},
    [{aclstore_sup,
      {aclstore_sup, start_link, []},
      permanent,
      10000,
      supervisor,
      [aclstore_sup]},
    {topicaclplugin_sup,
      {topicaclplugin_sup, start_link, []},
      permanent,
      10000,
      supervisor,
      [topicaclplugin_sup]}
    ]}}.
