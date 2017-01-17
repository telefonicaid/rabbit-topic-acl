%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. ene 2017 14:57
%%%-------------------------------------------------------------------
-module('acl-enforce_SUITE').
-author("dmoranj").

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0,
  init_per_testcase/2, end_per_testcase/2]).

-export([authorize_existing_user/1]).

all() -> [authorize_existing_user].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  aclstore_worker:install([node()]),
  ok = application:start(mnesia),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

init_per_testcase(_, Config) ->
  {ok, PID} = aclstore_worker:start_link(),
  aclstore:add_permission("janedoe", "root/messages", write),
  aclstore:add_permission("johndoe", "root/messages", read),
  [{server, PID} | Config].

end_per_testcase(_, _Config) ->
  mnesia:clear_table(aclstore_record),
  ok.

authorize_existing_user(_Config) ->
  ok.