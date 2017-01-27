%%%-------------------------------------------------------------------
%%% @author dmj
%%% @copyright (C) 2016, Telefonica Investigación y Desarrollo, S.A.U
%%% @doc
%%%
%%% This file is part of RabitMQ ACL Topic plugin.
%%%
%%% RabbitMQ ACL Topic plugin is free software: you can redistribute it and/or
%%% modify it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation, either version 3 of the License,
%%% or (at your option) any later version.
%%%
%%% RabbitMQ ACL Topic plugin is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%% See the GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public
%%% License along with RabbitMQ ACL Topic plugin.
%%% If not, see http://www.gnu.org/licenses/.
%%%
%%% For those usages not covered by the GNU Affero General Public License
%%% please contact with::iot_support@tid.es
%%%
%%% @end
%%% Created : 28. dic 2016 17:36
%%%-------------------------------------------------------------------
-module('acl-enforce_SUITE').
-author("dmoranj").

-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0,
  init_per_testcase/2, end_per_testcase/2]).

-export([authorize_existing_user/1, authorize_unexistent_user/1, authorize_readwrite/1]).

all() -> [authorize_existing_user, authorize_unexistent_user, authorize_readwrite].

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
  {ok, _} = aclstore_worker:start_link(),
  {ok, _} = aclenforce_worker:start_link(),
  aclstore:add_permission("janedoe", "/root/+", write),
  aclstore:add_permission("johndoe", "/root/messages", read),
  aclstore:add_permission("johndoe", "/root/issues", readwrite),
  Config.

end_per_testcase(_, _Config) ->
  mnesia:clear_table(aclstore_record),
  ok.

authorize_existing_user(_Config) ->
  true = aclenforce:authorize("janedoe", "/root/subpath", write),
  false = aclenforce:authorize("janedoe", "/root/subpath", read),
  true = aclenforce:authorize("johndoe", "/root/messages", read),
  false = aclenforce:authorize("johndoe", "/root/messages", write).

authorize_unexistent_user(_Config) ->
  false = aclenforce:authorize("jenniferdoe", "/root/subpath", write).

authorize_readwrite(_Config) ->
  true = aclenforce:authorize("johndoe", "/root/issues", write).
