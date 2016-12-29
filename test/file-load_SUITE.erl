%%%-------------------------------------------------------------------
%%% @author dmj
%%% @copyright (C) 2016, Telefonica Investigación y Desarrollo, S.A.U
%%% @doc
%%%
%%% This file is part of RabbitMQ ACL Topic plugin.
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
-module('file-load_SUITE').
-author("dmj").

-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1, all/0,
         init_per_testcase/2, end_per_testcase/2]).
-export([add_permission/1,permissions_by_user/1]).

all() -> [add_permission, permissions_by_user].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  aclstore:install([node()]),
  application:start(mnesia),
  application:start(aclstore),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

init_per_testcase(add_permission, Config) ->
  Config;

init_per_testcase(_, Config) ->
  aclstore:add_permission("janedoe", "root/messages", write),
  aclstore:add_permission("johndoe", "root/messages", read),
  Config.

end_per_testcase(_, _Config) ->
  ok.

add_permission(_Config) ->
  ok = aclstore:add_permission("johndoe", "root/subroot/+", write).

permissions_by_user(_Config) ->
  [{"root/subroot/+", write},
    {"root/messages",read}] = aclstore:get_permissions("johndoe").
