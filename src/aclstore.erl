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
-module(aclstore).
-author("dmoranj").

-export([start/2, stop/1]).

-export([add_permission/3, get_permissions/1, remove_permissions/1, read_permissions_file/1, load_permissions_file/1]).
-export([list_permissions/0, save_permissions_file/1, clear_permissions/0]).

start(normal, []) ->
  ok.

stop(_) -> ok.

add_permission(User, Topic, Permission) ->
  gen_server:call({global, aclstore_worker}, {add, User, Topic, Permission}).

get_permissions(User) ->
  gen_server:call({global, aclstore_worker}, {get, User}).

list_permissions() ->
  gen_server:call({global, aclstore_worker}, {list}).

remove_permissions(User) ->
  gen_server:call({global, aclstore_worker}, {remove, User}).

clear_permissions() ->
  gen_server: call({global, aclstore_worker}, {clear}).

read_permissions_file(Filename) ->
  gen_server:call({global, aclstore_worker}, {read_file, Filename}).

load_permissions_file(Filename) ->
  gen_server:call({global, aclstore_worker}, {load_file, Filename}).

save_permissions_file(Filename) ->
  gen_server:call({global, aclstore_worker}, {save_file, Filename}).