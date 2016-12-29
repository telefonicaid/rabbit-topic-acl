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
-module(aclstore).
-behaviour(application).
-author("dmj").

%% API
-export([start/2, stop/1, install/1]).
-export([add_permission/3]).

-record(aclstore_record, {
  user,
  topic,
  permission
}).

start(normal, []) ->
  mnesia:wait_for_tables([aclstore_record], 5000),
  aclstore_sup:start_link().

stop(_) -> ok.

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(aclstore_record,
    [{attributes, record_info(fields, aclstore_record)},
      {index, [#aclstore_record.topic]},
      {disc_copies, Nodes},
      {type, set}]),
  rpc:multicall(Nodes, application, stop, [mnesia]).

add_permission(User, Topic, Permission) ->
  F = fun() ->
      mnesia:write(#aclstore_record{
        user= User,
        topic = Topic,
        permission = Permission
      })
      end,
  mnesia:activity(transaction, F).
