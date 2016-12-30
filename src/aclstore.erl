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
-export([add_permission/3, get_permissions/1, remove_permissions/1, read_permissions_file/1]).
-export([extract_permissions/2, tokenize_line/1]).

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
      {type, bag}]),
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

get_permissions(User) ->
  F = fun() ->
        Permissions = mnesia:read({aclstore_record, User}),
        [{Topic, Permission} || {aclstore_record, _, Topic, Permission} <- Permissions ]
      end,
  mnesia:activity(transaction, F).

remove_permissions(User) ->
  F = fun() -> mnesia:delete({aclstore_record, User}) end,
  mnesia:activity(transaction, F).


extract_permissions(Item, {User, Permission_list}) ->
  case Item of
    {topic, [Permission, Topic]} -> {User, [{User, Permission, Topic}|Permission_list]};
    {user, [Name]} -> {Name, Permission_list};
    _ -> {error, syntax_error}
  end.

tokenize_line(Line) ->
  Tokenized = string:tokens(Line, " "),
  case hd(Tokenized) of
    "topic"  -> {topic, tl(Tokenized)};
    "user" -> {user, tl(Tokenized)};
    _ -> {error, syntax_error}
  end.

read_permissions_file(Filename) ->
   {ok, IFile} = file:read_file(Filename),
   Contents = binary_to_list(IFile),
   Tokenized = string:tokens(Contents, "\n"),
   Filtered = [X || X <- Tokenized, hd(X) =/= $#],
   Tokens = lists:map(fun tokenize_line/1, Filtered),
   {_, Permissions} = lists:foldl(fun extract_permissions/2, {global, []}, Tokens),
   Permissions.
