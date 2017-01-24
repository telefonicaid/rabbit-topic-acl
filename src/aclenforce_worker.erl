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
-module(aclenforce_worker).
-author("dmoranj").
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([]).

start_link() ->
  rabbit_log:info("Starting ACL Enforce worker "),

  {ok, Admin} = case application:get_env(rabbitmq_topic_acl, acladmin) of
    {ok, Value} -> {ok, Value};
    _ -> {ok, <<"guest">>}
  end,

  {ok, Password} = case application:get_env(rabbitmq_topic_acli, aclpassword) of
    {ok, Returnedpass} -> {ok, Returnedpass};
    _ -> {ok, <<"guest">>}
  end,

  {ok, Connection} = amqp_connection:start(#amqp_params_direct{
    username = Admin,
    password = Password
  }),
  {ok, Channel} = amqp_connection:open_channel(Connection),

  {ok, Exchange} = application:get_env(rabbitmq_topic_acl, trashexchange),
  {ok, Queue} = application:get_env(rabbitmq_topic_acl, trashqueue),

  amqp_channel:call(Channel, #'exchange.declare'{
    exchange = Exchange,
    durable = true,
    type = <<"topic">>}
  ),

  amqp_channel:call(Channel, #'queue.declare'{queue = Queue}),

  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_) ->
  {ok, {}}.

terminate(_, _) -> ok.

handle_cast(_, State) ->
  rabbit_log:debug("Handling generic synchronous message "),
  {noreply,State}.

handle_info(_, State) ->
  rabbit_log:debug("Handling incoming generic message "),
  {noreply,State}.

code_change(_, State, _) ->
  {ok, State}.

match_with_topic(Permissions, Topic) ->
  Authorizations = [Permission || {Topic_pattern, Permission} <- Permissions, topic_utils:match(Topic, Topic_pattern)],
  case sets:size(sets:from_list(Authorizations)) of
    0 -> none;
    1 -> hd(Authorizations);
    _ -> readwrite
  end.

is_accepted(Permission, Matched) ->
  case {Permission, Matched} of
    {_, readwrite} -> true;
    {read, read} -> true;
    {write, write} -> true;
    _ -> false
  end.

handle_call({authorize, User, Topic, Permission}, _From, State) ->
  rabbit_log:debug("Authorizing User [~s] for topic [~s] and permissions [~w]", [User, Topic, Permission]),
  Permissions = aclstore:get_permissions(User),
  Matched = match_with_topic(Permissions, Topic),

  {reply, is_accepted(Permission, Matched), State};

handle_call(_, _, State) ->
  rabbit_log:warning("Ignoring unknown handle "),
  {reply, ok, State}.
