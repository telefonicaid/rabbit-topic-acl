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
-module(topicaclplugin_worker).
-author("dmoranj").

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {channel, queue, tag, exchange}).

-define(RKFormat,
  "~4.10.0B.~2.10.0B.~2.10.0B.~1.10.0B.~2.10.0B.~2.10.0B.~2.10.0B").

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, Connection} = amqp_connection:start(#amqp_params_direct{
    username = <<"guest">>,
    password = <<"guest">>
  }),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  {ok, Exchange} = application:get_env(rabbitmq_topic_acl, exchange),

  amqp_channel:call(Channel, #'exchange.declare'{
    exchange = Exchange,
    durable = true,
    type = <<"topic">>}
  ),

  #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),

  Binding = #'queue.bind'{
    queue       = Queue,
    exchange    = Exchange,
    routing_key = <<"#">>},

  #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),

  Sub = #'basic.consume'{queue = Queue},
  #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, Sub),

  rabbit_log:info("Initiated custom plugin with Queue: ~s", [Queue]),
  {ok, #state{channel = Channel, queue = Queue, tag = Tag, exchange = Exchange }}.

handle_call(_Msg, _From, State) ->
  rabbit_log:debug("Handling generic asynchronous "),
  {reply, unknown_command, State}.

handle_cast(_, State) ->
  rabbit_log:debug("Handling generic synchronous message "),
  {noreply,State}.

handle_info({{'basic.deliver', _Queue, _, _, _, <<"add">>}, {'amqp_msg', _, Msg} }, State) ->
  [User, Permission, Topic] = string:tokens(string:strip(binary_to_list(Msg), both, $;), " "),
  aclstore:add_permission(User, list_to_atom(Permission), Topic),
  rabbit_log:debug("Adding new permission: ~s", [Msg]),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"clear">>}, {'amqp_msg', _, _Msg} }, State) ->
  rabbit_log:debug("Removing all permissions from memory. "),
  aclstore:clear_permissions(),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"save">>}, {'amqp_msg', _, _Msg} }, State) ->
  rabbit_log:debug("Saving permission list. "),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"refresh">>}, {'amqp_msg', _, _Msg} }, State) ->
  Permissions = aclstore:list_permissions(),
  Payload = aclstore_worker:to_text_format(Permissions),
  rabbit_log:debug("Refreshing permission list:\n\n~s\n", [Payload]),
  {noreply, State};

handle_info({'basic.consume_ok', _ }, State) ->
  rabbit_log:debug("Handling consume ACK"),
  {noreply, State};

handle_info({Info, _}, State) ->
  rabbit_log:warning("Unknown message: ~w", [Info]),
  {noreply, State}.

terminate(_, #state{channel = Channel}) ->
  amqp_channel:call(Channel, #'channel.close'{}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

