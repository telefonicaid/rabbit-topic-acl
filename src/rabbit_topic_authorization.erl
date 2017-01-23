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
-module(rabbit_topic_authorization).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1, authorize/3]).

-record(state, {
    user,
    vhost,
    administrator,
    exchange,
    queue
}).

-import(rabbit_misc, [r/3, format/2, protocol_error/3]).

-rabbit_boot_step({?MODULE,
                   [{description, "topic-based authorization"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"topic-based authorization">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"topic-based authorization">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).


init(Ch) ->
  {ok, Exchange} = application:get_env(rabbitmq_topic_acl, trashexchange),
  {ok, Queue} = application:get_env(rabbitmq_topic_acl, trashqueue),
  {ok, Admin} = application:get_env(rabbitmq_topic_acl, acladmin),

  #state{
    user=rabbit_channel:get_user(Ch),
    vhost=rabbit_channel:get_vhost(Ch),
    administrator = Admin,
    exchange = Exchange,
    queue = Queue
  }.

description() ->
  [{description,
    <<"Checks authorization based on routing keys">>}].

authorize(Username, RoutingKeyBin, Permission) ->
  Data = [Permission, Username, RoutingKeyBin],
  io:format("Intercepting: Permission [~w] User [~s] Routing [~s]\n", Data),
  aclenforce:authorize(binary_to_list(Username), binary_to_list(RoutingKeyBin), Permission).

intercept(#'basic.publish'{routing_key = RoutingKeyBin, exchange = Exchange} = Method,
          Content, 
          _State = #state{user = {_, Username, _, _}, vhost = _VHost, exchange = Trash}) ->

  io:format("Intercepting Method: ~w\n", [Method]),
  io:format("Intercepting Exchange: ~s\n", [binary_to_list(Exchange)]),

  case authorize(Username, RoutingKeyBin, write) of
    true ->
      io:format("Accepted\n"),
      {Method, Content};
    _ ->
      io:format("Rejected\n"),
      {Method#'basic.publish'{exchange = Trash}, Content}
  end;

intercept(#'exchange.bind'{routing_key = _RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = _User, vhost = _VHost}) ->
	  
	  io:format("Intercepting exchange.bind\n"),
	  {Method, Content};

intercept(#'exchange.unbind'{routing_key = _RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = _User, vhost = _VHost}) ->
	  
	  io:format("Intercepting exchange.unbind\n"),
	  {Method, Content};

intercept(#'queue.bind'{routing_key = RoutingKeyBin, queue = Queue} = Method,
          Content, 
          _State = #state{user = {_, Username, _, _}, vhost = _VHost, administrator= Admin, queue = Trash}) ->
	  
	io:format("Intercepting queue.bind: ~s\n", [Queue]),

  case authorize(Username, RoutingKeyBin, read) of
    true ->
      io:format("Accepted\n"),
      {Method, Content};
    _ ->
      io:format("Rejected\n"),
      if
        Username =/= Admin -> {Method#'queue.bind'{queue = Trash}, Content};
        true -> {Method, Content}
      end
  end;

intercept(#'queue.unbind'{routing_key = _RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = _User, vhost = _VHost}) ->
	  
	  io:format("Intercepting queue.unbind\n "),
	  {Method, Content};

intercept(Method, Content, _State) ->
    {Method, Content}.

applies_to() -> 
    ['basic.publish', 'queue.bind', 'queue.unbind', 'exchange.bind', 'exchange.unbind'].

