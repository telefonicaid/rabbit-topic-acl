%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Topic Authorization.
%%
%% The Initial Developer of the Original Code is Antoine Galataud for Airboxlab S.A.
%% Copyright (c) 2015 Airboxlab S.A. All rights reserved.
%%

-module(rabbit_topic_authorization).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-record(state, {
    user,
    vhost
}).

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
    #state{user=rabbit_channel:get_user(Ch), vhost=rabbit_channel:get_vhost(Ch)}.

description() ->
    [{description,
      <<"Checks authorization based on routing keys">>}].

intercept(#'basic.publish'{routing_key = RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = {_, Username, _, _}, vhost = _VHost}) ->

  Permissions = aclstore:get_permissions(binary_to_list(Username)),
  io:format("Intercepting basic.publish:\n\nContent: ~w\n User: ~s\n Routing: ~s\n", [Content, Username, RoutingKeyBin]),
  io:format("\nPermissions:\n\n~w\n", [Permissions]),
  {Method, Content};

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

intercept(#'queue.bind'{routing_key = _RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = _User, vhost = _VHost}) ->
	  
	  io:format("Intercepting queue.bind\n"),
	  {Method, Content};

intercept(#'queue.unbind'{routing_key = _RoutingKeyBin} = Method,
          Content, 
          _State = #state{user = _User, vhost = _VHost}) ->
	  
	  io:format("Intercepting queue.unbind\n "),
	  {Method, Content};

intercept(Method, Content, _State) ->
    {Method, Content}.

applies_to() -> 
    ['basic.publish', 'queue.bind', 'queue.unbind', 'exchange.bind', 'exchange.unbind'].

