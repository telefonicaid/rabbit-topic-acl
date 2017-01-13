%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 10:07
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
  {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
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

  io:format("Initiated custom plugin with Queue: ~s~n", [Queue]),
  {ok, #state{channel = Channel, queue = Queue, tag = Tag, exchange = Exchange }}.

handle_call(_Msg, _From, State) ->
  io:format("Handling generic asynchronous ~n"),
  {reply, unknown_command, State}.

handle_cast(_, State) ->
  io:format("Handling generic synchronous message ~n"),
  {noreply,State}.

handle_info({{'basic.deliver', _Queue, _, _, _, <<"add">>}, {'amqp_msg', _, Msg} }, State) ->
  [User, Topic, Permission] = string:tokens(string:strip(binary_to_list(Msg), both, $;), " "),
  aclstore:add_permission(User, Topic, list_to_atom(Permission)),
  io:format("Adding new permission: ~s~n~n", [Msg]),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"clear">>}, {'amqp_msg', _, _Msg} }, State) ->
  io:format("Removing all permissions from memory. ~n"),
  aclstore:clear_permissions(),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"save">>}, {'amqp_msg', _, _Msg} }, State) ->
  io:format("Saving permission list. ~n"),
  {noreply, State};

handle_info({{'basic.deliver', _Queue, _, _, _, <<"refresh">>}, {'amqp_msg', _, _Msg} }, State) ->
  Permissions = aclstore:list_permissions(),
  Payload = aclstore_worker:to_text_format(Permissions),
  io:format("Refreshing permission list:\n\n~s\n", [Payload]),
  {noreply, State};

handle_info({'basic.consume_ok', _ }, State) ->
  io:format("Handling consume ACK~n"),
  {noreply, State};

handle_info({Info, _}, State) ->
  io:format("Unknown message: ~w~n", [Info]),
  {noreply, State}.

terminate(_, #state{channel = Channel}) ->
  amqp_channel:call(Channel, #'channel.close'{}),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

