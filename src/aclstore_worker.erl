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
-module(aclstore_worker).
-behaviour(gen_server).
-author("dmj").

%% API
-export([extract_permissions/2, tokenize_line/1, install/1, read_permissions_file/1, load_permissions_file/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_tables/1]).

-record(aclstore_record, {
  user,
  topic,
  permission
}).

-record(state, {
  status
}).

start_link() ->
  io:format("Starting ACL worker ~n"),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_) ->
  mnesia:start(),
  create_tables([node()]),
  {ok, #state{status = ok}}.

terminate(_, _) -> ok.

handle_cast(_, State) ->
  io:format("Handling generic synchronous message ~n"),
  {noreply,State}.

handle_info(_, State) ->
  io:format("Handling incoming generic message ~n"),
  {noreply,State}.

code_change(_, State, _) ->
  {ok, State}.

create_tables(Nodes) ->
  io:format("Creating tables ~n"),
  case mnesia:create_table(aclstore_record,
    [{attributes, record_info(fields, aclstore_record)},
      {index, [#aclstore_record.topic]},
      {disc_copies, Nodes},
      {type, bag}]) of

    {atomic, ok} -> io:format("Table aclstore successfully created~n");
    {aborted, Reason} -> io:format("Could not create table ~w ~n", [Reason])
  end.

install(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  create_tables(Nodes),
  rpc:multicall(Nodes, application, stop, [mnesia]).

extract_permissions(Item, {User, Permission_list}) ->
  case Item of
    {topic, [Permission, Topic]} when Permission =:= "read"; Permission =:= "readwrite"; Permission =:= "write"  ->
      {User, [{User, list_to_atom(Permission), Topic}|Permission_list]};

    {user, [Name]} -> {Name, Permission_list};
    _ -> throw(syntax_error)
  end.

tokenize_line(Line) ->
  Tokenized = string:tokens(Line, " "),
  case hd(Tokenized) of
    "topic"  -> {topic, tl(Tokenized)};
    "user" -> {user, tl(Tokenized)};
    _ -> throw(syntax_error)
  end.

read_permissions_file(Filename) ->
  IFile = case file:read_file(Filename) of
            {ok, IFileObj} -> IFileObj;
            {error, enoent} -> throw(file_not_found)
          end,
  Contents = binary_to_list(IFile),
  Tokenized = string:tokens(Contents, "\n"),
  Filtered = [X || X <- Tokenized, hd(X) =/= $#],
  Tokens = lists:map(fun tokenize_line/1, Filtered),
  {_, Permissions} = lists:foldl(fun extract_permissions/2, {global, []}, Tokens),
  Permissions.

load_permissions_file(Filename) ->
  try
    Permissions = read_permissions_file(Filename),
    [add_permission(User, Permission, Topic) || {User, Permission, Topic} <- Permissions]
  of
    _ -> ok
  catch
    throw:syntax_error -> syntax_error;
    throw:file_not_found -> file_not_found
  end.

add_permission(User, Topic, Permission) ->
  F = fun() ->
    mnesia:write(#aclstore_record{
      user= User,
      topic = Topic,
      permission = Permission
    })
      end,
  case mnesia:activity(transaction, F) of
    ok -> io:format("Permission added successfully"),
      ok;
    Error -> io:format("Unexpected error adding user: ~w~n", [Error])
  end.

handle_call({add, User, Topic, Permission}, _From, State) ->
  io:format("Handling add ~n"),
  Result = add_permission(User, Topic, Permission),
  {reply, Result, State};

handle_call({get, User}, _From, State) ->
  F = fun() ->
        Permissions = mnesia:read({aclstore_record, User}),
        [{Topic, Permission} || {aclstore_record, _, Topic, Permission} <- Permissions ]
      end,
  Result = mnesia:activity(transaction, F),
  {reply, Result, State};

handle_call({list}, _From, State) ->
  F = fun() ->
    mnesia:foldl(fun({_Table, User, Topic, Perm}, NewAcc) -> [{User, Topic, Perm} | NewAcc] end, [], aclstore_record)
  end,
  Result = mnesia:activity(transaction, F),
  {reply, Result, State};

handle_call({remove, User}, _From, State) ->
  F = fun() -> mnesia:delete({aclstore_record, User}) end,
  Result = mnesia:activity(transaction, F),
  {reply, Result, State};

handle_call({read_file, Filename}, _From, State) ->
  Result = read_permissions_file(Filename),
  {reply, Result, State};

handle_call({load_file, Filename}, _From, State) ->
  Result = load_permissions_file(Filename),
  {reply, Result, State};

handle_call(_, _, State) ->
  io:format("Ignoring unknown handle ~n"),
  {reply, ok, State}.


