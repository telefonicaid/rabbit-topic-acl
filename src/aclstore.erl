%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. ene 2017 16:53
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