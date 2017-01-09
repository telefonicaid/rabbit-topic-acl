%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. ene 2017 12:17
%%%-------------------------------------------------------------------
-module(aclstore_client).
-author("dmoranj").

-export([add_permission/4, get_permissions/2, remove_permissions/2, read_permissions_file/2, load_permissions_file/2]).

add_permission(Pid, User, Topic, Permission) ->
  gen_server:call(Pid, {add, User, Topic, Permission}).

get_permissions(Pid, User) ->
  gen_server:call(Pid, {get, User}).


remove_permissions(Pid, User) ->
  gen_server:call(Pid, {remove, User}).


read_permissions_file(Pid, Filename) ->
  gen_server:call(Pid, {read_file, Filename}).


load_permissions_file(Pid, Filename) ->
  gen_server:call(Pid, {load_file, Filename}).

