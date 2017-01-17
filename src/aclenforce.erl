%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. ene 2017 14:54
%%%-------------------------------------------------------------------
-module(aclenforce).
-author("dmoranj").

%% API
-export([authorize/3]).


authorize(User, Topic, Permission) ->
  gen_server:call({global, aclenforce_worker}, {authorize, User, Topic, Permission}).