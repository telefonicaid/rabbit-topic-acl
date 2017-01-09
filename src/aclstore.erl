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

start(normal, []) ->
  mnesia:wait_for_tables([aclstore_record], 5000),
  aclstore_sup:start_link().

stop(_) -> ok.
