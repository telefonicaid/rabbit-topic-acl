%%%-------------------------------------------------------------------
%%% @author dmoranj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. ene 2017 17:59
%%%-------------------------------------------------------------------
-module(topic_utils).
-author("dmoranj").

%% API
-export([match/2]).

match(Topic, Expression) ->
  Regex = re:replace(Expression, "\\*|#", "[A-Za-z0-9]+", [global, {return, list}]),
  case re:run(Topic, "^" ++ Regex ++ "$", [{capture, none}]) of
    match -> true;
    _ -> false
  end.
