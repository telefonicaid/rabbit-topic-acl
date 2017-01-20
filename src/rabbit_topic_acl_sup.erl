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
-module(rabbit_topic_acl_sup).
-author("dmoranj").

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init(Args) ->
  io:format("~n~nStarted global supervisor~n~n~n"),

  {ok, {{one_for_one, 3, 10},
    [
      {aclstore_sup,
        {aclstore_sup, start_link, []},
        permanent,
        10000,
        supervisor,
        [aclstore_sup]},
      {aclenforce_sup,
        {aclenforce_sup, start_link, Args},
        permanent,
        10000,
        supervisor,
        [aclenforce_sup]},
      {topicaclplugin_sup,
        {topicaclplugin_sup, start_link, Args},
        permanent,
        10000,
        supervisor,
        [topicaclplugin_sup]}
    ]}}.
