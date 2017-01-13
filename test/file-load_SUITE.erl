%%%-------------------------------------------------------------------
%%% @author dmj
%%% @copyright (C) 2016, Telefonica Investigación y Desarrollo, S.A.U
%%% @doc
%%%
%%% This file is part of RabbitMQ ACL Topic plugin.
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
-module('file-load_SUITE').
-author("dmj").

-include_lib("common_test/include/ct.hrl").
-export([init_per_suite/1, end_per_suite/1, all/0,
         init_per_testcase/2, end_per_testcase/2]).
-export([add_permission/1,permissions_by_user/1,remove_permissions/1,load_permissions_file/1,
  install_permissions_file/1, syntax_error_unexistent_command/1, syntax_error_parameter_number/1,
  syntax_error_with_file/3, missing_permissions_file/1, syntax_error_wrong_permissions/1,
  permissions_for_unexistent_user/1, remove_unknownuser/1, list_permissions/1, save_permissions_file/1]).

all() -> [add_permission, permissions_by_user, remove_permissions, load_permissions_file, install_permissions_file,
          syntax_error_unexistent_command, syntax_error_parameter_number, missing_permissions_file,
          syntax_error_wrong_permissions, permissions_for_unexistent_user, remove_unknownuser,
          list_permissions, save_permissions_file].

init_per_suite(Config) ->
  Priv = ?config(priv_dir, Config),
  application:set_env(mnesia, dir, Priv),
  aclstore_worker:install([node()]),
  ok = application:start(mnesia),
  Config.

end_per_suite(_Config) ->
  application:stop(mnesia),
  ok.

init_per_testcase(_, Config) ->
  {ok, PID} = aclstore_worker:start_link(),
  aclstore:add_permission("janedoe", "root/messages", write),
  aclstore:add_permission("johndoe", "root/messages", read),
  [{server, PID} | Config].

end_per_testcase(_, _Config) ->
  mnesia:clear_table(aclstore_record),
  ok.

add_permission(_Config) ->
  ok = aclstore:add_permission("johndoe", "root/subroot/+", write).

permissions_by_user(_Config) ->
  [{"root/messages",read}] = aclstore:get_permissions("johndoe").

permissions_for_unexistent_user(_Config) ->
  [] = aclstore:get_permissions("johnunknown").

remove_permissions(_Config) ->
  ok = aclstore:remove_permissions("johndoe"),
  [] = aclstore:get_permissions("johndoe").

remove_unknownuser(_Config) ->
  ok = aclstore:remove_permissions("unknownuser").

list_permissions(_Config) ->
  [ {"janedoe", "root/messages", write},
    {"johndoe", "root/messages", read}] = aclstore:list_permissions().

load_permissions_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Filename = string:concat(DataDir, "aclfile1"),
  [{"jenniferdoe",read, "root/messages"},
    {"jackdoe",readwrite,"root/subroot/+"},
    {"jackdoe",read,"root/messages"},
    {global,read,"#"}] = aclstore:read_permissions_file(Filename).

save_permissions_file(Config) ->
  Privdir = ?config(priv_dir, Config),
  Filename = string:concat(Privdir, "aclfile_saved"),
  aclstore:save_permissions_file(Filename),
  [ {"johndoe", read, "root/messages"},
    {"janedoe", write, "root/messages"}] = aclstore:read_permissions_file(Filename).


install_permissions_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Filename = string:concat(DataDir, "aclfile1"),
  ok = aclstore:load_permissions_file(Filename),
  [{read, "root/messages"}] = aclstore:get_permissions("jenniferdoe").

syntax_error_with_file(Config, Filename, Error) ->
  DataDir = ?config(data_dir, Config),
  Fullname = string:concat(DataDir, Filename),
  Error = aclstore:load_permissions_file(Fullname).

syntax_error_unexistent_command(Config) ->
  syntax_error_with_file(Config, "aclfile_wrongcommand", parse_error).

syntax_error_parameter_number(Config) ->
  syntax_error_with_file(Config, "aclfile_too_many_params_topic", syntax_error),
  syntax_error_with_file(Config, "aclfile_too_many_params_user", syntax_error),
  syntax_error_with_file(Config, "aclfile_too_few_params_topic", syntax_error),
  syntax_error_with_file(Config, "aclfile_too_few_params_user", syntax_error).

syntax_error_wrong_permissions(Config) ->
  syntax_error_with_file(Config, "aclfile_wrong_permissions", syntax_error).

missing_permissions_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Filename = string:concat(DataDir, "aclfile_unexistent"),
  file_not_found = aclstore:load_permissions_file(Filename).

