%%%-------------------------------------------------------------------
%%% @author Mikael Karlsson <mikael.karlsson@creado.se
%%% @copyright (C) 2024 by Mikael Karlsson <mikael.karlsson@creado.se>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2024 by Mikael Karlsson <mikael.karlsson@creado.se>
%%%-------------------------------------------------------------------
-module(avm_ls_sup).
-moduledoc "Supervisor for `avm_ls_server` and `logger_manager`.".
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-doc "Starts the supervisor".
-spec start_link(Args::avm_ls_server:start_args()) -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init(Args) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    AChild = #{id => avm_ls_server,
               start => {avm_ls_server, start_link, [Args]},
               restart => permanent,
               shutdown => brutal_kill,
               type => worker,
               modules => [avm_ls_server, avm_ls_strip]},
    _Logger = #{id => logger,
               start => {logger_manager, start_link, [#{}]},
               restart => permanent,
               shutdown => brutal_kill,
               type => worker,
               modules => [logger_manager]},
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
