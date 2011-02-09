%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% @doc This is root supervisor for tcp_rpc application.
%%% @end
%%% This version build from Erlang and OTP in Action example
%%%---------------------------------------------------------------------------------------
-module(tcp_root_sup).

-behaviour(supervisor).

%% export Supervisor callbacks
-export([init/1]).

%% API
-export([start_link/0]).

-define(SERVER, ?MODULE).
-define(SOFT_SHUTDOWN, 2000).


%%%---------------------------------------------------------------------------------------
%%% Supervisor callbacks
%%%---------------------------------------------------------------------------------------

init([]) ->
	% ID, StartMFA, Restart, Shutdown, Type, process_dependencies
	Server = {tcp_server, {tcp_server, start_link, []},
		 permanent, ?SOFT_SHUTDOWN, worker, [tcp_server]},
	Children = [Server],
	RestartStrategy = {one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.


%%%---------------------------------------------------------------------------------------
%%% implementation of this modules API
%%%---------------------------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

