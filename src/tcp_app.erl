%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% This version build from Erlang and OTP in Action example
%%%---------------------------------------------------------------------------------------
-module(tcp_app).

-behaviour(application).

%% export application callbacks
-export([start/2, stop/1]).

%%%---------------------------------------------------------------------------------------
%%% application callbacks
%%%---------------------------------------------------------------------------------------

start(_Type, _StartArgs) ->
	case tcp_root_sup:start_link() of
		{ok, Pid} -> 
			{ok, Pid};
		Other ->
			{error, Other}

	end.

stop(_State) ->
	ok.
