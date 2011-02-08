%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% Test tcp server operation
%%%---------------------------------------------------------------------------------------
-module(tcp_server_test).

-include_lib("eunit/include/eunit.hrl").

-import(tcp_server, [start_link/0]).

start_test() ->
	{ok, _} = start_link().
