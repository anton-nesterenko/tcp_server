%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% Test rpc utils
%%%---------------------------------------------------------------------------------------
-module(rpc_utils_test).

-include_lib("eunit/include/eunit.hrl").

-import(rpc_utils, [split_out_mfa/1, execute_filtered/3]).

split_out_mfa_test() ->
	?assertMatch({aModule, aFunction, [argOne, argTwo]}, split_out_mfa("aModule:aFunction(argOne, argTwo).")).

execute_filters_all_stop_functions_test() ->
	{error, _} = execute_filtered(init, stop, []).

execute_filters_handles_standard_mfa_test() ->
	Result = execute_filtered(math, pi, []),
	Expected = math:pi(),
	?assertMatch(Expected, Result). 

