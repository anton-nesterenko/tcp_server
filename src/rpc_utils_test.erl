%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% Test rpc utils
%%%---------------------------------------------------------------------------------------
-module(rpc_utils_test).

-include_lib("eunit/include/eunit.hrl").

-import(rpc_utils, [split_out_mfa/1]).

split_out_mfa_test() ->
	?assertMatch({aModule, aFunction, [argOne, argTwo]}, split_out_mfa("aModule:aFunction(argOne, argTwo).")).
