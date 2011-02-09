%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% @doc RPC handler, converting raw data, received on socket, to module, fuction, arguments 
%%% 	 (MFA)to execute. 
%%% @end
%%%---------------------------------------------------------------------------------------
-module(rpc_utils).

-export([do_rpc/2, split_out_mfa/1, execute_filtered/3]).


do_rpc(Socket, RawData) ->
	try
		{M, F, A} = split_out_mfa(RawData),
		Result = execute_filtered(M, F, A),
		gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
	catch
		_Class:Err ->
			gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))

	end.

split_out_mfa(RawData) ->
	MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
	{match, [M, F, A]} = 
		re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$", [{capture, [1,2,3], list}, ungreedy]),
	{list_to_atom(M), list_to_atom(F), args_to_terms(A)}.


args_to_terms(RawArgs) ->
	{ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
	{ok, Args} = erl_parse:parse_term(Toks),
	Args.

execute_filtered(M, F, A) ->
	case F of
		stop -> {error, io_lib:fwrite("Forbidden rpc ~p,~p~n!",[M, F])};
		_ -> apply(M,F,A)
	end.
