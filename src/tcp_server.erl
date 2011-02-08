%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% @doc its a TCP Server. Its going to handle streaming data (mostly numeric)
%%% @end
%%% This version build from Erlang and OTP in Action example
%%%---------------------------------------------------------------------------------------

-module(tcp_server).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([start_link/0, start_link/1, get_count/0, stop/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

%%%---------------------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------------------

init([Port]) ->
	_Send_incoming_TCP_data_directly_to_process_as_messages = {active, true},
	{ok, LSock} = gen_tcp:listen(Port, [_Send_incoming_TCP_data_directly_to_process_as_messages]),
	{ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
	{reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	do_rpc(Socket, RawData),
	RequestCount = State#state.request_count,
	{noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
	{ok, _Sock} = gen_tcp:accept(LSock),
	{noreply, State}. 

terminate(_Reason, State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.


%%%---------------------------------------------------------------------------------------
%%% implementation of this modules API
%%%---------------------------------------------------------------------------------------

%%----------------------------------------------------------------------------------------
%% @doc Starts server
%%
%% @spec start_link(Port::Integer()) -> {ok, Pid}
%%		where
%%			Pid = pid()
%% @end
%%----------------------------------------------------------------------------------------

start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
	start_link(?DEFAULT_PORT).


%%----------------------------------------------------------------------------------------
%% @doc Get number of requests made to this server
%%
%% @spec get_count() -> {ok, Count}
%%		where
%%			Count = integer()
%% @end
%%----------------------------------------------------------------------------------------

get_count() ->
	gen_server:call(?SERVER, get_count).


%%----------------------------------------------------------------------------------------
%% @doc Stops this server
%%
%% @spec stop() -> ok
%% @end
%%----------------------------------------------------------------------------------------

stop() ->
	gen_server:cast(?SERVER, stop).

%%%---------------------------------------------------------------------------------------
%%% Internal functions
%%%---------------------------------------------------------------------------------------

do_rpc(Socket, RawData) ->
	try
		{M, F, A} = split_out_mfa(RawData),
		Result = apply(M, F, A),
		gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
	catch
		_Class:Err ->
			gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))

	end.

split_out_mfa(RawData) ->
	MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
	{match, [M, F, A]} = 
		re:run(MFA,
		       "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
		       [{capture, [1,2,3], list}, ungreedy]),
		       {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.


args_to_terms(RawArgs) ->
	{ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
	{ok, Args} = erl_parse:parse_term(Toks),
	Args.
