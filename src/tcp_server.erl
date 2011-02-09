%%%---------------------------------------------------------------------------------------
%%% @author James Pott <neurofen@gmail.com>
%%%
%%% @doc its a TCP Server. Its going to handle streaming data (mostly numeric)
%%% @end
%%% This version build from Erlang and OTP in Action example
%%%---------------------------------------------------------------------------------------

-module(tcp_server).
-behaviour(gen_server).
-import(rpc_utils, [do_rpc/2]).

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
%% @spec start_link(Port::integer()) -> {ok, Pid}
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


