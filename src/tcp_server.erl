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
	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
	{ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
	{reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Info, State) ->
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

