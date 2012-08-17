%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Poolboy worker instance. Provides a very simple factory worker
%%%            to handle computations sent with a MFA or FA arg set.
%%% @end
%%%-------------------------------------------------------------------------
-module(whirlpool_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init([]) ->
    {ok, undefined}.

handle_call({compute, {F, A}}, _From, State) ->
    {reply, erlang:apply(F, A), State};

handle_call({compute, {M, F, A}}, _From, State) ->
    {reply, erlang:apply(M, F, A), State}.

handle_cast({compute, Reply, {F, A}}, State) ->
    Result = erlang:apply(F, A),
    Reply ! {done, Result, A},
    {noreply, State};

handle_cast({compute, Reply, {M, F, A}}, State) ->
    Result = erlang:apply(M, F, A),
    Reply ! {done, Result, A},
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
