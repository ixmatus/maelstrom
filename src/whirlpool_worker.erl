%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Whirlpool gen_server worker - handles the actual worker
%%%            instances: handing work off to them, accessing a worker's
%%%            status, and stopping a worker (only if necessary).
%%% @end
%%%-------------------------------------------------------------------------
-module(whirlpool_worker).
-behavior(gen_server).

%% API
-export([start_link/1, work/3, status/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id=undefined}).
-define(SERVER, ?MODULE).

%%%=====================================================================
%%% API
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc  Starts the worker.
%% 
%% @spec start_link(Id::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%----------------------------------------------------------------------
start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id], []).

%%----------------------------------------------------------------------
%% @doc  Give work to the worker to perform; a collector pid() and tuple
%%       of the form {Fun, Args} must be passed here. The collector
%%       pid() is the process that handles collecting a worker's results
%%       since workers are assigned work asynchronously.
%%
%% @spec work(WId::worker_id(), CPid::pid(), PeiceOfWork::{Fun::fun(), Args::list()} | {Mod::atom(), Fun::fun(), Args::list()}) -> ok
%% @end
%%----------------------------------------------------------------------
work(WId, CPid, PeiceOfWork) ->
    gen_server:cast(WId, {work, CPid, PeiceOfWork}).

%%----------------------------------------------------------------------
%% @doc  Retrieve the status of the worker. NOTE: this method presently
%%       does not return anything particularly useful. Although, it
%%       could once I find some data about the worker that I would need.
%% 
%% @spec status(WId::worker_id()) -> {ok, "Some sort of status response here"}
%%
%% @end
%%----------------------------------------------------------------------
status(WId) ->
    gen_server:call(WId, status).

%%----------------------------------------------------------------------
%% @doc  Stop the worker and remove it from the worker pool. The
%%       supervisor will not restart it.
%%
%% @spec stop(WId::worker_id()) -> ok
%% @end
%%----------------------------------------------------------------------
stop(WId) ->
    gen_server:cast(WId, stop).

%%%=====================================================================
%%% Private API
%%%=====================================================================
%%----------------------------------------------------------------------
%% @doc  Enqueues the worker into the worker pool inside the gen_server.
%%
%% @spec enqueue(Id::maelstrom_server_id()) -> ok
%% @end
%%----------------------------------------------------------------------
-spec enqueue(Id::atom()) -> atom().
enqueue(Id) ->
    whirlpool:enqueue(Id),
    ok.

%%%=====================================================================
%%% gen_server callbacks
%%%=====================================================================
init([Id]) ->
    lager:info("Starting up... ~p", [Id]),
    %% Once the worker is started up (by the whirlpool_worker_sup) it
    %% needs to enqueue itself into the worker pool that is kept in
    %% the state variable of the maelstrom gen_server (whirlpool).
    enqueue(Id),
    {ok, #state{id=Id}}.

%%----------------------------------------------------------------------
%% Callback handlers
%%----------------------------------------------------------------------
handle_call(status, _From, State) ->
    {reply, {ok, "Some sort of status response here"}, State}.

handle_cast({work, CPid, {F, A}}, State) ->
    Result = erlang:apply(F, A),
    finish_worker(CPid, A, Result, State#state.id),
    {noreply, State};

handle_cast({work, CPid, {M, F, A}}, State) ->
    Result = erlang:apply(M, F, A),
    finish_worker(CPid, A, Result, State#state.id),
    {noreply, State};

handle_cast(stop, State) ->
    whirlpool:dequeue(State#state.id),
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

finish_worker(PID, Args, Result, ID) ->
    PID ! {done, Args, Result},
    whirlpool:checkin(ID).
