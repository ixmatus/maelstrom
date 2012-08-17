%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Wrapper on poolboy to create a pool instance and provide a
%%%            computing function.
%%% @end
%%%-------------------------------------------------------------------------
-module(whirlpool).

-export([compute/1, async_compute/1]).

%%----------------------------------------------------------------------
%% @doc  Given a tuple of {Module, Fun, Args} | {Fun, Args} this function
%%       will compute the given args with the given fun (or mod:fun) and
%%       return the results.
%% 
%% @spec compute(Payload::tuple()) -> any()
%% @end
%%----------------------------------------------------------------------
-spec compute(Payload::tuple()) -> any().
compute(Payload) ->
    poolboy:transaction(pool1, fun(Worker) ->
        gen_server:call(Worker, {compute, Payload}, 20000)
    end).

%%----------------------------------------------------------------------
%% @doc  Given a tuple of {Module, Fun, Args} | {Fun, Args} this function
%%       will compute the given args with the given fun (or mod:fun).
%%       
%%       Unlike its synchronous sibling, this requires a receiver PID to
%%       which the result of the computation can be sent (as a message).
%% 
%% @spec async_compute(Payload::tuple(), Receiver::pid()) -> any()
%% @end
%%----------------------------------------------------------------------
-spec async_compute(Payload::tuple()) -> any().
async_compute(Payload) ->
    poolboy:transaction(pool1, fun(Worker) ->
        gen_server:cast(Worker, {compute, Payload})
    end).
