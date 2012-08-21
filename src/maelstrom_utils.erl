%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Convenience interface module to whirlpool - the most common
%%%            use case would be to map a function over a list. This module
%%%            provides a map/2 and map/3.
%%%            
%%%            map/2 is a drop-in replacement for stdlib map/2 and will map
%%%            a given fun over a list of elements using maelstrom's worker
%%%            pool. NOTE: the pool must be started first!
%%%
%%%            If a worker crashes it will be restarted - but any data
%%%            passed to it cannot be recovered.
%%%            
%%% @end

-module(maelstrom_utils).
-export([map/2]).

%%----------------------------------------------------------------------
%% @doc  Returns the worker pool limit and the number of unused workers.
%% 
%% @spec workers() -> {{total, Integer}, {unused, Integer}}
%% where
%%  Integer = integer()
%% @end
%%----------------------------------------------------------------------

-spec map(Fun::fun(), Items::list()) -> list().
map(Fun, Items) ->
    map(Fun, Items, undefined).

collector(Parent, Expected, Acc) ->
    case length(Acc) < Expected of
        true  ->
            receive
                {done, Payload, Args} ->
                    collector(Parent, Expected, [{Payload, Args}|Acc]);
                send_payload ->
                    Parent ! {collector, Acc}
            end;
        false ->
            Parent ! {collector, Acc}
    end.

-spec map(Fun::fun(), list(), Acc::list()) -> no_return().
map(Fun, [H|T], Coll) ->
    
    %% Build the collector process
    Self = self(),
    {collector, Collector} = make_collector(Self, length(T)+1, Coll),
    
    %% Spawn a proxy-process to make it async (could use cast but I don't like that use of it)
    whirlpool:async_compute({Fun, [H]}, Collector),
    
    map(Fun, T, Collector);

map(Fun, [], Coll) ->
    receive
        {collector, Values} ->
            Values;
        {'DOWN', _MonitorRef, _Type, _Object, Info} ->
            lager:error("~p", [Info]),
            Coll ! send_payload,
            map(Fun, [], Coll)
    after
        600000 ->
            exit(took_too_long)
    end.

make_collector(Parent, Length, Coll) ->
    case Coll of
        undefined ->
            {collector, spawn_link(fun() -> collector(Parent, Length, []) end)};
        _Else     ->
            {collector, Coll}
    end.
