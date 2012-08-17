%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Supervisor for the whirlpool workers.
%%% @end
%%%-------------------------------------------------------------------------

-module(whirlpool_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("../includes/maelstrom.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {value, Limit} = ?GET_ENV(whirlpool, limit, 5),
    
    ChildSpecs = [make_child(X) || X <- lists:seq(0, Limit-1)],
    
    {ok, {{one_for_one, 20, 60}, ChildSpecs}}.

make_child(Id) ->
    UID = ?FORMAT("worker_~w", [Id]),
    AUID = list_to_atom(UID),
    
    {AUID, {whirlpool_worker, start_link, [AUID]},
            transient, 2000, worker, [whirlpool_worker]}.
