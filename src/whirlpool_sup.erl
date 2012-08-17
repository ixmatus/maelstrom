%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Primary supervisor for the whirlpool server and worker supervisor.
%%% @end
%%%-------------------------------------------------------------------------

-module(whirlpool_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    PoolSpec = {whirlpool,
        {whirlpool, start_link, []}, permanent, 2000, worker, [whirlpool]},
    
    WorkerSupSpec = {whirlpool_worker_sup,
        {whirlpool_worker_sup, start_link, []}, permanent, 2000, supervisor, [ml_worker_supervisor]},
    
    {ok, {{one_for_one, 5, 10}, [PoolSpec, WorkerSupSpec]}}.
