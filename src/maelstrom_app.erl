%%%-------------------------------------------------------------------------
%%% @author    Parnell Springmeyer <ixmatus@gmail.com>
%%% @copyright 2012 Parnell Springmeyer
%%% @doc       Interface for application:start/stop/takeover/&c...
%%% @end
%%%-------------------------------------------------------------------------

-module(maelstrom_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case maelstrom_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.
