%%----------------------------------------------------------------------
%% @doc  Get a setting set in the app.config file for the given app.
%%
%% @spec ?GET_ENV(App::atom(), Key::atom(), Default::any()) -> {value, any()}
%%----------------------------------------------------------------------
-define(GET_ENV, fun(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined   -> {value, Default};
        {ok, Value} -> {value, Value}
    end end).

%%----------------------------------------------------------------------
%% @doc  Format a string with args.
%%
%% @spec ?FORMAT(Str::string(), Args::list()) -> string()
%%----------------------------------------------------------------------
-define(FORMAT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
