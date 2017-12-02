-module(ridhm_pubsub_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [
                {"/", ridhm_pubsub_handler, []}
            ]}
        ]),
        {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
            env => #{dispatch => Dispatch}
        }),
        ridhm_pubsub_sup:start_link().

stop(_State) ->
    ok.
