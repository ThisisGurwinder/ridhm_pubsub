-module(ridhm_pubsub_http).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
    Opts = [],
    gen_server:start(?MODULE, [], Opts).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            % {"/signup", api_signup_handler, []},
            {"/api/channels/[:channel]", api_channels_handler, []},
            {"/api/channels/:channel/messages", api_messages_handler, []},
            {"/api/channels/:channel/presence", api_presence_handler, []},
            {"/stream", bullet_handler, [{handler, stream_handler}]}
        ]}
    ]),
    start_http(Dispatch),
    {ok, ok}.

handle_call(alloc, _From, State) ->
    {reply, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(shutdown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_http(Dispatch) ->
    Port = case application:get_env(ridhm_pubsub, port) of
                    undefined -> 8080;
                    {ok, ConfPort} -> ConfPort
            end,
    case application:get_env(ridhm_pubsub, ssl) of
        undefined ->
            {ok, _} = cowboy:start_http(http, 100, [
                {port, Port}
            ],
            [{env, [{dispatch, Dispatch}
                ]},
            {middlewares, [
                cowboy_router,
                ridhm_pubsub_cors,
                cowboy_handler
            ]}
        ]),
        io:format("HTTP server started listening at port ~p~n", [Port]);
    {ok, SSLConf} -> start_https(Dispatch, Port, SSLConf)
end.

start_https(Dispatch, Port, SSLConf) ->
    CertFile = case find_ssl_conf(certfile, SSLConf) of
                    error -> throw({ssl_config_error, "SSL Enabled but no certfile found"});
                    Value2 -> Value2
        end,
    KeyFile = case find_ssl_conf(keyfile, SSLConf) of
                    error -> throw({ssl_config_error, "SSL Enabled but no certfile found"});
                    Value3 -> Value3
    end,
    {ok, _} = cowboy:start_https(https, 100, [
                {port, Port},
                {certfile, CertFile},
                {keyfile, KeyFile}
            ],
            [{env, [{dispatch, Dispatch}
                ]},
            {middlewares,  [
                cowboy_router,
                ridhm_pubsub_cors,
                cowboy_handler
            ]}
        ]),
    io:format("HTTPS server started listening to port ~p~n", [Port]).

find_ssl_conf(_Options, []) -> error;
find_ssl_conf(Options, [{Options, Value} | _SSLConf]) -> Value;
find_ssl_conf(Options, [_Entry|SSLConf]) -> find_ssl_conf(Options, SSLConf).