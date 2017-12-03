-module(ridhm_pubsub_publisher_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [
            {ridhm_pubsub_publisher,
                {ridhm_pubsub_publisher, start_link, []},
                temporary,
                infinity,
                worker,
                [ridhm_pubsub_publisher]
            } ]} }.