-module(ridhm_pusub_subscriber_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_child({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        {ridhm_pubsub_subscriber,
            {ridhm_pubsub_subscriber, start_link, []},
            temporary,
            infinity,
            worker,
            [ridhm_pubsub_subscriber]
        } ]} }.