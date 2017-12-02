-module(rabbitmq_exchange_sup).

-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Channel) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Channel]).

init([Channel]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        {rabbitmq_exchange,
        {rabbitmq_exchange, start_link, [Channel]},
        temporary,
        infinity,
        worker,
        [rabbitmq_exchange] 
        } ]} }.