-module(ridhm_pubsub_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
        {ok, { {one_for_one, 5, 10}, [
                                        {ridhm_pubsub_broker_sup,
                                        {ridhm_pubsub_broker_sup, start_link, []},
                                        permanent,
                                        infinity,
                                        supervisor,
                                        [ridhm_pubsub_broker_sup]
                                        },
                                        {ridhm_pubsub_connection_sup,
                                        {ridhm_pubsub_connection_sup, start_link, []},
                                        permanent,
                                        infinity,
                                        supervisor,
                                        [ridhm_pubsub_connection_sup] 
                                        },
                                        {ridhm_pubsub_publisher_sup,
                                        {ridhm_pubsub_publisher_sup, start_link, []},
                                        temporary,
                                        infinity,
                                        supervisor,
                                        [ridhm_pubsub_publisher_sup] 
                                        },
                                        {ridhm_pubsub_subscriber_sup,
                                        {ridhm_pubsub_subscriber_sup, start_link, []},
                                        temporary,
                                        infinity,
                                        supervisor,
                                        [ridhm_pubsub_subscriber_sup] 
                                        },
                                        {ridhm_pubsub_http,
                                        {ridhm_pubsub_http, start_link, []},
                                        permanent,
                                        infinity,
                                        worker,
                                        [ridhm_pubsub_http] 
                                        },
                                        {ridhm_pubsub_router,
                                        {ridhm_pubsub_router, start_link, []},
                                        permanent,
                                        infinity,
                                        worker,
                                        [ridhm_pubsub_router] 
                                        },
                                        {ridhm_pubsub_presence,
                                        {ridhm_pubsub_presence, start_link, []},
                                        permanent,
                                        infinity,
                                        supervisor,
                                        [ridhm_pubsub_presence] 
                                        },
                                        {ridhm_pubsub_admin_connection,
                                        {ridhm_pubsub_admin_connection, start_link, []},
                                        permanent,
                                        infinity,
                                        worker,
                                        [ridhm_pubsub_admin_connection] 
                                        }
                                        ]} }.