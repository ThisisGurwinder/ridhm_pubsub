-module(rabbitmq_exchange).
-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).
-export([start_link/1, start/1, stop/1, declare_exchange/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {channel, exchange}).

start_link(Channel) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Channel], Opts).

start(Channel) ->
    Opts = [],
    gen_server:start(?MODULE, [Channel], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

declare_exchange(ExchangeServ, ExchangeSpecs) ->
    gen_server:call(ExchangeServ, {declare_exchange, ExchangeSpecs}).

init([Channel]) ->
    {ok, #state{channel = Channel}}.

handle_call({declare_exchange, {Exchange, Type}}, _From, State = #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'exchange.declare'{exchange = Exchange,
                                                    type = Type,
                                                    durable = false,
                                                    passive = true,
                                                    interval = false
                                                }),
    {reply, ok, State#state{exchange = Exchange}};

handle_call({publish, Message}, _From, State = #state{channel = Channel, exchange = Exchange}) ->
    amqp_channel:cast(Channel,
                        #'basic.publish'{
                            exchange = Exchange,
                            routing_key = <<"">>},
                        #'amqp_msg'{payload = Message}),
    {reply, ok, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.