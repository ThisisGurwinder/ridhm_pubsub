-module(ridhm_pubsub_subscriber).
-behaviour(gen_server).

-export([start/3, start_link/3]).
-export([stop/1]).
-export([subscribe/1, update_user/2]).
-export([handle_cast/2, handle_call/3, handle_info/2, init/1, terminate/2, code_change/3]).

-record(state, { channel, reply_pid, already_authorized, user_id }).

start_link(Channel, UserId, ReplyPid) ->
    Opts = [],
    gen_server:start_link(?MODULE, [Channel, UserId, ReplyPid], Opts).

start(Channel, UserId, ReplyPid) ->
    Opts = [],
    gen_server:start(?MODULE, [Channel, UserId, ReplyPid], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([Channel, UserId, ReplyPid]) ->
    erlang:monitor(process, ReplyPid),
    {ok, #state{channel = Channel, user_id = UserId, reply_pid = ReplyPid}}.

handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, normal, State};
handle_info({received_message, Msg, channel, _Channel}, State = #state{reply_pid = ReplyPid}) ->
    ReplyPid ! {received_message, Msg},
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, normal, State}.

handle_call(subscribe, _From, State = #state{channel = Channel, user_id = UserId}) ->
    Res = maybe_subscribe(Channel, UserId),
    {reply, Res, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({update_user, UserId}, State) ->
    {ok, State#state{user_id = UserId}};
handle_cast(_Message, State) ->
    {ok, State}.

terminate(_Reason, #state{channel = Channel, user_id = UserId}) ->
    ok = gen_server:cast(ridhm_pubsub_router, {unsubscribe, Channel, from, self(), user_id, UserId}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

subscribe(SubscriberPid) -> gen_server:cast(SubscriberPid, subscribe).

update_user(SubscriberPid, UserId) -> gen_server:cast(SubscriberPid, {update_user, UserId}).

maybe_subscribe(UserId, Channel) ->
    case can_subscribe(UserId, Channel) of
        true -> subscribe_in_router(UserId, Channel),
                ok;
        Error -> {error, Error}
end.

can_subscribe(UserId, Channel) ->
    case application:get_env(ridhm_pubsub, subscribe_authorization) of
            undefined -> true;
            {ok, AuthConfig} -> ridhm_pubsub_authorization:check_authorization(UserId, Channel, AuthConfig)
end.

subscribe_in_router(UserId, Channel) ->
    ok = gen_server:cast(ridhm_pubsub_router, {subscribe, Channel, from, self(), user_id, UserId}).