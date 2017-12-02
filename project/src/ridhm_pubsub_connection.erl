-module(ridhm_pubsub_connection).
-behaviour(gen_server).

-define(CONNTIMEOUT, 100000).

-export([start/2, start_link/2]).
-export([stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([timer_status/1]).

-record(state, {
    user_id,
    user_data,
    publishers,
    subscribers,
    transport, 
    transport_state,
    buffer,
    timer
}).

start_link(From, Type) ->
    Opts = [],
    gen_server:start_link(?MODULE, [From, Type], Opts).

start(From, Type) ->
    Opts = [],
    gen_server:start(?MODULE, [From, Type], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([From, intermittent]) ->
    process_flag(trap_exit, true),
    {ok, #state{publishers = dict:new(), 
                    subscribers = dict:new(),
                    transport = From,
                    user_id = anonymous,
                    transport_state = temporary,
                    buffer = [],
                    timer = erlang:start_timer(?CONNTIMEOUT, self(), trigger)
                }};

init([From, permanent]) ->
    process_flag(trap_exit, true),
    {ok, #state{publishers = dict:new(),
                    subscribers = dict:new(),
                    transport = From,
                    user_id = anonymous,
                    transport_state = temporary,
                    buffer = [],
                    timer = undefined
            }}.

handle_cast({process_message, Message}, State = #state{timer = Timer, transport_state = TS}) ->
    Timer2 = case TS of
                permanent -> undefined;
                _ ->
                    reset_timer(Timer)
            end,
    StateNew = try jsx:decode(Message) of
                    Msg ->
                        process_message(lists:keysort(1, Msg), State)
        catch _:_ ->
            self() ! {just_send, <<"non json message received">>},
            State
        end,
    {noreply, StateNew#state{timer = Timer2}};
handle_cast({keep_alive, From}, State = #state{timer = Timer, buffer = Buffer}) ->
    Timer2 = reset_timer(Timer),
    case Buffer of
        [] -> ok;
        Msgs -> flush_buffer(From, Msgs)
    end,
    {noreply, State#state{timer = Timer2, transport = From, transport_state = temporary, buffer = []}};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_info(transport_hiatus, State) ->
    {noreply, State#state{transport_state = hiatus}};
handle_info({'DOWN', _Ref, process, _Pid, _}, State) ->
    {stop, shutdown, State};

handle_info(timeout, State) ->
    {stop, shutdown, State};
handle_info({timeout, _TRef, _TMesg}, State) ->
    {stop, shutdown, State};
handle_info({presence_response, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, Msg, Buffer, TState),
    {noreply, State#state{buffer = NewBuffer}};
handle_info({received_message, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, Msg, Buffer, TState),
    {noreply, State#state{buffer = NewBuffer}};
handle_info({just_send, Msg}, State = #state{transport = Transport, buffer = Buffer, transport_state = TState}) ->
    NewBuffer = send_transport(Transport, jsx:encode([{<<"type">>, <<"info">>},
                                                        {<<"payload">>, Msg}]), Buffer, TState),
    {noreply, State#state{buffer = NewBuffer}};
handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_message([{<<"subscribe">>, Channel}], State = #state{subscribers = Subscribers, user_id = UserId}) ->
    NewSubs = case dict:find(Channel, Subscribers) of
                error ->
                    {ok, SubscriberPid} = ridhm_pubsub_subscriber_sup:start_child([Channel, UserId, self()]),
                    case ridhm_pubsub_subscriber:subscribe(SubscriberPid) of
                        ok -> dict:store(Channel, SubscriberPid, Subscribers);
                        {error, Error} -> self() ! {just_send, Error},
                                            Subscribers 
                    end;
                {ok, _} -> Subscribers
                end,
    State#state{subscribers = NewSubs};
process_message([{<<"channel">>, Channel}, {<<"publish">>, Message}], State = #state{publishers = Pubs, user_id = UserId, user_data = UserData}) ->
    CompleteMessage = jsx:encode([
                                    {<<"type">>, <<"message">>},
                                    {<<"message">>, Message},
                                    {<<"channel">>, Channel},
                                    {<<"user_id">>, UserId},
                                    {<<"user_data">>, UserData}
                                ]),
    NewPubs = case dict:find(Channel, Pubs) of
                        {ok, PublisherPid} ->
                            publish(PublisherPid, CompleteMessage),
                            Pubs;
                        error ->
                            {ok, PublisherPid} = ridhm_pubsub_publisher_sup:start_child([Channel, UserId, self()]),
                            publish(PublisherPid, CompleteMessage),
                            dict:store(Channel, PublisherPid, Pubs)
            end,
    State#state{publishers = NewPubs};
process_message([{<<"authenticate">>, AssumedUserId}, {<<"token">>, Token}], State = #state{user_id = anonymous}) ->
    case application:get_env(ridhm_pubsub, authenticate_url) of
        undefined -> self() ! {just_send, <<"Authentication Error: no authenticate url in config">>},
                    State;
        {ok, AuthenticateUrl} -> try_authenticate(AuthenticateUrl, AssumedUserId, Token, State)
    end;
process_message([{<<"authenticate">>, _AssumedUserId}, {<<"token">>, _Token}], State = #state{user_id = _UserId}) ->
    self ! {just_send, <<"Authenticated error: Already authenticated">>},
    State;

process_message([{<<"presence">>, Channel}], State = #state{subscribers = Subs}) ->
    case application:get_env(ridhm_pubsub, presence) of
        {ok, true} ->
            case dict:find(Channel, Subs) of
                error -> self() ! {just_send, <<"Cannot ask for presence when not subscribed to channel">>};
                {ok, _} ->
                    UserSubs = ridhm_pubsub_presence:presence(Channel),
                    self() ! {presence_response, jsx:encode([
                        {<<"type">>, <<"presence">>},
                        {<<"subscribers">>, UserSubs},
                        {<<"channel">>, Channel}
                    ])}
            end;
        _ ->
            self() ! {just_send, <<"presence is disabled">>}
        end,
        State;

process_message(_Message, State) ->
    self() ! {just_send, <<"unknown message received.">>},
    State.

try_authenticate(AuthenticateUrl, AssumedUserId, Token, State = #state{subscribers = Subs, publishers = Pubs}) ->
    case httpc:request(post, {AuthenticateUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(AssumedUserId)++"&token="++binary_to_list(Token)}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {UserId, UserData} = try lists:keysort(1, jsx:decode(binary:list_to_bin(Body))) of
                                        [{<<"authenticated">>, true}, {<<"user-data">>, ResUserData}] ->
                                            self() ! {just_send, <<"authenticated">>},
                                            update_users(AssumedUserId, Subs, Pubs),
                                            {AssumedUserId, ResUserData};
                                        _ ->
                                            self() ! {just_send, <<"Authentication Failed">>},
                                            {undefined, undefined}
                                    catch _:_ ->
                                            self() ! {just_send, <<"Authentication Error: Bad JSON response from server">>},
                                            {undefined, undefined}
                                end,
        State#state{user_id = UserId, user_data = UserData};
    _ -> self() ! {just_send, <<"Authentication Error:Bad Response from json">>},
        State
end.
        

update_users(UserId, Subs, Pubs) ->
    dict:map(fun(_Channel, Sub) ->
        ridhm_pubsub_subscriber:update_user(Sub, UserId) end, Subs),
    dict:map(fun(_Channel, Pub) ->
        ridhm_pubsub_subscriber:update_user(Pub, UserId) end, Pubs).

publish(PublisherPid, CompleteMessage) ->
    case ridhm_pubsub_publisher:publisher(PublisherPid, CompleteMessage) of
        ok -> ok;
        {error, Error} -> self() ! {just_send, Error}
    end.

reset_timer(Timer) ->
    case Timer of
        undefined ->
            undefined;
        TimerRef -> 
                case catch erlang:cancel_time(TimerRef) of
                    false ->
                        erlang:start_timer(?CONNTIMEOUT, self(), trigger);
                    _ ->
                        erlang:start_timer(?CONNTIMEOUT, self(), trigger)
                end 
    end.

send_transport(Transport, Msg, [], permanent) ->
    Transport ! {text, Msg};
send_transport(Transport, Msg, Buffer, temporary) ->
    flush_buffer(Transport, Buffer ++ [Msg]),
    [];
send_transport(_Transport, Msg, Buffer, hiatus) ->
    Buffer ++ [Msg].

flush_buffer(Transport, Msgs) ->
    Transport ! {list, Msgs}.

timer_status(#state{timer = Timer}) ->
    erlang:read_timer(Timer).