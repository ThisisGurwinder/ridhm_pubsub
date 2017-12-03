-module(ridhm_pubsub_presence).
-behaviour(gen_server).

-export([start_link/0]).
-export([stop/1]).
-export([presence/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).


start_link() ->
    Opts = [],
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], Opts).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

init([]) ->
    {ok, ok}.

handle_info(shutdown, State) ->
    {ok, shutdown, State}.

handle_call({presence, Channel}, _From, State) ->
    {UsersDup, _} = rpc:multicall(ridhm_pubsub_router, local_presence, [Channel]),
    UsersSub = sets:to_list(sets:from_list(lists:append(UsersDup))),
    UsersSub2 = lists:delete(anonymous, UsersSub),
    UsersSub3 = lists:delete(server, UsersSub2),
    {reply, UsersSub3, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

presence(Channel) ->
    gen_server:call(?MODULE, {presence, Channel}).