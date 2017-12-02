-module(api_presence_handler).

-export([init/3]).
-export([terminate/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([presence_to_json/2]).

init(_Type, Req, _Opts) ->
    {{IP, _Port}, _} = cowboy_req:peer(Req),
    case ridhm_pubsub_ip_authorization:authorize(IP) of
        true -> {upgrade, protocol, cowboy_rest};
        false ->
            {ok, Req2} = cowboy_req:reply(500, [
                {<<"content-type">>, <<"text/plain">>}
            ], "You are not authorized to access this endpoint. Check your configuration.", [Req]),
            {shutdown, Req2, no_state}
end.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, presence_to_json}
        ], Req, State}.

presence_to_json(Req, Channel) ->
    UsersSub = ridhm_pubsub_presence:presence(Channel),
    Body = jsx:encode(UsersSub),
    {Body, Req, Channel}.

resource_exists(Req, _State) ->
    {Channel, _Bin} = cowboy_req:binding(channel, Req),
    {true, Req, Channel}.

terminate(_Reason, _Req, _State) ->
    ok.