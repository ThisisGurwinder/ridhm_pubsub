-module(ridhm_pubsub_ip_authorization).
-export([authorize/1]).
-include_lib("eunit/include/eunit.hrl").

authorize(Ip) ->
    case application:get_env(ridhm_pubsub, restrict_ip_access_to) of
        undefined -> true;
        {ok, Ip} -> true;
        _someIp -> false
end.