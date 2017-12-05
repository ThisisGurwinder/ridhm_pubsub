-module(ridhm_pubsub_control).
-export([start/0, stop/0]).

start() ->
    OptSpecList =
        [
            {nodename, $n, "nodename", {string, undefined}, "Node Name"},
            {nodetojoin, $j, "node to join", {string, undefined}, "Node to Join"},
            {help, $h, undefined, undefined, "Show this help"}
    ],
    parse_arguments(OptSpecList).

stop() ->
    ok.

parse_arguments(OptSpecList) ->
    case getopt:parse(OptSpecList, init:get_plain_arguments()) of
        {ok, {ParsedArgs, ExtraArgs}} ->
            case lists:member(help, ParsedArgs) of
                false -> maybe_start(ParsedArgs, ExtraArgs);
                true -> getopt:usage(OptSpecList, "ridhm_pubsub_control")
            end;
        _ ->
            getopt:usage(OptSpecList, "ridhm_pubsub_control")
        end, 
        init:stop(0).

maybe_start(Args, Extra) ->
    case lists:member("start", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of
                        {nodename, Node} -> rpc:call(list_to_atom(Node), ridhm_pubsub_app, start, []);
                        false -> io:format("node name option required (run with -h for help)~n")
            end;
        false -> maybe_stop(Args, Extra)
end.

maybe_stop(Args, Extra) ->
    case lists:member("stop", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of
                    {nodename, Node} -> rpc:call(list_to_atom(Node), ridhm_pubsub_app, stop, []);
                    false -> io:format("node name option required (run with -h for help)~n")
        end;
        false -> maybe_join_cluster(Args,Extra)
    end.

maybe_join_cluster(Args, Extra) ->
    case lists:member("join_cluster", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of
                    {nodename, Node} ->
                        case lists:keyfind(nodetojoin, 1, Args) of
                            {nodetojoin, Node2} -> io:format("~p~n", [rpc:call(list_to_atom(Node), ridhm_pubsub_app, join_cluster, [list_to_atom(Node2)])]);
                            false -> io:format("nodetojoin option required (run with -h for help)~n")
                        end;
                    false -> io:format("nodename option required (run with -h for help)~n")
                    end;
        false -> maybe_cluster_status(Args, Extra)
end.

maybe_cluster_status(Args, Extra) ->
    case lists:member("cluster_status", Extra) of
        true -> case lists:keyfind(nodename, 1, Args) of
                        {nodename, Node} ->
                            io:format("~p~n", [rpc:call(list_to_atom(Node), ridhm_pubsub_app, cluster_status, [])]);
                        false ->
                            io:format("cluster status option required (run with -h for help)~n")
                end;
        false ->
            ok
end.