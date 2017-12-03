-module(ridhm_pubsub_util).
-export([start_app_deps/1]).

-spec start_app_deps(App :: atom()) -> StartedApps :: list().
start_app_deps(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            start_app_deps([Dep | [App]], []);
        {error, {Reason, _}} ->
            [{Reason, App}];
        ok ->
            [{ok, App}]
end.

start_app_deps([], Acc) ->
    Acc;
start_app_deps([H | T] = L, Acc) ->
    case application:start(H) of
        {error, {not_started, Dep}} ->
            start_app_deps([Dep | L], Acc);
        {error, {Reason, _}} ->
            start_app_deps(T, [{Reason, H} | Acc]);
        ok ->
            start_app_deps(T, [{ok, H} | Acc])
end.