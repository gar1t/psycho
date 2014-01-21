-module(app_id).

-export([start/0, restart/0]).

start() ->
    e2_application:start_with_dependencies(app_id).

restart() ->
    application:stop(app_id),
    application:start(app_id),
    e2_log:info("app_id restarted").
