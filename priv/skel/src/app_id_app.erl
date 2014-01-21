-module(app_id_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, [app_id_http]}.
