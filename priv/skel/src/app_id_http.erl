-module(app_id_http).

-export([start_link/0, app/1]).

-define(PORT, 8080).

start_link() ->
    psycho_server:start(?PORT, ?MODULE).

app(_Env) ->
    {{200, "OK"}, [{"Content-Type", "text/plain"}], "Hello app_id"}.
