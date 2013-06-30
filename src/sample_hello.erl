-module(sample_hello).

-export([start_server/1, app/1]).

-include("http_status.hrl").

start_server(Port) ->
    psycho_server:start(Port, ?MODULE).

app(_Env) ->
    {?status_ok, [], "hello!"}.
