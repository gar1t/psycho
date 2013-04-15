-module(sample_hello).

-export([app/1]).

-include("http_status.hrl").

app(_Env) ->
    {?status_ok, [], "hello!"}.
