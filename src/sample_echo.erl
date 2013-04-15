-module(sample_echo).

-export([app/1]).

app(Env) ->
    {{200, "OK"}, [{"Content-Type", "text/plain"}], echo_env(Env)}.

echo_env(Env) ->
    ["ENVIRONMENT:\n",
     io_lib:format("~p", [Env]), "\n"
    ].
