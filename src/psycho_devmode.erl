-module(psycho_devmode).

-export([start/0]).

start() ->
    application:start(sasl),
    psycho_reloader:start().
