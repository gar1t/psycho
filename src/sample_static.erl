-module(sample_static).

-export([start_server/2]).

start_server(Port, Dir) ->
    psycho_server:start(Port, psycho_static:create_app(Dir)).
