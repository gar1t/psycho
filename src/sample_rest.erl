%% sample_rest
%% 
%% This module illustrates a simple approach to building RESTful apps.
%% The only novelty here is the use of `psycho_util:dispatch_app` to
%% split the inbound Env into parts that are useful for dispatching.
%% In this case we're dispatching on method, path, and env - which
%% creates a reasonably declarative mapping between requests and
%% method handlers.

-module(sample_rest).

-export([start_server/1]).

%%===================================================================
%% Start / init
%%===================================================================

start_server(Port) ->
    psycho_server:start(Port, create_app()).

create_app() ->
    psycho_util:dispatch_app(fun handle/3, [method, path, env]).

%%===================================================================
%% Main handler
%%===================================================================

handle("GET", Path, Env)    -> handle_get(Path, Env);
handle("PUT", Path, Env)    -> handle_put(Path, Env);
handle(Method, _Path, _Env) -> bad_method(Method).

%%===================================================================
%% Method specific handlers
%%===================================================================

handle_get(Path, _Env) -> ok(["GET ", Path]).

handle_put(Path, _Env) -> ok(["PUT ", Path]).

%%===================================================================
%% Response helpers
%%===================================================================

ok(Text) ->
    {{200, "OK"}, [{"Content-Type", "text/plain"}], Text}.

bad_method(Method) ->
    Msg = ["Bad method ", Method],
    {{400, "Bad Request"}, [{"Content-Type", "text/plain"}], Msg}.
