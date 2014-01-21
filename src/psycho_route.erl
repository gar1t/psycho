-module(psycho_route).

-export([create_app/1, create_app/2, route/2, route/3]).

create_app(Routes) ->
    create_app(Routes, []).

create_app(Routes, Options) ->
    fun(Env) -> route(Env, Routes, Options) end.

route(Env, Routes) ->
    route(Env, Routes, []).

route(Env0, Routes, Options) ->
    {{Path, _, _}, Env} = psycho_util:ensure_parsed_request_path(Env0),
    dispatch(Routes, Path, Env, Options).

dispatch([{Path, App}|_], Path, Env, _Options) ->
    App(Env);
dispatch([{{starts_with, Str}, App}|Rest], Path, Env, Options) ->
    maybe_dispatch(starts_with(Str, Path), App, Rest, Path, Env, Options);
dispatch([{{matches, Regex}, App}|Rest], Path, Env, Options) ->
    maybe_dispatch(matches(Regex, Path), App, Rest, Path, Env, Options);
dispatch([{{exact, Path}, App}|_], Path, Env, _Options) ->
    App(Env);
dispatch([{'_', App}|_], _, Env, _Options) ->
    App(Env);
dispatch([_|Rest], Path, Env, Options) ->
    dispatch(Rest, Path, Env, Options);
dispatch([], _Path, Env, Options) ->
    (not_found_handler(Options))(Env).

starts_with([Char|RestPrefix], [Char|RestStr]) ->
    starts_with(RestPrefix, RestStr);
starts_with([], _Str) -> true;
starts_with(_, _) -> false.

matches(Regex, Str) ->
    match == re:run(Str, Regex, [{capture, none}]).

maybe_dispatch(true, App, _Rest, _Path, Env, _Options) ->
    App(Env);
maybe_dispatch(false, _App, Rest, Path, Env, Options) ->
    dispatch(Rest, Path, Env, Options).

not_found_handler(Options) ->
    proplists:get_value(not_found_handler, Options, fun default_not_found/1).

default_not_found(_Env) ->
    {{404, "Not Found"}, [{"Content-Type", "text/plain"}], "Not Found"}.
