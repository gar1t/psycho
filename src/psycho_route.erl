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
    Method = psycho:env_val(request_method, Env),
    dispatch(Routes, Method, Path, Env, Options).

dispatch([{Route, App}|Rest], Method, Path, Env, Options) ->
    maybe_dispatch(
      path_matches(Route, Path),
      App, Rest, Method, Path, Env, Options);
dispatch([{Method, Route, App}|Rest], Method, Path, Env, Options) ->
    maybe_dispatch(
      path_matches(Route, Path),
      App, Rest, Method, Path, Env, Options);
dispatch([Invalid|_], _Method, _Path, _Env, _Options) ->
    error({invald_route, Invalid});
dispatch([], _Method, _Path, Env, Options) ->
    (not_found_handler(Options))(Env).

path_matches(Path, Path) -> true;
path_matches({starts_with, Prefix}, Path) -> starts_with(Prefix, Path);
path_matches({matches, Regex}, Path) -> regex_matches(Regex, Path);
path_matches({exact, Path}, Path) -> true;
path_matches('_', _) -> true;
path_matches(_, _) -> false.

starts_with([Char|RestPrefix], [Char|RestStr]) ->
    starts_with(RestPrefix, RestStr);
starts_with([], _Str) -> true;
starts_with(_, _) -> false.

regex_matches(Regex, Str) ->
    match == re:run(Str, Regex, [{capture, none}]).

maybe_dispatch(true, App, _Rest, _Method, _Path, Env, _Options) ->
    App(Env);
maybe_dispatch(false, _App, Rest, Method, Path, Env, Options) ->
    dispatch(Rest, Method, Path, Env, Options).

not_found_handler(Options) ->
    proplists:get_value(not_found_handler, Options, fun default_not_found/1).

default_not_found(_Env) ->
    {{404, "Not Found"}, [{"Content-Type", "text/plain"}], "Not Found"}.
