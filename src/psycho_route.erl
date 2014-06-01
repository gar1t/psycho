-module(psycho_route).

-export([create_app/1, create_app/2, route/2, route/3,
         dispatch_app/1, dispatch_app/2,
         default_not_found_app/0]).

%%%===================================================================
%%% Route app
%%%===================================================================

create_app(Routes) ->
    create_app(Routes, []).

create_app(Routes, Options) ->
    fun(Env) -> route(Routes, Env, Options) end.

route(Routes, Env) ->
    route(Routes, Env, []).

route(Routes, Env0, Options) ->
    {{Path, _, _}, Env} = psycho_util:ensure_parsed_request_path(Env0),
    Method = psycho:env_val(request_method, Env),
    dispatch(Routes, Method, Path, Env, Options).

dispatch([{Route, App}|Rest], Method, Path, Env, Options) ->
    maybe_dispatch(
      path_matches(Route, Path),
      App, Rest, Method, Path, Env, Options);
dispatch([{RouteMethod, Route, App}|Rest], Method, Path, Env, Options) ->
    maybe_dispatch(
      RouteMethod =:= Method andalso path_matches(Route, Path),
      App, Rest, Method, Path, Env, Options);
dispatch([Invalid|_], _Method, _Path, _Env, _Options) ->
    error({invald_route, Invalid});
dispatch([], _Method, _Path, Env, Options) ->
    psycho:call_app(not_found_handler(Options), Env).

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
    psycho:call_app(App, Env);
maybe_dispatch(false, _App, Rest, Method, Path, Env, Options) ->
    dispatch(Rest, Method, Path, Env, Options).

not_found_handler(Options) ->
    proplists:get_value(not_found_handler, Options, fun default_not_found/1).

%%%===================================================================
%%% Default page handlers
%%%===================================================================

default_not_found_app() ->
    fun(Env) -> default_not_found(Env) end.

default_not_found(_Env) ->
    {{404, "Not Found"}, [{"Content-Type", "text/plain"}], "Not Found"}.

%%%===================================================================
%%% Dispatch app
%%%===================================================================

dispatch_app(Spec) ->
    dispatch_app(Spec, []).

dispatch_app(Spec, Options) ->
    fun(Env) -> dispatch(app_for_path(Spec, Options, Env), Options) end.

app_for_path({module, Prefix, Suffix}, _Options, Env) ->
    mod_for_path(Env, Prefix, Suffix).

mod_for_path(Env0, Prefix, Suffix) ->
    {{Path, _, _}, Env} = psycho_util:ensure_parsed_request_path(Env0),
    MaybeMod = try_mod_for_parts([Prefix, path_to_mod_part(Path), Suffix]),
    {MaybeMod, Env}.

path_to_mod_part("/" ++ Path) ->
    path_to_mod_part(Path);
path_to_mod_part("") ->
    "index";
path_to_mod_part(Path) ->
    replace(Path, $/, $_, []).

replace([Replace|Rest], Replace, With, Acc) ->
    replace(Rest, Replace, With, [With|Acc]);
replace([C|Rest], Replace, With, Acc) ->
    replace(Rest, Replace, With, [C|Acc]);
replace([], _Replace, _With, Acc) ->
    lists:reverse(Acc).

try_mod_for_parts(Parts) ->
    Flattened = lists:flatten(Parts),
    try list_to_existing_atom(Flattened) of
        A -> {ok, A}
    catch
        error:badarg -> error
    end.

dispatch({{ok, App}, Env}, _Options) ->
    psycho:call_app(App, Env);
dispatch({error, Env}, Options) ->
    psycho:call_app(not_found_handler(Options), Env).
