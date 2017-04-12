-module(psycho).

-export([call_app/2, call_app_with_data/3,
         priv_dir/0,
         env/2, env/3,
         env_val/2, env_val/3, set_env/3,
         env_header/2, env_header/3,
         parsed_request_path/1]).

call_app(M, Env) when is_atom(M) ->
    erlang:apply(M, app, [Env]);
call_app({M, F}, Env) when is_atom(M) ->
    erlang:apply(M, F, [Env]);
call_app({M, F, A}, Env) when is_atom(M) ->
    erlang:apply(M, F, A ++ [Env]);
call_app(F, Env) when is_function(F) ->
    erlang:apply(F, [Env]);
call_app({F, A}, Env) when is_function(F) ->
    erlang:apply(F, A ++ [Env]).

call_app_with_data(M, Env, Data) when is_atom(M) ->
    erlang:apply(M, app, [Data, Env]);
call_app_with_data({M, F}, Env, Data) when is_atom(M) ->
    erlang:apply(M, F, [Data, Env]);
call_app_with_data({M, F, A}, Env, Data) when is_atom(M) ->
    erlang:apply(M, F, A ++ [Data, Env]);
call_app_with_data(F, Env, Data) when is_function(F) ->
    erlang:apply(F, [Data, Env]);
call_app_with_data({F, A}, Env, Data) when is_function(F) ->
    erlang:apply(F, A ++ [Data, Env]).

priv_dir() ->
    priv_dir(code:which(?MODULE)).

priv_dir(BeamFile) when is_list(BeamFile) ->
    filename:join([filename:dirname(BeamFile), "..", "priv"]);
priv_dir(Other) ->
    error({psycho_priv_dir, Other}).

env(Name, Env) ->
    env(Name, Env, undefined).

env(Name, Env, Default) ->
    case lists:keyfind(Name, 1, Env) of
        {_, Value} -> Value;
        _ -> Default
    end.

env_val(Name, Env) -> env(Name, Env).

env_val(Name, Env, Default) -> env(Name, Env, Default).

set_env(Name, Value, Env) ->
    [{Name, Value}|lists:keydelete(Name, 1, Env)].

env_header(Name, Env) ->
    env_header(Name, Env, undefined).

env_header(Name, Env, Default) ->
    Headers = env_val(http_headers, Env, []),
    case lists:keyfind(Name, 1, Headers) of
        {_, Value} -> Value;
        _ -> Default
    end.

parsed_request_path(Env) ->
    handle_parsed_request_path(env_val(parsed_request_path, Env), Env).

handle_parsed_request_path(undefined, Env) ->
    psycho_util:parse_request_path(env_val(request_path, Env));
handle_parsed_request_path(Parsed, _Env) ->
    Parsed.
