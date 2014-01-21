-module(psycho).

-export([call_app/2, priv_dir/0,
         env_val/2, env_val/3,
         env_header/2, env_header/3]).

call_app(M, Env) when is_atom(M) -> M:app(Env);
call_app({M, F}, Env) -> M:F(Env);
call_app({M, F, A}, Env) -> erlang:apply(M, F, [A ++ [Env]]);
call_app(Fun, Env) when is_function(Fun) -> Fun(Env).

priv_dir() ->
    priv_dir(code:which(?MODULE)).

priv_dir(BeamFile) when is_list(BeamFile) ->
    filename:join([filename:dirname(BeamFile), "..", "priv"]);
priv_dir(Other) ->
    error({psycho_priv_dir, Other}).

env_val(Name, Env) ->
    env_val(Name, Env, undefined).

env_val(Name, Env, Default) ->
    case lists:keyfind(Name, 1, Env) of
        {_, Value} -> Value;
        _ -> Default
    end.

env_header(Name, Env) ->
    env_header(Name, Env, undefined).

env_header(Name, Env, Default) ->
    Headers = env_val(http_headers, Env, []),
    case lists:keyfind(Name, 1, Headers) of
        {_, Value} -> Value;
        _ -> Default
    end.
