-module(psycho).

-export([priv_dir/0]).

priv_dir() ->
    priv_dir(code:which(?MODULE)).

priv_dir(BeamFile) when is_list(BeamFile) ->
    filename:join([filename:dirname(BeamFile), "..", "priv"]);
priv_dir(Other) ->
    error({psycho_priv_dir, Other}).
