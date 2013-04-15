-module(psycho_reloader).

-export([start/0]).

-export([init/1]).

-include_lib("kernel/include/file.hrl").

-define(SLEEP_INTERVAL, 1000).

start() ->
    proc_lib:start(?MODULE, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(init_beam_timestamps()).

init_beam_timestamps() ->
    dict:from_list(module_timestamps(reloadable_modules())).

reloadable_modules() ->
    filter_non_sticky(filter_real_paths(code:all_loaded())).

filter_real_paths(Mods) ->
    lists:filter(filter_real_path_fun(), Mods).

filter_real_path_fun() ->
    fun({_Mod, Path}) -> is_list(Path) end.

filter_non_sticky(Mods) ->
    lists:filter(filter_non_sticky_fun(), Mods).

filter_non_sticky_fun() ->
    fun({Mod, _Path}) -> not code:is_sticky(Mod) end.

module_timestamps(Loaded) ->
    [{{Mod, Path}, beam_timestamp(Path)} || {Mod, Path} <- Loaded].

beam_timestamp(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{mtime=Time}} -> Time;
        {error, _Err} -> undefined
    end.

loop(PrevTimes) ->
    sleep(),
    CurTimes = init_beam_timestamps(),
    check_modules(PrevTimes, CurTimes),
    loop(CurTimes).

sleep() -> timer:sleep(?SLEEP_INTERVAL).

check_modules(PrevTimes, CurTimes) ->
    reload_modules(changed_modules(PrevTimes, CurTimes)).

changed_modules(PrevTimes, CurTimes) ->
    dict:fold(changed_module_folder(CurTimes), [], PrevTimes).

changed_module_folder(CurTimes) ->
    fun(ModInfo, PrevTime, Changed) ->
            add_if_changed(ModInfo, PrevTime, CurTimes, Changed) end.

add_if_changed(Loaded, PrevTime, CurTimes, Changed) ->
    case PrevTime /= beam_timestamp(Loaded, CurTimes) of
        true -> add_changed_module(Loaded, Changed);
        false -> Changed
    end.

beam_timestamp(Loaded, Times) ->
    case dict:find(Loaded, Times) of
        {ok, Time} -> Time;
        error -> undefined
    end.

add_changed_module({Mod, _Path}, Changed) ->
    [Mod|Changed].

reload_modules([Mod|Rest]) ->
    io:format("Reloading ~p... ", [Mod]),
    code:purge(Mod),
    handle_code_load_file(code:load_file(Mod)),
    reload_modules(Rest);
reload_modules([]) -> ok.

handle_code_load_file({module, _}) ->
    io:format("ok~n");
handle_code_load_file({error, nofile}) ->
    ok;
handle_code_load_file({error, Err}) ->
    io:format("error: ~p~n", [Err]).
