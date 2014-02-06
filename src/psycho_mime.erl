-module(psycho_mime).

-export([init/0,
         type_from_path/1,
         type_from_path/2,
         type_from_extension/1,
         type_from_extension/2]).

-define(TABLE, psycho_mime_types).
-define(MIME_TYPES_FILE, "mime.types").

init() ->
    init_table(load_priv_types()).

load_priv_types() ->
    case file:consult(priv_mime_types()) of
        {ok, [Types]} -> Types;
        _ -> []
    end.

priv_mime_types() ->
    filename:join(psycho:priv_dir(), ?MIME_TYPES_FILE).

init_table(Types) ->
    ensure_table(),
    ets:insert(?TABLE, table_objects(Types)).

ensure_table() ->
    TableOpts = [named_table, public, set, {read_concurrency, true}],
    try ets:new(?TABLE, TableOpts) of
        ?TABLE -> ok
    catch
        error:badarg -> ok
    end.

table_objects(Types) ->
    acc_table_objects(Types, []).

acc_table_objects([{Type, Extensions}|Rest], Acc) ->
    acc_table_objects(Type, Extensions, Rest, Acc);
acc_table_objects([], Acc) -> Acc.

acc_table_objects(Type, [Extension|RestExts], RestTypes, Acc) ->
    acc_table_objects(Type, RestExts, RestTypes, [{Extension, Type}|Acc]);
acc_table_objects(_Type, [], RestTypes, Acc) ->
    acc_table_objects(RestTypes, Acc).

type_from_path(Path) ->
    type_from_path(Path, undefined).

type_from_path(Path, Default) ->
    type_from_extension(extension(Path), Default).

extension(Path) ->
    strip_period(filename:extension(Path)).

strip_period([$.|Ext]) -> Ext;
strip_period(Other) -> Other.

type_from_extension(Extension) ->
    type_from_extension(Extension, undefined).

type_from_extension(Extension, Default) when is_list(Extension) ->
    try ets:lookup(?TABLE, Extension) of
        [{_, Type}] -> Type;
        _ -> Default
    catch
        error:badarg -> error(not_initialized)
    end.
