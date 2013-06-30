-module(psycho_mime).

-export([type_from_path/1, type_from_extension/1]).

type_from_path(Path) ->
    type_from_extension(extension(Path)).

extension(Path) ->
    strip_period(filename:extension(Path)).

strip_period([$.|Ext]) -> Ext;
strip_period(Other) -> Other.

%% TODO: serve from ETS table or dynamic module
type_from_extension("xml") -> "application/xml";
type_from_extension("txt") -> "text/plain";
type_from_extension("erl") -> "text/plain";
type_from_extension(_) -> "application/octet-stream".
