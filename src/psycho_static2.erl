-module(psycho_static2).

-export([create_app/1, create_app/2, serve_file/3]).

-export([handle_request/3]).

-include_lib("kernel/include/file.hrl").
-include("http_status.hrl").

-define(chunk_read_size, 409600).
-define(DEFAULT_CONTENT_TYPE, "text/plain").
-define(DEFAULT_FILE, "index.html").

-record(read_state, {path, file}).

%% ===================================================================
%% App
%% ===================================================================

create_app(Dir) ->
    create_app(Dir, []).

create_app(Dir, Opts) ->
    fun(Env) -> ?MODULE:handle_request(Env, Dir, Opts) end.

handle_request(Env, Dir, Opts) ->
    Path = request_path(Env, Dir, Opts),
    serve_file_(resolve_path(Path, Opts), Env, file_opts(Opts)).

request_path(Env, Dir, Opts) ->
    RelPath = relative_request_path(Env),
    filename:join(Dir, maybe_strip_path(RelPath, Opts)).

relative_request_path(Env) ->
    {Path, _, _} = psycho:parsed_request_path(Env),
    strip_leading_slashes(Path).

strip_leading_slashes([$/|Rest]) -> strip_leading_slashes(Rest);
strip_leading_slashes([$\\|Rest]) -> strip_leading_slashes(Rest);
strip_leading_slashes(RelativePath) -> RelativePath.

maybe_strip_path(Path, Opts) ->
    case lists:keyfind(strip_prefix, 1, Opts) of
        {_, Prefix} -> strip_prefix(Prefix, Path, Opts);
        false -> Path
    end.

strip_prefix(Prefix, Path, Opts) ->
    case lists:split(length(Prefix), Path) of
        {Prefix, Stripped} -> Stripped;
        _ -> not_found(Path, Opts)
    end.

resolve_path(Path, Opts) ->
    resolve_path(Path, Opts, 0).

resolve_path(Path, Opts, Calls) when Calls =< 1 ->
    case file_info(Path) of
        {ok, #file_info{type=regular}=Info} ->
            {Path, Info};
        {ok, #file_info{type=directory}} ->
            resolve_dir(Path, Opts, Calls);
        _ ->
            not_found(Path, Opts)
    end;
resolve_path(Path, Opts, _N) ->
    internal_error({too_many_calls, Path, Opts}).

file_info(Path) ->
    file:read_file_info(Path, [{time, universal}]).

resolve_dir(Dir, Opts, Calls) ->
    Default = proplists:get_value(default_file, Opts, ?DEFAULT_FILE),
    NewPath = filename:join(Dir, Default),
    resolve_path(NewPath, Opts, Calls + 1).

file_opts(Opts) ->
    case proplists:get_value(default_cache_control, Opts) of
        undefined -> [];
        Val -> [{cache_control, Val}]
    end.

%% ===================================================================
%% Serve file
%% ===================================================================

serve_file(Path, Env, Opts) ->
    case file_info(Path) of
        {ok, #file_info{type=regular}=Info} ->
            serve_file_({Path, Info}, Env, Opts);
        _ ->
            not_found(Path, Opts)
    end.

serve_file_({Path, Info}, Env, Opts) ->
    ETag = etag(Info, Opts),
    IfNoneMatch = psycho:env_header("If-None-Match", Env),
    file_or_not_modified(IfNoneMatch, ETag, Path, Info, Opts).

file_or_not_modified(ETag, ETag, _Path, _Info, _Opts) ->
    not_modified(ETag);
file_or_not_modified(_, ETag, Path, Info, Opts) ->
    ok_file(ETag, Path, Info, Opts).

not_modified(ETag) ->
    {{304, "Not Modified"}, [{"ETag", ETag}]}.

ok_file(ETag, Path, Info, Opts) ->
    Headers =
        headers(
          [{"Last-Modified",    fun() -> last_modified(Info) end},
           {"Content-Type",     fun() -> content_type(Path, Opts) end},
           {"Content-Length",   fun() -> content_length(Info) end},
           {"Cache-Control",    fun() -> cache_control(Opts) end},
           {"ETag",             fun() -> ETag end},
           {"Content-Encoding", fun() -> content_encoding(Opts) end}]),
    Body = body_iterable(Path, open_file(Path)),
    {{200, "OK"}, Headers, Body}.

%% -------------------------------------------------------------------
%% Header support
%% -------------------------------------------------------------------

headers(Specs) ->
    lists:foldl(fun apply_header/2, [], Specs).

apply_header({Name, F}, Acc) ->
    case F() of
        undefined -> Acc;
        I when is_integer(I) -> [{Name, integer_to_list(I)}|Acc];
        Val -> [{Name, Val}|Acc]
    end.

last_modified(#file_info{mtime=MTime}) ->
    psycho_util:http_date(MTime).

content_type(Path, Opts) ->
    case proplists:get_value(content_type, Opts) of
        undefined -> psycho_mime:type_from_path(Path, ?DEFAULT_CONTENT_TYPE);
        Val -> Val
    end.

content_length(#file_info{size=Size}) -> Size.

etag(Info, Opts) ->
    case proplists:get_value(etag, Opts) of
        undefined -> default_etag(Info);
        Val -> Val
    end.

default_etag(#file_info{mtime=MTime}) ->
    integer_to_list(erlang:phash2(MTime, 4294967296)).

cache_control(Opts) ->
    proplists:get_value(cache_control, Opts).

content_encoding(Opts) ->
    proplists:get_value(content_encoding, Opts).

%% -------------------------------------------------------------------
%% File I/O
%% -------------------------------------------------------------------

open_file(Path) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, File} -> File;
        {error, Err} -> internal_error({read_file, Path, Err})
    end.

body_iterable(Path, File) ->
    {fun read_file_chunk/1, init_read_state(Path, File)}.

init_read_state(Path, File) -> #read_state{path=Path, file=File}.

read_file_chunk(#read_state{file=File}=State) ->
    handle_read_file(file:read(File, ?chunk_read_size), State).

handle_read_file({ok, Data}, State) ->
    {continue, Data, State};
handle_read_file(eof, #read_state{path=Path, file=File}) ->
    close_file(Path, File),
    stop;
handle_read_file({error, Err}, #read_state{path=Path, file=File}) ->
    psycho_log:error({read_file, Path, Err}),
    close_file(Path, File),
    stop.

close_file(Path, File) ->
    case file:close(File) of
        ok -> ok;
        {error, Err} ->
            psycho_log:error({close_file, Path, Err})
    end.

%% ===================================================================
%% Response helpers
%% ===================================================================

not_found(Path, Opts) ->
    case proplists:get_value(not_found_handler, Opts) of
        undefined -> default_not_found();
        Handler -> Handler(Path)
    end.

default_not_found() ->
    throw(
      {?status_not_found,
       [{"Content-Type", "text/plain"}],
       "Not found"}).

internal_error(Err) ->
    psycho_log:error(Err),
    throw(
      {?status_internal_server_error,
       [{"Content-Type", "text/plain"}],
       "Internal Error"}).
