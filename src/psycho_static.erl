-module(psycho_static).

-export([create_app/1, serve_file/2]).

-include_lib("kernel/include/file.hrl").
-include("http_status.hrl").

-define(chunk_read_size, 409600).
-define(DEFAULT_CONTENT_TYPE, "text/plain").
-define(DEFAULT_FILE, "index.html").

-record(read_state, {path, file}).

%% TODO: Options to include:
%%
%% - Content type proplist
%% - Default file for a dir
%%
create_app(Dir) ->
    fun(Env) -> ?MODULE:serve_file(Dir, Env) end.

serve_file(Dir, Env) ->
    {Info, Path} = resolved_file_info(requested_path(Dir, Env)),
    LastModified = last_modified(Info),
    ContentType = content_type(Path),
    Size = file_size(Info),
    RawHeaders =
        [{"Last-Modified", LastModified},
         {"Content-Type", ContentType},
         {"Content-Length", Size}],
    Headers = remove_undefined(RawHeaders),
    Body = body_iterable(Path, open_file(Path)),
    {{200, "OK"}, Headers, Body}.

requested_path(Dir, Env) ->
    filename:join(Dir, relative_request_path(Env)).

relative_request_path(Env) ->
    strip_leading_slashes(psycho:env_val(request_path, Env, "")).

strip_leading_slashes([$/|Rest]) -> strip_leading_slashes(Rest);
strip_leading_slashes([$\\|Rest]) -> strip_leading_slashes(Rest);
strip_leading_slashes(RelativePath) -> RelativePath.

resolved_file_info(Path) ->
    handle_file_or_dir_info(file_info(Path), Path).

file_info(Path) ->
    file:read_file_info(Path, [{time, universal}]).

handle_file_or_dir_info({ok, #file_info{type=directory}}, DirPath) ->
    FilePath = apply_default_file(DirPath),
    handle_file_info(file_info(FilePath), FilePath);
handle_file_or_dir_info(Info, Path) ->
    handle_file_info(Info, Path).

apply_default_file(Dir) ->
    filename:join(Dir, ?DEFAULT_FILE).

handle_file_info({ok, #file_info{type=regular}=Info}, Path) ->
    {Info, Path};
handle_file_info({ok, _}, _Path) ->
    not_found();
handle_file_info({error, _}, _Path) ->
    not_found().

last_modified(#file_info{mtime=MTime}) ->
    psycho_util:http_date(MTime).

content_type(Path) ->
    psycho_mime:type_from_path(Path, ?DEFAULT_CONTENT_TYPE).

file_size(#file_info{size=Size}) -> integer_to_list(Size).

remove_undefined(L) ->
    [{Name, Val} || {Name, Val} <- L, Val /= undefined].

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

not_found() ->
    throw({?status_not_found,
           [{"Content-Type", "text/plain"}],
           "Not found"}).

internal_error(Err) ->
    psycho_log:error(Err),
    throw({?status_internal_server_error,
           [{"Content-Type", "text/plain"}],
           "Internal Error"}).
