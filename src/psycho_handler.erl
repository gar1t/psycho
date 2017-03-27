-module(psycho_handler).

-behavior(proc).

-export([start_link/2]).

-export([init/1, handle_msg/3]).

-include("http_status.hrl").

-record(state, {sock, tag_http, tag_close, tag_error, app,
                client_ver, env, req_headers, req_content_len, recv_len,
                resp_status, resp_headers, resp_header_names, resp_body,
                resp_chunked, close}).

-define(dtoc(D), D + 48).
-define(period, 46).
-define(server, "psycho").
-define(CRLF, "\r\n").

-define(is_string(X), is_list(X) orelse is_binary(X)).
-define(is_resp_body_binary(S), is_binary(S#state.resp_body)).

-define(IDLE_TIMEOUT, 300000).
-define(DEFAULT_RECV_LEN, 32768).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link(Sock, App) ->
    proc:start_link(?MODULE, [Sock, App]).

init([Sock, App]) ->
    {ok, init_state(Sock, App)}.

init_state(Sock, App) ->
    init_socket(Sock),
    {Http, Close, Error} = psycho_socket:tags(Sock),
    #state{sock=Sock,
           tag_http=Http,
           tag_close=Close,
           tag_error=Error,
           app=App,
           client_ver=undefined,
           env=[],
           req_headers=[],
           req_content_len=undefined,
           recv_len=0,
           resp_status=undefined,
           resp_headers=[],
           close=undefined}.

init_socket(Sock) ->
    ok = psycho_socket:setopts(Sock, [{nodelay, true}]).

%%%===================================================================
%%% Process messages / HTTP request state handling
%%%===================================================================

handle_msg({Http, _, {http_request, Method, Path, Ver}}, _From,
           #state{tag_http=Http} = State) ->
    set_socket_active_once(State),
    {noreply, set_request(Method, Path, Ver, State)};
handle_msg({Http, _, {http_header, _, Name, _, Value}}, _From,
           #state{tag_http=Http} = State) ->
    set_socket_active_once(State),
    {noreply, add_req_header(Name, Value, State)};
handle_msg({Http, _, http_eoh}, _From, #state{tag_http=Http} = State) ->
    set_socket_raw_passive(State),
    dispatch_to_app(finalize_request(State));
handle_msg({Close, _}, _From, #state{tag_close=Close}) ->
    {stop, normal};
handle_msg({Error, _, Reason}, _From, #state{tag_error=Error}) ->
    {stop, {tcp_error, Reason}}.

set_socket_active_once(#state{sock=S}) ->
    ok = psycho_socket:setopts(S, [{active, once}]).

%%%===================================================================
%%% Request init / pre app dispatch
%%%===================================================================

set_request(Method, Path, Ver, State) ->
    State#state{
      env=init_env(Method, Path, Ver),
      client_ver=Ver}.

init_env(Method, Path, Ver) ->
    [{request_method, request_method(Method)},
     {request_path, request_path(Path)},
     {request_protocol, request_protocol(Ver)}].

request_method(M) when is_atom(M) -> atom_to_list(M);
request_method(M) -> M.

request_path({abs_path, Path}) -> Path.

request_protocol({M, N}) ->
    lists:flatten(["HTTP/", ?dtoc(M), ?period, ?dtoc(N)]).

add_req_header('Content-Type', Value, State) ->
    Special = {content_type, Value},
    add_header({"Content-Type", Value}, add_env(Special, State));
add_req_header('Content-Length', Value, State) ->
    Special = {content_length, content_length(Value)},
    add_header({"Content-Length", Value}, add_env(Special, State));
add_req_header(Name, Value, State) ->
    add_header({header_name(Name), Value}, State).

add_env(Val, #state{env=Env}=S) ->
    S#state{env=[Val|Env]}.

header_name(N) when is_atom(N) -> atom_to_list(N);
header_name(N) -> N.

add_header(H, #state{req_headers=Hs}=S) ->
    S#state{req_headers=[H|Hs]}.

content_length(L) -> list_to_integer(L).

set_socket_raw_passive(#state{sock=Sock}) ->
    ok = psycho_socket:setopts(Sock, [{packet, raw}, {active, false}]).

finalize_request(State) ->
    apply_state_transforms(
      [fun set_persistent_connection/1,
       fun set_req_content_len/1,
       fun finalize_env/1],
      State).

apply_state_transforms([T|Rest], State) ->
    apply_state_transforms(Rest, T(State));
apply_state_transforms([], State) -> State.

set_persistent_connection(#state{client_ver=Ver}=S) ->
    Connection = req_header("Connection", S),
    set_persistent_connection(Ver, Connection, S).

req_header(Name, #state{req_headers=Headers}) ->
    case lists:keyfind(Name, 1, Headers) of
        {_, Val} -> Val;
        false -> undefined
    end.

set_persistent_connection({1, 1}, "close",      S) -> set_close(true, S);
set_persistent_connection({1, 1}, _,            S) -> set_close(false, S);
set_persistent_connection({1, 0}, "Keep-Alive", S) -> set_close(false, S);
set_persistent_connection({1, 0}, _,            S) -> set_close(true, S).

set_close(Close, S) ->
    S#state{close=Close}.

set_req_content_len(S) ->
    S#state{req_content_len=env_val(content_length, S)}.

env_val(Name, #state{env=Env}) ->
    psycho:env_val(Name, Env).

finalize_env(#state{env=Env, req_headers=Headers}=S) ->
    S#state{env=[{http_headers, Headers}|Env]}.

%%%===================================================================
%%% App dispatch
%%%===================================================================

dispatch_to_app(#state{app=App, env=Env}=State) ->
    handle_app_result(catch psycho:call_app(App, Env), State).

handle_app_result({{I, _}=Status, Headers, Body}, State) when is_integer(I) ->
    respond({Status, Headers, Body}, State);
handle_app_result({{I, _}=Status, Headers}, State) when is_integer(I) ->
    respond({Status, Headers, []}, State);
handle_app_result({recv_body, App, Env}, State) ->
    handle_app_recv_body(App, [], setenv(Env, State));
handle_app_result({recv_body, App, Env, Options}, State) ->
    handle_app_recv_body(App, Options, setenv(Env, State));
handle_app_result({recv_form_data, App, Env}, State) ->
    handle_app_recv_form_data(App, [], setenv(Env, State));
handle_app_result({recv_form_data, App, Env, Options}, State) ->
    handle_app_recv_form_data(App, Options, setenv(Env, State));
handle_app_result({'EXIT', Err}, State) ->
    respond(internal_error(), State),
    error({app_error, Err});
handle_app_result(Other, State) ->
    respond(internal_error(), State),
    error({bad_return_value, Other}).

setenv(Env, S) -> S#state{env=Env}.

%%%===================================================================
%%% recv_body support
%%%===================================================================

handle_app_recv_body(App, Options, State) ->
    Length = proplists:get_value(recv_length, Options, ?DEFAULT_RECV_LEN),
    Timeout = proplists:get_value(recv_timeout, Options, ?IDLE_TIMEOUT),
    maybe_send_continue(State),
    Received = safe_recv(Length, Timeout, State),
    handle_recv_body(Received, App, State).

handle_recv_body({ok, Body}, App, #state{env=Env}=State) ->
    AppResult = (catch psycho:call_app_with_data(App, Env, Body)),
    error_on_recv_after_eof(is_eof(Body), AppResult, App),
    handle_app_result(AppResult, increment_recv_len(Body, State));
handle_recv_body({error, Error}, _App, _State) ->
    {stop, {recv_error, Error}}.

%%%===================================================================
%%% recv_form_data support
%%%===================================================================

handle_app_recv_form_data(App, Options, State) ->
    ContentType = env_val(content_type, State),
    handle_app_recv_form_data(ContentType, App, Options, State).

-define(URLENCODED, "application/x-www-form-urlencoded").
-define(MULTIPART, "multipart/form-data;").

handle_app_recv_form_data(?URLENCODED, App, Options, State) ->
    handle_app_recv_urlencoded_data(App, Options, State);
handle_app_recv_form_data(?MULTIPART ++ TypeParams, App, Options, State) ->
    handle_app_recv_multipart(TypeParams, App, Options, State);
handle_app_recv_form_data(ContentType, App, Options, State) ->
    handle_app_recv_unknown(ContentType, App, Options, State).

%%%===================================================================
%%% urlencoded form data
%%%===================================================================

handle_app_recv_urlencoded_data(App, Options, State) ->
    Timeout = proplists:get_value(recv_timeout, Options, ?IDLE_TIMEOUT),
    maybe_send_continue(State),
    Received = recv_remaining(Timeout, State),
    handle_app_recv_urlencoded_data_(Received, App, State).

handle_app_recv_urlencoded_data_({ok, Encoded}, App, #state{env=Env}=State) ->
    Data = decode_urlencoded_form_data(Encoded),
    AppResult = (catch psycho:call_app_with_data(App, Env, {ok, Data})),
    error_on_recv_after_eof(AppResult, App),
    handle_app_result(AppResult, increment_recv_len(Encoded, State));
handle_app_recv_urlencoded_data_({error, Error}, _App, _State) ->
    {stop, {recv_error, Error}}.

decode_urlencoded_form_data(Data) ->
    psycho_util:parse_query_string(Data).

%%%===================================================================
%%% multipart form data
%%%===================================================================

handle_app_recv_multipart(TypeParams, App, Options, State) ->
    maybe_send_continue(State),
    start_recv_multipart(TypeParams, App, Options, State).

start_recv_multipart(TypeParams, App, Options, State) ->
    Length = proplists:get_value(recv_length, Options, ?DEFAULT_RECV_LEN),
    Timeout = proplists:get_value(recv_timeout, Options, ?IDLE_TIMEOUT),
    PartHandler = proplists:get_value(part_handler, Options),
    MP = new_multipart(TypeParams, PartHandler, State),
    Recv = safe_recv_fun(Length, Timeout),
    handle_app_recv_multipart(Recv(State), Recv, MP, App, State).

new_multipart(TypeParams, PartHandler, #state{env=Env}=State) ->
    Boundary = boundary_param(TypeParams, State),
    psycho_multipart:new(Boundary, PartHandler, Env).

-define(BOUNDARY_PATTERN, <<"[; +]boundary=(.*?)(;|$)">>).

boundary_param(Str, State) ->
    handle_boundary_re(
      re:run(Str, ?BOUNDARY_PATTERN, [{capture, [1], binary}]),
      State).

handle_boundary_re({match, [Boundary]}, _State) -> Boundary;
handle_boundary_re(nomatch, State) ->
    respond(internal_error("Invalid multipart content type\n"), State).

safe_recv_fun(Length, Timeout) ->
    fun(State) -> safe_recv(Length, Timeout, State) end.

handle_app_recv_multipart({ok, <<>>}, _Recv, MP, App, State) ->
    handle_app_recv_multipart_finished(MP, App, State);
handle_app_recv_multipart({ok, Data}, Recv, MP, App, State) ->
    handle_app_recv_multipart_data(Data, Recv, MP, App, State);
handle_app_recv_multipart({error, Error}, _Recv, _MP, _App, _State) ->
    {stop, {recv_error, Error}}.

handle_app_recv_multipart_finished(MP, App, State) ->
    Data = psycho_multipart:form_data(MP),
    Env = psycho_multipart:user_data(MP),
    AppResult = (catch psycho:call_app_with_data(App, Env, {ok, Data})),
    error_on_recv_after_eof(AppResult, App),
    handle_app_result(AppResult, State).

handle_app_recv_multipart_data(Data, Recv, MP, App, State) ->
    handle_updated_multipart(
      psycho_multipart:data(Data, MP),
      Recv, App, increment_recv_len(Data, State)).

handle_updated_multipart(MP, Recv, App, State) ->
    handle_app_recv_multipart(Recv(State), Recv, MP, App, State).

%%%===================================================================
%%% unknown form data
%%%===================================================================

handle_app_recv_unknown(ContentType, App, Options, State) ->
    Timeout = proplists:get_value(recv_timeout, Options, ?IDLE_TIMEOUT),
    maybe_send_continue(State),
    Received = recv_remaining(Timeout, State),
    handle_app_recv_unknown_(Received, ContentType, App, State).

handle_app_recv_unknown_({ok, Body}, ContentType, App, #state{env=Env}=State) ->
    Err = {content_type, ContentType, Body},
    AppResult = (catch psycho:call_app_with_data(App, Env, {error, Err})),
    error_on_recv_after_eof(AppResult, App),
    handle_app_result(AppResult, increment_recv_len(Body, State));
handle_app_recv_unknown_({error, Error}, _Type, _App, _State) ->
    {stop, {recv_error, Error}}.

%%%===================================================================
%%% recv related general/shared functions
%%%===================================================================

maybe_send_continue(#state{client_ver={1, 0}}) -> ok;
maybe_send_continue(State) ->
    case expect_continue(State) of
        true -> send_continue(State);
        false -> ok
    end.

expect_continue(State) ->
    req_header("Expect", State) == "100-continue".

send_continue(#state{sock=Sock}) ->
    Line = ["HTTP/1.1 100 Continue", ?CRLF, ?CRLF],
    ok = psycho_socket:send(Sock, Line).

safe_recv(Length, Timeout, State) ->
    recv(safe_recv_len(Length, State), Timeout, State).

safe_recv_len(_Requested, #state{req_content_len=undefined}) ->
    0;
safe_recv_len(Requested, #state{req_content_len=Total, recv_len=Received}) ->
    min(Requested, Total - Received).

recv(Length, Timeout, #state{sock=Sock}) when Length > 0 ->
    psycho_socket:recv(Sock, Length, Timeout);
recv(_Length, _Timeout, _State) ->
    {ok, <<>>}.

recv_remaining(Timeout, State) ->
    recv(remaining_len(State), Timeout, State).

remaining_len(#state{req_content_len=undefined}) -> 0;
remaining_len(#state{req_content_len=Total, recv_len=Received}) ->
    Total - Received.

internal_error() ->
    internal_error("Server error\n").

internal_error(Msg) ->
    {?status_internal_server_error, [{"Content-Type", "text/plain"}], Msg}.

is_eof(<<>>) -> true;
is_eof(_) -> false.

error_on_recv_after_eof(Result, App) ->
    error_on_recv_after_eof(true, Result, App).

error_on_recv_after_eof(true, Result, App) ->
    error_on_recv(Result, App);
error_on_recv_after_eof(false, _Result, _App) ->
    ok.

error_on_recv({recv_body, _, _}, App) ->
    error({recv_body_after_eof, App});
error_on_recv({recv_body, _, _, _}, App) ->
    error({recv_body_after_eof, App});
error_on_recv({recv_form_data, _, _}, App) ->
    error({recv_form_data_after_eof, App});
error_on_recv({recv_form_data, _, _, _}, App) ->
    error({recv_form_data_after_eof, App});
error_on_recv(_, _App) -> ok.

increment_recv_len(Data, #state{recv_len=Received}=S) ->
    S#state{recv_len=size(Data) + Received}.

%%%===================================================================
%%% Response
%%%===================================================================

respond({Status, Headers, Body}, State) ->
    respond(finalize_response(set_response(Status, Headers, Body, State))).

set_response(Status, Headers, Body, S) ->
    S#state{
      resp_status=validate_status(Status),
      resp_headers=Headers,
      resp_header_names=header_names(Headers),
      resp_body=Body}.

validate_status({Code, Desc}=Status)
  when is_integer(Code),
       ?is_string(Desc) -> Status;
validate_status(Status) -> error({bad_status, Status}).

header_names(Headers) ->
    [string:to_lower(Name) || {Name, _} <- Headers].

finalize_response(State) ->
    apply_state_transforms(
      [fun check_content_len/1,
       fun check_date/1,
       fun check_server/1,
       fun check_keep_alive/1],
      State).

check_content_len(State) ->
    ContentLenStatus = resp_header_status("content-length", State),
    #state{client_ver=Ver} = State,
    check_content_len(ContentLenStatus, Ver, State).

resp_header_status(Name, #state{resp_header_names=Names}) ->
    case lists:member(Name, Names) of
        true -> defined;
        false -> undefined
    end.

check_content_len(defined,   _,      S) -> S;
check_content_len(undefined, _,      S) when ?is_resp_body_binary(S)
                                        -> set_resp_content_len(S);
check_content_len(undefined, {1, 1}, S) -> set_resp_chunked(S);
check_content_len(undefined, {1, 0}, S) -> set_close(true, S).

set_resp_content_len(#state{resp_body=Body}=State) ->
    Len = integer_to_list(size(Body)),
    add_resp_header("Content-Length", Len, State).

add_resp_header(Name, Value, #state{resp_headers=Hs}=S) ->
    S#state{resp_headers=[{Name, Value}|Hs]}.

set_resp_chunked(State) ->
    ChunkedState = State#state{resp_chunked=true},
    add_resp_header("Transfer-Encoding", "chunked", ChunkedState).

check_date(State) ->
    check_date(resp_header_status("date", State), State).

check_date(defined, State) -> State;
check_date(undefined, State) ->
    add_resp_header("Date", psycho_datetime:rfc1123(), State).

check_server(State) ->
    check_server(resp_header_status("server", State), State).

check_server(defined, State) -> State;
check_server(undefined, State) ->
    add_resp_header("Server", ?server, State).

check_keep_alive(State) ->
    check_keep_alive(resp_header_status("connection", State), State).

check_keep_alive(defined, State) -> State;
check_keep_alive(undefined, #state{close=true}=State) -> State;
check_keep_alive(undefined, #state{close=false}=State) ->
    add_resp_header("Connection", "keep-alive", State).

respond(State) ->
    respond_status(State),
    respond_headers(State),
    respond_body(State),
    close_or_keep_alive(State).

respond_status(#state{sock=Sock, resp_status={Code, Reason}}) ->
    Line = ["HTTP/1.1 ", integer_to_list(Code), " ", Reason, ?CRLF],
    ok = psycho_socket:send(Sock, Line).

respond_headers(#state{resp_headers=Headers, sock=Sock}) ->
    respond_headers(Headers, Sock).

respond_headers([{Name, Value}|Rest], Sock) ->
    ok = psycho_socket:send(Sock, [Name, ": ", header_value(Value), ?CRLF]),
    respond_headers(Rest, Sock);
respond_headers([], Sock) ->
    ok = psycho_socket:send(Sock, ?CRLF).

header_value(L) when is_list(L) -> L;
header_value(B) when is_binary(B) -> B;
header_value(I) when is_integer(I) -> integer_to_list(I).

respond_body(#state{sock=Sock, resp_body={Fun, IterState}})
  when is_function(Fun) ->
    send_body_iter(Sock, Fun, IterState);
respond_body(#state{sock=Sock, resp_body=Body}=State) ->
    send_data(Sock, maybe_encode_chunk(Body, State)).

send_data(Sock, Data) ->
    ok = psycho_socket:send(Sock, Data).

maybe_encode_chunk(Data, #state{resp_chunked=true}) ->
    [encode_chunk(Data), last_chunk()];
maybe_encode_chunk(Chunk, _) -> Chunk.

encode_chunk(Data) ->
    [encoded_chunk_size(Data), ?CRLF, Data, ?CRLF].

encoded_chunk_size(Chunk) ->
    integer_to_list(iolist_size(Chunk), 16).

last_chunk() -> ["0", ?CRLF, ?CRLF].

send_body_iter(Sock, Fun, IterState) ->
    handle_body_iter(apply_body_iter(Fun, IterState), Sock, Fun).

apply_body_iter(Fun, IterState) -> Fun(IterState).

handle_body_iter({continue, Data, IterState}, Sock, Fun) ->
    send_data(Sock, Data),
    send_body_iter(Sock, Fun, IterState);
handle_body_iter(stop, _Sock, _Fun) ->
    ok;
handle_body_iter({stop, Data}, Sock, _Fun) ->
    send_data(Sock, Data).

%%%===================================================================
%%% Finalize request
%%%===================================================================

close_or_keep_alive(#state{close=true}=S) -> close(S);
close_or_keep_alive(State) ->
    handle_unread_data(unread_data(State), State).

unread_data(#state{req_content_len=undefined}) ->
    false;
unread_data(#state{req_content_len=Len, recv_len=Recv}) ->
    Recv < Len.

handle_unread_data(true, State) ->
    close(State);
handle_unread_data(false, State) ->
    keep_alive(State).

close(#state{sock=Sock}) ->
    ok = psycho_socket:close(Sock),
    {stop, normal}.

keep_alive(S) ->
    {noreply, reset_state(S)}.

reset_state(#state{sock=Sock, app=App}) ->
    ok = psycho_socket:setopts(Sock, [{active, once}, {packet, http}]),
    init_state(Sock, App).
