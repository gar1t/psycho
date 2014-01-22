-module(psycho_handler).

-behavior(proc).

-export([start_link/2]).

-export([init/1, handle_msg/3]).

-include("http_status.hrl").

-record(state, {sock, app,
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
-define(RECV_ALL_MAX, 1000000).

start_link(Sock, App) ->
    proc:start_link(?MODULE, [Sock, App]).

init([Sock, App]) ->
    {ok, init_state(Sock, App)}.

init_state(Sock, App) ->
    init_socket(Sock),
    #state{sock=Sock,
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
    ok = inet:setopts(Sock, [{nodelay, true}]).

handle_msg({http, _, {http_request, Method, Path, Ver}}, _From, State) ->
    set_socket_active_once(State),
    {noreply, set_request(Method, Path, Ver, State)};
handle_msg({http, _, {http_header, _, Name, _, Value}}, _From, State) ->
    set_socket_active_once(State),
    {noreply, add_req_header(Name, Value, State)};
handle_msg({http, _, http_eoh}, _From, State) ->
    set_socket_raw_passive(State),
    dispatch_to_app(finalize_request(State));
handle_msg({tcp_closed, _}, _From, _State) ->
    {stop, normal}.

set_socket_active_once(#state{sock=S}) ->
    ok = inet:setopts(S, [{active, once}]).

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
    ok = inet:setopts(Sock, [{packet, raw}, {active, false}]).

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

dispatch_to_app(#state{app=App, env=Env}=State) ->
    handle_app_result(catch psycho:call_app(App, Env), State).

handle_app_result({{I, _}=Status, Headers, Body}, State) when is_integer(I) ->
    respond(Status, Headers, Body, State);
handle_app_result({{I, _}=Status, Headers}, State) when is_integer(I) ->
    respond(Status, Headers, State);
handle_app_result({recv_body, Length, App}, State) ->
    handle_recv(recv(Length, ?IDLE_TIMEOUT, State), App, State);
handle_app_result({recv_body, Length, Timeout, App}, State) ->
    handle_recv(recv(Length, Timeout, State), App, State);
handle_app_result({recv_form_data, App}, State) ->
    handle_recv_form_data(recv_all(?IDLE_TIMEOUT, State), App, State);
handle_app_result({recv_form_data, Timeout, App}, State) ->
    handle_recv_form_data(recv_all(Timeout, State), App, State);
handle_app_result({'EXIT', Err}, State) ->
    respond(?status_internal_server_error, [], State),
    error({app_error, Err});
handle_app_result(Other, State) ->
    respond(?status_internal_server_error, [], State),
    error({bad_return_value, Other}).

recv(Length, Timeout, #state{sock=Sock}) ->
    gen_tcp:recv(Sock, Length, Timeout).

handle_recv({ok, Data}, App, #state{env=Env}=State) ->
    handle_app_result(
      catch psycho:call_app_with_data(App, Env, Data),
      increment_recv_len(size(Data), State)).

increment_recv_len(I, #state{recv_len=Len}=S) ->
    S#state{recv_len=I + Len}.

recv_all(_Timeout, #state{req_content_len=undefined}) ->
    {ok, {0, []}};
recv_all(_Timeout, #state{req_content_len=Len}) when Len > ?RECV_ALL_MAX ->
    error({recv_all_content_len, Len});
recv_all(Timeout, #state{sock=Sock, req_content_len=Len}) ->
    gen_tcp:recv(Sock, Len, Timeout).

handle_recv_form_data({ok, Data}, App, #state{env=Env}=State) ->
    ContentType = env_val(content_type, State),
    Decoded = decode_form_data(ContentType, Data),
    handle_app_result(
      catch App(Decoded, Env),
      increment_recv_len(size(Data), State)).

decode_form_data("application/x-www-form-urlencoded", Data) ->
    psycho_util:parse_query_string(Data);
decode_form_data(Other, _Data) ->
    error({unknown_form_data_type, Other}).

respond(Status, Headers, State) ->
    respond(Status, Headers, [], State).

respond(Status, Headers, Body, State) ->
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
    ok = gen_tcp:send(Sock, Line).

respond_headers(#state{resp_headers=Headers, sock=Sock}) ->
    respond_headers(Headers, Sock).

respond_headers([{Name, Value}|Rest], Sock) ->
    ok = gen_tcp:send(Sock, [Name, ": ", header_value(Value), ?CRLF]),
    respond_headers(Rest, Sock);
respond_headers([], Sock) ->
    ok = gen_tcp:send(Sock, ?CRLF).

header_value(L) when is_list(L) -> L;
header_value(I) when is_integer(I) -> integer_to_list(I).

respond_body(#state{sock=Sock, resp_body={Fun, IterState}})
  when is_function(Fun) ->
    send_body_iter(Sock, Fun, IterState);
respond_body(#state{sock=Sock, resp_body=Body}=State) ->
    send_data(Sock, maybe_encode_chunk(Body, State)).

send_data(Sock, Data) ->
    ok = gen_tcp:send(Sock, Data).

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

close_or_keep_alive(#state{close=true}=S) -> close(S);
close_or_keep_alive(State) ->
    handle_unread_data(unread_data(State), State).

unread_data(#state{req_content_len=undefined}) ->
    false;
unread_data(#state{req_content_len=Len, recv_len=Recv}) ->
    Recv < Len.

handle_unread_data(true, State) ->
    %% TEMP msg - remove
    io:format("**** closing socket due to unread data~n"),
    close(State);
handle_unread_data(false, State) ->
    keep_alive(State).

close(#state{sock=Sock}) ->
    ok = gen_tcp:close(Sock),
    {stop, normal}.

keep_alive(S) ->
    {noreply, reset_state(S)}.

reset_state(#state{sock=Sock, app=App}) ->
    ok = inet:setopts(Sock, [{active, once}, {packet, http}]),
    init_state(Sock, App).
