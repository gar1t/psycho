-module(psycho_socket).

-export([listen/2,
         tags/1,
         accept/1,
         accept/2,
         controlling_process/2,
         setopts/2,
         peername/1,
         send/2,
         recv/2,
         recv/3,
         close/1]).


-define(DEFAULT_BACKLOG,  128).
-define(DEFAULT_RECBUF,   8192).
-define(SSL_DETECT_KEYS,  [key, keyfile, cert, certfile]).
-define(SSL_OPTION_KEYS,  [verify, verify_fun, fail_if_no_peer_cert, depth,
                           cert, certfile, key, keyfile, password, cacerts,
                           cacertfile, dh, dhfile, ciphers, user_lookup_fun,
                           reuse_sessions, reuse_session,
                           client_preferred_next_protocols, log_alert,
                           server_name_indication, sni_hosts, sni_fun]).

-record(psycho_socket, {
          type = undefined,
          socket = undefined
         }).


listen(Binding, Options) ->
    Port = binding_port(Binding),
    ListenOpts = listen_options(Binding, Options),
    EnableSSL = enable_ssl(Options),
    listen_socket(Port, ListenOpts, EnableSSL).


tags(#psycho_socket{type =ssl }) ->
    {ssl, ssl_closed, ssl_error};
tags(#psycho_socket{type = tcp }) ->
    {http, tcp_closed, tcp_error}.


accept(Socket) ->
    accept(Socket, infinity).

accept(#psycho_socket{socket = Sock, type=ssl}, Timeout) ->
    handle_ssl_transport(ssl:transport_accept(Sock, Timeout), Timeout);
accept(#psycho_socket{socket = Sock, type=tcp}, Timeout) ->
    socket_or_error(gen_tcp:accept(Sock, Timeout), false).


controlling_process(#psycho_socket{socket = Sock, type=ssl}, Pid) ->
    ssl:controlling_process(Sock, Pid);
controlling_process(#psycho_socket{socket = Sock, type=tcp}, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).


setopts(#psycho_socket{socket = Sock, type=ssl}, Options) ->
    ssl:setopts(Sock, Options);
setopts(#psycho_socket{socket = Sock, type=tcp}, Options) ->
    inet:setopts(Sock, Options).


peername(#psycho_socket{socket = Sock, type=ssl}) ->
    ssl:peername(Sock);
peername(#psycho_socket{socket = Sock, type=tcp}) ->
    inet:peername(Sock).


send(#psycho_socket{socket = Sock, type=ssl}, Data) ->
    ssl:send(Sock, Data);
send(#psycho_socket{socket = Sock, type=tcp}, Data) ->
    gen_tcp:send(Sock, Data).


recv(Socket, Length) ->
    recv(Socket, Length, infinity).

recv(#psycho_socket{socket = Sock, type=ssl}, Length, Timeout) ->
    ssl:recv(Sock, Length, Timeout);
recv(#psycho_socket{socket = Sock, type=tcp}, Length, Timeout) ->
    gen_tcp:recv(Sock, Length, Timeout).


close(#psycho_socket{socket = Sock, type=tcp}) ->
    gen_tcp:close(Sock).


listen_socket(Port, ListenOpts, true) ->
    socket_or_error(ssl:listen(Port, ListenOpts), true);
listen_socket(Port, ListenOpts, false) ->
    socket_or_error(gen_tcp:listen(Port, ListenOpts), false).


binding_port(Port) when is_integer(Port) -> Port;
binding_port({_Addr, Port}) when is_integer(Port) -> Port;
binding_port(Other) -> error({invalid_binding, Other}).


listen_options(Binding, Opts) ->
    Options = ssl_binding_options(Binding, Opts),
    lists:merge([binary,
                 {active,    false},
                 {reuseaddr, true},
                 {backlog,   backlog_opt(Opts)},
                 {recbuf,    recbuf_opt(Opts)}], Options).


binding_opt({Addr, _Port}) ->
    Addr;
binding_opt(_) ->
    undefined.


backlog_opt(Opts) ->
    proplists:get_value(backlog, Opts, ?DEFAULT_BACKLOG).


recbuf_opt(Opts) ->
    proplists:get_value(recbuf, Opts, ?DEFAULT_RECBUF).


enable_ssl(Opts) ->
    OptionKeys = proplists:get_keys(Opts),
    ContainsSSL =
        fun(Key, Result) ->
                Result orelse lists:member(Key, ?SSL_DETECT_KEYS)
        end,
    lists:foldl(ContainsSSL, false, OptionKeys).


ssl_binding_options(Binding, Options) ->
    add_binding(ssl_options(Options), binding_opt(Binding)).


ssl_options(Options) ->
    Filter = fun({Key, _Value}) ->
                     lists:member(Key, ?SSL_OPTION_KEYS);
                (_) ->
                     false
             end,
    lists:filter(Filter, Options).


add_binding(Opts, undefined) ->
    Opts;
add_binding(Opts, Addr) ->
    [{ip, Addr} | Opts ].


handle_ssl_transport({ok, NewSocket}, Timeout) ->
    handle_ssl_handshake(ssl:ssl_accept(NewSocket, Timeout), NewSocket);
handle_ssl_transport(Error, _Timeout) ->
    Error.


handle_ssl_handshake(ok, TransportSocket) ->
    socket_or_error({ok, TransportSocket}, true);
handle_ssl_handshake({ok, SslSocket}, _) ->
    socket_or_error({ok, SslSocket}, true);
handle_ssl_handshake(Error, _) ->
    Error.


socket_or_error({ok, Sock}, false) ->
    {ok, #psycho_socket{ type = tcp, socket = Sock}};
socket_or_error({ok, Sock}, true) ->
    {ok, #psycho_socket{ type = ssl, socket = Sock}};
socket_or_error(Error, _) ->
    Error.
