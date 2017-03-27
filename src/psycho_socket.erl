-module(psycho_socket).

-export([listen/2,
         accept/1,
         accept/2,
         controlling_process/2,
         send/2,
         recv/2,
         recv/3,
         close/1]).


-define(DEFAULT_BACKLOG,  128).
-define(DEFAULT_RECBUF,   8192).
-define(DEFAULT_SSL,      false).

-record(psycho_socket, {
          type = tcp,
          socket = undefined
         }).

listen(Binding, Options) ->
    EnableSSL = enable_ssl(Options),
    listen_socket(Binding, Options, EnableSSL).

accept(Socket) ->
    accept(Socket, infinity).

accept(#psycho_socket{socket = Sock, type=ssl}, Timeout) ->
    handle_ssl_transport(ssl:ssl_transport_accept(Sock, Timeout), Timeout);
accept(#psycho_socket{socket = Sock, type=tcp}, Timeout) ->
    socket_or_error(gen_tcp:accept(Sock, Timeout), false).

controlling_process(#psycho_socket{socket = Sock, type=ssl}, Pid) ->
    ssl:controlling_process(Sock, Pid);
controlling_process(#psycho_socket{socket = Sock, type=tcp}, Pid) ->
    gen_tcp:controlling_process(Sock, Pid).

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

listen_socket(Binding, Options, false) ->
    Port = binding_port(Binding),
    ListenOpts = listen_options(Binding, Options, false),
    socket_or_error(gen_tcp:listen(Port, ListenOpts), false).


binding_port(Port) when is_integer(Port) -> Port;
binding_port({_Addr, Port}) when is_integer(Port) -> Port;
binding_port(Other) -> error({invalid_binding, Other}).

listen_options(Binding, Opts, false) ->
    Binding = binding_opt(Binding),
    add_binding([binary,
                 {active,    false},
                 {reuseaddr, true},
                 {backlog,   backlog_opt(Opts)},
                 {recbuf,    recbuf_opt(Opts)}], Binding).

binding_opt({Addr, _Port}) ->
    Addr;
binding_opt(_) ->
    undefined.

backlog_opt(Opts) ->
    proplists:get_value(backlog, Opts, ?DEFAULT_BACKLOG).

recbuf_opt(Opts) ->
    proplists:get_value(recbuf, Opts, ?DEFAULT_RECBUF).

enable_ssl(Opts) ->
    proplists:get_value(ssl, Opts, ?DEFAULT_SSL).


add_binding(Opts, undefined) ->
    Opts;
add_binding(Opts, Addr) ->
    [{ip, Addr} | Opts ].


handle_ssl_transport({ok, NewSocket}, Timeout) ->
    socket_or_error(ssl:ssl_accept(NewSocket, Timeout), true);
handle_ssl_transport(Error, _Timeout) ->
    Error.

socket_or_error({ok, Sock}, false) ->
    {ok, #psycho_socket{ type = tcp, socket = Sock}};
socket_or_error({ok, Sock}, true) ->
    {ok, #psycho_socket{ type = ssl, socket = Sock}};
socket_or_error(Error, _) ->
    Error.
