-module(psycho_server).

-behavior(proc).

-export([start/2, start/3,
         start_link/2, start_link/3]).

-export([init/1, handle_msg/3]).

-record(state, {handler_sup, lsock, app}).

-define(backlog, 128).

%%%===================================================================
%%% Start / init
%%%===================================================================

start(Binding, App) ->
    start(Binding, App, []).

start(Binding, Apps, Options) ->
    proc:start(?MODULE, [Binding, Apps, Options]).

start_link(Binding, Apps) ->
    start_link(Binding, Apps, []).

start_link(Binding, Apps, Options) ->
    proc:start_link(?MODULE, [Binding, Apps, Options]).

init([Binding, Apps, Options]) ->
    HandlerSup = start_handler_sup(),
    LSock = listen(Binding, Options),
    {ok, init_state(HandlerSup, LSock, Apps), {first_msg, accept}}.

start_handler_sup() ->
    {ok, Sup} = psycho_handler_sup:start_link(),
    Sup.

listen(Binding, Options) ->
    Port = binding_port(Binding),
    ListenOpts = listen_options(Binding, Options),
    handle_listen(gen_tcp:listen(Port, ListenOpts)).

binding_port(Port) when is_integer(Port) -> Port;
binding_port({_Addr, Port}) when is_integer(Port) -> Port; 
binding_port(Other) -> error({invalid_binding, Other}).

listen_options(_Binding, _Options) ->
    %% TODO - use Binding to specify bound IP addr, expose to Options?
    [binary,
     {active, false},
     {reuseaddr, true},
     {backlog, ?backlog}].

handle_listen({ok, LSock}) -> LSock;
handle_listen({error, Err}) -> error({listen, Err}).

init_state(HandlerSup, LSock, App) ->
    #state{handler_sup=HandlerSup, lsock=LSock, app=App}.

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg(accept, noreply, State) ->
    handle_accept(accept(State), State),
    {next_msg, accept, State}.

accept(#state{lsock=LSock}) ->
    gen_tcp:accept(LSock).

handle_accept({ok, Sock}, State) ->
    dispatch_request(Sock, State).

dispatch_request(Sock, #state{handler_sup=Sup, app=App}) ->
    handle_start_handler(psycho_handler_sup:start_handler(Sup, Sock, App)).

handle_start_handler({ok, _Pid}) -> ok;
handle_start_handler(Other) -> psycho_log:error(Other).
