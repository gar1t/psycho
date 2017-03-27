-module(psycho_server).

-behavior(proc).

-export([start/2, start/3, start_link/2, start_link/3]).

-export([init/1, handle_msg/3]).

-record(state, {handler_sup, lsock, app, mod, accept_timeout}).

-define(DEFAULT_BACKLOG,  128).
-define(DEFAULT_RECBUF,   8192).
-define(DEFAULT_ACCEPT_TIMEOUT_FOR_CB, 500).

%%%===================================================================
%%% Start / init
%%%===================================================================

start(Binding, App) ->
    start(Binding, App, []).

start(Binding, App, Options) ->
    proc:start(?MODULE, [Binding, App, Options]).

start_link(Binding, App) ->
    start_link(Binding, App, []).

start_link(Binding, App, Options) ->
    proc:start_link(?MODULE, [Binding, App, Options]).

init([Binding, App, Options]) ->
    HandlerSup = start_handler_sup(),
    Mod = maybe_cb_init(Options),
    LSock = listen(Binding, Options),
    State = #state{
               handler_sup=HandlerSup,
               lsock=LSock,
               app=App,
               mod=Mod,
               accept_timeout=accept_timeout_opt(Mod, Options)},
    {ok, State, {first_msg, accept}}.

maybe_cb_init(Options) ->
    case proplists:get_value(proc_callback, Options) of
        undefined -> undefined;
        {Mod, InitArg} ->
            Mod:init(InitArg),
            Mod
    end.

start_handler_sup() ->
    {ok, Sup} = psycho_handler_sup:start_link(),
    Sup.

listen(Binding, Options) ->
    handle_listen(psycho_socket:listen(Binding, Options)).

handle_listen({ok, LSock}) -> LSock;
handle_listen({error, Err}) -> error({listen, Err}).

accept_timeout_opt(undefined, _Opts) ->
    infinity;
accept_timeout_opt(_CbMod, Opts) ->
    proplists:get_value(
      accept_timeout, Opts, ?DEFAULT_ACCEPT_TIMEOUT_FOR_CB).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg(accept, noreply, State) ->
    handle_accept(accept(State), State),
    {next_msg, accept, State};
handle_msg(_Msg, _From, #state{mod=undefined}=State) ->
    {noreply, State};
handle_msg(Msg, From, #state{app=App, mod=Mod}=State) ->
    handle_msg_cb(Mod:handle_msg(Msg, From, App), State).

handle_msg_cb({noreply, App}, State) ->
    {noreply, State#state{app=App}};
handle_msg_cb({reply, Reply, App}, State) ->
    {reply, Reply, State#state{app=App}};
handle_msg_cb({stop, Reason}, State) ->
    {stop, Reason, State}.

accept(#state{lsock=LSock, accept_timeout=Timeout}) ->
    psycho_socket:accept(LSock, Timeout).

handle_accept({ok, Sock}, State) ->
    dispatch_request(Sock, State);
handle_accept({error, timeout}, _State) ->
    ok.

dispatch_request(Sock, #state{handler_sup=Sup, app=App}) ->
    handle_start_handler(psycho_handler_sup:start_handler(Sup, Sock, App)).

handle_start_handler({ok, _Pid}) -> ok;
handle_start_handler(Other) -> psycho_log:error(Other).
