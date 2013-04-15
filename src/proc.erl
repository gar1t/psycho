-module(proc).

-export([start/2, start/3,
         start_link/2, start_link/3,
         call/2, call/3, reply/2, cast/2]).

-export([init/5]).

-export([system_continue/3, system_terminate/4, system_code_change/4]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{handle_msg, 3}].

-record(state, {parent, mod, mod_state, debug}).
-record(init, {starter, name}).

-define(undef_fun(Mod, Fun), {undef, [{Mod, Fun, _, _}|_]}).

%%%===================================================================
%%% Start
%%%===================================================================

start(Module, Args) ->
    start(Module, Args, []).

start(Module, Args, Options) ->
    start_impl(start, self(), self, Module, Args, Options).

start_link(Module, Args) ->
    start_link(Module, Args, []).

start_link(Module, Args, Options) ->
    start_impl(start_link, self(), self(), Module, Args, Options).

start_impl(Start, Starter, Parent, Module, Args, Options) ->
    Timeout = timeout_option(Options),
    SpawnOpts = spawn_options(Options),
    InitArgs = [Starter, Parent, Module, Args, Options],
    proc_lib:Start(?MODULE, init, InitArgs, Timeout, SpawnOpts).

timeout_option(Options) ->
    proplists:get_value(timeout, Options, infinity).

spawn_options(Options) ->
    proplists:get_value(spawn_opts, Options, []).

%%%===================================================================
%%% Init
%%%===================================================================

init(Starter, MaybeParent, Mod, Args, Options) ->
    Parent = parent_or_self(MaybeParent),
    Name = name_option(Options, Mod),
    Debug = debug_options(Options),
    Init = #init{starter=Starter, name=Name},
    State = #state{parent=Parent, mod=Mod, debug=Debug},
    maybe_register(Init),
    handle_mod_init(mod_init(Mod, Args), Init, State).

parent_or_self(self) -> self();
parent_or_self(Parent) -> Parent.

name_option(Options, Mod) ->
    case proplists:get_bool(registered, Options) of
        true -> Mod;
        false -> proplists:get_value(registered, Options)
    end.

debug_options(Options) ->
    sys:debug_options(proplists:get_value(debug, Options, [])).

maybe_register(#init{name=undefined}) -> ok;
maybe_register(#init{name=Name}) -> register(Name, self()).

mod_init(Mod, Args) ->
    handle_undefined_init(catch Mod:init(Args), Mod, Args).

handle_undefined_init({'EXIT', ?undef_fun(Mod, init)}, Mod, InitArgs) ->
    {ok, InitArgs};
handle_undefined_init(Result, _Mod, _InitArgs) ->
    Result.

handle_mod_init({ok, ModState}, Init, State) ->
    init_ack({ok, self()}, Init),
    loop(set_mod_state(ModState, State));
handle_mod_init({ok, ModState, {first_msg, Msg}}, Init, State) ->
    init_ack({ok, self()}, Init),
    erlang:send(self(), Msg),
    loop(set_mod_state(ModState, State));
handle_mod_init(Other, Init, _State) ->
    AckRet = init_exit_ack_ret(Other),
    ExitReason = exit_reason(Other),
    init_exit(AckRet, ExitReason, Init).

init_ack(Ret, #init{starter=Starter}) ->
    proc_lib:init_ack(Starter, Ret).

set_mod_state(ModState, State) ->
    State#state{mod_state=ModState}.

init_exit_ack_ret({stop, Reason}) -> {error, Reason};
init_exit_ack_ret(ignore) -> ignore;
init_exit_ack_ret({'EXIT', Reason}) -> {error, Reason};
init_exit_ack_ret(Other) -> {error, {bad_return_value, Other}}.

exit_reason({stop, Reason}) -> Reason;
exit_reason(ignore) -> normal;
exit_reason({'EXIT', Reason}) -> Reason;
exit_reason(Other) -> {bad_return_value, Other}.

init_exit(AckRet, Reason, Init) ->
    maybe_unregister(Init),
    init_ack(AckRet, Init),
    exit(Reason).

maybe_unregister(#init{name=undefined}) -> ok;
maybe_unregister(#init{name=Name}) -> unregister(Name).

%%%===================================================================
%%% Call
%%%===================================================================

call(Proc, Msg) ->
    call(Proc, Msg, infinity).

call(Proc, Msg, Timeout) ->
    MRef = erlang:monitor(process, Proc),
    erlang:send(Proc, {'$call', {self(), MRef}, Msg}),
    receive
        {MRef, Reply} -> handle_call_reply(MRef, Reply);
        {'DOWN', MRef, _, _, Reason} -> exit(Reason)
    after
        Timeout -> handle_call_timeout(MRef)
    end.

handle_call_reply(MRef, Reply) ->
    erlang:demonitor(MRef, [flush]),
    Reply.

handle_call_timeout(MRef) ->
    erlang:demonitor(MRef),
    maybe_consume_down(MRef),
    exit(timeout).

maybe_consume_down(MRef) ->
    receive
        {'DOWN', MRef, _, _, _} -> ok
    after
        0 -> ok
    end.

%%%===================================================================
%%% Reply
%%%===================================================================

reply({Pid, Ref}, Reply) ->
    erlang:send(Pid, {Ref, Reply}).

%%%===================================================================
%%% Cast
%%%===================================================================

cast(Proc, Msg) ->
    erlang:send(Proc, {'$cast', Msg}).

%%%===================================================================
%%% Loop
%%%===================================================================

loop(State) ->
    #state{parent=Parent} = State,
    receive
	{system, From, Req} ->
            handle_system_msg(From, Req, State);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, State);
	Msg ->
            handle_msg(Msg, State)
    end.

%%%===================================================================
%%% System msg dispatch
%%%===================================================================

handle_system_msg(From, Req, State) ->
    #state{parent=Parent, debug=Debug} = State,
    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug, State).

%%%===================================================================
%%% Terminate
%%%===================================================================

terminate(Reason, State) ->
    handle_mod_terminate(mod_terminate(Reason, State), Reason).

mod_terminate(Reason, #state{mod=Mod, mod_state=ModState}) ->
    handle_undefined_terminate(catch Mod:terminate(Reason, ModState), Mod).

handle_undefined_terminate({'EXIT', ?undef_fun(Mod, terminate)}, Mod) -> ok;
handle_undefined_terminate(Other, _Mod) -> Other.

handle_mod_terminate({'EXIT', Err}, _Reason) -> exit(Err);
handle_mod_terminate(_Result, Reason) -> exit(Reason).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg({'$call', From, Msg}, State) ->
    handle_mod_call(mod_call(From, Msg, State), From, State);
handle_msg({'$cast', Msg}, State) ->
    handle_mod_dispatch(mod_dispatch(Msg, State), State);
handle_msg(Msg, State) ->
    handle_mod_dispatch(mod_dispatch(Msg, State), State).

mod_call(From, Msg, #state{mod=Mod, mod_state=ModState}) ->
    Mod:handle_msg(Msg, From, ModState).

handle_mod_call({reply, Reply, ModState}, From, State) ->
    reply(From, Reply),
    loop(set_mod_state(ModState, State));
handle_mod_call(Other, _From, State) ->
    handle_mod_dispatch(Other, State).

mod_dispatch(Msg, #state{mod=Mod, mod_state=ModState}) ->
    Mod:handle_msg(Msg, noreply, ModState).

handle_mod_dispatch({noreply, ModState}, State) ->
    loop(set_mod_state(ModState, State));
handle_mod_dispatch({stop, Reason}, State) ->
    terminate(Reason, State);
handle_mod_dispatch({stop, Reason, ModState}, State) ->
    terminate(Reason, set_mod_state(ModState, State));
handle_mod_dispatch({next_msg, Msg, ModState}, State) ->
    erlang:send(self(), Msg),
    loop(set_mod_state(ModState, State));
handle_mod_dispatch(Other,  State) ->
    terminate({bad_return_value, Other}, State).

%%%===================================================================
%%% System callbacks
%%%===================================================================

system_continue(_Parent, _Debug, State) ->
    loop(State).

system_terminate(Reason, _Parent, _Debug, State) ->
    terminate(Reason, State).

system_code_change(State, _Module, _OldVsn, _Extra) ->
    %% TODO
    {ok, State}.
