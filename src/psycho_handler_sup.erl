-module(psycho_handler_sup).

-behavior(proc).

-export([start_link/0, start_handler/3]).

-export([init/1, handle_msg/3]).

start_link() ->
    proc:start_link(?MODULE, []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, []}.

start_handler(Sup, Sock, Apps) ->
    Result = proc:call(Sup, {start_handler, Sock, Apps}),
    maybe_handoff_socket(Result, Sock),
    Result.

maybe_handoff_socket({ok, Pid}, Sock) ->
    ok = psycho_socket:controlling_process(Sock, Pid),
    ok = psycho_socket:setopts(Sock, [{active, once}, {packet, http}]);
maybe_handoff_socket(_Other, _Sock) ->
    ok.

handle_msg({start_handler, Sock, Apps}, _From, State) ->
    handle_start_handler(Sock, Apps, State);
handle_msg({'EXIT', _Handler, normal}, noreply, State) ->
    {noreply, State};
handle_msg({'EXIT', Handler, Reason}, noreply, State) ->
    log_error(Handler, Reason),
    {noreply, State}.

handle_start_handler(Sock, Apps, State) ->
    Result = psycho_handler:start_link(Sock, Apps),
    {reply, Result, State}.

log_error(Handler, Err) ->
    psycho_log:error({handler_error, Handler, Err}).
