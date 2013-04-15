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
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = inet:setopts(Sock, [{active, once}, {packet, http}]);
maybe_handoff_socket(_Other, _Sock) ->
    ok.

handle_msg({start_handler, Sock, Apps}, _From, State) ->
    handle_start_handler(Sock, Apps, State);
handle_msg({'EXIT', _Handler, _Reason}, noreply, State) ->
    {noreply, State};
handle_msg(Other, _From, State) ->
    %% TEMP
    io:format("****** SUP GOT ~p~n", [Other]),
    {noreply, State}.

handle_start_handler(Sock, Apps, State) ->
    Result = psycho_handler:start_link(Sock, Apps),
    {reply, Result, State}.

