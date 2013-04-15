-module(sample_proc).

-behavior(proc).

-export([start_link/0, ping/1, ping_async/1]).

-export([handle_msg/3]).

start_link() ->
    proc:start_link(?MODULE, []).

ping(Proc) ->
    proc:call(Proc, ping).

ping_async(Proc) ->
    proc:call(Proc, ping_async).

handle_msg(ping, _From, State) ->
    {reply, pong, State};
handle_msg(ping_async, From, State) ->
    proc_lib:spawn_link(reply_async_fun(From, pong)),
    {noreply, State}.

reply_async_fun(From, Reply) ->
    fun() -> proc:reply(From, Reply) end.
