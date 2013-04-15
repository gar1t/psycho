-module(psycho_log).

-export([error/1]).

error(Err) ->
    error_logger:error_report(Err).
