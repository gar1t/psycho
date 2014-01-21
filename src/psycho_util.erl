-module(psycho_util).

-export([http_date/1,
         ensure_parsed_request_path/1,
         parse_request_path/1]).

http_date(UTC) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = UTC,
    DayNum = calendar:day_of_the_week({Year, Month, Day}),
    io_lib:format(
      "~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
      [day_name(DayNum), Day, month_name(Month), Year, Hour, Min, Sec]).

day_name(1) -> "Mon";
day_name(2) -> "Tue";
day_name(3) -> "Wed";
day_name(4) -> "Thu";
day_name(5) -> "Fri";
day_name(6) -> "Sat"; 
day_name(7) -> "Sun".

month_name(1)  -> "Jan";
month_name(2)  -> "Feb";
month_name(3)  -> "Mar";
month_name(4)  -> "Apr";
month_name(5)  -> "May";
month_name(6)  -> "Jun";
month_name(7)  -> "Jul";
month_name(8)  -> "Aug";
month_name(9)  -> "Sep";
month_name(10) -> "Oct";
month_name(11) -> "Nov";
month_name(12) -> "Dec".

ensure_parsed_request_path(Env) ->
    handle_parsed_request_path(
      lists:keyfind(parsed_request_path, 1, Env), Env).

handle_parsed_request_path(false, Env) ->
    {_, RequestPath} = lists:keyfind(request_path, 1, Env),
    Parsed = parse_request_path(RequestPath),
    {Parsed, [{parsed_request_path, Parsed}|Env]};
handle_parsed_request_path({_, Parsed}, Env) ->
    {Parsed, Env}.

parse_request_path(Path) ->
    {ParsedPath, QueryString} = split_once(Path, $?, ""),
    {ParsedPath, QueryString, parse_query_string(QueryString)}.

split_once([Delim|Rest], Delim, Acc) ->
    {lists:reverse(Acc), Rest};
split_once([Char|Rest], Delim, Acc) ->
    split_once(Rest, Delim, [Char|Acc]);
split_once([], _Delim, Acc) ->
    {lists:reverse(Acc), ""}.

parse_query_string(QS) ->
    [split_once(Part, $=, "") || Part <- split_query_string(QS)].

split_query_string("") -> [];
split_query_string(QS) ->
    split_all(QS, $&, "", []).

split_all([Delim|Rest], Delim, Cur, Acc) ->
    split_all(Rest, Delim, "", [lists:reverse(Cur)|Acc]);
split_all([Char|Rest], Delim, Cur, Acc) ->
    split_all(Rest, Delim, [Char|Cur], Acc);
split_all([], _Delim, Last, Acc) ->
    lists:reverse([lists:reverse(Last)|Acc]).
