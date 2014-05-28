-module(psycho_datetime).

-export([rfc1123/0,
         iso8601/1,
         iso8601/2]).

rfc1123() ->
    {{YYYY, MM, DD}, {Hour, Min, Sec}} = calendar:universal_time(),
    DayNumber = calendar:day_of_the_week({YYYY, MM, DD}),
    io_lib:format(
      "~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
      [day(DayNumber), DD, month(MM), YYYY, Hour, Min, Sec]).

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

iso8601({Mega, Sec, Micro}) ->
    DateTime = calendar:now_to_datetime({Mega, Sec, Micro}),
    iso8601(DateTime, Micro);
iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    iolist_to_binary(
      io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0B "
        "~2.10.0B:~2.10.0B:~2.10.0B",
        [Year, Month, Day, Hour, Min, Sec]));
iso8601({Mega, Sec}) ->
    DateTime = calendar:now_to_datetime({Mega, Sec, 0}),
    iso8601(DateTime);
iso8601(TimestampMs) when is_integer(TimestampMs) ->
    iso8601(timestamp_to_now(TimestampMs));
iso8601(undefined) -> <<"">>.

iso8601({{Year, Month, Day}, {Hour, Min, Sec}}, Micro) ->
    iolist_to_binary(
      io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0BT"
        "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
        [Year, Month, Day, Hour, Min, Sec, trunc(Micro / 1000)])).

timestamp_to_now(EpochMs) ->
    Mega = EpochMs div 1000000000,
    Sec = (EpochMs - Mega * 1000000000) div 1000,
    Micro = (EpochMs - (Mega * 1000000000) - (Sec * 1000)) * 1000,
    {Mega, Sec, Micro}.
