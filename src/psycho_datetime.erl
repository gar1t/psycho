-module(psycho_datetime).

-export([rfc1123/0]).

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
