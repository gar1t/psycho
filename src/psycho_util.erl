-module(psycho_util).

-export([http_date/1]).

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
