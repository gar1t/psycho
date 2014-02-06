-module(psycho_util).

-export([http_date/1,
         ensure_parsed_request_path/1,
         parse_request_path/1,
         parse_query_string/1,
         ensure_parsed_cookie/1,
         parse_cookie/1,
         cookie_header/2,
         encrypt/2, decrypt/2,
         validate/2, format_validate_error/1]).

-import(psycho, [env_val/2, env_header/3]).

%%%===================================================================
%%% Date functions
%%%===================================================================

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

%%%===================================================================
%%% Request path
%%%===================================================================

ensure_parsed_request_path(Env) ->
    handle_parsed_request_path(env_val(parsed_request_path, Env), Env).

handle_parsed_request_path(undefined, Env) ->
    Parsed = parse_request_path(env_val(request_path, Env)),
    {Parsed, [{parsed_request_path, Parsed}|Env]};
handle_parsed_request_path(Parsed, Env) ->
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

%%%===================================================================
%%% Query string
%%%===================================================================

parse_query_string(QS) when is_list(QS) ->
    [qs_nameval(Part) || Part <- split_qs(QS)];
parse_query_string(QS) when is_binary(QS) ->
    parse_query_string(binary_to_list(QS)).

split_qs("") -> [];
split_qs(QS) ->
    split_all(QS, $&, "", []).

split_all([Delim|Rest], Delim, Cur, Acc) ->
    split_all(Rest, Delim, "", [lists:reverse(Cur)|Acc]);
split_all([Char|Rest], Delim, Cur, Acc) ->
    split_all(Rest, Delim, [Char|Cur], Acc);
split_all([], _Delim, Last, Acc) ->
    lists:reverse([lists:reverse(Last)|Acc]).

qs_nameval(Str) ->
    {Name, Value} = split_once(Str, $=, ""),
    {unescape_qs_val(Name), unescape_qs_val(Value)}.

unescape_qs_val(Str) ->
    unescape_qs_val(Str, []).

unescape_qs_val([$%, H1, H2|Rest], Acc) ->
    Ch = list_to_integer([H1, H2], 16),
    unescape_qs_val(Rest, [Ch|Acc]);
unescape_qs_val([$+|Rest], Acc) ->
    unescape_qs_val(Rest, [$ |Acc]);
unescape_qs_val([Ch|Rest], Acc) ->
    unescape_qs_val(Rest, [Ch|Acc]);
unescape_qs_val([], Acc) ->
    lists:reverse(Acc).

%%%===================================================================
%%% Cookies
%%%===================================================================

ensure_parsed_cookie(Env) ->
    handle_parsed_cookie(env_val(parsed_cookie, Env), Env).

handle_parsed_cookie(undefined, Env) ->
    Parsed = parse_cookie(env_header("Cookie", Env, "")),
    {Parsed, [{parsed_cookie, Parsed}|Env]};
handle_parsed_cookie(Parsed, Env) ->
    {Parsed, Env}.

parse_cookie(C) when is_list(C) ->
    [cookie_nameval(Part) || Part <- split_cookie(C)];
parse_cookie(C) when is_binary(C) ->
    parse_cookie(binary_to_list(C)).

split_cookie(C) ->
    split_all(C, $;, "", []).

cookie_nameval(Str) ->
    {Name, Value} = split_once(Str, $=, ""),
    {strip_spaces(Name), strip_spaces(Value)}.

strip_spaces(S) -> string:strip(S, both, $ ).

cookie_header(Name, Value) ->
    {"Set-Cookie", [Name, "=", Value, "; Version=1"]}.

%%%===================================================================
%%% Crypto
%%%===================================================================

-define(NULL_IV_128, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).

decrypt(Data, Key) ->
    PaddedKey = pad(Key, 16),
    unpad(crypto:aes_cbc_128_decrypt(PaddedKey, ?NULL_IV_128, Data)).

encrypt(Data, Key) ->
    PaddedData = pad(Data, 16),
    PaddedKey = pad(Key, 16),
    crypto:aes_cbc_128_encrypt(PaddedKey, ?NULL_IV_128, PaddedData).

pad(Bin, BlockSize) ->
    PadCount = BlockSize - (size(Bin) rem BlockSize),
    Pad = binary:copy(<<PadCount>>, PadCount),
    <<Bin/binary, Pad/binary>>.

unpad(Bin) ->
    try binary:part(Bin, 0, size(Bin) - binary:last(Bin)) of
        Unpadded -> {ok, Unpadded}
    catch
        error:badarg -> error
    end.

%%%===================================================================
%%% Form validation
%%%===================================================================

validate(Data, [{Field, Checks}|Rest]) ->
    Value = proplists:get_value(Field, Data),
    handle_apply_checks(
      apply_checks(Value, Checks, Field, Data),
      Data, Rest);
validate(Data, []) ->
    {ok, Data}.

apply_checks(Value, [Check|Rest], Field, Data) ->
    handle_apply_check(
      apply_check(Value, Check, Data),
      Check, Value, Rest, Field, Data);
apply_checks(_Value, [], _Field, _Data) ->
    ok.

apply_check(Value, required, _Data) ->
    Value /= undefined;
apply_check(Value, {must_equal, {field, Field}}, Data) ->
    Value == proplists:get_value(Field, Data);
apply_check(Value, {must_equal, Target}, _Data) ->
    Value == Target;
apply_check(Value, {min_length, MinLength}, _Data) ->
    Value /= undefined andalso iolist_size(Value) >= MinLength;
apply_check(_Value, Check, _Data) ->
    error({invalid_check, Check}).

handle_apply_check(true, _Check, Value, Rest, Field, Data) ->
    apply_checks(Value, Rest, Field, Data);
handle_apply_check(false, Check, _Value, _Rest, Field, _Data) ->
    {error, {Field, Check}}.

handle_apply_checks(ok, Data, Rest) ->
    validate(Data, Rest);
handle_apply_checks({error, Err}, _Data, _Rest) ->
    {error, Err}.

%% TODO: this is misconceived hard-coded EN -- where should this be?
format_validate_error({Field, required}) ->
    io_lib:format("~s is required", [Field]);
format_validate_error({Field, {must_equal, {field, Target}}}) ->
    io_lib:format("~s must match ~s", [Field, Target]);
format_validate_error({Field, {must_equal, Target}}) ->
    io_lib:format("~s must be ~s", [Field, Target]);
format_validate_error({Field, {min_length, MinLength}}) ->
    io_lib:format(
      "~s must be at least ~b characters long",
      [Field, MinLength]);
format_validate_error(Other) ->
    io_lib:format("~p", [Other]).
