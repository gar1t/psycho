-module(psycho_util).

-export([http_date/1,
         ensure_parsed_request_path/1,
         parse_request_path/1,
         parse_query_string/1,
         split_path/1,
         ensure_parsed_cookie/1,
         parse_cookie/1,
         cookie_header/2, cookie_header/3,
         encrypt/2, decrypt/2,
         validate/2, format_validate_error/1,
         parse_content_disposition/1,
         content_disposition/2,
         app_dir/1, priv_dir/1, priv_dir/2,
         dispatch_on/2, dispatch_app/2]).

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

split_path([$/|RestPath]) ->
    split_path(RestPath);
split_path(Path) ->
    split_all(Path, $/, "", []).

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
    cookie_header(Name, Value, []).

cookie_header(Name, Value, Options) ->
    Base = [Name, "=", Value, "; Version=1"],
    {"Set-Cookie", append_cookie_options(Options, Base)}.

append_cookie_options([], Val) -> Val;
append_cookie_options([{path, Path}|Rest], Val) ->
    append_cookie_options(Rest, [Val, "; Path=", Path]);
append_cookie_options([Other|_], _Val) ->
    error({invalid_cookie_option, Other}).

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

%% TODO: This validation scheme is incomplete. Add more checks (e.g.
%% max_length, pattern, etc.) and possibly more conversion.

validate(Data, Schema) ->
    validate(Data, Schema, []).

validate(DataIn, [{Field, Checks}|Rest], DataOut) ->
    Value = proplists:get_value(Field, DataIn),
    handle_apply_checks(
      apply_checks(Value, Checks, Field, DataIn),
      DataIn, Rest, DataOut);
validate(_DataIn, [], DataOut) ->
    {ok, DataOut}.

apply_checks(Value, [Check|Rest], Field, Data) ->
    handle_apply_check(
      apply_check(Check, Value, Data),
      Check, Value, Rest, Field, Data);
apply_checks(Value, [], Field, _Data) ->
    {ok, {Field, Value}}.

apply_check(required, Value, _Data) ->
    Value /= undefined;
apply_check({must_equal, {field, Field}}, Value, Data) ->
    Value == proplists:get_value(Field, Data);
apply_check({must_equal, Target}, Value, _Data) ->
    Value == Target;
apply_check(binary, Value, _Data) ->
    {true, list_to_binary(Value)};
apply_check(integer, Value, _Data) ->
    try_integer(Value);
apply_check(float, Value, _Data) ->
    try_float(Value);
apply_check(number, Value, _Data) ->
    try_number(Value);
apply_check({min_length, MinLength}, Value, _Data) ->
    Value /= undefined andalso iolist_size(Value) >= MinLength;
apply_check(Check, _Value, _Data) ->
    error({invalid_check, Check}).

try_integer(undefined) ->
    {true, undefined};
try_integer(Value) ->
    try list_to_integer(Value) of
        I -> {true, I}
    catch
        _:_ -> false
    end.

try_float(undefined) ->
    {true, undefined};
try_float(Value) ->
    try list_to_float(Value) of
        F -> {true, F}
    catch
        _:_ ->
            try list_to_integer(Value) of
                I -> {true, float(I)}
            catch
                _:_ -> false
            end
    end.

try_number(undefined) ->
    {true, undefined};
try_number(Value) ->
    try list_to_integer(Value) of
        I -> {true, I}
    catch
        _:_ ->
            try list_to_float(Value) of
                F -> {true, F}
            catch
                _:_ -> false
            end
    end.

handle_apply_check(true, _Check, Value, Rest, Field, Data) ->
    apply_checks(Value, Rest, Field, Data);
handle_apply_check({true, NewValue}, _Check, _Value, Rest, Field, Data) ->
    apply_checks(NewValue, Rest, Field, Data);
handle_apply_check(false, Check, _Value, _Rest, Field, _Data) ->
    {error, {Field, Check}}.

handle_apply_checks({ok, Validated}, DataIn, Rest, DataOut) ->
    validate(DataIn, Rest, maybe_add_validated(Validated, DataOut));
handle_apply_checks({error, Err}, _DataIn, _Rest, _DataOut) ->
    {error, Err}.

maybe_add_validated({_, undefined}, Data) -> Data;
maybe_add_validated(Validated, Data) -> [Validated|Data].

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

%%%===================================================================
%%% Parsing content-disposition multipart part header
%%%===================================================================

parse_content_disposition(S) ->
    handle_split_content_disp(re:split(S, "; *", [{return, list}])).

handle_split_content_disp(["form-data"|Vars]) ->
    acc_content_disp_namevals(Vars, []).

acc_content_disp_namevals([S|Rest], Acc) ->
    handle_disp_nameval_match(disp_nameval_match(S), Rest, Acc);
acc_content_disp_namevals([], Acc) ->
    Acc.

disp_nameval_match(S) ->
    re:run(S, "(.*?)=\"(.*?)\"", [{capture, all_but_first, list}]).

handle_disp_nameval_match({match, [Name, Val]}, Rest, Acc) ->
    acc_content_disp_namevals(Rest, [{Name, Val}|Acc]);
handle_disp_nameval_match(nomatch, Rest, Acc) ->
    acc_content_disp_namevals(Rest, Acc).

content_disposition(Name, PartHeaders) ->
    Unparsed = proplists:get_value("Content-Disposition", PartHeaders),
    Parsed = parse_content_disposition(Unparsed),
    proplists:get_value(Name, Parsed).

%%%===================================================================
%%% Module directory functions
%%%===================================================================

app_dir(Mod) ->
    BeamDir = filename:dirname(code:which(Mod)),
    filename:dirname(BeamDir).

priv_dir(Mod) ->
    filename:join(app_dir(Mod), "priv").

priv_dir(Mod, Subdir) ->
    filename:join(priv_dir(Mod), Subdir).

%%%===================================================================
%%% Dispatch helpers
%%%===================================================================

dispatch_on(Parts, Env0) ->
    {ParsedPath, Env} = psycho_util:ensure_parsed_request_path(Env0),
    [dispatch_part(Part, ParsedPath, Env) || Part <- Parts].

dispatch_part(env, _ParsedPath, Env) ->
    Env;
dispatch_part(method, _ParsedPath, Env) ->
    psycho:env_val(request_method, Env);
dispatch_part(path, {Path, _, _}, _Env) ->
    Path;
dispatch_part(split_path, {Path, _, _}, _Env) ->
    psycho_util:split_path(Path);
dispatch_part(parsed_query_string, {_, _, ParsedQS}, _Env) ->
    ParsedQS;
dispatch_part(parsed_path, ParsedPath, _Env) ->
    ParsedPath;
dispatch_part(query_string, {_, QS, _}, _Env) ->
    QS;
dispatch_part(Other, _Path, _Env) ->
    error({dispatch_part, Other}).

dispatch_app(Dispatch, On) ->
    fun(Env) -> apply(Dispatch, dispatch_on(On, Env)) end.
