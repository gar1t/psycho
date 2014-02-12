-module(psycho_multipart).

-export([new/1, new/3, data/2, form_data/1, user_data/1]).

-record(mp, {boundary_delim, parts, name, headers, last, acc, cb, cb_data}).

new(Boundary) ->
    new(Boundary, undefined, undefined).

new(Boundary, Callback, Data) when is_binary(Boundary) ->
    #mp{
       boundary_delim=boundary_delim(Boundary),
       parts=[],
       name=undefined,
       headers=pending,
       last=undefined,
       acc=[],
       cb=Callback,
       cb_data=Data}.

boundary_delim(Boundary) ->
    <<"--", Boundary/binary>>.

data(Data, MP) ->
    try_boundary(Data, MP).

try_boundary(Data, #mp{boundary_delim=Delim}=MP) ->
    Window = search_window(Data, MP),
    handle_boundary_match(binary:match(Window, Delim), Window, Data, MP).

search_window(Data, #mp{last=undefined}) -> Data;
search_window(Data, #mp{last=Last}) -> join(Last, Data).

join(B1, B2) -> <<B1/binary, B2/binary>>.

handle_boundary_match(nomatch, Window, Data, MP) ->
    try_headers(Window, Data, MP);
handle_boundary_match({Pos, Len}, Window, _Data, MP) ->
    <<Prev:Pos/binary, _:Len/binary, Next/binary>> = Window,
    new_part(Next, finalize_part(Prev, MP)).

finalize_part(<<>>, MP) -> MP;
finalize_part(LastData, MP) ->
    add_part(notify_part_end(handle_last_data(LastData, MP))).

handle_last_data(Data, MP) ->
    handle_data(<<>>, MP#mp{last=strip_trailing_crlf(Data)}).

strip_trailing_crlf(Bin) ->
    N = size(Bin) - 2,
    case Bin of
        <<Stripped:N/binary, "\r\n">> -> Stripped;
        _ -> Bin
    end.

notify_part_end(#mp{cb=undefined}=MP) -> MP;
notify_part_end(#mp{headers=dropping}=MP) -> MP;
notify_part_end(#mp{name=Name, cb=Callback, cb_data=CbData}=MP) ->
    Result = Callback({data, Name, <<>>}, CbData),
    handle_data_cb(Result, <<>>, MP).

try_headers(Data, MP) ->
    try_headers(Data, Data, MP).

-define(HEADERS_DELIM, <<"\r\n\r\n">>).

try_headers(Window, Data, #mp{headers=pending}=MP) ->
    handle_match_headers(
      binary:match(Window, ?HEADERS_DELIM),
      Window, Data, MP);
try_headers(_Window, Data, MP) ->
    handle_data(Data, MP).

handle_data(Data, #mp{cb=undefined}=MP) ->
    push_data(Data, MP);
handle_data(_Data, #mp{headers=dropping}=MP) ->
    MP;
handle_data(Data, #mp{last=Last, name=Name, cb=Callback, cb_data=CbData}=MP) ->
    Result = Callback({data, Name, Last}, CbData),
    handle_data_cb(Result, Data, MP).

handle_data_cb({continue, CbData}, Data, MP) ->
    push_data(Data, MP#mp{cb_data=CbData});
handle_data_cb({continue, Data, CbData}, _, MP) ->
    push_data(Data, MP#mp{cb_data=CbData});
handle_data_cb({drop, CbData}, Data, MP) ->
    push_data(Data, MP#mp{last=undefined, cb_data=CbData}).

push_data(Data, #mp{last=undefined}=MP) ->
    MP#mp{last=Data};
push_data(Data, #mp{last=Last, acc=Acc}=MP) ->
    MP#mp{last=Data, acc=[Last|Acc]}.

handle_match_headers(nomatch, _Window, Data, MP) ->
    push_data(Data, MP);
handle_match_headers({Pos, Len}, Window, _Data, MP) ->
    <<Prev:Pos/binary, _:Len/binary, Next/binary>> = Window,
    start_body(Next, finalize_headers(Prev, MP)).

finalize_headers(Data, MP) ->
    Raw = raw_headers(Data, MP),
    Headers = parse_headers(Raw),
    Name = form_data_name(Headers),
    notify_headers(Name, Headers, MP).

raw_headers(LastData, #mp{acc=Acc}) ->
    iolist_to_binary(lists:reverse([LastData|Acc])).

parse_headers(<<"\r\n", Raw/binary>>) ->
    [parse_header(Part) || Part <- split_headers(Raw)];
parse_headers(_) -> [].

split_headers(Raw) ->
    binary:split(Raw, <<"\r\n">>, [global]).

parse_header(Raw) ->
    case binary:split(Raw, <<":">>) of
        [Name, RawVal] -> {binary_to_list(Name), header_val(RawVal)};
        [Name] -> {binary_to_list(Name), <<>>}
    end.

header_val(<<" ", Val/binary>>) -> binary_to_list(Val);
header_val(Val) -> binary_to_list(Val).

notify_headers(Name, Headers, #mp{cb=undefined}=MP) ->
    MP#mp{name=Name, headers=Headers};
notify_headers(Name, Headers, #mp{cb=Callback, cb_data=CbData}=MP) ->
    Result = Callback({part, Name, Headers}, CbData),
    handle_part_cb(Result, Name, Headers, MP).

handle_part_cb({continue, CbData}, Name, Headers, MP) ->
    MP#mp{name=Name, headers=Headers, cb_data=CbData};
handle_part_cb({continue, {Name, Headers}, CbData}, _, _, MP) ->
    MP#mp{name=Name, headers=Headers, cb_data=CbData};
handle_part_cb({drop, CbData}, Name, _, MP) ->
    MP#mp{name=Name, headers=dropping, cb_data=CbData}.

start_body(Data, MP) ->
    MP#mp{last=Data, acc=[]}.

add_part(#mp{headers=dropping}=MP) ->
    reset_part(MP);
add_part(#mp{name=Name, headers=Headers}=MP) ->
    Body = finalize_body(MP),
    Part = {Name, {Headers, Body}},
    reset_part(add_part(Part, MP)).

add_part(Part, #mp{parts=Parts}=MP) ->
    MP#mp{parts=[Part|Parts]}.

reset_part(MP) ->
    MP#mp{name=undefined, headers=pending, last=undefined, acc=[]}.

finalize_body(#mp{last=Last, acc=Acc}) ->
    iolist_to_binary(lists:reverse([Last|Acc])).

-define(FORM_DATA_NAME_RE, <<"form-data; *name=\"(.*?)\"">>).

form_data_name(Headers) ->
    Disp = proplists:get_value("Content-Disposition", Headers, ""),
    handle_form_data_name_re(
      re:run(Disp, ?FORM_DATA_NAME_RE, [{capture, [1], list}])).

handle_form_data_name_re({match, [Name]}) -> Name;
handle_form_data_name_re(nomatch) -> <<>>.

new_part(Data, MP) ->
    try_headers(Data, MP).

form_data(#mp{parts=Parts}) ->
    lists:reverse(Parts).

user_data(#mp{cb_data=Data}) ->
    Data.
