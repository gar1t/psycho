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
    handle_headers_state(headers_state(MP), Data, MP).

headers_state(#mp{last=undefined, headers=pending}) -> not_started;
headers_state(#mp{headers=pending})                 -> pending;
headers_state(_)                                    -> finalized.

handle_headers_state(not_started, Data, MP) ->
    try_boundary(Data, MP);
handle_headers_state(pending, Data, MP) ->
    try_headers(Data, MP);
handle_headers_state(finalized, Data, MP) ->
    try_boundary(Data, MP).

try_boundary(Data, MP) ->
    Window = search_window(Data, MP),
    handle_boundary_split(split_boundary(Window, MP), Data, MP).

search_window(Data, #mp{last=undefined}) -> Data;
search_window(Data, #mp{last=Last}) -> <<Last/binary, Data/binary>>.

split_boundary(Data, #mp{boundary_delim=Delim}) ->
    binary:split(Data, Delim).

handle_boundary_split([Window], Data, MP) ->
    handle_window(Window, Data, MP);
handle_boundary_split([<<>>, After], _Data, MP) ->
    try_headers(After, After, MP);
handle_boundary_split([Before, After], _Data, MP) ->
    try_headers(After, After, finalize_part(handle_last_data(Before, MP))).

handle_window(Window, Data, #mp{headers=pending}=MP) ->
    try_headers(Window, Data, MP);
handle_window(_Window, Data, MP) ->
    handle_body_data(Data, MP).

try_headers(Data, MP) ->
    try_headers(search_window(Data, MP), Data, MP).

try_headers(Window, Data, MP) ->
    handle_headers_split(split_on_headers_delim(Window), Data, MP).

-define(HEADERS_DELIM, <<"\r\n\r\n">>).

split_on_headers_delim(Window) ->
    binary:split(Window, ?HEADERS_DELIM).

handle_headers_split([_Window], Data, MP) ->
    push_data(Data, MP);
handle_headers_split([Before, After], _Data, MP) ->
    start_body(After, finalize_headers(replace_last(Before, MP))).

replace_last(Last, MP) -> MP#mp{last=Last}.

finalize_headers(MP) ->
    Headers = parse_headers(raw_headers(MP)),
    Name = form_data_name(Headers),
    handle_finalized_headers(Name, Headers, MP).

raw_headers(#mp{last=Last, acc=Acc}) ->
    iolist_to_binary(lists:reverse([Last|Acc])).

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

-define(FORM_DATA_NAME_RE, <<"form-data; *name=\"(.*?)\"">>).

form_data_name(Headers) ->
    Disp = proplists:get_value("Content-Disposition", Headers, ""),
    handle_form_data_name_re(
      re:run(Disp, ?FORM_DATA_NAME_RE, [{capture, [1], list}])).

handle_form_data_name_re({match, [Name]}) -> Name;
handle_form_data_name_re(nomatch) -> <<>>.

handle_finalized_headers(Name, Headers, #mp{cb=undefined}=MP) ->
    MP#mp{name=Name, headers=Headers};
handle_finalized_headers(Name, Headers, MP) ->
    handle_part_cb(part_cb(Name, Headers, MP), Name, Headers, MP).

part_cb(Name, Headers, #mp{cb=Callback, cb_data=CbData}) ->
    Callback({part, Name, Headers}, CbData).

handle_part_cb({continue, CbData}, Name, Headers, MP) ->
    MP#mp{name=Name, headers=Headers, cb_data=CbData};
handle_part_cb({continue, {Name, Headers}, CbData}, _, _, MP) ->
    MP#mp{name=Name, headers=Headers, cb_data=CbData};
handle_part_cb({drop, CbData}, Name, _, MP) ->
    MP#mp{name=Name, headers=dropping, cb_data=CbData}.

start_body(Data, MP) ->
    try_boundary(Data, MP#mp{last=undefined, acc=[]}).

handle_last_data(Data, #mp{cb=undefined}=MP) ->
    replace_last(strip_trailing_crlf(Data), MP);
handle_last_data(_Data, #mp{headers=dropping}=MP) ->
    MP;
handle_last_data(Data, MP) ->
    Stripped = strip_trailing_crlf(Data),
    handle_last_data_cb(data_cb(Stripped, MP), Stripped, MP).

data_cb(Data, #mp{name=Name, cb=Callback, cb_data=CbData}) ->
    Callback({data, Name, Data}, CbData).

strip_trailing_crlf(Bin) ->
    N = size(Bin) - 2,
    case Bin of
        <<Stripped:N/binary, "\r\n">> -> Stripped;
        _ -> Bin
    end.

handle_last_data_cb({continue, CbData}, Data, MP) ->
    replace_last(Data, MP#mp{cb_data=CbData});
handle_last_data_cb({continue, OtherData, CbData}, _Data, MP) ->
    replace_last(OtherData, MP#mp{cb_data=CbData});
handle_last_data_cb({drop, CbData}, _Data, MP) ->
    MP#mp{cb_data=CbData}.

finalize_part(#mp{headers=dropping}=MP) ->
    reset_part_attrs(MP);
finalize_part(MP) ->
    handle_finalized_part(new_part(MP), MP).

reset_part_attrs(MP) ->
    MP#mp{name=undefined, headers=pending, last=undefined, acc=[]}.

new_part(#mp{name=Name, headers=Headers, last=Last, acc=Acc}) ->
    Body = iolist_to_binary(lists:reverse([Last|Acc])),
    {Name, {Headers, Body}}.

handle_finalized_part(Part, #mp{cb=undefined}=MP) ->
    reset_part_attrs(add_part(Part, MP));
handle_finalized_part(Part, MP) ->
    handle_eof_cb(eof_cb(MP), Part, MP).

eof_cb(#mp{cb=Callback, cb_data=CbData, name=Name}) ->
    Callback({data, Name, eof}, CbData).

add_part(Part, #mp{parts=Parts}=MP) ->
    MP#mp{parts=[Part|Parts]}.

handle_eof_cb({continue, CbData}, Part, MP) ->
    reset_part_attrs(add_part(Part, MP#mp{cb_data=CbData}));
handle_eof_cb({drop, CbData}, _Part, MP) ->
    reset_part_attrs(MP#mp{cb_data=CbData}).

handle_body_data(Data, #mp{cb=undefined}=MP) ->
    push_data(Data, MP);
handle_body_data(Data, #mp{headers=dropping}=MP) ->
    replace_last(Data, MP);
handle_body_data(Data, #mp{last=undefined}=MP) ->
    push_data(Data, MP);
handle_body_data(Data, #mp{last=Last}=MP) ->
    handle_data_cb(data_cb(Last, MP), Data, MP).

handle_data_cb({continue, CbData}, Data, MP) ->
    push_data(Data, MP#mp{cb_data=CbData});
handle_data_cb({continue, NewLast, CbData}, Data, MP) ->
    push_data(Data, MP#mp{last=NewLast, cb_data=CbData});
handle_data_cb({drop, CbData}, Data, MP) ->
    replace_last(Data, MP#mp{cb_data=CbData}).

push_data(Data, #mp{last=undefined}=MP) ->
    MP#mp{last=Data};
push_data(Data, #mp{last=Last, acc=Acc}=MP) ->
    MP#mp{last=Data, acc=[Last|Acc]}.

form_data(#mp{parts=Parts}) ->
    lists:reverse(Parts).

user_data(#mp{cb_data=Data}) ->
    Data.
