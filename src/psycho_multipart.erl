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
    try_headers(After, After, finalize_part(replace_last(Before, MP))).

handle_window(Window, Data, #mp{headers=pending}=MP) ->
    try_headers(Window, Data, MP);
handle_window(_Window, Data, MP) ->
    push_data(Data, MP).

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

handle_finalized_headers(Name, Headers, MP) ->
    MP#mp{name=Name, headers=Headers}.

start_body(Data, MP) ->
    try_boundary(Data, MP#mp{last=undefined, acc=[]}).

finalize_part(MP) ->
    reset_part_attrs(add_part(new_part(MP), MP)).

push_data(Data, #mp{last=undefined}=MP) ->
    MP#mp{last=Data};
push_data(Data, #mp{last=Last, acc=Acc}=MP) ->
    MP#mp{last=Data, acc=[Last|Acc]}.

new_part(#mp{name=Name, headers=Headers}=MP) ->
    {Name, {Headers, body(MP)}}.

body(MP) -> iolist_to_binary(body_parts(MP)).

body_parts(#mp{last=Last, acc=Acc}) ->
    lists:reverse([strip_trailing_crlf(Last)|Acc]).

strip_trailing_crlf(Bin) ->
    N = size(Bin) - 2,
    case Bin of
        <<Stripped:N/binary, "\r\n">> -> Stripped;
        _ -> Bin
    end.

add_part(Part, #mp{parts=Parts}=MP) ->
    MP#mp{parts=[Part|Parts]}.

reset_part_attrs(MP) ->
    MP#mp{name=undefined, headers=pending, last=undefined, acc=[]}.

form_data(#mp{parts=Parts}) ->
    lists:reverse(Parts).

user_data(#mp{cb_data=Data}) ->
    Data.
