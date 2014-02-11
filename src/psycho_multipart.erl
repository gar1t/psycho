-module(psycho_multipart).

-export([new/3, data/2, form_data/1]).

-record(mp, {boundary_delim, parts, headers, acc}).

new(Boundary, _Callback, _Data) when is_binary(Boundary) ->
    #mp{
       boundary_delim=boundary_delim(Boundary),
       parts=[],
       headers=pending,
       acc=[]}.

boundary_delim(Boundary) ->
    <<"--", Boundary/binary>>.

data(Data, MP) ->
    try_boundary(Data, MP).

try_boundary(Data, #mp{boundary_delim=Delim, acc=Acc}=MP) ->
    Window = search_window(Data, Acc),
    handle_boundary_match(binary:match(Window, Delim), Window, Data, MP).

search_window(Data, []) ->
    Data;
search_window(Data, [Last|_]) ->
    join(Last, Data).

join(B1, B2) ->
    <<B1/binary, B2/binary>>.

handle_boundary_match(nomatch, Window, Data, MP) ->
    try_headers(Window, Data, MP);
handle_boundary_match({Pos, Len}, Window, _Data, MP) ->
    <<Prev:Pos/binary, _:Len/binary, Next/binary>> = Window,
    new_part(Next, finalize_part(Prev, MP)).

finalize_part(<<>>, MP) -> MP;
finalize_part(Data, #mp{acc=Acc}=MP) ->
    add_cur(try_headers(Data, MP#mp{acc=pop_last(Acc)})).

pop_last([]) -> [];
pop_last([_|Rest]) -> Rest.

-define(HEADERS_DELIM, <<"\r\n\r\n">>).

try_headers(Data, MP) ->
    try_headers(Data, Data, MP).

try_headers(Window, Data, #mp{headers=pending}=MP) ->
    handle_match_headers(
      binary:match(Window, ?HEADERS_DELIM),
      Window, Data, MP);
try_headers(_Window, Data, #mp{acc=Acc}=MP) ->
    MP#mp{acc=[Data|Acc]}.

handle_match_headers(nomatch, _Window, Data, #mp{acc=Acc}=MP) ->
    MP#mp{acc=[Data|Acc]};
handle_match_headers({Pos, Len}, Window, _Data, MP) ->
    <<Prev:Pos/binary, _:Len/binary, Next/binary>> = Window,
    start_body(Next, finalize_headers(Prev, MP)).

finalize_headers(Data, #mp{acc=Acc}=MP) ->
    Raw = iolist_to_binary(lists:reverse([Data|pop_last(Acc)])),
    Headers = parse_headers(Raw),
    MP#mp{headers=Headers}.

parse_headers(<<"\r\n", Raw/binary>>) ->
    [parse_header(Part) || Part <- split_headers(Raw)].

split_headers(Raw) ->
    binary:split(Raw, <<"\r\n">>, [global]).

parse_header(Raw) ->
    case binary:split(Raw, <<":">>) of
        [Name, RawVal] -> {Name, header_val(RawVal)};
        [Name] -> {Name, <<>>}
    end.

header_val(<<" ", Val/binary>>) -> Val;
header_val(Val) -> Val.

add_cur(#mp{headers=Headers, acc=Acc, parts=Parts}=MP) ->
    Part = {Headers, finalize_body(Acc)},
    MP#mp{parts=[Part|Parts], headers=pending, acc=[]}.

start_body(Data, MP) -> MP#mp{acc=[Data]}.

finalize_body([]) -> <<>>;
finalize_body([Last|Rest]) ->
    LastTrimmed = strip_trailing_crlf(Last),
    iolist_to_binary(lists:reverse([LastTrimmed|Rest])).

strip_trailing_crlf(Bin) ->
    N = size(Bin) - 2,
    case Bin of
        <<Stripped:N/binary, "\r\n">> -> Stripped;
        _ -> Bin
    end.

new_part(Data, MP) ->
    try_headers(Data, MP).

form_data(#mp{parts=Parts, acc=Acc}) ->
    [{parts, lists:reverse(Parts)},
     {acc, Acc}].
