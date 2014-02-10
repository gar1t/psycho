-module(psycho_multipart).

-export([new/1, data/2, form_data/1]).

-record(mp, {delim, parts, part_acc, last}).

new(Boundary) when is_binary(Boundary) ->
    Delim = <<"--", Boundary/binary>>,
    #mp{delim=Delim, parts=[], part_acc=[], last=(<<>>)}.

data(Data, #mp{delim=Delim, last=Last}=MP) ->
    JoinedData = join(Last, Data),
    handle_match(binary:match(JoinedData, Delim), JoinedData, MP).

join(<<>>, Data) -> Data;
join(Last, Data) -> <<Last/binary, Data/binary>>.

handle_match(nomatch, Data, MP) ->
    acc_part(Data, MP);
handle_match({Pos, Len}, Data, MP) ->
    <<Head:Pos/binary, _:Len/binary, Rest/binary>> = Data,
    data(Rest, maybe_new_part(Head, MP)).

acc_part(Data, #mp{part_acc=Acc}=MP) ->
    MP#mp{part_acc=[Data|Acc], last=Data}.

maybe_new_part(<<>>, MP) -> MP;
maybe_new_part(Head, #mp{part_acc=PartAcc, parts=Parts}=MP) ->
    Part = lists:reverse([Head|PartAcc]),
    MP#mp{parts=[Part|Parts], part_acc=[], last=(<<>>)}.

form_data(MP) ->
    form_data_from_parts(parts(MP)).

parts(#mp{part_acc=[], parts=Parts}) ->
    lists:reverse(Parts);
parts(#mp{part_acc=[<<"--\r\n">>], parts=Parts}) ->
    lists:reverse(Parts);
parts(#mp{part_acc=PartAcc, parts=Parts}) ->
    lists:reverse([lists:reverse(PartAcc)|Parts]).

form_data_from_parts(Parts) ->
    [parse_part(Part) || Part <- Parts].

parse_part(Part) ->
    case binary:split(iolist_to_binary(Part), <<"\r\n\r\n">>) of
        [<<"\r\n", RawHeaders/binary>>, RawBody] ->
            {parse_headers(RawHeaders), strip_trailing_crlf(RawBody)};
        [Body] ->
            {<<>>, Body}
    end.

parse_headers(Raw) ->
    Raw.

strip_trailing_crlf(Raw) ->
    N = size(Raw) - 2,
    <<Stripped:N/binary, "\r\n">> = Raw,
    Stripped.
