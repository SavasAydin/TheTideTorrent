-module(torrent_decoder).
-export([decode/1]).

-record(metadata, {string,
                   integer,
                   list,
                   dict}).

decode(<<N,$:,String/binary>>) when is_integer(N) ->
    #metadata{string = decode_string(N, String)};
decode(<<$i,Int/binary>>) ->
    #metadata{integer = decode_integer(Int)};
decode(<<$l,List/binary>>) ->
    #metadata{list = decode_list(List)};
decode(<<$d,Dict/binary>>) ->
    #metadata{dict = decode_dict(Dict)}.

decode_string(Len, Bin) ->
    string:substr(binary_to_list(Bin), 1, Len).

decode_integer(Bin) ->
    decode_integer(binary_to_list(Bin), []).

decode_integer([$e | _], Result) ->
    lists:reverse(Result);
decode_integer([$-, $0 | _], []) ->
    error(invalid_integer_format);
decode_integer([$- | Rest], []) ->
    decode_integer(Rest, [$-]);
decode_integer([$0, I | _], []) when I >= $0 andalso I =< $9 ->
    error(invalid_integer_format);
decode_integer([I | Rest], Result) when I >= $0 andalso I =< $9 ->
    decode_integer(Rest, [I | Result]);
decode_integer(_, _) ->
    error(invalid_integer_format).

decode_list(Bin) ->
    decode_list(binary_to_list(Bin), []).

decode_list([$e], Res) ->
    lists:reverse(Res);
decode_list([N | Tail], Acc) ->
    {[$:|Element], Rest} = take_element(N, Tail),
    decode_list(Rest, [Element | Acc]).

decode_dict(Bin) ->
    decode_dict(binary_to_list(Bin), maps:new()).

decode_dict([$e], Res) ->
    Res;
decode_dict([N | Tail], Acc) ->
    {[$:|Key], [Num|Dict]} = take_element(N, Tail),
    {[$:|Value], Rest} = take_element(Num, Dict),
    decode_dict(Rest, maps:put(Key, Value, Acc)).

take_element(N, Encoded) ->
    {Len,_} = string:to_integer([N]),
    lists:split(Len+1, Encoded).
