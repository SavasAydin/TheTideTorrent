-module(torrent_decoder).
-export([decode/1]).

decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode([$i | Int]) ->
    decode_integer(Int);
decode([$l | List]) ->
    decode_list(List);
decode([$d | Dict]) ->
    decode_dict(Dict, maps:new());
decode(Str) ->
    decode_string(Str).

decode_dict([$e | Rest], Res) ->
    {Res, Rest};
decode_dict(Dict, Acc) ->
    {Key, Dict2} = decode(Dict),
    {Value, Rest}  = decode(Dict2),
    decode_dict(Rest, maps:put(Key, Value, Acc)).

decode_string(Str) ->
    get_element(Str).

decode_integer(Int) ->
    decode_integer(Int, []).

decode_integer([$e | Rest], Result) ->
    {lists:reverse(Result), Rest};
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

decode_list(L) ->
    decode_list(L, []).

decode_list([$e | Rest], Res) ->
    {lists:reverse(Res), Rest};
decode_list(L, Acc) ->
    {E, Rest} = get_element(L),
    decode_list(Rest, [E | Acc]).

get_element(D) ->
    {Len, D2} = get_length(D, []),
    {[$: | Element], Rest} = lists:split(list_to_integer(Len)+1, D2),
    {Element, Rest}.

get_length([N|T], Acc) when N >= $0 andalso N =< $9 ->
    get_length(T, [N|Acc]);
get_length([$:|T], Acc) ->
    {lists:reverse(Acc), [$:|T]}.
