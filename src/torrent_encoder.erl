-module(torrent_encoder).
-export([encode/1,
         generate_hash_info/1]).

encode({int, Int}) ->
    "i" ++ Int ++ "e";
encode({list, L}) ->
    Elements = [encode(X) || X <- L],
    lists:concat([l | Elements]) ++ "e";
encode(Dict) when is_map(Dict) ->
    Pairs = [encode(K) ++ encode(V) || {K, V} <- maps:to_list(Dict)],
    lists:concat([d |Pairs]) ++ "e";
encode(Str) ->
    integer_to_list(length(Str)) ++ ":" ++ Str.

generate_hash_info(Data) ->
    binary_to_list(crypto:hash(sha, encode(maps:get("info", Data)))).
