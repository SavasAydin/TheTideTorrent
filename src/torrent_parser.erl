-module(torrent_parser).

-export([parse/1
	]).

%% parse(ByteString) ->
%%     [IntString, String] = string:tokens(binary_to_list(ByteString),":"),
%%     {Int,_} = string:to_integer(IntString),
%%     case length(String) == Int of
%% 	true ->
%% 	    String;
%% 	false ->
%% 	    {error, incorrect_length}
%%     end.

parse(MetaData) when is_binary(MetaData) ->
    parse(binary_to_list(MetaData));
parse([$i | Rest ]) ->
    parse_integer(Rest).

parse_integer([$e | _Rest]) ->
    [];
parse_integer([Integer | Rest]) ->
    [Integer |  parse_integer(Rest)].

