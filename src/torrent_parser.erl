-module(torrent_parser).

-export([parse/1
	]).

parse(<<$i,ByteInteger/binary>> = Bin) ->
    InStringFormat = parse_integer(Bin),
    {Integer,[]} = string:to_integer(InStringFormat),
    Integer.

parse_integer(<<$i,$0,Integer,Rest/binary>>) when Integer >= $0 andalso
						  Integer =< $9 ->
    throw({error, invalid_integer});
parse_integer(<<$i,$-,Integer,Rest/binary>>) when Integer > $0 andalso
						  Integer =< $9 ->
    [$-, Integer | parse_integer(Rest)];
parse_integer(<<$i,Rest/binary>>) ->
    parse_integer(Rest);
parse_integer(<<$e,_/binary>>) ->
    [];
parse_integer(<<Integer,Rest/binary>>) when Integer >= $0 andalso
					    Integer =< $9 ->
    [Integer | parse_integer(Rest)];
parse_integer(_) ->
    throw({error, invalid_integer}).

    
