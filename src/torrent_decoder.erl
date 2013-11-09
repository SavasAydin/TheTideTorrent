-module(torrent_decoder).

-export([decode/1
	]).

decode(<<$i,ByteInteger/binary>> = Bin) ->
    InStringFormat = decode_integer(Bin),
    {Integer,[]} = string:to_integer(InStringFormat),
    Integer.

decode_integer(<<$i,$0,Integer,Rest/binary>>) when Integer >= $0 andalso
						   Integer =< $9 ->
    throw({error, invalid_integer});
decode_integer(<<$i,$-,Integer,Rest/binary>>) when Integer > $0 andalso
						  Integer =< $9 ->
    [$-, Integer | decode_integer(Rest)];
decode_integer(<<$i,Rest/binary>>) ->
    decode_integer(Rest);
decode_integer(<<$e,_/binary>>) ->
    [];
decode_integer(<<Integer,Rest/binary>>) when Integer >= $0 andalso
					    Integer =< $9 ->
    [Integer | decode_integer(Rest)];
decode_integer(_) ->
    throw({error, invalid_integer}).

    
