-module(torrent_decoder).

-export([decode/1
	]).

-record(metadata, {string_byte,
		  integer,
		  list,
		  dictionary}).

decode(<<$i,ByteInteger/binary>>) ->
    InStringFormat = decode_integer(ByteInteger, []),
    #metadata{integer = list_to_integer(InStringFormat)}.

decode_integer(<<$e,_/binary>>,Result) ->
    lists:reverse(Result);
decode_integer(<<$-,$0,_/binary>>,Result) when Result == [] ->
    error(invalid_integer_format);
decode_integer(<<$-,Rest/binary>>,Result) when Result == [] ->
    decode_integer(Rest, [$-|Result]);
decode_integer(<<$0,Integer,_/binary>>,Result) when Result == [] andalso
						    Integer >= $0 andalso
						    Integer =< $9 ->
    error(invalid_integer_format);
decode_integer(<<Integer,Rest/binary>>, Result) when Integer >= $0 andalso
					    Integer =< $9 ->
    decode_integer(Rest, [Integer|Result]);
decode_integer(_,_) ->
    error(invalid_integer_format).
    

