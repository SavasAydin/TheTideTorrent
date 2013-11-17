-module(torrent_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

-record(metadata, {string_byte,
		  integer,
		  list,
		  dictionary}).

decode_integer_test_() ->
    [fun valid_integer_alberts_birthdate/0,
     fun valid_integer_end_with_e_disclosure/0,
     fun valid_integer_zero/0,
     fun valid_negative_integer/0,
     fun invalid_integer_format_due_to_no_e_enclosure/0,
     fun invalid_integer_format_due_to_not_integer/0,
     fun invalid_integer_due_to_dot/0,
     fun invalid_integer_due_to_minus_after_an_integer/0,
     fun invalid_integer_due_to_zero_with_another_integer/0,
     fun invalid_integer_due_to_minus_before_zero/0
    ].

valid_integer_alberts_birthdate() ->
    Metadata = torrent_decoder:decode(<<"i20130922e">>),
    ?assertEqual(20130922, Metadata#metadata.integer). 


valid_integer_end_with_e_disclosure() ->
    Metadata = torrent_decoder:decode(<<"i20130922e5:">>),
    ?assertEqual(20130922, Metadata#metadata.integer). 


valid_integer_zero() ->
    Metadata = torrent_decoder:decode(<<"i0e">>),
    ?assertEqual(0, Metadata#metadata.integer). 

valid_negative_integer() ->
    Metadata = torrent_decoder:decode(<<"i-3e">>),
    ?assertEqual(-3, Metadata#metadata.integer).


invalid_integer_format_due_to_no_e_enclosure() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"i12345">>)).

invalid_integer_format_due_to_not_integer() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"inot_inte">>)).
    
invalid_integer_due_to_dot() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"i2013.09.22e">>)).

invalid_integer_due_to_minus_after_an_integer() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"i2-3e">>)).    

invalid_integer_due_to_zero_with_another_integer() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"i02e">>)).    

invalid_integer_due_to_minus_before_zero() ->
    ?assertException(error, 
		     invalid_integer_format, 
		     torrent_decoder:decode(<<"i-0e">>)).    

