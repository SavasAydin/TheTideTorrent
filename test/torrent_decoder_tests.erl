-module(torrent_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

decode_integer_test_() ->
    [fun valid_integer_alberts_birthdate/0,
     fun valid_integer_end_with_e_disclosure/0,
     fun valid_integer_zero/0,
     fun valid_negative_integer/0,
     fun invalid_integer_format_due_to_not_integer/0,
     fun invalid_integer_due_to_dot/0,
     fun invalid_integer_due_to_minus_after_an_integer/0,
     fun invalid_integer_due_to_zero_with_another_integer/0,
     fun invalid_integer_due_to_minus_before_zero/0
    ].

valid_integer_alberts_birthdate() ->
    ?assertEqual(20130922, 
		 torrent_decoder:decode(<<"i20130922e">>)).

valid_integer_end_with_e_disclosure() ->
    ?assertEqual(20130922, 
		 torrent_decoder:decode(<<"i20130922e5:">>)).

valid_integer_zero() ->
    ?assertEqual(0, 
		 torrent_decoder:decode(<<"i0e">>)).    

valid_negative_integer() ->
    ?assertEqual(-3, 
		 torrent_decoder:decode(<<"i-3e">>)).    

invalid_integer_format_due_to_not_integer() ->
  ?assertThrow({error, invalid_integer}, 
		 torrent_decoder:decode(<<"inot_integere">>)).
    
invalid_integer_due_to_dot() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_decoder:decode(<<"i2013.09.22e">>)).

invalid_integer_due_to_minus_after_an_integer() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_decoder:decode(<<"i2-3e">>)).    

invalid_integer_due_to_zero_with_another_integer() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_decoder:decode(<<"i02e">>)).    

invalid_integer_due_to_minus_before_zero() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_decoder:decode(<<"i-0e">>)).    

