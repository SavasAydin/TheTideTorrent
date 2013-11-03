-module(torrent_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_integer_test_() ->
    [fun valid_integer_alberts_birthdate/0,
     fun valid_integer_end_with_e_disclosure/0,
     fun valid_integer_zero/0,
     fun valid_negative_integer/0,
     fun invalid_integer_due_to_dot/0,
     fun invalid_integer_due_to_minus_after_an_integer/0,
     fun invalid_integer_due_to_zero_with_another_integer/0,
     fun invalid_integer_due_to_minus_before_zero/0
    ].

valid_integer_alberts_birthdate() ->
    ?assertEqual(20130922, 
		 torrent_parser:parse(<<"i20130922e">>)).

valid_integer_end_with_e_disclosure() ->
    ?assertEqual(20130922, 
		 torrent_parser:parse(<<"i20130922e5:">>)).

valid_integer_zero() ->
    ?assertEqual(0, 
		 torrent_parser:parse(<<"i0e">>)).    

valid_negative_integer() ->
    ?assertEqual(-3, 
		 torrent_parser:parse(<<"i-3e">>)).    

invalid_integer_due_to_dot() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_parser:parse(<<"i2013.09.22e">>)).

invalid_integer_due_to_minus_after_an_integer() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_parser:parse(<<"i2-3e">>)).    

invalid_integer_due_to_zero_with_another_integer() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_parser:parse(<<"i02e">>)).    

invalid_integer_due_to_minus_before_zero() ->
    ?assertThrow({error, invalid_integer}, 
		 torrent_parser:parse(<<"i-0e">>)).    

