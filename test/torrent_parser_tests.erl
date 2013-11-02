-module(torrent_parser_tests).

-include_lib("eunit/include/eunit.hrl").

valid_parse_integer_test() ->
    ?assertEqual("20130922", 
		 torrent_parser:parse(<<"i20130922e">>)).

parse_integer_until_e_eventhough_there_is_more_to_parse_test() ->
    ?assertEqual("20130922", 
		 torrent_parser:parse(<<"i20130922e5:">>)).

