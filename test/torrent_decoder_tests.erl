-module(torrent_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

-record(metadata, {string,
                  integer,
                  list,
                  dict}).

integer_is_enclosed_in_i_and_e_test() ->
    Res = torrent_decoder:decode(<<"i20130922e">>),
    ?assertEqual("20130922", Res#metadata.integer).

anything_after_e_is_ignored_test() ->
    Res = torrent_decoder:decode(<<"i20130922e5:">>),
    ?assertEqual("20130922", Res#metadata.integer).

integer_zero_test() ->
    Res = torrent_decoder:decode(<<"i0e">>),
    ?assertEqual("0", Res#metadata.integer).

negative_integer_test() ->
    Res = torrent_decoder:decode(<<"i-3e">>),
    ?assertEqual("-3", Res#metadata.integer).

invalid_integer_missing_e_ending_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"i12345">>)).

not_an_integer_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"inot_inte">>)).

invalid_integer_due_to_dot_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"i2013.09.22e">>)).

invalid_integer_due_to_minus_after_an_integer_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"i2-3e">>)).

invalid_integer_due_to_zero_with_another_integer_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"i02e">>)).

invalid_integer_due_to_minus_before_zero_test() ->
    ?assertException(error,
                     invalid_integer_format,
                     torrent_decoder:decode(<<"i-0e">>)).

list_is_enclosed_in_l_and_e_test() ->
    Res = torrent_decoder:decode(<<"l3:ant5:horse1:5e">>),
    ?assertEqual(["ant", "horse", "5"], Res#metadata.list).

byte_string_encoded_string_length_followed_by_string_test() ->
    Res = torrent_decoder:decode(<<"4:dose">>),
    ?assertEqual("dose", Res#metadata.string).

empty_byte_string_test() ->
    Res = torrent_decoder:decode(<<"0:">>),
    ?assertEqual("", Res#metadata.string).

dictionary_is_enclosed_in_d_and_e_test() ->
    Res = torrent_decoder:decode(<<"d3:cow3:moo4:spam1:3e">>),
    ?assertEqual(#{"cow" => "moo", "spam" => "3"}, Res#metadata.dict).

