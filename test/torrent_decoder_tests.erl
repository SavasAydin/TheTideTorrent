-module(torrent_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

integer_is_enclosed_in_i_and_e_test() ->
    Res = torrent_decoder:decode(<<"i20130922e">>),
    ?assertEqual({"20130922", []}, Res).

anything_after_e_is_ignored_test() ->
    Res = torrent_decoder:decode(<<"i20130922e5:">>),
    ?assertEqual({"20130922", "5:"}, Res).

integer_zero_test() ->
    Res = torrent_decoder:decode(<<"i0e">>),
    ?assertEqual({"0", []}, Res).

negative_integer_test() ->
    Res = torrent_decoder:decode(<<"i-3e">>),
    ?assertEqual({"-3", []}, Res).

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
    ?assertEqual({["ant", "horse", "5"], []}, Res).

byte_string_encoded_string_length_followed_by_string_test() ->
    Res = torrent_decoder:decode(<<"4:dose">>),
    ?assertEqual({"dose", []}, Res).

empty_byte_string_test() ->
    Res = torrent_decoder:decode(<<"0:">>),
    ?assertEqual({"", []}, Res).

dictionary_is_enclosed_in_d_and_e_test() ->
    Res = torrent_decoder:decode(<<"d3:cow3:moo4:spam1:3e">>),
    ?assertEqual({#{"cow" => "moo", "spam" => "3"}, []}, Res).

dictinary_with_a_long_value_test() ->
    Res = torrent_decoder:decode(<<"d3:key24:a.value_with_long:lengthe">>),
    ?assertEqual({#{"key" => "a.value_with_long:length"}, []}, Res).

decode_metainfo_file_test() ->
    {ok, Bin} = file:read_file("./priv/alice.torrent"),
    Res = torrent_decoder:decode(Bin),
    ?assertEqual({#{"creation date" => "1452468725091",
                    "encoding" => "UTF-8",
                    "info" => #{"length" => "163783",
                                "name" => "alice.txt",
                                "piece length" => "16384",
                                "pieces" => [36,192,99,82,184,241,141,203,196,131,20,34,77,108,162,
                                             38,14,24,242,191,210,203,185,139,225,63,229,126,97,253,
                                             2,36,169,2,24,60,125,94,174,101,65,191,31,23,187,228,
                                             99,219,57,27,105,129,220,175,47,249,67,66,88,219,90,69,
                                             8,190,16,91,237,212,48,81,204,248,77,212,226,202,22,
                                             118,93,234,188,70,204,161,101,0,254,14,115,49,160,146,
                                             35,157,73,49,209,157,253,65,108,71,131,71,193,148,236,
                                             27,225,45,208,104,88,119,25,194,42,248,106,155,141,75,
                                             83,107,165,237,219,6,68,242,128,7,123,191,216,99,203,
                                             123,152,96,234,210,60,79,60,124,15,71,156,53,40,2,159,
                                             159,239,184,150,117,135,129,171,163,218,137,252,11,185,
                                             71,71,168,84,170,129,181,158,238,69,34,2,103,217,14,2,
                                             89,218,191,146,13,129,88,40,232,215,93,177,130,205,43,
                                             248,100]}},
                  []},
                 Res).
