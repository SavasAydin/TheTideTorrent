-module(torrent_decoder_tests).

-include_lib("eunit/include/eunit.hrl").

integer_is_enclosed_in_i_and_e_test() ->
    Res = torrent_decoder:decode(<<"i20130922e">>),
    ?assertEqual({{int, "20130922"}, []}, Res).

anything_after_e_is_ignored_test() ->
    Res = torrent_decoder:decode(<<"i20130922e5:">>),
    ?assertEqual({{int, "20130922"}, "5:"}, Res).

integer_zero_test() ->
    Res = torrent_decoder:decode(<<"i0e">>),
    ?assertEqual({{int, "0"}, []}, Res).

negative_integer_test() ->
    Res = torrent_decoder:decode(<<"i-3e">>),
    ?assertEqual({{int, "-3"}, []}, Res).

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
    ?assertEqual({{list, ["ant", "horse", "5"]}, []}, Res).

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
    {ok, Bin} = file:read_file("./priv/example_pdf.torrent"),
    Res = torrent_decoder:decode(Bin),
    ?assertEqual({#{"announce" => "http://tracker.kicks-ass.net:80/announce",
                    "created by" => "uTorrent/1870",
                    "creation date" => {int, "1543782548"},
                    "encoding" => "UTF-8",
                    "info" => #{"length" => {int, "66465"},
                                "name" => "letters_numbers_ shapes_tracing1.pdf",
                                "piece length" => {int, "16384"},
                                "pieces" => [76,172,47,120,198,197,228,126,89,99,201,86,168,79,196,
                                             41,7,9,17,153,103,73,169,171,67,135,141,221,60,46,165,
                                             87,55,94,51,203,38,237,73,117,217,62,190,169,226,101,
                                             208,149,115,57,160,131,207,135,97,214,153,37,136,220,
                                             10,149,199,19,147,183,154,37,64,25,34,16,251,171,171,3,
                                             65,44,14,222,131,247,225,13,126,146,176,147,119,209,
                                             189,75,141,59,157,53,114,187,192,94]}},
                  []},
                 Res).
