-module(torrent_encoder_test).
-include_lib("eunit/include/eunit.hrl").

encode_dictionary_test() ->
    Res = torrent_encoder:encode(#{"key" => "value"}),
    ?assertEqual("d3:key5:valuee", Res).

encode_string_test() ->
    Res = torrent_encoder:encode("key"),
    ?assertEqual("3:key", Res).

encode_integer_test() ->
    Res = torrent_encoder:encode({int, "12345"}),
    ?assertEqual("i12345e", Res).

encode_list_test() ->
    Res = torrent_encoder:encode({list, ["key", "value"]}),
    ?assertEqual("l3:key5:valuee", Res).

generate_hash_info_test() ->
    Res = torrent_encoder:generate_hash_info(example_metadata()),
    Expected = [14,79,41,80,6,117,178,120,227,245,211,
                184,78,142,178,32,238,224,51,98],
    ?assertEqual(Expected, Res).

example_metadata() ->
    #{"announce" => "http://tracker.kicks-ass.net:80/announce",
       "created by" => "uTorrent/1870",
       "creation date" => "1543782548",
       "encoding" => "UTF-8",
       "info" => #{"length" => "66465",
                   "name" => "letters_numbers_ shapes_tracing1.pdf",
                   "piece length" => "16384",
                   "pieces" => [76,172,47,120,198,197,228,126,89,99,201,86,168,79,196,
                                41,7,9,17,153,103,73,169,171,67,135,141,221,60,46,165,
                                87,55,94,51,203,38,237,73,117,217,62,190,169,226,101,
                                208,149,115,57,160,131,207,135,97,214,153,37,136,220,
                                10,149,199,19,147,183,154,37,64,25,34,16,251,171,171,3,
                                65,44,14,222,131,247,225,13,126,146,176,147,119,209,
                                189,75,141,59,157,53,114,187,192,94]}}.
