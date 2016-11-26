-module(zpevnik_tests).
-include_lib("eunit/include/eunit.hrl").

get_song_title_test() ->
    %?assertEqual(#, zpevnik:#()),
    ?assertEqual("nazev", zpevnik:get_song_title("{t:nazev}")),
    %?assertEqual("nazev", zpevnik:get_song_title("{title:nazev}")),
    %?assertEqual("nazev", zpevnik:get_song_title("{  title  :  nazev  }")),
    ?assertEqual("nazev", zpevnik:get_song_title("{  t  :  nazev  }")),
    ?assertEqual("", zpevnik:get_song_title("{}")).

split_by_directive_test() ->
    ?assertEqual(["text ~n", "~ndalsi"], zpevnik:split_by_songs("text ~n{ns}~ndalsi")),
    ?assertEqual(["text ~n", "~ndalsi"], zpevnik:split_by_songs("{ ns }~ntext ~n{ns}~ndalsi")).

split_by_empty_line_test() ->
    ?assertEqual(["text ", "dalsi"], zpevnik:split_by_empty_lines("text \r\n\r\ndalsi")).

split_by_new_lines_test() ->
    ?assertEqual(["text", "dalsi"], zpevnik:split_by_new_lines("text\ndalsi")).

line_is_type_empty_test() ->
    ?assertEqual(empty, zpevnik:is_line_type_empty("  ")),
    ?assertEqual(false, zpevnik:is_line_type_empty("  ...")).

line_is_type_directive_test() ->
    ?assertEqual(directive, zpevnik:is_line_type_directive("{  t  :  nazev  }")).

line_type_test() ->
    ?assertEqual(empty, zpevnik:line_type("  ")),
    ?assertEqual(directive, zpevnik:line_type("{  t  :  nazev  }")),
    ?assertEqual(normal, zpevnik:line_type(" text ")).
