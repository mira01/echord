-module('zpevnik').
-compile(['export_all']).


%%%%%%%%%
% Parse %
%%%%%%%%%
parse(String) ->
    {sheet, lists:map(fun get_song/1, split_by_songs(String))}.

get_song(String) ->
    Title = get_song_title(String),
    Content = parse_song_content(String),
    {'song', [Title | Content]}.

get_song_title(String) ->
    find_directive_value(String, "t").
    
get_lines(String) ->
    split_by_new_lines(String).

line_type(String) -> 
    line_type(String, [fun is_line_type_directive/1, fun is_line_type_empty/1]).
line_type(_String, []) -> normal;
line_type(String, FunsList) ->
    [H|T] = FunsList,
    case Type = H(String) of
        false -> line_type(String, T);
        _ -> Type
    end.

is_line_type_directive(String) ->
    case string:substr(String, 1, 1) == "{" of
        true -> directive;
        false -> false
    end.

is_line_type_empty(String) ->
    case re:run(String, "^\s*$") of
        {match, _} -> empty;
                _  -> false
    end.

parse_song_content(String) ->
    Lines = get_lines(String),
    LinesTuples = [{line_type(X), string:strip(X, both, $\n)} || X <- Lines],
    skip_first(skip_last(LinesTuples)).
    
split_by_songs(String) ->
    split_by_directive(String, "ns").

split_by_directive(String, Name) ->
    split_by(String, "{\s*" ++ Name ++ "(.*)\s*(:.*)?}").

split_by_new_lines(String) ->
    {match, Matches} = re:run(String, ".*?\n|.*$", [global]),
    strings_by_matches(String, Matches).

split_by_empty_lines(String) ->
    split_by(String, "\r\n\r\n(\r\n)*").

split_by(String, Pattern) ->
    lists:filter(fun(X) when X == []->
                   false;
              (_) -> true
            end,
            re:split(String, Pattern, [{return, list}])
            ).
parse_line(String) ->
    Parts = split_by(String, "(\\[.*?\\])"),
    lists:map(fun make_chord/1, Parts).

make_chord(String) ->
    case (re:run(String, "\\[(.+?)\\]")) of
        {match, [_Whole, Chord]} -> {chord, string_by_match(String, Chord)};
        nomatch -> String
    end.

strings_by_matches(String, List) ->
    strings_by_matches(String, List, []).
strings_by_matches(_String, [], Acc) ->
    lists:reverse(Acc);
strings_by_matches(String, [[{Start, Len}]|T], Acc) ->
    strings_by_matches(String, T, [string_by_match(String, {Start, Len}) | Acc]).
string_by_match(String, {Start, Len}) ->
    string:substr(String, Start + 1, Len).

find_directive_value(String, Name) ->
    case  ( re:run(String, "{\s*" ++ Name ++ "\s*:\s*(.*?)\s*}") ) of 
        {match, [_, {Start, Len}]} -> {Start, Len};
        _                          -> {Start, Len} = {0,0}
    end,
    string:substr(String, Start + 1, Len).

%%%%%%%%%%%%%
% formating %
%%%%%%%%%%%%%
format({sheet, Content}) ->
    lists:concat([
    "<html>
    <head>
    <meta charset='UTF-8'>
    </head>
    <body>",
    format(Content),
    "</body>
    </html>"
    ]);
format({song, [Title|Content]}) ->
    lists:concat([
        "<div class='song'>
        <h1>",
        Title,
        "</h1>",
        format(Content),
        "</div>"
    ]);
format({empty, _}) ->
    "<br/>";
format({Type, Content}) ->
    lists:concat(["<div class='", Type, "'>", format(Content), "</div>"]);
format([]) ->
    "";
format([H|T]) ->
    [format(H) | format(T)];
format(Else) ->
    Else.
format() ->
    "".
    

%%%%%%%%%%%%%%%%%
% small helpers %
%%%%%%%%%%%%%%%%%
skip_first(List) ->
    [_First| T] = List,
    T.

skip_last(List) ->
    [_Last | T] = lists:reverse(List),
    lists:reverse(T).



%% Read File functions
test() ->
    {ok, File} = file:read_file("test.chord"),
    io:format("~ts", [format(parse(File))]).
test_format() ->
    {ok, File} = file:read_file("test.chord"),
    io:format("~p", [parse(File)]).
    %res = file:write_file("~/format.html", iolist_to_binary(format(parse(File)))),
    %io:format("~ts", [res]).
