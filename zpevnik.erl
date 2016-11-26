-module('zpevnik').
-compile(['export_all']).


%%%%%%%%%
% Parse %
%%%%%%%%%
parse(String) ->
    lists:map(fun get_song/1, split_by_songs(String)).

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

strings_by_matches(String, List) ->
    strings_by_matches(String, List, []).
strings_by_matches(_String, [], Acc) ->
    lists:reverse(Acc);
strings_by_matches(String, [[{Start, Len}]|T], Acc) ->
    strings_by_matches(String, T, [string:substr(String, Start + 1, Len) | Acc]).

find_directive_value(String, Name) ->
    case  ( re:run(String, "{\s*" ++ Name ++ "\s*:\s*(.*?)\s*}") ) of 
        {match, [_, {Start, Len}]} -> {Start, Len};
        _                          -> {Start, Len} = {0,0}
    end,
    string:substr(String, Start + 1, Len).

%%%%%%%%%%%%%
% formating %
%%%%%%%%%%%%%

format({song, Content}) ->
    "<div class='song'>" ++ Content ++ "</div>";
format([]) ->
    "";
format([H|T]) ->
    format(H) ++ format(T);
format(_) ->
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
    io:format("~p", [parse(File)]).
    %res = file:write_file("~/format.html", io_lib:fwrite("~p", [format(parse(File)))),
    %io:format("~p", [res]).
