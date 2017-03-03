-module(index).
-export([get_file_contents/1,show_file_contents/1, create_index/1, format_lines/1, index_file/1, print_index/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.


% index file
% create index of all words in file and print index to output
index_file(File) ->
  Lines = get_file_contents(File),
  Index = create_index(format_lines(Lines)),
  print_index(Index).


%
% getting file lines
%

% format file lines for as index input (unique list of words for every line)
format_lines(Lines) ->
  lists:map(fun(Line) -> format_line(Line) end, Lines).

% get unique list of words in line
format_line(Line) ->
    unique_list(remove_empty(split_line(Line))).

% split line to list of words
split_line(Line) ->
    string:tokens(Line, " \t\n.,!?").

% remove empty strings from list
remove_empty([]) ->
    [];
remove_empty([X|Xs]) ->
    case X == "" of
        true -> remove_empty(Xs);
        false -> [X|remove_empty(Xs)]
    end.

% make list of unique elements (remove duplicates)
unique_list(List) ->
    make_unique_list(List, []).

make_unique_list([], Out) ->
    Out;
make_unique_list([X|Xs], Out) ->
    case lists:member(X,Out) of
        true ->
            make_unique_list(Xs, Out);
        false ->
            make_unique_list(Xs, [X|Out])
    end.


%
% create index
%

% create file index (index is stored in orddict)
create_index(Lines) ->
    Index = orddict:new(),
    index_lines(1, Lines, Index).

% indexs lines
index_lines(_LineNumber, [], Index) ->
    Index;
index_lines(LineNumber, [Line|Lines], Index) ->
    NewIndex = index_line(LineNumber, Line, Index),
    index_lines(LineNumber+1, Lines, NewIndex).

% index single line (index every word in line)
index_line(_LineNumber, [], Index) ->
    Index;
index_line(LineNumber, [Word|Words], Index) ->
    case orddict:find(Word, Index) of
        {ok, Value} ->
            UpdatedIndex = orddict:store(Word, [LineNumber|Value], Index),
            index_line(LineNumber, Words, UpdatedIndex);
        error ->
            UpdatedIndex = orddict:store(Word, [LineNumber], Index),
            index_line(LineNumber, Words, UpdatedIndex)
    end.


%
% print index
%

% print index
print_index(Index) ->
    IndexList = orddict:to_list(Index),
    print_index_list(IndexList).

% print every items in index
print_index_list([]) ->
    ok;
print_index_list([H|T]) ->
    print_index_item(H),
    print_index_list(T).

% print single index item
print_index_item({Key, Value}) ->
    io:format("{"),
    io:format("~p", [Key]),
    print_list(Value),
    io:format("}~n").

% print list
print_list(List) ->
        io:format("["),
        print_list_item(List),
        io:format("]").

print_list_item([]) ->
  ok;
print_list_item([H|T]) ->
  io:format("~p,", [H]),
  print_list_item(T).

