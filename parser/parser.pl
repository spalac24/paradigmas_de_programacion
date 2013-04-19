main:-
    open('file.in', read, Str),
    read_file(Str,Lines),
    close(Str),
	open('file.out', write, Stre),
	process(Stre,Lines),
	close(Stre).

process(_,[]) :- !.
process(Str,[X|XS]):-
	getList(X,L),write(Str,L),nl(Str),
	process(Str,XS).


getList(X:-Y,[X|Z]):-nonvar(X),getList(Y,Z), !.
getList((X,Y),[X|Z]) :-nonvar(X),getList(Y,Z), !.
getList(X,[X]).



%code from http://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog
read_file(Stream,[]) :-
    at_end_of_stream(Stream),!.

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).
