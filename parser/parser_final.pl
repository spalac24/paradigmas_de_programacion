writeFile(File):-
	tell(File),
	repeat,
	read(Clause),
	writeC(Clause),
	!,
	told.
	

readFile(File) :-
	see(File),
	repeat,
	read(Clause),
%	getList(Clause,L),
	process(Clause),
	!,
	seen.
	
writeC(end_of_file) :- !.
writeC(Clause) :- write(Clause), write('.'), nl, fail.

process(end_of_file) :- !.
process(X):-
	getList(X,L),write(L),nl,fail.

getList(end_of_file,[]) :- fail.

getList(X:-Y,[X|Z]):-nonvar(X),getList(Y,Z), !.
getList((X,Y),[X|Z]) :-nonvar(X),getList(Y,Z), !.
getList(X,[X]) :- !.

readProcessFile(File) :-
	nl,nl,
	see(File),
	repeat,
	read(Clause),
	process(Clause),
	!,
	seen.
