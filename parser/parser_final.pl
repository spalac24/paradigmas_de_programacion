%writeFile(File) Lee, desde stdin, clausulas de prolog hacia File.
writeFile(File):-
	tell(File),
	bucle,
	told.

% readFile(File) Lee, desde el archivo File, hacia stdout, clausulas de prolog.
readFile(File) :-
	see(File),
	bucle,
	seen.

bucle :- repeat,
	read(Clause),
	numbervars(Clause,0,_),
	writeC(Clause),
	!. 

%writeC(Clause) escribe la clausula con un punto al final. Falla si se llego al fin del archivo
writeC(end_of_file) :- !.
writeC(eof) :- !.
writeC(Clause) :- write(Clause), write('.'), nl, !,fail.

%process(X) transforma la clausula X en una lista y la imprime. Falla si se llega a fin del archivo.
process(end_of_file) :- !.
process(X):-
	getList(X,L),write(L),nl,fail.


%getList(C,L) es verdadero si L es la lista cuya cabeza es la cabeza de la regla, y su cuerpo es el cuerpo de la regla.
getList(X,[X]) :- var(X),!.
getList(X :- Y,[X|Z]):- getBody(Y,Z),!.
getList(X,[X]) :- not(X= end_of_file) .
getList(end_of_file,_) :- !,fail.

%getBody(B,L) es la lista que corresponde a los elementos del cuerpo de una regla
getBody((X,Y),[X|Z]) :- getBody(Y,Z), !.
getBody(X,[X]).

%readProcessFile(File) lee File e imprime las listas de cada clausula

writeProcessedFile(From,To) :- 
	tell(To), readProcessFile(From), told.
readProcessFile(File) :-
	see(File),
	read(In),
	getProcessedFile(Lista,In),
	numbervars(Lista,0,_),write(Lista),write('.'),nl,
	seen.

getProcessedFile([],end_of_file) :- !.
getProcessedFile([L|Ls],In) :- getList(In,L), read(In2),  getProcessedFile(Ls,In2).

%prop1 : Hacia la disciplina de constructores: detectar constantes, functores y los simbolos de predicados.

propiedad(File) :-
	see(File),
	read(Lista),
	getAns(Lista,C1,F1,S1),append(C1,C),append(F1,F),append(S1,S),write('constantes'),nl,
	write(C),nl,write('functores'),nl,write(F),nl,write('simbolos de predicado'),nl,write(S),nl,seen.

getAns([],[],[],[]) :- !.
getAns([L|Ls], [X|Xs], [Y|Ys], [Z|Zs]) :-prop1(L,X,Y,Z),getAns(Ls,Xs,Ys,Zs).


prop1([],[],[],[]).
prop1([Hr|Br],Cons,Func,Symb) :- getCons([Hr],C1), getCons(Br,C2), append(C1,C2,Cons2), getFunc([Hr],F1),
	getFunc(Br,F2), append(F1,F2,Func2), getSymb([Hr],S1), getSymb(Br,S2), append(S1,S2,Symb), diff(Func2,Symb,Func), diff(Cons2,Symb,Cons).

diff([],_,[]):-!.
diff([X|Xs],Y,L) :- member(X,Y),!,diff(Xs,Y,L).
diff([X|Xs],Y,[X|L]) :- not(member(X,Y)), !,diff(Xs,Y,L).

getCons([],[]):- !.
getCons([X|Xs],[X|Ys]) :- atom(X), functor(X,_,0), !, getCons(Xs,Ys). %si es una variable atomica
getCons([X|Xs],L) :- compound(X), !,X =.. L1, L1 = [_|L2], getCons(L2,L),getCons(Xs,Ys), append(L,Ys,L).%extrae de un functor las constantes
getCons([X|Xs],Ys) :- var(X),!,getCons(Xs,Ys). %si es una variable, la ignora
%similarmente para getFunc

getFunc([],[]):- !.
getFunc([X|Xs],[Xf|L]) :- compound(X), not(var(X)), !, functor(X,Xf,_),X =.. L1, L1 = [_|L2], getFunc(Xs,Ys), getFunc(L2,L3), append(L3,Ys,L).
getFunc([X|Xs],Ys) :- (atom(X);var(X)),!, getFunc(Xs,Ys). 

getSymb([],[]).
getSymb([X|Xs],[Y|Ys]) :- functor(X,Y,_), getSymb(Xs,Ys).
