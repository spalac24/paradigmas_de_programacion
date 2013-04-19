%eliminar(Lista, Posicion , Resultado)

eliminar([_|Xs],0,Xs) :- !.
eliminar([X|Xs],N,[X|Y]) :- N > 0, M is N-1, eliminar(Xs,M,Y).
