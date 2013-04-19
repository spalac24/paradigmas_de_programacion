%insertar(Lista, Elemento, Posicion, Resultado)

insertar(L,A,0,[A|L]) :- !.
insertar([X|Y],A,N,[X|Z]) :- N>0, M is (N-1),insertar(Y,A,M,Z).
