max(X,Y,X) :- X > Y,!.
max(_,Y,Y).


%declaracion de que es un arbol
arbol(nil).
arbol(N,L,R) :- integer(N),arbol(L),arbol(R).

%reto
maximo(arbol(N,nil,nil),N).
maximo(arbol(N,L,nil),X) :- L \= nil,  maximo(L,Y), max(N,Y,X).
maximo(arbol(N,nil,R),X) :- R \= nil, maximo(R,Y), max(N,Y,X).
maximo(arbol(N,L,R),X) :- nil \= L, nil \= R, maximo(L,ML),maximo(R,MR), max(ML,MR,MH), max(MH,N,X).
maximo(_,_) :- fail.
