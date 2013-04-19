%torres de hanoi
%move(N,X,Y,Z) mueve N piezas de la torre X a la Y usando la torre Z como auxiliar
%idea tomada de http://www.cs.toronto.edu/~hojjat/384w09/simple-prolog-examples.html

move(1,X,Y,_) :- write('move disc from '), write(X), write(' to '),write(Y),nl,!.
move(N,X,Y,Z) :- M is N-1, move(M,X,Z,Y), move(1,X,Y,_), move(M,Z,Y,X).
