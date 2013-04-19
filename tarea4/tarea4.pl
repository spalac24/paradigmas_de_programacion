nat(0).
nat(s(X)) :- nat(X).

suma_n(X,0,X) :- nat(X).
suma_n(X,s(Y),s(Z)) :- suma_n(X,Y,Z).

mult_n(X,0,0):- nat(X).
mult_n(X,s(Y),Z) :- mult_n(X,Y,K), suma_n(K,X,Z).

potencia_n(X,0,s(0)) :- nat(X).
potencia_n(X,s(Y),Z) :- potencia_n(X,Y,K), mult_n(X,K,Z).

factorial_n(0,s(0)).
factorial_n(s(X),Z) :- factorial_n(X,K),mult_n(K,s(X),Z).

ent((1,2)).
ent((X,Y)) :- nat(X),nat(Y).

suma_z((X,Y),(A,B),(C,D)) :- ent((X,Y)), ent((A,B)), ent((C,D)),suma_n(X,A,C), suma_n(Y,B,D). 
