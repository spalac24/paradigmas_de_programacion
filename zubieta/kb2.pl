happy(yolanda).
listens2Music(mia).
listens2Musig(yolanda) :- happy(yolanda).
playsAirGuitar(mia) :- listens2Music(mia).
playsAirGuitar(yolanda):- listens2Music(yolanda).

wizard(ron). 
wizard(X):-  hasBroom(X),  hasWand(X). 
hasWand(harry). 
quidditchPlayer(harry). 
hasBroom(X):-  quidditchPlayer(X).


mascota(X) :- animal(X), ladra(X).
mascota(X) :- animal(X), vuela(X).
animal(rocky).
animal(piolin).
animal(hob).
ladra(rocky).
vuela(piolin).
ruge(hob).

padre(pedro, juana).
padre(pedro, juan).
padre(pedro, jose).
padre(juan, libia).
padre(juan, toribio).
padre(libia, tibursia). 
padre(libia, adela).
padre(jose, pipo).
padre(jose, pepe).

padre(X,Z) :- padre(X,Y), padre(Y,Z), X \= Y, Y \= Z.


word(astante, a,s,t,a,n,t,e).
word(astoria, a,s,t,o,r,i,a).
word(baratto, b,a,r,a,t,t,o).
word(cobalto, c,o,b,a,l,t,o).
word(pistola, p,i,s,t,o,l,a).
word(statale, s,t,a,t,a,l,e).

fillable(U,V,W,X,Y,Z) :- word(U,V11,V12,V13,V14,V15,V16,V17),
                         word(V,V21,V22,V23,V24,V25,V26,V27),
                         word(W,V31,V32,V33,V34,V35,V36,V37),
                         word(X,H11,H12,H13,H14,H15,H16,H17),
                         word(Y,H21,H22,H23,H24,H25,H26,H27),
                         word(Z,H31,H32,H33,H34,H35,H36,H37),
                         V12 = H12, V22 = H14, V32 = H16,
                         V14 = H22, V24 = H24, V34 = H26,
                         V16 = H32, V26 = H34, V36 = H36,
                         U \= V, U \= W, U \= X, U \= Y, U \= Z,
                         V \= U, V \= W, V \= X, V \= Y, V \= Z,
                         W \= V, W \= U, W \= X, W \= Y, W \= Z,
                         X \= V, X \= W, X \= U, X \= Y, X \= Z,
                         Y \= V, Y \= W, Y \= X, Y \= U, Y \= Z,
                         Z \= V, Z \= W, Z \= X, Z \= Y, Z \= U.

% is_prime(P) :- P is a prime number

is_prime(2).
is_prime(3).
is_prime(P) :- integer(P),
               P > 3,
               P mod 2 =\= 0, 
               \+ has_factor(P,3).  

% has_factor(N,L) :- N has an odd factor F >= L.

has_factor(N,L) :- N mod L =:= 0.
has_factor(N,L) :- L * L < N, 
                   L2 is L + 2,
                   has_factor(N,L2).

fib(1,1).
fib(2,1).
fib(N,R):- N >= 3,
           N1 is N-1,
           N2 is N-2,
	   fib(N1,R1),
           fib(N2,R2),
           R is R1+R2.

