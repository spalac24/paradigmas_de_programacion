esposos(pedro,maria).
esposos(juan,bertha).
%esposos(X,Y) :- esposos(Y,X).

progenitor(pedro,juana).
progenitor(pedro,juan).
progenitor(pedro,jose).

progenitor(juan,libia).
progenitor(juan,toribio).

progenitor(libia,tibursia).
progenitor(libia,adela).

progenitor(jose,pipo).
progenitor(jose,pepe).

progenitor(X,Z) :- progenitor(X,Y) , progenitor(Y,Z).

%progenitor(Z,Y) :- esposos(X,Z), progenitor(X,Y).

