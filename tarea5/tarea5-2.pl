%% is_ancestor(X,Y) verifica que X sea ancestro de Y, esto es, existe un camino de X a Y.
%% funciona de manera optima cuando el grafo es un arbol, sin embargo algunas funciones tambien
%% pueden responder cuando se trabaja con otros grafos

child(a,b).
child(b,c).
child(b,d).
child(d,e).
child(c,f).
child(e,f).
child(f,g).

is_ancestor(X,Y) :- child(X,Y).
is_ancestor(X,Z) :- child(X,Y), is_ancestor(Y,Z).
%% solucion tomada de http://stackoverflow.com/questions/679826/prolog-path-finding



