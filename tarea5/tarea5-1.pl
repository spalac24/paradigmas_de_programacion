%% dfs of a tree


child(a,b).
child(a,c).
child(b,d).
child(b,e).
child(e,h).
child(e,k).
child(d,i).
child(d,j).
child(f,l).

dfs(X) :- write(X), nl,child(X,Y), dfs(Y).
