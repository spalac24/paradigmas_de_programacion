p(_,_):-fail.
p(_,2).

s(X) :- s(X). %FAILS FOR STACK
