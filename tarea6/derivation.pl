expr(X) :- float(X);integer(X).
expr(x).
expr(add(X,Y)) :- expr(X),expr(Y).
expr(neg(X)) :- expr(X).
expr(mul(X,Y)) :- expr(X),expr(Y).
expr(pow(X,Y)) :- expr(X),expr(Y).
expr(ln(X)) :- expr(X).


deriv(x,1) :- !.
deriv(X,0) :- integer(X);float(X).
deriv(add(X,Y),Z) :- deriv(X,A), deriv(Y,B), Z = add(A,B).
deriv(neg(X),neg(Z)) :- deriv(X,Z). 
deriv(mul(X,Y),Z) :- deriv(X,A), deriv(Y,B), Z = add(mul(A,Y),mul(X,B)).
% (f(x) ^ g(x))' = f^(g-1)*(gf'+f*log(f)*g')
deriv(pow(F,G),Z) :- deriv(F,FP), deriv(G,GP), Z = mul(pow(F,add(G,neg(1))),add(mul(G,FP),mul(mul(F,ln(F)),GP))).
deriv(ln(X),Z) :- deriv(X,XP), Z = mul(XP,pow(X,-1)).


exp(X) :- number(X).
exp(x).
exp(A+B) :- exp(A), exp(B).
exp(-A) :- exp(A).
exp(A*B) :- exp(A), exp(B).
exp(A**B) :- exp(A), exp(B).
exp(ln(A)):-exp(A).

der(x,1).
der(X,0) :- number(X).
der(A+B,C+D) :- der(A,C), der(B,D).
der(A*B,(C*B)+(D*A)) :- der(A,C), der(B,D).
der(-A,-B) :- der(A,B).
% (f(x) ^ g(x))' = f^(g-1)*(gf'+f*log(f)*g')
der(F**G,(F**(G-1))*(G*FP+F*ln(F)*GP)) :- der(F,FP), der(G,GP).
der(ln(F),FP*(F**(-1))) :- der(F,FP).

