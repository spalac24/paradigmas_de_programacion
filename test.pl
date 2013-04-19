mult(suma(x,pot(x,2)),mult(2,x))

deriv(mult(X,Y),Z) :- suma(deriv(X),deriv(Y),Z). 
