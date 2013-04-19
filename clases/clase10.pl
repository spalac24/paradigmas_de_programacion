hermano(ana,pablo).
hermano(mimi,juana).
progenitor(pablo,jaimito).
progenitor(juana,caty).
hembra(caty).
varon(jaimito).
tia(X,Y) :- hermano(X,Z) , progenitor(Z,Y).
