%%%%%%%%%%%%%%%%%%%%% ENTREGA 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Leer las cláusulas y guardarlas.
saveFile(File) :- write('Entre las clausulas y finalice con punto, para finalizar escriba eof.'), nl,
					tell(File),
					repeat,
						read(Clause),
						writeClause(Clause),
						!,
					told.

% Leer un archivo guardado.
readFile(File) :- see(File),
				  repeat,
					read(Clause),
					writeClause(Clause),
					!,
				  seen.

% Función auxiliar para saveFile y readFile.
% Lee una cláusula y la escribe con un '.' al final.
% Manda un fail mientras que no lea el end_of_file para que el ciclo continúe.
writeClause(eof):- !.
writeClause(end_of_file):- !.
writeClause(Clause):- write(Clause), write('.'), nl, fail.

%%%%%%%%%%%%%%%%%%%%% ENTREGA 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lee un archivo y mete sus claúsulas al arreglo List.
convert(File, List) :- see(File), makeList([], List), seen.

% Guarda las cláusulas que lee en una lista con listas que contienen
% los antecedentes y consecuentes 
makeList(Acum,List):- read(Clause), 
                      (Clause == end_of_file -> reverse(Acum, List);
 					  (transform(Clause, Func), makeList([Func|Acum],List) ) ).

% Lee una cláusula y retorna una lista cuya cabeza es el consecuente
% y cuerpo es el antecedente.
% Si la cláusula no tiene antecedente se agrega al cuerpo la variable
% empty.
transform( (:-(X,Y)) ,  [X,Y]) :- !.
transform( X , [X, empty]).

% Toma una lista con las cláusulas separadas como antecedente y consecuente
% e imprime el programa que corresponde a esta lista.
programFromList([]) :- !.
programFromList([Head|Tail]):- interpret(Head), programFromList(Tail).

% Escribe una cláusula de la forma [consecuente|[atecedentes]]
interpret([Head|[empty]]) :- write(Head), write('.'), nl, !.
interpret([Head|[Tail]]) :- write(Head), write(' :- '), write(Tail), write('.'), nl.

%%%%%%%%%%%%%%%%%%%%% ENTREGA 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Separar los componentes del programa para que en una lista queden
% las variables, los predicados, los functores y las constantes.
components(File, Const, Pred, Var, Func) :- convert(File, List),
					decomposition(List, [], [], [], [], Const2, Pred2, Var2, Func2),
					removeDuplicates(Const2, Const),
					removeDuplicates(Pred2, Pred),
					% Var retorna el numero de variables diferentes en Var2
					term_variables(Var2, Var),
					numbervars(Var, 0, _),
					removeDuplicates(Func2, Func).

% Quita los duplicados de una lista
removeDuplicates([],[]).
removeDuplicates([Head|Tail], Result) :-
				(member(Head, Tail) ->
						removeDuplicates(Tail, Result);
						removeDuplicates(Tail, Partial), Result = [Head|Partial]).


% Toma el programa en forma de lista y retorna arreglos con las variables, constantes
% predicados y functores.
decomposition([], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					reverse(AcumConst, Const), reverse(AcumPred, Pred),
					reverse(AcumVar, Var), reverse(AcumFunc, Func).
decomposition([Clause|Program], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					decomposeClause(Clause, AcumConst, AcumPred, AcumVar, AcumFunc, C, P, V, F),
					append(C, AcumConst, AcumConst2), append(P, AcumPred, AcumPred2),
					append(V, AcumVar, AcumVar2), append(F, AcumFunc, AcumFunc2),
					decomposition(Program, AcumConst2, AcumPred2, AcumVar2, AcumFunc2, Const, Pred, Var, Func).

% Toma una cláusula de la forma [consecuente| [antecedentes]] y halla sus variables, Predicados
% constantes y functores
decomposeClause([Head|[empty]], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					decomposePred(Head, AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func), !.
decomposeClause([Head|[Tail]], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					decomposePred(Head, AcumConst, AcumPred, AcumVar, AcumFunc, C, P, V, F),
					append(C, AcumConst, AcumConst2), append(P, AcumPred, AcumPred2),
					append(V, AcumVar, AcumVar2), append(F, AcumFunc, AcumFunc2),
					Tail =.. [Comma|Predicates],
					%% Si la cola se compone de varios predicados
					(Comma == (',') -> 
						decomposePredicateList(Predicates, AcumConst2, AcumPred2, AcumVar2, AcumFunc, Const, Pred, Var, Func);
						%% Si la cola se compone de un solo predicado
						decomposePred(Tail, AcumConst2, AcumPred2, AcumVar2, AcumFunc, Const, Pred, Var, Func) ).
						
% Dada la lista de antecedentes halla las varialbes, constantes, predicados y functores que aparecen allí.
decomposePredicateList([], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					Const = AcumConst, Pred = AcumPred, Var = AcumVar, Func = AcumFunc.
decomposePredicateList([Head|[Tail]], AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :- 
					decomposePred(Head, AcumConst, AcumPred, AcumVar, AcumFunc, C, P, V, F),
					append(C, AcumConst, AcumConst2), append(P, AcumPred, AcumPred2),
					append(V, AcumVar, AcumVar2), append(F, AcumFunc, AcumFunc2),
					Tail =.. [Comma|Predicates],
					(Comma == (',') -> 
						decomposePredicateList(Predicates, AcumConst2, AcumPred2, AcumVar2, AcumFunc2, Const, Pred, Var, Func);
						decomposePred(Tail, AcumConst2, AcumPred2, AcumVar2, AcumFunc2, C2, P2, V2, F2),
						append(C2, AcumConst2, AcumConst3), append(P2, AcumPred2, AcumPred3),
						append(V2, AcumVar2, AcumVar3), append(F2, AcumFunc2, AcumFunc3),
						decomposePredicateList([], AcumConst3, AcumPred3, AcumVar3, AcumFunc3, Const, Pred, Var, Func)).

% Dado un predicado de la forma p(X, Y, Z, ...) halla las variables, constantes, functores y el predicado
% que aparece allí.
decomposePred(Predicate, AcumConst, AcumPred, AcumVar, AcumFunc, Const, Pred, Var, Func) :-
					Predicate =.. [P|Args], length(Args, Arity),
					insert([P|Arity], AcumPred, Pred),
					decomposeArgs(Args, AcumConst, AcumVar, AcumFunc, Const, Var, Func).

% Inserta el elemento X a la ListA y lo mete a ListB si X no estaba ya presente en ListA 
insert(X, ListA, ListB) :- (member(X, ListA) -> ListB = ListA ;
																append([X], ListA, ListB)).

% Toma los argumentos de un predicado y, según su tipo los inserta a una lista donde se almacenan
decomposeArgs([], AcumConst, AcumVar, AcumFunc, Const, Var, Func) :- Const = AcumConst,
																	 						Var = AcumVar,
																	 						Func = AcumFunc.
decomposeArgs([Head|Tail], AcumConst, AcumVar, AcumFunc, Const, Var, Func) :- 
					(var(Head) -> 
						AcumVar2 = [Head|AcumVar], 
						decomposeArgs(Tail, AcumConst, AcumVar2, AcumFunc, Const, Var, Func) ;
						(atom(Head) ->
							insert(Head, AcumConst, AcumConst2),
							decomposeArgs(Tail, AcumConst2, AcumVar, AcumFunc, Const, Var, Func);
							 	Head =.. [Name|Args], length(Args, Arity), 
								insert([Name|Arity], AcumFunc, AcumFunc2),
								decomposeArgs(Args, AcumConst, AcumVar, AcumFunc2, Const2, Var2, Func2),
								decomposeArgs(Tail, Const2, Var2, Func2, Const, Var, Func) )).

% Retorna los elementos X que pertenecen al universo de Herbrand del programa especificado 
% en el archivo File.
getUniverse(File, X) :- components(File, Const, _, _, Func), buildUniverse(X, Const, Func).

% Dadas las constantes y los functores (con su respectiva aridad) que aparecen en un programa
% retorna los elementos X que pertenecen al universo de Herbrand de ese programa.
buildUniverse(X, Constants, Functors) :- member(X, Constants).
buildUniverse(X, Constants, Functors) :- member(Y, Functors), head(Y, P), arg(2, Y, A),
										(A == 1 -> X =.. [P, Z], member(Z, Constants);
										(A == 2 -> X =.. [P, Z, W], member(Z, Constants), 
																	member(W, Constants);
										(A == 3 -> X =.. [P, Z, W, V], member(Z, Constants), 
																	   member(W, Constants), 
																	   member(W, Constants)))).
buildUniverse(X, Constants, Functors) :- member(Y, Functors), head(Y, P), arg(2, Y, A),
										(A == 1 -> buildUniverse(Z, Constants, Functors), X =.. [P, Z];
										(A == 2 -> buildUniverse(Z, Constants, Functors), 
												   buildUniverse(W, Constants, Functors), X =.. [P, Z, W];
										(A == 3 -> buildUniverse(Z, Constants, Functors), 
												   buildUniverse(W, Constants, Functors), 
												   buildUniverse(V, Constants, Functors), X =.. [P, Z, W, V]))).

% Retorna los elementos X que pertenecen a la base de Herbrand del programa especificado 
% en el archivo File.
getBase(File, X) :- components(File, Const, Pred, _, Func), buildBase(X, Pred, Const, Func).

% Dados los predicados, las constantes y los functores que aparecen en un programa retorna
% los elementos X que pertenecen a la base de Herband del programa.
buildBase(X, Predicates, Constants, Functors) :- member(Y, Predicates), head(Y, P), arg(2,Y, A),
											(A == 1 -> buildUniverse(Z, Constants, Functors), X =.. [P, Z];
											(A == 2 -> buildUniverse(Z, Constants, Functors), 
													   buildUniverse(W, Constants, Functors), X =.. [P, Z, W];
											(A == 3 -> buildUniverse(Z, Constants, Functors), 
													   buildUniverse(W, Constants, Functors), 
													   buildUniverse(V, Constants, Functors), X =.. [P, Z, W, V]))).

head([H|_], H).
