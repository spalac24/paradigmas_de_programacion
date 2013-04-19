%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(test).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('test.test',test,1,'test.test',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'([],['TCons'('Prelude.Int',[])]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'test.test'(_G127154,_G127155,_G127156,_G127157):-freeze(_G127156,'blocked_test.test'(_G127154,_G127155,_G127156,_G127157)).
'blocked_test.test'(_G127192,_G127345,_G127348,_G127351):-hnf('Prelude.take'(_G127192,'Prelude.enumFromThen'(0,2)),_G127345,_G127348,_G127351).

:-costCenters(['']).




%%%%% Number of shared variables: 0
