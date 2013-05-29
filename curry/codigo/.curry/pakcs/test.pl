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
functiontype('test.f',f,1,'test.f',nofix,'FuncType'(_G51773,_G51773)).
functiontype('test.g',g,1,'test.g',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'test.test'(_G55514,_G55515,_G55516,_G55517):-freeze(_G55516,'blocked_test.test'(_G55514,_G55515,_G55516,_G55517)).
'blocked_test.test'(_G55552,_G55705,_G55708,_G55711):-hnf('Prelude.take'(_G55552,'Prelude.enumFromThen'(0,2)),_G55705,_G55708,_G55711).

'test.f'(_G56318,_G56319,_G56320,_G56321):-freeze(_G56320,'blocked_test.f'(_G56318,_G56319,_G56320,_G56321)).
'blocked_test.f'(_G56356,_G56363,_G56366,_G56369):-hnf(_G56356,_G56363,_G56366,_G56369).

'test.g'(_G56659,_G56660,_G56661,_G56662):-freeze(_G56661,'blocked_test.g'(_G56659,_G56660,_G56661,_G56662)).
'blocked_test.g'(_G56697,_G56777,_G56780,_G56783):-hnf('Prelude.*'(2,_G56697),_G56777,_G56780,_G56783).

:-costCenters(['']).




%%%%% Number of shared variables: 0
