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
functiontype('test.f',f,1,'test.f',nofix,'FuncType'(_G55315,_G55315)).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'test.test'(_G57860,_G57861,_G57862,_G57863):-freeze(_G57862,'blocked_test.test'(_G57860,_G57861,_G57862,_G57863)).
'blocked_test.test'(_G57898,_G58051,_G58054,_G58057):-hnf('Prelude.take'(_G57898,'Prelude.enumFromThen'(0,2)),_G58051,_G58054,_G58057).

'test.f'(_G58664,_G58665,_G58666,_G58667):-freeze(_G58666,'blocked_test.f'(_G58664,_G58665,_G58666,_G58667)).
'blocked_test.f'(_G58702,_G58709,_G58712,_G58715):-hnf(_G58702,_G58709,_G58712,_G58715).

:-costCenters(['']).




%%%%% Number of shared variables: 0
