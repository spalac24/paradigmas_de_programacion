%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(nums).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('nums.f',f,1,'nums.f',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Bool',[]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'nums.f'(_G38672,_G38673,_G38674,_G38675):-freeze(_G38674,'blocked_nums.f'(_G38672,_G38673,_G38674,_G38675)).
'blocked_nums.f'(_G38710,_G38790,_G38793,_G38796):-hnf('Prelude.<='(_G38710,2),_G38790,_G38793,_G38796).

:-costCenters(['']).




%%%%% Number of shared variables: 0
