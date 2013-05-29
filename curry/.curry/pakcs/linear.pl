%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(linear).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('linear.sub',sub,2,'linear.sub',nofix,'FuncType'('TCons'('Prelude.Int',[]),'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[])))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'linear.sub'(_G44624,_G44625,_G44626,_G44627,_G44628):-freeze(_G44627,'blocked_linear.sub'(_G44624,_G44625,_G44626,_G44627,_G44628)).
'blocked_linear.sub'(_G44667,_G44676,_G44858,_G44861,_G44864):-hnf('Prelude.cond'('Prelude.=:='(_G44667,_G44676),0),_G44858,_G44861,_G44864).
'blocked_linear.sub'(_G44667,_G44676,_G45359,_G45362,_G45365):-hnf(_G44667,_G45359,_G45362,_G45365).

:-costCenters(['']).




%%%%% Number of shared variables: 0
