%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(outermost).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('outermost.loop',loop,0,'outermost.loop',nofix,'TCons'('Prelude.Int',[])).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'outermost.loop'(_G125657,_G125658,_G125659):-freeze(_G125658,'blocked_outermost.loop'(_G125657,_G125658,_G125659)).
'blocked_outermost.loop'(_G125768,_G125771,_G125774):-hnf('Prelude.+'(1,'outermost.loop'),_G125768,_G125771,_G125774).

:-costCenters(['']).




%%%%% Number of shared variables: 0
