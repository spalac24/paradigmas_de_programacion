%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(last).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('last.last',last,1,'last.last',nofix,'FuncType'('TCons'([],[_G39214]),_G39214)).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'last.last'(_G41752,_G41753,_G41754,_G41755):-freeze(_G41754,'blocked_last.last'(_G41752,_G41753,_G41754,_G41755)).
'blocked_last.last'(_G41790,_G42123,_G42126,_G42129):-makeShare(_G41811,_G42198),hnf('Prelude.cond'('Prelude.=:='('Prelude.++'(_G41802,[_G42198]),_G41790),_G42198),_G42123,_G42126,_G42129).

:-costCenters(['']).




%%%%% Number of shared variables: 1
