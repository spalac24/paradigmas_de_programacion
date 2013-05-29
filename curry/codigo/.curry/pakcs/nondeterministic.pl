%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(nondeterministic).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('nondeterministic.choose',choose,2,'nondeterministic.choose',nofix,'FuncType'(_G58087,'FuncType'(_G58087,_G58087))).
functiontype('nondeterministic.one23',one23,0,'nondeterministic.one23',nofix,'TCons'('Prelude.Int',[])).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'nondeterministic.choose'(_G62191,_G62192,_G62193,_G62194,_G62195):-freeze(_G62194,'blocked_nondeterministic.choose'(_G62191,_G62192,_G62193,_G62194,_G62195)).
'blocked_nondeterministic.choose'(_G62234,_G62243,_G62279,_G62282,_G62285):-hnf(_G62234,_G62279,_G62282,_G62285).
'blocked_nondeterministic.choose'(_G62234,_G62243,_G62484,_G62487,_G62490):-hnf(_G62243,_G62484,_G62487,_G62490).

'nondeterministic.one23'(_G63190,_G63191,_G63192):-freeze(_G63191,'blocked_nondeterministic.one23'(_G63190,_G63191,_G63192)).
'blocked_nondeterministic.one23'(_G63367,_G63370,_G63373):-hnf('nondeterministic.choose'(1,'nondeterministic.choose'(2,3)),_G63367,_G63370,_G63373).

:-costCenters(['']).




%%%%% Number of shared variables: 0
