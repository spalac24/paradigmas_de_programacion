%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(nondet).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('nondet.choose',choose,2,'nondet.choose',nofix,'FuncType'(_G36061,'FuncType'(_G36061,_G36061))).
functiontype('nondet.one23',one23,0,'nondet.one23',nofix,'TCons'('Prelude.Int',[])).
functiontype('nondet.last',last,1,'nondet.last',nofix,'FuncType'('TCons'([],[_G38322]),_G38322)).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'nondet.choose'(_G40968,_G40969,_G40970,_G40971,_G40972):-freeze(_G40971,'blocked_nondet.choose'(_G40968,_G40969,_G40970,_G40971,_G40972)).
'blocked_nondet.choose'(_G41011,_G41020,_G41056,_G41059,_G41062):-hnf(_G41011,_G41056,_G41059,_G41062).
'blocked_nondet.choose'(_G41011,_G41020,_G41231,_G41234,_G41237):-hnf(_G41020,_G41231,_G41234,_G41237).

'nondet.one23'(_G41697,_G41698,_G41699):-freeze(_G41698,'blocked_nondet.one23'(_G41697,_G41698,_G41699)).
'blocked_nondet.one23'(_G41874,_G41877,_G41880):-hnf('nondet.choose'(1,'nondet.choose'(2,3)),_G41874,_G41877,_G41880).

'nondet.last'(_G42527,_G42528,_G42529,_G42530):-freeze(_G42529,'blocked_nondet.last'(_G42527,_G42528,_G42529,_G42530)).
'blocked_nondet.last'(_G42565,_G42898,_G42901,_G42904):-makeShare(_G42586,_G42973),hnf('Prelude.cond'('Prelude.=:='('Prelude.++'(_G42577,[_G42973]),_G42565),_G42973),_G42898,_G42901,_G42904).

:-costCenters(['']).




%%%%% Number of shared variables: 1
