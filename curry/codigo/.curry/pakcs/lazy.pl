%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(lazy).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('lazy.from',from,1,'lazy.from',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'([],['TCons'('Prelude.Int',[])]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'lazy.from'(_G597305,_G597306,_G597307,_G597308):-freeze(_G597307,'blocked_lazy.from'(_G597305,_G597306,_G597307,_G597308)).
'blocked_lazy.from'(_G597343,[_G597575|'lazy.from'('Prelude.+'(_G597575,1))],_G597539,_G597542):-makeShare(_G597343,_G597575),_G597539=_G597542.

:-costCenters(['']).




%%%%% Number of shared variables: 1
