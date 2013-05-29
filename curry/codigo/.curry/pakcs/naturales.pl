%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(naturales).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('naturales.Zero','Zero',0,'Zero',0,'TCons'('naturales.Natural',[])).
constructortype('naturales.Succ','Succ',1,'Succ',1,'FuncType'('TCons'('naturales.Natural',[]),'TCons'('naturales.Natural',[]))).
constructortype('naturales.Leaf','Leaf',0,'Leaf',0,'TCons'('naturales.Tree',[_G55612])).
constructortype('naturales.Node','Node',3,'Node',1,'FuncType'(_G55778,'FuncType'('TCons'('naturales.Tree',[_G55778]),'FuncType'('TCons'('naturales.Tree',[_G55778]),'TCons'('naturales.Tree',[_G55778]))))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-costCenters(['']).




%%%%% Number of shared variables: 0
