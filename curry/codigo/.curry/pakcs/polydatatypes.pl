%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(polydatatypes).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('polydatatypes.Leaf','Leaf',0,'Leaf',0,'TCons'('polydatatypes.BinTree',[_G608847])).
constructortype('polydatatypes.Branch','Branch',3,'Branch',1,'FuncType'(_G609067,'FuncType'('TCons'('polydatatypes.BinTree',[_G609067]),'FuncType'('TCons'('polydatatypes.BinTree',[_G609067]),'TCons'('polydatatypes.BinTree',[_G609067]))))).
constructortype('polydatatypes.Empty','Empty',0,'Empty',0,'TCons'('polydatatypes.Lista',[_G609738])).
constructortype('polydatatypes.Lis','Lis',2,'Lis',1,'FuncType'(_G609943,'FuncType'('TCons'('polydatatypes.Lista',[_G609943]),'TCons'('polydatatypes.Lista',[_G609943])))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-costCenters(['']).




%%%%% Number of shared variables: 0
