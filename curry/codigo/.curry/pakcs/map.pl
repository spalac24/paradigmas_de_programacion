%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(map).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('map.map',map,2,'map.map',nofix,'FuncType'('FuncType'(_G38213,_G38214),'FuncType'('TCons'([],[_G38213]),'TCons'([],[_G38214])))).
functiontype('map.min3',min3,0,'map.min3',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('map.min3\'2E_\'23lambda1','map.min3._#lambda1',1,'map.min3\'2E_\'23lambda1',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'map.map'(_G43156,_G43157,_G43158,_G43159,_G43160):-freeze(_G43159,'blocked_map.map'(_G43156,_G43157,_G43158,_G43159,_G43160)).
'blocked_map.map'(_G43199,_G43208,_G43611,_G43614,_G43617):-hnf(_G43208,_G43935,_G43614,_G43923),'blocked_map.map_2'(_G43935,_G43199,_G43611,_G43923,_G43617).

'blocked_map.map_2'(_G44057,_G44058,_G44059,_G44060,_G44061):-freeze(_G44060,'blocked_blocked_map.map_2'(_G44057,_G44058,_G44059,_G44060,_G44061)).
'blocked_blocked_map.map_2'([],_G43199,[],_G44162,_G44162).
'blocked_blocked_map.map_2'([_G43308|_G43317],_G43199,['Prelude.apply'(_G44453,_G43308)|'map.map'(_G44453,_G43317)],_G44383,_G44386):-!,makeShare(_G43199,_G44453),_G44383=_G44386.
'blocked_blocked_map.map_2'('FAIL'(_G45105),_G43199,'FAIL'(_G45105),_G45112,_G45112):-nonvar(_G45105).

'map.min3'(_G45348,_G45349,_G45350):-freeze(_G45349,'blocked_map.min3'(_G45348,_G45349,_G45350)).
'blocked_map.min3'(_G45386,_G45389,_G45392):-hnf(partcall(1,'map.min3\'2E_\'23lambda1',[]),_G45386,_G45389,_G45392).

'map.min3\'2E_\'23lambda1'(_G46051,_G46052,_G46053,_G46054):-freeze(_G46053,'blocked_map.min3\'2E_\'23lambda1'(_G46051,_G46052,_G46053,_G46054)).
'blocked_map.min3\'2E_\'23lambda1'(_G46089,_G46169,_G46172,_G46175):-hnf('Prelude.-'(3,_G46089),_G46169,_G46172,_G46175).

:-costCenters(['']).




%%%%% Number of shared variables: 1
