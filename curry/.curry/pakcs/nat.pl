%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(nat).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('nat.add',add,2,'nat.add',nofix,'FuncType'('TCons'('nat.Nat',[]),'FuncType'('TCons'('nat.Nat',[]),'TCons'('nat.Nat',[])))).
functiontype('nat.leq',leq,2,'nat.leq',nofix,'FuncType'('TCons'('nat.Nat',[]),'FuncType'('TCons'('nat.Nat',[]),'TCons'('Prelude.Bool',[])))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('nat.Z','Z',0,'Z',0,'TCons'('nat.Nat',[])).
constructortype('nat.S','S',1,'S',1,'FuncType'('TCons'('nat.Nat',[]),'TCons'('nat.Nat',[]))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'nat.add'(_G62594,_G62595,_G62596,_G62597,_G62598):-freeze(_G62597,'blocked_nat.add'(_G62594,_G62595,_G62596,_G62597,_G62598)).
'blocked_nat.add'(_G62637,_G62646,_G62924,_G62927,_G62930):-hnf(_G62646,_G63248,_G62927,_G63236),'blocked_nat.add_2'(_G63248,_G62637,_G62924,_G63236,_G62930).

'blocked_nat.add_2'(_G63370,_G63371,_G63372,_G63373,_G63374):-freeze(_G63373,'blocked_blocked_nat.add_2'(_G63370,_G63371,_G63372,_G63373,_G63374)).
'blocked_blocked_nat.add_2'('nat.Z',_G62637,_G63505,_G63508,_G63511):-hnf(_G62637,_G63505,_G63508,_G63511).
'blocked_blocked_nat.add_2'('nat.S'(_G62739),_G62637,'nat.S'('nat.add'(_G62637,_G62739)),_G63765,_G63765):-!.
'blocked_blocked_nat.add_2'('FAIL'(_G64149),_G62637,'FAIL'(_G64149),_G64156,_G64156):-nonvar(_G64149).

'nat.leq'(_G64374,_G64375,_G64376,_G64377,_G64378):-freeze(_G64377,'blocked_nat.leq'(_G64374,_G64375,_G64376,_G64377,_G64378)).
'blocked_nat.leq'(_G64417,_G64426,_G64771,_G64774,_G64777):-hnf(_G64417,_G65095,_G64774,_G65083),'blocked_nat.leq_1'(_G65095,_G64426,_G64771,_G65083,_G64777).

'blocked_nat.leq_1'(_G65217,_G65218,_G65219,_G65220,_G65221):-freeze(_G65220,'blocked_blocked_nat.leq_1'(_G65217,_G65218,_G65219,_G65220,_G65221)).
'blocked_blocked_nat.leq_1'('nat.Z',_G64426,'Prelude.True',_G65355,_G65355).
'blocked_blocked_nat.leq_1'('nat.S'(_G64526),_G64426,_G65751,_G65754,_G65757):-!,hnf(_G64426,_G66345,_G65754,_G66333),'blocked_blocked_nat.leq_1_nat.S_2'(_G66345,_G64526,_G65751,_G66333,_G65757).

'blocked_blocked_nat.leq_1_nat.S_2'(_G66524,_G66525,_G66526,_G66527,_G66528):-freeze(_G66527,'blocked_blocked_blocked_nat.leq_1_nat.S_2'(_G66524,_G66525,_G66526,_G66527,_G66528)).
'blocked_blocked_blocked_nat.leq_1_nat.S_2'('nat.Z',_G64526,'Prelude.False',_G66662,_G66662).
'blocked_blocked_blocked_nat.leq_1_nat.S_2'('nat.S'(_G64629),_G64526,_G67020,_G67023,_G67026):-!,hnf('nat.leq'(_G64526,_G64629),_G67020,_G67023,_G67026).
'blocked_blocked_blocked_nat.leq_1_nat.S_2'('FAIL'(_G67394),_G64526,'FAIL'(_G67394),_G67401,_G67401):-nonvar(_G67394).
'blocked_blocked_nat.leq_1'('FAIL'(_G67430),_G64426,'FAIL'(_G67430),_G67437,_G67437):-nonvar(_G67430).

:-costCenters(['']).




%%%%% Number of shared variables: 0
