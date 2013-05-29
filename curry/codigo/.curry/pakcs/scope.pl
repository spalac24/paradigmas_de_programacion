%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(scope).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('scope.f',f,2,'scope.f',nofix,'FuncType'(_G217984,'FuncType'('TCons'([],[_G217984]),'TCons'('Prelude.Bool',[])))).
functiontype('scope.pow',pow,2,'scope.pow',nofix,'FuncType'('TCons'('Prelude.Int',[]),'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[])))).
functiontype('scope.fact',fact,1,'scope.fact',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('scope.fact\'2Efact_aux\'2E15','scope.fact.fact_aux.15',2,'scope.fact\'2Efact_aux\'2E15',nofix,'FuncType'('TCons'('Prelude.Int',[]),'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[])))).
functiontype('scope.fact_slow',fact_slow,1,'scope.fact_slow',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('scope.zipp1',zipp1,1,'scope.zipp1',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Int',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[])])]))).
functiontype('scope.zipp1\'2Eg\'2E27','scope.zipp1.g.27',1,'scope.zipp1\'2Eg\'2E27',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('scope.zipp1\'2Ef\'2E27','scope.zipp1.f.27',1,'scope.zipp1\'2Ef\'2E27',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('scope.zipp2',zipp2,1,'scope.zipp2',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Int',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[])])]))).
functiontype('scope.zipp2\'2Eg\'2E35','scope.zipp2.g.35',1,'scope.zipp2\'2Eg\'2E35',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('scope.zipp2\'2Ef\'2E35','scope.zipp2.f.35',1,'scope.zipp2\'2Ef\'2E35',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'scope.f'(_G233902,_G233903,_G233904,_G233905,_G233906):-freeze(_G233905,'blocked_scope.f'(_G233902,_G233903,_G233904,_G233905,_G233906)).
'blocked_scope.f'(_G233945,_G233954,_G234369,_G234372,_G234375):-hnf(_G233954,_G234693,_G234372,_G234681),'blocked_scope.f_2'(_G234693,_G233945,_G234369,_G234681,_G234375).

'blocked_scope.f_2'(_G234815,_G234816,_G234817,_G234818,_G234819):-freeze(_G234818,'blocked_blocked_scope.f_2'(_G234815,_G234816,_G234817,_G234818,_G234819)).
'blocked_blocked_scope.f_2'([],_G233945,'Prelude.False',_G234920,_G234920).
'blocked_blocked_scope.f_2'([_G234054|_G234063],_G233945,_G235246,_G235249,_G235252):-hnf('Prelude.cond'('Prelude.=:='(_G233945,_G234054),'Prelude.True'),_G235246,_G235249,_G235252).
'blocked_blocked_scope.f_2'([_G234054|_G234063],_G233945,_G235866,_G235869,_G235872):-hnf('scope.f'(_G233945,_G234063),_G235866,_G235869,_G235872).
'blocked_blocked_scope.f_2'('FAIL'(_G236220),_G233945,'FAIL'(_G236220),_G236227,_G236227):-nonvar(_G236220).

'scope.pow'(_G236487,_G236488,_G236489,_G236490,_G236491):-freeze(_G236490,'blocked_scope.pow'(_G236487,_G236488,_G236489,_G236490,_G236491)).
'blocked_scope.pow'(_G236530,_G236539,_G237501,_G237504,_G237507):-makeShare(_G236539,_G236993),hnf('Prelude.=='(_G236993,1),_G237891,_G237504,_G237876),'blocked_scope.pow_ComplexCase'(_G237891,_G236530,_G236993,_G237501,_G237876,_G237507).

'blocked_scope.pow_ComplexCase'(_G238059,_G238060,_G238061,_G238062,_G238063,_G238064):-freeze(_G238063,freeze(_G238059,'blocked_blocked_scope.pow_ComplexCase'(_G238059,_G238060,_G238061,_G238062,_G238063,_G238064))).
'blocked_blocked_scope.pow_ComplexCase'('Prelude.True',_G236530,_G236993,_G238256,_G238259,_G238262):-hnf(_G236530,_G238256,_G238259,_G238262).
'blocked_blocked_scope.pow_ComplexCase'('Prelude.False',_G236530,_G236993,_G238639,_G238642,_G238645):-!,makeShare(_G236530,_G238698),hnf('Prelude.*'(_G238698,'scope.pow'(_G238698,'Prelude.-'(_G236993,1))),_G238639,_G238642,_G238645).
'blocked_blocked_scope.pow_ComplexCase'('FAIL'(_G239445),_G236530,_G236993,'FAIL'(_G239445),_G239452,_G239452).

'scope.fact'(_G239732,_G239733,_G239734,_G239735):-freeze(_G239734,'blocked_scope.fact'(_G239732,_G239733,_G239734,_G239735)).
'blocked_scope.fact'(_G239770,_G239850,_G239853,_G239856):-hnf('scope.fact\'2Efact_aux\'2E15'(1,_G239770),_G239850,_G239853,_G239856).

'scope.fact\'2Efact_aux\'2E15'(_G240741,_G240742,_G240743,_G240744,_G240745):-freeze(_G240744,'blocked_scope.fact\'2Efact_aux\'2E15'(_G240741,_G240742,_G240743,_G240744,_G240745)).
'blocked_scope.fact\'2Efact_aux\'2E15'(_G240784,_G240793,_G241800,_G241803,_G241806):-makeShare(_G240793,_G241241),hnf('Prelude.=='(_G241241,1),_G242496,_G241803,_G242481),'blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'(_G242496,_G240784,_G241241,_G241800,_G242481,_G241806).

'blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'(_G242715,_G242716,_G242717,_G242718,_G242719,_G242720):-freeze(_G242719,freeze(_G242715,'blocked_blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'(_G242715,_G242716,_G242717,_G242718,_G242719,_G242720))).
'blocked_blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'('Prelude.True',_G240784,_G241241,_G242912,_G242915,_G242918):-hnf(_G240784,_G242912,_G242915,_G242918).
'blocked_blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'('Prelude.False',_G240784,_G241241,_G243346,_G243349,_G243352):-!,makeShare(_G241241,_G243405),hnf('scope.fact\'2Efact_aux\'2E15'('Prelude.*'(_G243405,_G240784),'Prelude.-'(_G243405,1)),_G243346,_G243349,_G243352).
'blocked_blocked_scope.fact\'2Efact_aux\'2E15_ComplexCase'('FAIL'(_G244284),_G240784,_G241241,'FAIL'(_G244284),_G244291,_G244291).

'scope.fact_slow'(_G244661,_G244662,_G244663,_G244664):-freeze(_G244663,'blocked_scope.fact_slow'(_G244661,_G244662,_G244663,_G244664)).
'blocked_scope.fact_slow'(_G244699,_G245577,_G245580,_G245583):-makeShare(_G244699,_G245087),hnf('Prelude.=='(_G245087,0),_G246068,_G245580,_G246056),'blocked_scope.fact_slow_ComplexCase'(_G246068,_G245087,_G245577,_G246056,_G245583).

'blocked_scope.fact_slow_ComplexCase'(_G246253,_G246254,_G246255,_G246256,_G246257):-freeze(_G246256,freeze(_G246253,'blocked_blocked_scope.fact_slow_ComplexCase'(_G246253,_G246254,_G246255,_G246256,_G246257))).
'blocked_blocked_scope.fact_slow_ComplexCase'('Prelude.True',_G245087,1,_G246448,_G246448).
'blocked_blocked_scope.fact_slow_ComplexCase'('Prelude.False',_G245087,_G246797,_G246800,_G246803):-!,makeShare(_G245087,_G246840),hnf('Prelude.*'(_G246840,'scope.fact_slow'('Prelude.-'(_G246840,1))),_G246797,_G246800,_G246803).
'blocked_blocked_scope.fact_slow_ComplexCase'('FAIL'(_G247555),_G245087,'FAIL'(_G247555),_G247562,_G247562).

'scope.zipp1'(_G247856,_G247857,_G247858,_G247859):-freeze(_G247858,'blocked_scope.zipp1'(_G247856,_G247857,_G247858,_G247859)).
'blocked_scope.zipp1'(_G247894,_G248054,_G248057,_G248060):-makeShare(_G247894,_G248093),hnf('Prelude.zip'(_G248093,'Prelude.map'(partcall(1,'scope.zipp1\'2Ef\'2E27',[]),_G248093)),_G248054,_G248057,_G248060).

'scope.zipp1\'2Eg\'2E27'(_G249148,_G249149,_G249150,_G249151):-freeze(_G249150,'blocked_scope.zipp1\'2Eg\'2E27'(_G249148,_G249149,_G249150,_G249151)).
'blocked_scope.zipp1\'2Eg\'2E27'(_G249186,_G249266,_G249269,_G249272):-hnf('Prelude.+'(_G249186,1),_G249266,_G249269,_G249272).

'scope.zipp1\'2Ef\'2E27'(_G250024,_G250025,_G250026,_G250027):-freeze(_G250026,'blocked_scope.zipp1\'2Ef\'2E27'(_G250024,_G250025,_G250026,_G250027)).
'blocked_scope.zipp1\'2Ef\'2E27'(_G250062,_G250109,_G250112,_G250115):-hnf('scope.zipp1\'2Eg\'2E27'(_G250062),_G250109,_G250112,_G250115).

'scope.zipp2'(_G250693,_G250694,_G250695,_G250696):-freeze(_G250695,'blocked_scope.zipp2'(_G250693,_G250694,_G250695,_G250696)).
'blocked_scope.zipp2'(_G250731,_G250891,_G250894,_G250897):-makeShare(_G250731,_G250930),hnf('Prelude.zip'(_G250930,'Prelude.map'(partcall(1,'scope.zipp2\'2Ef\'2E35',[]),_G250930)),_G250891,_G250894,_G250897).

'scope.zipp2\'2Eg\'2E35'(_G251985,_G251986,_G251987,_G251988):-freeze(_G251987,'blocked_scope.zipp2\'2Eg\'2E35'(_G251985,_G251986,_G251987,_G251988)).
'blocked_scope.zipp2\'2Eg\'2E35'(_G252023,_G252103,_G252106,_G252109):-hnf('Prelude.+'(_G252023,2),_G252103,_G252106,_G252109).

'scope.zipp2\'2Ef\'2E35'(_G252861,_G252862,_G252863,_G252864):-freeze(_G252863,'blocked_scope.zipp2\'2Ef\'2E35'(_G252861,_G252862,_G252863,_G252864)).
'blocked_scope.zipp2\'2Ef\'2E35'(_G252899,_G252946,_G252949,_G252952):-hnf('scope.zipp2\'2Eg\'2E35'(_G252899),_G252946,_G252949,_G252952).

:-costCenters(['']).




%%%%% Number of shared variables: 8
