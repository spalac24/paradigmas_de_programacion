%PAKCS1.11 swi5 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').


:-curryModule(firstprog).

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('firstprog.nine',nine,0,'firstprog.nine',nofix,'TCons'('Prelude.Int',[])).
functiontype('firstprog.square',square,1,'firstprog.square',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('firstprog.loop',loop,0,'firstprog.loop',nofix,'TCons'('Prelude.Int',[])).
functiontype('firstprog.fac',fac,1,'firstprog.fac',nofix,'FuncType'('TCons'('Prelude.Int',[]),'TCons'('Prelude.Int',[]))).
functiontype('firstprog.:-:',:-:,2,'firstprog.:-:',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('Prelude.Bool',[]),'TCons'('Prelude.Bool',[])))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'firstprog.nine'(_G70264,_G70265,_G70266):-freeze(_G70265,'blocked_firstprog.nine'(_G70264,_G70265,_G70266)).
'blocked_firstprog.nine'(9,_G70298,_G70298).

'firstprog.square'(_G70761,_G70762,_G70763,_G70764):-freeze(_G70763,'blocked_firstprog.square'(_G70761,_G70762,_G70763,_G70764)).
'blocked_firstprog.square'(_G70799,_G70879,_G70882,_G70885):-makeShare(_G70799,_G70918),hnf('Prelude.*'(_G70918,_G70918),_G70879,_G70882,_G70885).

'firstprog.loop'(_G71639,_G71640,_G71641):-freeze(_G71640,'blocked_firstprog.loop'(_G71639,_G71640,_G71641)).
'blocked_firstprog.loop'(_G71750,_G71753,_G71756):-hnf('Prelude.+'(1,'firstprog.loop'),_G71750,_G71753,_G71756).

'firstprog.fac'(_G72402,_G72403,_G72404,_G72405):-freeze(_G72404,'blocked_firstprog.fac'(_G72402,_G72403,_G72404,_G72405)).
'blocked_firstprog.fac'(_G72440,_G73312,_G73315,_G73318):-makeShare(_G72440,_G72828),hnf('Prelude.=='(_G72828,0),_G73767,_G73315,_G73755),'blocked_firstprog.fac_ComplexCase'(_G73767,_G72828,_G73312,_G73755,_G73318).

'blocked_firstprog.fac_ComplexCase'(_G73946,_G73947,_G73948,_G73949,_G73950):-freeze(_G73949,freeze(_G73946,'blocked_blocked_firstprog.fac_ComplexCase'(_G73946,_G73947,_G73948,_G73949,_G73950))).
'blocked_blocked_firstprog.fac_ComplexCase'('Prelude.True',_G72828,1,_G74141,_G74141).
'blocked_blocked_firstprog.fac_ComplexCase'('Prelude.False',_G72828,_G74484,_G74487,_G74490):-!,makeShare(_G72828,_G74527),hnf('Prelude.*'(_G74527,'firstprog.fac'('Prelude.-'(_G74527,1))),_G74484,_G74487,_G74490).
'blocked_blocked_firstprog.fac_ComplexCase'('FAIL'(_G75242),_G72828,'FAIL'(_G75242),_G75249,_G75249).

'firstprog.:-:'(_G75633,_G75634,_G75635,_G75636,_G75637):-freeze(_G75636,'blocked_firstprog.:-:'(_G75633,_G75634,_G75635,_G75636,_G75637)).
'blocked_firstprog.:-:'(_G75676,_G75685,_G76052,_G76055,_G76058):-hnf(_G75676,_G76484,_G76055,_G76472),'blocked_firstprog.:-:_1'(_G76484,_G75685,_G76052,_G76472,_G76058).

'blocked_firstprog.:-:_1'(_G76624,_G76625,_G76626,_G76627,_G76628):-freeze(_G76627,'blocked_blocked_firstprog.:-:_1'(_G76624,_G76625,_G76626,_G76627,_G76628)).
'blocked_blocked_firstprog.:-:_1'('Prelude.True',_G75685,_G76914,_G76917,_G76920):-hnf(_G75685,_G77715,_G76917,_G77706),'blocked_blocked_firstprog.:-:_1_Prelude.True_1'(_G77715,_G76914,_G77706,_G76920).

'blocked_blocked_firstprog.:-:_1_Prelude.True_1'(_G77923,_G77924,_G77925,_G77926):-freeze(_G77925,'blocked_blocked_blocked_firstprog.:-:_1_Prelude.True_1'(_G77923,_G77924,_G77925,_G77926)).
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.True_1'('Prelude.False','Prelude.True',_G78116,_G78116).
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.True_1'('Prelude.True','Prelude.False',_G78527,_G78527):-!.
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.True_1'('FAIL'(_G78797),'FAIL'(_G78797),_G78804,_G78804):-nonvar(_G78797).
'blocked_blocked_firstprog.:-:_1'('Prelude.False',_G75685,_G79083,_G79086,_G79089):-!,hnf(_G75685,_G79899,_G79086,_G79890),'blocked_blocked_firstprog.:-:_1_Prelude.False_1'(_G79899,_G79083,_G79890,_G79089).

'blocked_blocked_firstprog.:-:_1_Prelude.False_1'(_G80119,_G80120,_G80121,_G80122):-freeze(_G80121,'blocked_blocked_blocked_firstprog.:-:_1_Prelude.False_1'(_G80119,_G80120,_G80121,_G80122)).
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.False_1'('Prelude.True','Prelude.True',_G80306,_G80306).
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.False_1'('Prelude.False','Prelude.False',_G80726,_G80726):-!.
'blocked_blocked_blocked_firstprog.:-:_1_Prelude.False_1'('FAIL'(_G80999),'FAIL'(_G80999),_G81006,_G81006):-nonvar(_G80999).
'blocked_blocked_firstprog.:-:_1'('FAIL'(_G81031),_G75685,'FAIL'(_G81031),_G81038,_G81038):-nonvar(_G81031).

:-costCenters(['']).




%%%%% Number of shared variables: 3
