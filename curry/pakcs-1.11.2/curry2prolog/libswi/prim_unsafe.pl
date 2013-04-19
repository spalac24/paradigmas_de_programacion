:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module(prim_readshowterm).
prim_unsafePerformIO(_G118,_G119,_G120,_G121):-freeze(_G120,blocked_prim_unsafePerformIO(_G118,_G119,_G120,_G121)).
blocked_prim_unsafePerformIO(_G152,_G153,_G154,_G155):-worldToken(_G157),prim_apply(_G152,_G157,'$io'(_G159),_G154,_G165),user:hnf(_G159,_G153,_G165,_G155).
prim_spawnConstraint(_G350,_G351,_G352,_G353,_G354):-freeze(_G353,blocked_prim_spawnConstraint(_G350,_G351,_G352,_G353,_G354)).
blocked_prim_spawnConstraint(_G389,_G390,_G391,_G392,_G393):-user:hnf(_G389,_G396,_G392,_G398),user:hnf(_G390,_G391,_G392,_G393).
prim_isVar(_G447,_G448):-var(_G447)->_G448='Prelude.True';_G448='Prelude.False'.
prim_identicalVar(_G13,_G14,_G15):-var(_G14),var(_G13),_G14==_G13->_G15='Prelude.True';_G15='Prelude.False'.
prim_showAnyTerm(_G67,_G68):-copy_term(_G67,_G71),groundTermVars(_G71,0,_G75),show_term(_G71,unqualified,_G68,[]).
prim_showAnyQTerm(_G109,_G110):-copy_term(_G109,_G113),groundTermVars(_G113,0,_G117),show_term(_G113,qualified,_G110,[]).
prim_showAnyExpression(_G296,_G297,_G298,_G299):-freeze(_G298,blocked_prim_showAnyExpression(_G296,_G297,_G298,_G299)).
blocked_prim_showAnyExpression(_G330,_G331,_G332,_G333):-removeShares(_G330,_G336),copy_term(_G336,_G339),groundTermVars(_G339,0,_G343),show_term(_G339,unqualified,_G331,[]),_G332=_G333.
prim_showAnyQExpression(_G145,_G146,_G147,_G148):-freeze(_G147,blocked_prim_showAnyQExpression(_G145,_G146,_G147,_G148)).
blocked_prim_showAnyQExpression(_G179,_G180,_G181,_G182):-shares2let(_G184,_G179,_G186),bindSingleLets(_G184),copy_term(_G186,_G191),groundTermVars(_G191,0,_G195),show_term(_G191,qualified,_G180,[]),_G181=_G182.
shares2let(_G249,_G250,_G250):-var(_G250),!.
shares2let(_G286,makeShare(_G283,_G284),_G288):-!,shares2let(_G286,_G283,_G288),writeErr('MAKESHARE OCCURRED'),nlErr.
shares2let(_G332,share(_G330),_G334):-lookupMutable(_G332,_G330,_G334),!.
shares2let(_G370,share(_G368),_G372):-!,get_mutable(_G374,_G368), (_G374='$eval'(_G377)->true;_G377=_G374),shares2let(_G370,_G377,_G393),addOL(_G370, (_G368,_G398,_G393,_G372)).
shares2let(_G444,_G445,_G446):-_G445=..[_G448|_G449],shares2letL(_G444,_G449,_G456),_G446=..[_G448|_G456].
shares2letL(_G25,[],[]).
shares2letL(_G54,[_G48|_G49],[_G51|_G52]):-shares2let(_G54,_G48,_G51),shares2letL(_G54,_G49,_G52).
lookupMutable(_G94,_G95,_G96):-var(_G94),!,fail.
lookupMutable([ (_G137,_G134,_G131,_G132)|_G141],_G144,_G145):-_G144==_G137,!,_G145=_G134,_G132=let(_G145,_G131).
lookupMutable([_G193|_G194],_G197,_G198):-lookupMutable(_G194,_G197,_G198).
addOL(_G229,_G230):-var(_G229),!,_G229=[_G230|_G235].
addOL([_G267|_G268],_G271):-addOL(_G268,_G271).
bindSingleLets(_G297):-var(_G297),!.
bindSingleLets([ (_G327,_G324,_G321,_G322)|_G331]):- (var(_G322)->_G322=_G321;true),bindSingleLets(_G331).
groundTermVars(_G368,_G369,_G370):-var(_G368),!,_G368='_'(_G369),_G370 is _G369+1.
groundTermVars(_G419,_G420,_G420):-atom(_G419),!.
groundTermVars(_G453,_G454,_G455):-_G453=..[_G457|_G458],groundTermsVars(_G458,_G454,_G455).
groundTermsVars([],_G26,_G26).
groundTermsVars([_G48|_G49],_G52,_G53):-groundTermVars(_G48,_G52,_G57),groundTermsVars(_G49,_G57,_G53).
prim_readsAnyQTerm(_G97,['Prelude.(,)'(_G91,_G92)]):-map2M(basics:char_int,_G97,_G105),readTerm(_G105,any_qualified,_G109,_G110),ungroundTermVars(_G110,_G91,_G114),map2M(basics:char_int,_G92,_G109),!.
prim_readsAnyQTerm(_G156,[]).
prim_readsAnyUnqualifiedTerm(_G180,_G181,['Prelude.(,)'(_G174,_G175)]):- (_G180=[]->_G187=any;map2M(prim_readshowterm:prefix2prefixdot,_G180,_G187)),map2M(basics:char_int,_G181,_G208),readTerm(_G208,any_unqualified(_G187),_G214,_G215),ungroundTermVars(_G215,_G174,_G219),map2M(basics:char_int,_G175,_G214),!.
prim_readsAnyUnqualifiedTerm(_G268,_G269,[]).
prim_readsAnyQExpression(_G297,['Prelude.(,)'(_G291,_G292)]):-map2M(basics:char_int,_G297,_G305),readTerm(_G305,any_expression,_G309,_G310),ungroundTermVars(_G310,_G313,_G314),let2share(_G313,_G291),map2M(basics:char_int,_G292,_G309),!.
prim_readsAnyQExpression(_G362,[]).
let2share(_G380,_G380):-var(_G380),!.
let2share(share(_G409),share(_G409)):-!.
let2share(let(_G437,_G438),_G437):-!,create_mutable(_G438,_G444),_G437=share(_G444).
let2share(_G25,_G26):-_G25=..[_G28|_G29],map2M(user:let2share,_G29,_G39),_G26=..[_G28|_G39].
ungroundTermVars('_'(_G74),_G77,_G78):-!,getVarIndex(_G78,_G74,_G77).
ungroundTermVars(_G112,_G112,_G114):-atom(_G112),!.
ungroundTermVars(_G146,_G147,_G148):-_G146=..[_G150|_G151],ungroundTermsVars(_G151,_G157,_G148),_G147=..[_G150|_G157].
ungroundTermsVars([],[],_G199).
ungroundTermsVars([_G220|_G221],[_G223|_G224],_G228):-ungroundTermVars(_G220,_G223,_G228),ungroundTermsVars(_G221,_G224,_G228).
getVarIndex(_G266,_G267,_G268):-var(_G266),!,_G266=[_G267=_G268|_G276].
getVarIndex([_G312=_G313|_G316],_G312,_G320):-!,_G313=_G320.
getVarIndex([_G353|_G354],_G357,_G358):-getVarIndex(_G354,_G357,_G358).
