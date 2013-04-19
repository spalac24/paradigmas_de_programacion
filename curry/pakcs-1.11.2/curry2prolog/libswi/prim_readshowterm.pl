:-module(prim_readshowterm,[prim_showQTerm/2,prim_showTerm/2,show_term/4,prim_readsQTerm/2,prim_readsUnqualifiedTerm/3,readTerm/4,skipWhiteSpace/2,isShowableArg/1]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
prim_showQTerm(_G161,_G162):-show_term(_G161,qualified,_G162,[]),!.
prim_showTerm(_G193,_G194):-show_term(_G193,unqualified,_G194,[]),!.
show_term(_G225,_G226,_G227,_G227):-var(_G225),!,writeErr('*** Internal error in ReadShowTerm.showTerm: free variable'),nlErr.
show_term('_'(_G272),_G278,[_G274|_G275],_G280):-!,char_int(_G274,95),number_codes(_G272,_G286),map2M(basics:char_int,_G292,_G286),diffList(_G292,_G275,_G280).
show_term([],_G347,[_G340,_G343|_G344],_G344):-!,char_int(_G340,91),char_int(_G343,93).
show_term(_G392,_G393,_G394,_G395):-integer(_G392),!,showNumber(_G392,_G394,_G395).
show_term(_G438,_G439,_G440,_G441):-float(_G438),!,showNumber(_G438,_G440,_G441).
show_term(_G4,_G5,[_G1|_G2],_G7):-isCharCons(_G4),!,char_int(_G1,39),char_int(_G4,_G15),show_termchar(_G15,_G2,_G19),_G19=[_G1|_G7].
show_term('Prelude.()',_G78,[_G71,_G74|_G75],_G75):-!,char_int(_G71,40),char_int(_G74,41).
show_term('VAR',_G124,_G125,_G126):-!,atom2String('VAR',_G129),diffList(_G129,_G125,_G126).
show_term(_G170,_G171,_G172,_G173):-atom(_G170),!,atom2String(_G170,_G178), (_G171=qualified->_G183=_G178;char_int(_G189,46),removeQualifier(_G178,_G189,_G183)), (isId(_G170)->diffList(_G183,_G172,_G173);char_int(_G211,40),char_int(_G214,41),append([_G211|_G183],[_G214|_G173],_G172)),!.
show_term(_G283,_G284,[_G280|_G281],_G286):-isString(_G283),!,char_int(_G280,34),show_termstring(_G283,_G281,_G286).
show_term([_G335|_G336],_G342,[_G338|_G339],_G344):-isCompleteList(_G336,_G347),!,char_int(_G338,91),show_term(_G335,_G342,_G339,_G355),show_termcomplist(_G347,_G342,_G355,_G344).
show_term([_G403|_G404],_G410,[_G406|_G407],_G412):-!,char_int(_G406,40),char_int(_G417,41),show_termlist([_G403|_G404],_G410,_G407,_G426),_G426=[_G417|_G412].
show_term('Dynamic.Dynamic'(_G1),_G4,_G5,_G6):-!,functor(_G1,_G9,_G10),atom_codes(_G9,_G13), (append([36,68,89,78,95],_G31,_G13)->atom_codes(_G34,_G31);_G34=_G9),!,show_term(_G34,_G4,_G5,_G6).
show_term(_G98,_G99,[_G95|_G96],_G101):-isShowableArg(_G98),!,functor(_G98,_G106,_G107),char_int(_G95,40),char_int(_G112,41), (isTupleCons(_G106)->show_termtuple(1,_G107,_G98,_G99,_G96,_G122);show_term(_G106,_G99,_G96,_G130),show_termargs(1,_G107,_G98,_G99,_G130,_G122)),_G122=[_G112|_G101].
showNumber(_G198,_G199,_G200):-number_codes(_G198,_G203),map2M(basics:char_int,_G209,_G203), (_G198>=0->diffList(_G209,_G199,_G200);char_int(_G222,40),char_int(_G225,41),append([_G222|_G209],[_G225|_G200],_G199)).
show_termstring([],[_G278|_G279],_G279):-char_int(_G278,34).
show_termstring([_G313|_G314],_G317,_G318):-char_int(_G313,_G321),show_termchar(_G321,_G317,_G325),show_termstring(_G314,_G325,_G318).
show_termchar(34,[_G362,_G365|_G366],_G366):-!,cp_string([_G362,_G365],[92,34]).
show_termchar(39,[_G415,_G418|_G419],_G419):-!,cp_string([_G415,_G418],[92,39]).
show_termchar(92,[_G1,_G4|_G5],_G5):-!,cp_string([_G1,_G4],[92,92]).
show_termchar(10,[_G51,_G54|_G55],_G55):-!,cp_string([_G51,_G54],[92,110]).
show_termchar(13,[_G104,_G107|_G108],_G108):-!,cp_string([_G104,_G107],[92,114]).
show_termchar(9,[_G157,_G160|_G161],_G161):-!,cp_string([_G157,_G160],[92,116]).
show_termchar(8,[_G210,_G213|_G214],_G214):-!,cp_string([_G210,_G213],[92,98]).
show_termchar(_G275,[_G263,_G266,_G269,_G272|_G273],_G273):- (_G275<32;_G275>126),!,_G294 is _G275//100+48,_G306 is _G275 mod 100//10+48,_G315 is _G275 mod 10+48,cp_string([_G263,_G266,_G269,_G272],[92,_G294,_G306,_G315]).
show_termchar(_G388,[_G385|_G386],_G386):-char_int(_G385,_G388).
show_termlist(_G420,_G421,_G422,_G423):-nonvar(_G420),_G420=[_G427|_G428],!,show_term(_G427,_G421,_G422,_G436),char_int(_G438,58),_G436=[_G438|_G442],show_termlist(_G428,_G421,_G442,_G423).
show_termlist(_G1,_G2,_G3,_G4):-show_term(_G1,_G2,_G3,_G4).
show_termcomplist([],_G44,[_G40|_G41],_G41):-char_int(_G40,93).
show_termcomplist([_G80|_G81],_G87,[_G83|_G84],_G89):-char_int(_G83,44),show_term(_G80,_G87,_G84,_G97),show_termcomplist(_G81,_G87,_G97,_G89).
show_termargs(_G139,_G140,_G141,_G142,_G143,_G144):-_G139>_G140->_G143=_G144;char_int(_G155,32),_G143=[_G155|_G159],arg(_G139,_G141,_G166),show_term(_G166,_G142,_G159,_G171),_G176 is _G139+1,show_termargs(_G176,_G140,_G141,_G142,_G171,_G144).
show_termtuple(_G241,_G242,_G243,_G244,_G245,_G246):-_G241=_G242->arg(_G241,_G243,_G253),show_term(_G253,_G244,_G245,_G246);arg(_G241,_G243,_G253),show_term(_G253,_G244,_G245,_G273),char_int(_G275,44),_G273=[_G275|_G279],_G287 is _G241+1,show_termtuple(_G287,_G242,_G243,_G244,_G279,_G246).
isShowableArg('Ports.internalPort'(_G352,_G353,_G354,_G355)):-_G353=<0,writeErr('ERROR: cannot serialize internal port!'),nlErr,!,fail.
isShowableArg(_G393).
removeQualifier(_G406,_G407,_G408):-removeQualifier(_G406,_G407,_G406,_G408).
removeQualifier([],_G441,_G442,_G442).
removeQualifier([_G1|_G2],_G5,_G6,_G7):-_G1=_G5->_G7=_G2;removeQualifier(_G2,_G5,_G6,_G7).
diffList([],_G53,_G53).
diffList([_G75|_G76],[_G75|_G79],_G83):-diffList(_G76,_G79,_G83).
prim_readsQTerm(_G120,['Prelude.(,)'(_G114,_G115)]):-map2M(basics:char_int,_G120,_G128),readTerm(_G128,qualified,_G132,_G114),map2M(basics:char_int,_G115,_G132),!.
prim_readsQTerm(_G172,[]).
prim_readsUnqualifiedTerm(_G196,_G197,['Prelude.(,)'(_G190,_G191)]):- (_G196=[]->_G203=any;map2M(prim_readshowterm:prefix2prefixdot,_G196,_G203)),map2M(basics:char_int,_G197,_G224),readTerm(_G224,unqualified(_G203),_G230,_G190),map2M(basics:char_int,_G191,_G230),!.
prim_readsUnqualifiedTerm(_G277,_G278,[]).
prefix2prefixdot(_G300,_G301):-map2M(basics:char_int,_G300,_G308),append(_G308,[46],_G301).
readTerm(_G341,_G342,_G343,_G344):-skipWhiteSpace(_G341,_G347),readTerm0(_G347,_G342,_G343,_G344).
readTerm0([_G1|_G2],_G3,_G4,_G5):-isLetter(_G1),!,readTermS([_G1|_G2],_G3,_G14,_G15),skipWhiteSpace(_G14,_G18), (_G15=partcall->readTerm(_G18,_G3,_G25,_G26),skipWhiteSpace(_G25,_G29),readPartCallFunc(_G29,_G32,_G33),atom_codes(_G35,_G33),readTermArgs(_G32,_G3,_G4,[_G38]),_G5=partcall(_G26,_G35,_G38);readTermArgs(_G18,_G3,_G4,_G74),_G5=..[_G15|_G74]).
readTerm0([36|_G125],_G128,_G129,_G130):-_G125=[68,89,78|_G139],!, (readQVarOpId(_G125,_G145,_G146)->true;readParseError([36|_G125])),atom_codes(_G162,[36|_G146]),skipWhiteSpace(_G145,_G166),readTermArgs(_G166,_G128,_G129,_G171),_G130=..[_G162|_G171].
readTerm0(_G226,_G227,_G228,_G229):-readTermS(_G226,_G227,_G228,_G229).
readTermS([95|_G266],_G271,_G272,'_'(_G268)):- (_G271=any_qualified;_G271=any_unqualified(_G278);_G271=any_expression),!,numberconst(_G292,_G266,_G272),!,number_codes(_G268,_G292).
readTermS([_G340|_G341],_G344,_G345,_G346):-_G340>47,_G340<58,numberconst(_G357,[_G340|_G341],_G345),!,number_codes(_G346,_G357).
readTermS([45,_G408|_G409],_G412,_G413,_G414):-_G408>47,_G408<58,numberconst(_G425,[_G408|_G409],_G413),!,number_codes(_G429,_G425),_G414 is 0-_G429.
readTermS([91,93|_G5],_G8,_G5,[]):-!.
readTermS([91|_G39],_G42,_G43,_G44):-!,readCompList(_G39,_G42,_G43,_G44).
readTermS([39|_G84],_G87,_G88,_G89):-!, (readChar(_G84,_G88,_G89)->true;readCharParseError(_G87,[39|_G84])).
readTermS([34|_G140],_G143,_G144,_G145):-!,readString(_G140,_G144,_G145).
readTermS([40,41|_G187],_G190,_G187,'Prelude.()'):-!.
readTermS([40|_G224],_G227,_G228,_G229):-!,readTerm(_G224,_G227,_G233,_G234),skipWhiteSpace(_G233,_G237), (_G237=[58|_G240]->readList(_G240,_G227,_G247,_G248),_G229=[_G234|_G248],_G247=[41|_G228];_G237=[44|_G240]->readTuple(_G240,_G227,_G247,_G248),_G247=[41|_G228],_G291=[_G234|_G248],length(_G291,_G295),_G300=[41],prefixComma(_G300,_G295,_G305),append([80,114,101,108,117,100,101,46,40],_G305,_G336),atom_codes(_G338,_G336),_G229=..[_G338|_G291];_G237=[41|_G228]->_G229=_G234;readTermArgs(_G237,_G227,[41|_G228],_G291),_G229=..[_G234|_G291]).
readTermS(_G0,_G1,_G2,_G3):- (readQVarOpId(_G0,_G2,_G7)->true;readParseError(_G0)),atom_codes(_G17,_G7),readIdTerm(_G17,_G1,_G0,_G3).
readPartCallFunc([_G54|_G55],_G58,[]):-isWhiteSpace(_G54),!,skipWhiteSpace(_G55,_G58).
readPartCallFunc([_G97|_G98],_G104,[_G97|_G101]):-readPartCallFunc(_G98,_G104,_G101).
readCharParseError(unchecked,_G137):-!,writeErr('ERROR: FlatCurry file contains illegal character: ...'),take(20,_G137,_G143),putChars(user_error,_G143),writeErr(...),nlErr,writeErr('Hint: do not use UTF encoding but 8bit chars (check your locale settings)'),nlErr,!,fail.
readCharParseError(_G200,_G201):-pakcsrc(readtermerrors,yes),writeErr('ERROR in ReadShowTerm.readTerm: illegal character in remaining string:'),nlErr,putChars(user_error,_G201),nlErr,!,fail.
readParseError(_G250):-pakcsrc(readtermerrors,yes),writeErr('ERROR in ReadShowTerm.readTerm: cannot parse remaining string:'),nlErr,putChars(user_error,_G250),nlErr,!,fail.
readIdTerm('VAR',_G296,_G297,'VAR'):-!.
readIdTerm(_G329,unchecked,_G331,_G329):-!.
readIdTerm(_G363,qualified,_G365,_G366):- (constructorOrFunctionType(_G363,_G369,_G370,_G371)->_G366=_G363;tryAddQualifier(_G363,_G366)),!.
readIdTerm(_G419,unqualified(_G417),_G421,_G422):-addQualifier(_G417,_G419,_G422).
readIdTerm(_G457,any_qualified,_G459,_G457):-constructorOrFunctionType(_G457,_G463,_G464,_G465),!.
readIdTerm(_G1,any_expression,_G3,_G1):-constructorOrFunctionType(_G1,_G7,_G8,_G9),!.
readIdTerm(let,any_expression,_G45,let):-!.
readIdTerm(_G79,any_unqualified(_G77),_G81,_G82):-addQualifier(_G77,_G79,_G82).
readIdTerm(_G117,_G118,_G119,_G120):-pakcsrc(readtermerrors,yes),writeErr('ERROR in ReadShowTerm.readTerm: Unknown symbol: '),writeErr(_G117),nlErr,writeErr('in remaining term string: '),putChars(user_error,_G119),nlErr,!,fail.
tryAddQualifier(_G187,_G188):-user:constructortype(_G188,_G191,_G192,_G187,_G194,_G195),user:constructortype(_G200,_G201,_G202,_G187,_G204,_G205),\+_G188=_G200,!,writeErr('WARNING: Unqualified symbol "'),writeErr(_G187),writeErr('" not unique due to multiple imports.'),nlErr.
tryAddQualifier(_G263,_G264):-user:constructortype(_G264,_G267,_G268,_G263,_G270,_G271),!.
addQualifier(any,_G301,_G302):-constructorOrFunctionType(_G302,_G301,_G306,_G307),constructorOrFunctionType(_G309,_G301,_G311,_G312),\+_G302=_G309,!,writeErr('WARNING: Unqualified symbol "'),writeErr(_G301),writeErr('" not unique due to multiple imports.'),nlErr.
addQualifier(any,_G372,_G373):-constructorOrFunctionType(_G373,_G372,_G377,_G378),!.
addQualifier([_G408|_G409],_G412,_G413):-atom_codes(_G412,_G416),append(_G408,_G416,_G420),atom_codes(_G413,_G420),constructorOrFunctionType(_G413,_G426,_G427,_G428),!.
addQualifier([_G467|_G468],_G471,_G472):-addQualifier(_G468,_G471,_G472).
addQualifier([],_G2,_G3):-writeErr('ERROR: Unknown unqualified symbol: '),writeErr(_G2),nlErr,fail.
readCompList(_G46,_G47,_G48,[_G43|_G44]):-readTerm(_G46,_G47,_G53,_G43),skipWhiteSpace(_G53,_G57), (_G57=[93|_G60]->_G48=_G60,_G44=[];_G57=[44|_G60],readCompList(_G60,_G47,_G48,_G44)).
readList(_G129,_G130,_G131,_G132):-readTerm(_G129,_G130,_G136,_G137),skipWhiteSpace(_G136,_G140), (_G140=[58|_G143]->_G132=[_G137|_G149],readList(_G143,_G130,_G131,_G149);_G132=_G137,_G131=_G140).
readTuple(_G212,_G213,_G214,_G215):-readTerm(_G212,_G213,_G219,_G220),skipWhiteSpace(_G219,_G223), (_G223=[44|_G226]->_G215=[_G220|_G232],readTuple(_G226,_G213,_G214,_G232);_G215=[_G220],_G214=_G223).
readTermArgs([],_G299,[],[]):-!.
readTermArgs([41|_G333],_G339,[41|_G333],[]):-!.
readTermArgs([44|_G373],_G379,[44|_G373],[]):-!.
readTermArgs([58|_G413],_G419,[58|_G413],[]):-!.
readTermArgs([93|_G453],_G459,[93|_G453],[]):-!.
readTermArgs(_G4,_G5,_G6,[_G1|_G2]):-readTermS(_G4,_G5,_G11,_G1),skipWhiteSpace(_G11,_G15),readTermArgs(_G15,_G5,_G6,_G2).
readChar([92,_G60,_G63,_G66,39|_G70],_G70,_G74):-_G60>=48,_G60<52,!,_G103 is (_G60-48)*100+ (_G63-48)*10+_G66-48,char_int(_G74,_G103).
readChar([92,_G149,39|_G153],_G153,_G157):-!,readStringChar(_G149,_G160),char_int(_G157,_G160).
readChar([_G196,39|_G200],_G200,_G204):-char_int(_G204,_G196).
readString([34|_G235],_G235,[]):-!.
readString([92,_G269,_G272,_G275|_G276],_G282,[_G278|_G279]):-_G269>=48,_G269<52,!,_G312 is (_G269-48)*100+ (_G272-48)*10+_G275-48,char_int(_G278,_G312),readString(_G276,_G282,_G279).
readString([92,_G365|_G366],_G372,[_G368|_G369]):-!,readStringChar(_G365,_G376),char_int(_G368,_G376),readString(_G366,_G372,_G369).
readString([_G419|_G420],_G426,[_G422|_G423]):-char_int(_G422,_G419),readString(_G420,_G426,_G423).
readStringChar(34,34):-!.
readStringChar(92,92):-!.
readStringChar(110,10):-!.
readStringChar(114,13):-!.
readStringChar(116,9):-!.
readStringChar(98,8):-!.
readStringChar(_G113,_G113).
readQVarOpId([_G131|_G132],_G138,[_G131|_G135]):-isOpIdChar(_G131)->readOpId(_G132,_G138,_G135);readModOrVar(_G132,_G138,_G135).
readModOrVar([_G182|_G183],_G189,[_G182|_G186]):-isVarIdChar(_G182),!, (_G182=46->readQVarOpId(_G183,_G189,_G186);readModOrVar(_G183,_G189,_G186)).
readModOrVar(_G242,_G242,[]).
readOpId([_G265|_G266],_G272,[_G265|_G269]):-isOpIdChar(_G265),!,readOpId(_G266,_G272,_G269).
readOpId(_G312,_G312,[]).
skipWhiteSpace([_G335|_G336],_G339):-isWhiteSpace(_G335),!,skipWhiteSpace(_G336,_G339).
skipWhiteSpace([123,45|_G377],_G380):-!,skipComment(_G377,_G380).
skipWhiteSpace(_G409,_G409).
skipComment([],[]):-!, (pakcsrc(readtermerrors,yes)->writeErr('ERROR in ReadShowTerm.readTerm: incomplete comment'),nlErr,fail;fail).
skipComment([45,125|_G5],_G6):-!,skipWhiteSpace(_G5,_G6).
skipComment([_G20|_G21],_G24):-!,skipComment(_G21,_G24).
isWhiteSpace(32).
isWhiteSpace(10).
isWhiteSpace(13).
isWhiteSpace(12).
isWhiteSpace(9).
isVarIdChar(65).
isVarIdChar(66).
isVarIdChar(67).
isVarIdChar(68).
isVarIdChar(69).
isVarIdChar(70).
isVarIdChar(71).
isVarIdChar(72).
isVarIdChar(73).
isVarIdChar(74).
isVarIdChar(75).
isVarIdChar(76).
isVarIdChar(77).
isVarIdChar(78).
isVarIdChar(79).
isVarIdChar(80).
isVarIdChar(81).
isVarIdChar(82).
isVarIdChar(83).
isVarIdChar(84).
isVarIdChar(85).
isVarIdChar(86).
isVarIdChar(87).
isVarIdChar(88).
isVarIdChar(89).
isVarIdChar(90).
isVarIdChar(97).
isVarIdChar(98).
isVarIdChar(99).
isVarIdChar(100).
isVarIdChar(101).
isVarIdChar(102).
isVarIdChar(103).
isVarIdChar(104).
isVarIdChar(105).
isVarIdChar(106).
isVarIdChar(107).
isVarIdChar(108).
isVarIdChar(109).
isVarIdChar(110).
isVarIdChar(111).
isVarIdChar(112).
isVarIdChar(113).
isVarIdChar(114).
isVarIdChar(115).
isVarIdChar(116).
isVarIdChar(117).
isVarIdChar(118).
isVarIdChar(119).
isVarIdChar(120).
isVarIdChar(121).
isVarIdChar(122).
isVarIdChar(48).
isVarIdChar(49).
isVarIdChar(50).
isVarIdChar(51).
isVarIdChar(52).
isVarIdChar(53).
isVarIdChar(54).
isVarIdChar(55).
isVarIdChar(56).
isVarIdChar(57).
isVarIdChar(46).
isVarIdChar(95).
isVarIdChar(39).
isLetter(65).
isLetter(66).
isLetter(67).
isLetter(68).
isLetter(69).
isLetter(70).
isLetter(71).
isLetter(72).
isLetter(73).
isLetter(74).
isLetter(75).
isLetter(76).
isLetter(77).
isLetter(78).
isLetter(79).
isLetter(80).
isLetter(81).
isLetter(82).
isLetter(83).
isLetter(84).
isLetter(85).
isLetter(86).
isLetter(87).
isLetter(88).
isLetter(89).
isLetter(90).
isLetter(97).
isLetter(98).
isLetter(99).
isLetter(100).
isLetter(101).
isLetter(102).
isLetter(103).
isLetter(104).
isLetter(105).
isLetter(106).
isLetter(107).
isLetter(108).
isLetter(109).
isLetter(110).
isLetter(111).
isLetter(112).
isLetter(113).
isLetter(114).
isLetter(115).
isLetter(116).
isLetter(117).
isLetter(118).
isLetter(119).
isLetter(120).
isLetter(121).
isLetter(122).
