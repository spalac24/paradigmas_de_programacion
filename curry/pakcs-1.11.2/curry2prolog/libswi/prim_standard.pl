:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
isFail(_G0):-nonvar(_G0),_G0='FAIL'(_G4).
checkFailValue(_G146,_G147,_G148,_G149,_G150):-freeze(_G149,blocked_checkFailValue(_G146,_G147,_G148,_G149,_G150)).
blocked_checkFailValue(_G185,_G186,_G187,_G188,_G189):-nonvar(_G186),_G186='FAIL'(_G193)->_G187='FAIL'([_G185|_G193]),_G188=_G189;_G187=_G186,_G188=_G189.
normalizeAndCheck(_G393,_G394,_G395,_G396):-freeze(_G395,blocked_normalizeAndCheck(_G393,_G394,_G395,_G396)).
blocked_normalizeAndCheck(_G427,_G428,_G429,_G430):-user:nf(_G427,_G428,_G429,_G435),normalizeAndCheckNF(_G428,_G435,_G430).
normalizeAndCheckNF(_G140,_G141,_G142):-freeze(_G141,blocked_normalizeAndCheckNF(_G140,_G141,_G142)).
blocked_normalizeAndCheckNF(_G169,_G170,_G171):-isFail(_G169)->_G169='FAIL'(_G175),evaluator:writeFailSource(_G175);_G170=_G171.
prim_concurrent_and(_G359,_G360,_G361,_G362,_G363):-freeze(_G362,blocked_prim_concurrent_and(_G359,_G360,_G361,_G362,_G363)).
blocked_prim_concurrent_and(_G398,_G399,_G400,_G401,_G402):-user:hnf(_G398,_G405,_G401,_G407),user:hnf(_G399,_G413,_G401,_G415),waitConcurrentConjunction(_G405,_G413,_G400,_G407,_G415,_G402).
prim_success('Prelude.success').
prim_cond(_G124,_G125,_G126,_G127,_G128):-freeze(_G127,blocked_prim_cond(_G124,_G125,_G126,_G127,_G128)).
blocked_prim_cond(_G163,_G164,_G165,_G166,_G167):-user:hnf(_G163,_G170,_G166,_G172),prim_checkcond(_G170,_G163,_G164,_G165,_G172,_G167).
prim_checkcond(_G365,_G366,_G367,_G368,_G369,_G370):-freeze(_G369,freeze(_G365,blocked_prim_checkcond(_G365,_G366,_G367,_G368,_G369,_G370))).
blocked_prim_checkcond('Prelude.success',_G413,_G414,_G415,_G416,_G417):-user:hnf(_G414,_G415,_G416,_G417).
blocked_prim_checkcond('FAIL'(_G37),_G39,_G40,'FAIL'(['Prelude.cond'(_G39,_G40)|_G37]),_G46,_G46).
prim_letrec(_G189,_G190,_G191,_G192,_G193):-freeze(_G192,blocked_prim_letrec(_G189,_G190,_G191,_G192,_G193)).
blocked_prim_letrec(_G228,_G229,'Prelude.success',_G231,_G232):-var(_G229),!,_G228=_G229,_G231=_G232.
blocked_prim_letrec(_G284,_G285,'Prelude.success',_G287,_G288):-create_mutable(_G285,_G291),_G284=share(_G291),_G287=_G288.
prim_Int_plus(_G340,_G341,_G342):-_G342 is _G341+_G340.
prim_Int_minus(_G375,_G376,_G377):-_G377 is _G376-_G375.
prim_Int_times(_G410,_G411,_G412):-_G412 is _G411*_G410.
prim_Int_div(_G445,_G446,_G447):-_G447 is _G446//_G445.
prim_Int_mod(_G42,_G43,_G44):-_G44 is _G43 mod _G42.
prim_negateFloat(_G72,_G73):-_G73 is -_G72.
prim_ord(_G101,_G102):-char_int(_G101,_G102).
prim_chr(_G128,_G129):-_G128>=0,_G128<256,!,char_int(_G129,_G128).
prim_chr(_G170,_G171):-raise_exception('chr: argument out of range').
prim_Monad_bind(_G321,_G322,_G323,_G324,_G325):-freeze(_G324,blocked_prim_Monad_bind(_G321,_G322,_G323,_G324,_G325)).
blocked_prim_Monad_bind(_G363,_G360,partcall(1,prim_Monad_bindWorld,[_G360,_G363]),_G373,_G373).
prim_Monad_bindWorld(_G166,_G167,_G168,_G169,_G170,_G171):-freeze(_G170,blocked_prim_Monad_bindWorld(_G166,_G167,_G168,_G169,_G170,_G171)).
blocked_prim_Monad_bindWorld(_G210,_G211,_G212,_G213,_G214,_G215):-prim_apply(_G210,_G212,'$io'(_G217),_G214,_G223),prim_apply(_G211,_G217,_G227,_G223,_G229),prim_apply(_G227,_G212,_G213,_G229,_G215).
prim_Monad_seq(_G402,_G403,_G404,_G405,_G406):-freeze(_G405,blocked_prim_Monad_seq(_G402,_G403,_G404,_G405,_G406)).
blocked_prim_Monad_seq(_G444,_G441,partcall(1,prim_Monad_seqWorld,[_G441,_G444]),_G454,_G454).
prim_Monad_seqWorld(_G191,_G192,_G193,_G194,_G195,_G196):-freeze(_G195,blocked_prim_Monad_seqWorld(_G191,_G192,_G193,_G194,_G195,_G196)).
blocked_prim_Monad_seqWorld(_G235,_G236,_G237,_G238,_G239,_G240):-prim_apply(_G235,_G237,_G244,_G239,_G246),prim_apply(_G236,_G237,_G238,_G246,_G240).
prim_return(_G406,_G407,_G408,_G409):-freeze(_G408,blocked_prim_return(_G406,_G407,_G408,_G409)).
blocked_prim_return(_G440,partcall(1,prim_returnWorld,[_G440]),_G449,_G449).
prim_returnWorld(_G193,_G194,_G195,_G196,_G197):-freeze(_G196,blocked_prim_returnWorld(_G193,_G194,_G195,_G196,_G197)).
blocked_prim_returnWorld(_G232,_G235,'$io'(_G232),_G237,_G237).
prim_putChar(_G267,'Prelude.()'):-char_int(_G267,_G271),put_code(_G271), (_G271=10->flush_output;true).
prim_getChar(_G311):-get_code(_G313),char_int(_G311,_G313).
prim_readFile(_G79,_G80):-map2M(basics:char_int,_G79,_G87),isURL(_G87),!,append([114,101,97,100,70,105,108,101,32,34],_G87,_G123),append(_G123,[34,58,32,85,82,76,115,32,110,111,32,108,111,110,103,101,114,32,115,117,112,112,111,114,116,101,100,32,105,110,32,114,101,97,100,70,105,108,101,33],_G247),atom_codes(_G249,_G247),raise_exception(_G249).
prim_readFile(_G293,_G294):-string2Atom(_G293,_G297),open(_G297,read,_G301), (compileWithSharing(function)->makeShare('Prelude.prim_readFileContents'(_G301),_G294);_G294='Prelude.prim_readFileContents'(_G301)).
isURL(_G348):-append([104,116,116,112,58,47,47],_G372,_G348),!.
isURL(_G395):-append([102,116,112,58,47,47],_G416,_G395),!.
prim_readFileContents(_G199,_G200,_G201,_G202):-freeze(_G201,blocked_prim_readFileContents(_G199,_G200,_G201,_G202)).
blocked_prim_readFileContents(_G233,_G234,_G235,_G236):-atEndOfStream(_G233),!,_G234=[],close(_G233),_G235=_G236.
blocked_prim_readFileContents(_G289,_G290,_G291,_G292):-get_code(_G289,_G295),char_int(_G297,_G295),_G290=[_G297|_G301], (compileWithSharing(function)->makeShare('Prelude.prim_readFileContents'(_G289),_G301);_G301='Prelude.prim_readFileContents'(_G289)),_G291=_G292.
prim_writeFile(_G94,_G95,_G96,_G97,_G98):-freeze(_G97,blocked_prim_writeFile(_G94,_G95,_G96,_G97,_G98)).
blocked_prim_writeFile(_G136,_G133,partcall(1,prim_writeFileWorld,[_G133,_G136]),_G146,_G146).
prim_writeFileWorld(_G314,_G315,_G316,_G317,_G318,_G319):-freeze(_G318,blocked_prim_writeFileWorld(_G314,_G315,_G316,_G317,_G318,_G319)).
blocked_prim_writeFileWorld(_G358,_G359,_G360,_G361,_G362,_G363):-user:derefAll(_G358,_G366),string2Atom(_G366,_G372),open(_G372,write,_G376),prim_writeFileContents(_G376,_G359,_G360,_G361,_G362,_G363).
prim_appendFile(_G199,_G200,_G201,_G202,_G203):-freeze(_G202,blocked_prim_appendFile(_G199,_G200,_G201,_G202,_G203)).
blocked_prim_appendFile(_G241,_G238,partcall(1,prim_appendFileWorld,[_G238,_G241]),_G251,_G251).
prim_appendFileWorld(_G422,_G423,_G424,_G425,_G426,_G427):-freeze(_G426,blocked_prim_appendFileWorld(_G422,_G423,_G424,_G425,_G426,_G427)).
blocked_prim_appendFileWorld(_G109,_G110,_G111,_G112,_G113,_G114):-user:derefAll(_G109,_G117),string2Atom(_G117,_G123),open(_G123,append,_G127),prim_writeFileContents(_G127,_G110,_G111,_G112,_G113,_G114).
prim_writeFileContents(_G326,_G327,_G328,_G329,_G330,_G331):-freeze(_G330,blocked_prim_writeFileContents(_G326,_G327,_G328,_G329,_G330,_G331)).
blocked_prim_writeFileContents(_G370,_G371,_G372,_G373,_G374,_G375):-user:hnf(_G371,_G378,_G374,_G380),prim_writeFileContents1(_G378,_G370,_G372,_G373,_G380,_G375).
prim_writeFileContents1(_G244,_G245,_G246,_G247,_G248,_G249):-freeze(_G248,freeze(_G244,blocked_prim_writeFileContents1(_G244,_G245,_G246,_G247,_G248,_G249))).
blocked_prim_writeFileContents1([],_G294,_G295,'$io'('Prelude.()'),_G297,_G298):-flush_output(_G294),close(_G294),_G297=_G298.
blocked_prim_writeFileContents1([_G350|_G351],_G354,_G355,_G356,_G357,_G358):-user:hnf(_G350,_G361,_G357,_G363),put_writeFileContents(_G354,_G361,_G351,_G355,_G356,_G363,_G358).
blocked_prim_writeFileContents1('FAIL'(_G416),_G421,_G422,'FAIL'(_G416),_G424,_G424).
put_writeFileContents(_G270,_G271,_G272,_G273,_G274,_G275,_G276):-freeze(_G275,freeze(_G271,blocked_put_writeFileContents(_G270,_G271,_G272,_G273,_G274,_G275,_G276))).
blocked_put_writeFileContents(_G326,'FAIL'(_G322),_G328,_G329,'FAIL'(_G322),_G331,_G331):-!.
blocked_put_writeFileContents(_G375,_G376,_G377,_G378,_G379,_G380,_G381):-char_int(_G376,_G384),put_code(_G375,_G384),prim_writeFileContents(_G375,_G377,_G378,_G379,_G380,_G381).
prim_catch(_G214,_G215,_G216,_G217,_G218):-freeze(_G217,blocked_prim_catch(_G214,_G215,_G216,_G217,_G218)).
blocked_prim_catch(_G256,_G253,partcall(1,prim_catchWorld,[_G253,_G256]),_G266,_G266).
prim_catchWorld(_G422,_G423,_G424,_G425,_G426,_G427):-freeze(_G426,blocked_prim_catchWorld(_G422,_G423,_G424,_G425,_G426,_G427)).
blocked_prim_catchWorld(_G139,_G140,_G141,_G142,_G143,_G144):-on_exception(_G152,prim_apply(_G139,_G141,_G142,_G143,_G144), (prologError2Atom(_G152,_G153),atom2String(_G153,_G156),_G160='Prelude.IOError'(_G156),applyErrorFunction(_G140,_G160,_G141,_G142,_G143,_G144))),!.
blocked_prim_catchWorld(_G223,_G224,_G225,_G226,_G227,_G228):-atom2String('FAILURE ERROR: Execution failed',_G231),applyErrorFunction(_G224,'Prelude.IOError'(_G231),_G225,_G226,_G227,_G228).
applyErrorFunction(_G282,_G283,_G284,_G285,_G286,_G287):-prim_apply(_G282,_G283,_G291,_G286,_G293),prim_apply(_G291,_G284,_G285,_G293,_G287).
prim_catchFail(_G463,_G464,_G465,_G466,_G467):-freeze(_G466,blocked_prim_catchFail(_G463,_G464,_G465,_G466,_G467)).
blocked_prim_catchFail(_G148,_G145,partcall(1,prim_catchFailWorld,[_G145,_G148]),_G158,_G158).
prim_catchFailWorld(_G326,_G327,_G328,_G329,_G330,_G331):-freeze(_G330,blocked_prim_catchFailWorld(_G326,_G327,_G328,_G329,_G330,_G331)).
blocked_prim_catchFailWorld(_G370,_G371,_G372,_G373,_G374,_G375):-on_exception(_G383,prim_apply(_G370,_G372,_G373,_G374,_G375), (printError(_G383),fail)),!.
blocked_prim_catchFailWorld(_G432,_G433,_G434,_G435,_G436,_G437):-prim_apply(_G433,_G434,_G435,_G436,_G437).
prim_apply(_G253,_G254,_G255,_G256,_G257):-freeze(_G256,blocked_prim_apply(_G253,_G254,_G255,_G256,_G257)).
blocked_prim_apply(_G292,_G293,_G294,_G295,_G296):-user:hnf(_G292,_G299,_G295,_G301),prim_hnf_apply(_G299,_G293,_G294,_G301,_G296).
prim_hnf_apply(_G169,_G170,_G171,_G172,_G173):-freeze(_G172,freeze(_G169,blocked_prim_hnf_apply(_G169,_G170,_G171,_G172,_G173))).
blocked_prim_hnf_apply('FAIL'(_G211),_G216,'FAIL'(_G211),_G218,_G218):-!.
blocked_prim_hnf_apply(partcall(_G254,_G255,_G256),_G259,_G260,_G261,_G262):-!, (_G254=1-> (_G259=='$world'->rev([_G262,_G261,_G260,_G259|_G256],_G283),_G288=..[_G255|_G283],call(user:_G288);prim_hnf_apply_call(_G256,_G255,_G259,_G260,_G261,_G262));_G321 is _G254-1,_G260=partcall(_G321,_G255,[_G259|_G256]),_G261=_G262).
blocked_prim_hnf_apply('Dynamic.Dynamic'(_G382),_G387,'Dynamic.Dynamic'(_G384),_G389,_G390):-!,user:hnf(_G382,_G393,_G389,_G395),_G393=..[_G400|_G401],append(_G401,[_G387],_G411),_G384=..[_G400|_G411],_G395=_G390.
blocked_prim_hnf_apply(_G163,_G164,_G165,_G166,_G167):-_G163=..[_G169|_G170],append(_G170,[_G164],_G180),_G165=..[_G169|_G180],_G166=_G167.
prim_hnf_apply_call([],_G234,_G235,_G236,_G237,_G238):-!,_G246=..[_G234,_G235],user:hnf(_G246,_G236,_G237,_G238).
prim_hnf_apply_call([_G300],_G304,_G305,_G306,_G307,_G308):-!,_G319=..[_G304,_G300,_G305],user:hnf(_G319,_G306,_G307,_G308).
prim_hnf_apply_call([_G373,_G376],_G380,_G381,_G382,_G383,_G384):-!,_G398=..[_G380,_G376,_G373,_G381],user:hnf(_G398,_G382,_G383,_G384).
prim_hnf_apply_call([_G163,_G166,_G169],_G173,_G174,_G175,_G176,_G177):-!,_G194=..[_G173,_G169,_G166,_G163,_G174],user:hnf(_G194,_G175,_G176,_G177).
prim_hnf_apply_call(_G245,_G246,_G247,_G248,_G249,_G250):-rev([_G247|_G245],_G256),_G261=..[_G246|_G256],user:hnf(_G261,_G248,_G249,_G250).
prim_applySeq(_G434,_G435,_G436,_G437,_G438):-freeze(_G437,blocked_prim_applySeq(_G434,_G435,_G436,_G437,_G438)).
blocked_prim_applySeq(_G169,_G170,_G171,_G172,_G173):-user:hnf(_G170,_G176,_G172,_G178),prim_applySeqHNF(_G169,_G176,_G171,_G178,_G173).
prim_applySeqHNF(_G350,_G351,_G352,_G353,_G354):-freeze(_G353,blocked_prim_applySeqHNF(_G350,_G351,_G352,_G353,_G354)).
blocked_prim_applySeqHNF(_G389,_G390,_G391,_G392,_G393):-isFail(_G390)->_G391=_G390,_G392=_G393;user:hnf(_G389,_G410,_G392,_G412),prim_hnf_apply(_G410,_G390,_G391,_G412,_G393).
prim_applyNormalForm(_G292,_G293,_G294,_G295,_G296):-freeze(_G295,blocked_prim_applyNormalForm(_G292,_G293,_G294,_G295,_G296)).
blocked_prim_applyNormalForm(_G331,_G332,_G333,_G334,_G335):-user:nf(_G332,_G338,_G334,_G340),prim_applyNormalFormNF(_G331,_G338,_G333,_G340,_G335).
prim_applyNormalFormNF(_G533,_G534,_G535,_G536,_G537):-freeze(_G536,blocked_prim_applyNormalFormNF(_G533,_G534,_G535,_G536,_G537)).
blocked_prim_applyNormalFormNF(_G572,_G573,_G574,_G575,_G576):-isFail(_G573)->_G574=_G573,_G575=_G576;user:hnf(_G572,_G593,_G575,_G595),prim_hnf_apply(_G593,_G573,_G574,_G595,_G576).
prim_applyNotFree(_G776,_G777,_G778,_G779,_G780):-freeze(_G779,blocked_prim_applyNotFree(_G776,_G777,_G778,_G779,_G780)).
blocked_prim_applyNotFree(_G815,_G816,_G817,_G818,_G819):-user:hnf('Prelude.ensureNotFree'(_G816),_G824,_G818,_G826),prim_applyNotFreeHNF(_G815,_G824,_G817,_G826,_G819).
prim_applyNotFreeHNF(_G1013,_G1014,_G1015,_G1016,_G1017):-freeze(_G1016,blocked_prim_applyNotFreeHNF(_G1013,_G1014,_G1015,_G1016,_G1017)).
blocked_prim_applyNotFreeHNF(_G1052,_G1053,_G1054,_G1055,_G1056):-isFail(_G1053)->_G1054=_G1053,_G1055=_G1056;user:hnf(_G1052,_G1073,_G1055,_G1075),prim_hnf_apply(_G1073,_G1053,_G1054,_G1075,_G1056).
prim_applyGroundNormalForm(_G1283,_G1284,_G1285,_G1286,_G1287):-freeze(_G1286,blocked_prim_applyGroundNormalForm(_G1283,_G1284,_G1285,_G1286,_G1287)).
blocked_prim_applyGroundNormalForm(_G1322,_G1323,_G1324,_G1325,_G1326):-user:nf(_G1323,_G1329,_G1325,_G1331),waitUntilGround(_G1329,_G1331,_G1338),prim_applyGroundNormalFormNF(_G1322,_G1329,_G1324,_G1338,_G1326).
prim_applyGroundNormalFormNF(_G340,_G341,_G342,_G343,_G344):-freeze(_G343,blocked_prim_applyGroundNormalFormNF(_G340,_G341,_G342,_G343,_G344)).
blocked_prim_applyGroundNormalFormNF(_G379,_G380,_G381,_G382,_G383):-isFail(_G380)->_G381=_G380,_G382=_G383;user:hnf(_G379,_G400,_G382,_G402),prim_hnf_apply(_G400,_G380,_G381,_G402,_G383).
prim_seq(_G556,_G557,_G558,_G559,_G560):-freeze(_G559,blocked_prim_seq(_G556,_G557,_G558,_G559,_G560)).
blocked_prim_seq(_G595,_G596,_G597,_G598,_G599):-user:hnf(_G595,_G602,_G598,_G604),prim_seqHNF(_G602,_G596,_G597,_G604,_G599).
prim_seqHNF(_G764,_G765,_G766,_G767,_G768):-freeze(_G767,blocked_prim_seqHNF(_G764,_G765,_G766,_G767,_G768)).
blocked_prim_seqHNF(_G803,_G804,_G805,_G806,_G807):-isFail(_G803)->_G805=_G803,_G806=_G807;user:hnf(_G804,_G805,_G806,_G807).
prim_ensureNotFree(_G1000,_G1001,_G1002,_G1003):-freeze(_G1002,blocked_prim_ensureNotFree(_G1000,_G1001,_G1002,_G1003)).
blocked_prim_ensureNotFree(_G1034,_G1035,_G1036,_G1037):-user:hnf(_G1034,_G1040,_G1036,_G1042),prim_ensureNotFreeHNF(_G1040,_G1035,_G1042,_G1037).
prim_ensureNotFreeHNF(_G1226,_G1227,_G1228,_G1229):-freeze(_G1228,blocked_prim_ensureNotFreeHNF(_G1226,_G1227,_G1228,_G1229)).
blocked_prim_ensureNotFreeHNF(_G1260,_G1261,_G1262,_G1263):-isFail(_G1260)->_G1261=_G1260,_G1262=_G1263;prim_ensureHnfNotFree(_G1260,_G1261,_G1262,_G1263).
prim_ensureHnfNotFree(_G1478,_G1479,_G1480,_G1481):-freeze(_G1480,freeze(_G1478,blocked_prim_ensureHnfNotFree(_G1478,_G1479,_G1480,_G1481))).
blocked_prim_ensureHnfNotFree(_G241,_G241,_G243,_G243).
prim_error(_G269,_G270):-string2Atom(_G269,_G273),raise_exception(_G273).
prim_failed(_G412,_G413,_G414):-freeze(_G413,blocked_prim_failed(_G412,_G413,_G414)).
blocked_prim_failed(_G441,_G442,_G443):-prim_failure(partcall(0,'Prelude.failed',[]),[],_G441,_G442,_G443).
prim_failure(_G596,_G597,_G598,_G599,_G600):-freeze(_G599,blocked_prim_failure(_G596,_G597,_G598,_G599,_G600)).
blocked_prim_failure(_G635,_G636,_G637,_G638,_G639):-printConsFailure(no),!,fail.
blocked_prim_failure(_G682,_G683,_G684,_G685,_G686):-hasPrintedFailure,!,fail.
blocked_prim_failure(_G727,_G728,_G729,_G730,_G731):-_G729='FAIL'([_G727,_G728]),_G730=_G731.
prim_compare(_G899,_G900,_G901,_G902,_G903):-freeze(_G902,blocked_prim_compare(_G899,_G900,_G901,_G902,_G903)).
blocked_prim_compare(_G938,_G939,_G940,_G941,_G942):-user:hnf(_G938,_G945,_G941,_G947),user:hnf(_G939,_G953,_G947,_G955),prim_compareHNF(_G945,_G953,_G940,_G955,_G942).
prim_compareHNF(_G1172,_G1173,_G1174,_G1175,_G1176):-freeze(_G1175,freeze(_G1173,freeze(_G1172,blocked_prim_compareHNF(_G1172,_G1173,_G1174,_G1175,_G1176)))).
blocked_prim_compareHNF('FAIL'(_G1217),_G1222,'FAIL'(_G1217),_G1224,_G1224):-!.
blocked_prim_compareHNF(_G1264,'FAIL'(_G1260),'FAIL'(_G1260),_G1267,_G1267):-!.
blocked_prim_compareHNF(_G1303,_G1304,_G1305,_G1306,_G1307):-number(_G1303),!, (_G1303=_G1304->_G1305='Prelude.EQ';_G1303<_G1304->_G1305='Prelude.LT';_G1305='Prelude.GT'),_G1306=_G1307.
blocked_prim_compareHNF(_G1383,_G1384,_G1385,_G1386,_G1387):-isCharCons(_G1383),!,char_int(_G1383,_G1392),char_int(_G1384,_G1395), (_G1392=_G1395->_G1385='Prelude.EQ';_G1392<_G1395->_G1385='Prelude.LT';_G1385='Prelude.GT'),_G1386=_G1387.
blocked_prim_compareHNF(_G265,_G266,_G267,_G268,_G269):-functor(_G265,_G272,_G273),functor(_G266,_G276,_G277),user:constructortype(_G272,_G280,_G273,_G282,_G283,_G284),user:constructortype(_G276,_G290,_G277,_G292,_G293,_G294),!, (_G283<_G293->_G267='Prelude.LT',_G268=_G269;_G283>_G293->_G267='Prelude.GT',_G268=_G269;prim_compareArgs(1,_G273,_G265,_G266,_G267,_G268,_G269)).
prim_compareArgs(_G521,_G522,_G523,_G524,_G525,_G526,_G527):-freeze(_G526,blocked_prim_compareArgs(_G521,_G522,_G523,_G524,_G525,_G526,_G527)).
blocked_prim_compareArgs(_G570,_G571,_G572,_G573,_G574,_G575,_G576):-_G570>_G571,!,_G574='Prelude.EQ',_G575=_G576.
blocked_prim_compareArgs(_G637,_G638,_G639,_G640,_G641,_G642,_G643):-arg(_G637,_G639,_G647),arg(_G637,_G640,_G651),prim_compare(_G647,_G651,_G655,_G642,_G657), (_G655='Prelude.EQ'->_G665 is _G637+1,prim_compareArgs(_G665,_G638,_G639,_G640,_G641,_G657,_G643);_G641=_G655,_G657=_G643).
prim_try(_G847,_G848,_G849,_G850):-freeze(_G849,blocked_prim_try(_G847,_G848,_G849,_G850)).
blocked_prim_try(_G881,_G882,_G883,_G883):-raise_exception('Prelude.try not yet implemented!').
prim_findall(_G1028,_G1029,_G1030,_G1031):-freeze(_G1030,blocked_prim_findall(_G1028,_G1029,_G1030,_G1031)).
blocked_prim_findall(_G1062,_G1063,_G1064,_G1065):-hnfAndWaitUntilGround(_G1062,_G1068,_G1064,_G1070),prim_findall_exec(_G1068,_G1063,_G1070,_G1065).
prim_findall_exec(_G1235,_G1236,_G1237,_G1238):-freeze(_G1237,blocked_prim_findall_exec(_G1235,_G1236,_G1237,_G1238)).
blocked_prim_findall_exec(_G1269,_G1270,_G1271,_G1272):-hasPrintedFailure->findall(_G1275,prim_apply(_G1269,_G1275,'Prelude.success',_G1271,_G1278),_G1270),_G1271=_G1272;asserta(hasPrintedFailure),findall(_G1275,prim_apply(_G1269,_G1275,'Prelude.success',_G1271,_G1299),_G1270),retract(hasPrintedFailure),_G1271=_G1272.
waitUntilGround(_G1489,_G1490,_G1491):-freeze(_G1490,freeze(_G1489,blocked_waitUntilGround(_G1489,_G1490,_G1491))).
blocked_waitUntilGround(share(_G295),_G298,_G299):-!,get_mutable(_G301,_G295), (_G301='$eval'(_G304)->true;_G304=_G301),waitUntilGround(_G304,_G298,_G299).
blocked_waitUntilGround(_G356,_G357,_G358):-functor(_G356,_G361,_G362),waitUntilGroundArgs(1,_G362,_G356,_G357,_G358).
waitUntilGroundArgs(_G531,_G532,_G533,_G534,_G535):-freeze(_G534,blocked_waitUntilGroundArgs(_G531,_G532,_G533,_G534,_G535)).
blocked_waitUntilGroundArgs(_G570,_G571,_G572,_G573,_G574):-_G570>_G571,!,_G573=_G574.
blocked_waitUntilGroundArgs(_G621,_G622,_G623,_G624,_G625):-arg(_G621,_G623,_G629),waitUntilGround(_G629,_G624,_G633),_G638 is _G621+1,waitUntilGroundArgs(_G638,_G622,_G623,_G633,_G625).
prim_findfirst(_G806,_G807,_G808,_G809):-freeze(_G808,blocked_prim_findfirst(_G806,_G807,_G808,_G809)).
blocked_prim_findfirst(_G840,_G841,_G842,_G843):-hnfAndWaitUntilGround(_G840,_G846,_G842,_G848),prim_findfirst_exec(_G846,_G841,_G848,_G843).
prim_findfirst_exec(_G1019,_G1020,_G1021,_G1022):-freeze(_G1021,blocked_prim_findfirst_exec(_G1019,_G1020,_G1021,_G1022)).
blocked_prim_findfirst_exec(_G1053,_G1054,_G1055,_G1056):-hasPrintedFailure->prim_findfirstWithPF(_G1053,_G1054,_G1055),_G1055=_G1056;asserta(hasPrintedFailure),prim_findfirstWithoutPF(_G1053,_G1054,_G1055),_G1055=_G1056.
prim_findfirstWithPF(_G1118,_G1119,_G1120):-prim_apply(_G1118,_G1123,'Prelude.success',_G1120,_G1126),!,_G1119=_G1123.
prim_findfirstWithoutPF(_G1162,_G1163,_G1164):-prim_apply(_G1162,_G1167,'Prelude.success',_G1164,_G1170),retract(hasPrintedFailure),!,_G1163=_G1167.
prim_findfirstWithoutPF(_G1211,_G1212,_G1213):-retract(hasPrintedFailure),fail.
prim_getOneSolution(_G1377,_G1378,_G1379,_G1380):-freeze(_G1379,blocked_prim_getOneSolution(_G1377,_G1378,_G1379,_G1380)).
blocked_prim_getOneSolution(_G1411,partcall(1,prim_getOneSolutionWorld,[_G1411]),_G1420,_G1420).
prim_getOneSolutionWorld(_G448,_G449,_G450,_G451,_G452):-freeze(_G451,blocked_prim_getOneSolutionWorld(_G448,_G449,_G450,_G451,_G452)).
blocked_prim_getOneSolutionWorld(_G487,_G488,_G489,_G490,_G491):-hnfAndWaitUntilGround(_G487,_G494,_G490,_G496),prim_getOneSol_exec(_G494,_G489,_G496,_G491).
prim_getOneSol_exec(_G671,_G672,_G673,_G674):-freeze(_G673,blocked_prim_getOneSol_exec(_G671,_G672,_G673,_G674)).
blocked_prim_getOneSol_exec(_G705,_G706,_G707,_G708):-hasPrintedFailure->prim_getOneSolWithPF(_G705,_G706,_G707,_G708);asserta(hasPrintedFailure),prim_getOneSolWithoutPF(_G705,_G706,_G707,_G708).
prim_getOneSolWithPF(_G760,_G761,_G762,_G763):-prim_apply(_G760,_G766,'Prelude.success',_G762,_G769),!,_G761='$io'('Prelude.Just'(_G766)),_G769=_G763.
prim_getOneSolWithPF(_G821,'$io'('Prelude.Nothing'),_G823,_G823).
prim_getOneSolWithoutPF(_G849,_G850,_G851,_G852):-prim_apply(_G849,_G855,'Prelude.success',_G851,_G858),retract(hasPrintedFailure),!,_G850='$io'('Prelude.Just'(_G855)),_G858=_G852.
prim_getOneSolWithoutPF(_G915,'$io'('Prelude.Nothing'),_G917,_G918):-retract(hasPrintedFailure),_G917=_G918.
hnfAndWaitUntilGround(_G1095,_G1096,_G1097,_G1098):-freeze(_G1097,blocked_hnfAndWaitUntilGround(_G1095,_G1096,_G1097,_G1098)).
blocked_hnfAndWaitUntilGround(_G1129,_G1130,_G1131,_G1132):-user:hnf(_G1129,_G1130,_G1131,_G1137),hnfAndWaitUntilGroundHNF(_G1130,_G1137,_G1132).
hnfAndWaitUntilGroundHNF(_G1324,_G1325,_G1326):-freeze(_G1325,blocked_hnfAndWaitUntilGroundHNF(_G1324,_G1325,_G1326)).
blocked_hnfAndWaitUntilGroundHNF(_G1353,_G1354,_G1355):-isFail(_G1353)->_G1354=_G1355;waitUntilGround(_G1353,_G1354,_G1355).
unifEq(_G349,_G350,_G351,_G352,_G353):-freeze(_G352,blocked_unifEq(_G349,_G350,_G351,_G352,_G353)).
blocked_unifEq(_G367,_G368,_G369,_G370,_G371):-user:hnf(_G367,_G374,_G370,_G376),unifEq1(_G374,_G368,_G369,_G376,_G371).
unifEq1(_G520,_G521,_G522,_G523,_G524):-freeze(_G523,blocked_unifEq1(_G520,_G521,_G522,_G523,_G524)).
blocked_unifEq1(_G559,_G560,'Prelude.success',_G562,_G563):-var(_G559),!,user:occursNot(_G559,_G560),makeShare(_G560,_G559),_G562=_G563.
blocked_unifEq1('FAIL'(_G624),_G629,'FAIL'(_G624),_G631,_G631):-!.
blocked_unifEq1(_G667,_G668,_G669,_G670,_G671):-replaceMultipleVariables(_G667,_G674,_G675),user:hnf(_G668,_G678,_G670,_G680),unifEqHnf(_G674,_G678,_G687,_G680,_G689),unifEq2(_G687,_G675,_G669,_G689,_G671).
unifEq2(_G836,_G837,_G838,_G839,_G840):-freeze(_G839,blocked_unifEq2(_G836,_G837,_G838,_G839,_G840)).
blocked_unifEq2(_G875,_G876,_G877,_G878,_G879):-isFail(_G875)->_G877=_G875,_G878=_G879;user:hnf(_G876,_G877,_G878,_G879).
replaceMultipleVariables(_G939,_G940,_G941):-_G939=..[_G943|_G944],replaceMultipleVariablesInArgs(_G944,inConstructorCall,_G951,_G952),_G940=..[_G943|_G952],getSEqConstraints(_G951,_G941).
getControlVar(_G997,_G998,_G999,_G1000):-var(_G999),!,_G999=[control(_G997,_G998,_G1000,_G1007)|_G1010].
getControlVar(_G1058,_G1059,[control(_G1050,_G1051,_G1052,_G1053)|_G1056],_G1061):-_G1058==_G1050,!, (_G1059=inConstructorCall,_G1051=inConstructorCall-> (var(_G1053)->_G1053='Prelude.=:='(_G1058,_G1052),_G1061=_G1058;_G1061=_G1052);_G1061=_G1052, (var(_G1053)->_G1052='Prelude.&>'('Prelude.ifVar'(_G1106,'Prelude.=:='(_G1106,'Prelude.()'),'Prelude.=:='(_G1109,'Prelude.()')),_G1058),_G1053='Prelude.ifVar'(_G1109,'Prelude.success','Prelude.=:='(_G1058,_G1058));true)).
getControlVar(_G1185,_G1186,[_G1182|_G1183],_G1188):-getControlVar(_G1185,_G1186,_G1183,_G1188).
getSEqConstraints(_G1224,'Prelude.success'):-var(_G1224),!,_G1224=[].
getSEqConstraints([control(_G1259,_G1260,_G1261,_G1262)|_G1265],_G1268):-var(_G1262),!,_G1259=_G1261,getSEqConstraints(_G1265,_G1268).
getSEqConstraints([control(_G1308,_G1309,_G1310,_G1311)|_G1314],'Prelude.&'(_G1311,_G1317)):-getSEqConstraints(_G1314,_G1317).
replaceMultipleVariablesInArgs([],_G1347,_G1348,[]).
replaceMultipleVariablesInArgs([_G1374|_G1375],_G1381,_G1382,[_G1377|_G1378]):-var(_G1374),!,getControlVar(_G1374,_G1381,_G1382,_G1377),replaceMultipleVariablesInArgs(_G1375,_G1381,_G1382,_G1378).
replaceMultipleVariablesInArgs([_G361|_G362],_G366,_G367,[_G364|_G365]):-_G361=..[_G369|_G370], (user:functiontype(_G369,_G376,_G377,_G378,_G379,_G380)->_G385=inFunctionCall;_G385=_G366),replaceMultipleVariablesInArgs(_G370,_G385,_G367,_G400),_G364=..[_G369|_G400],replaceMultipleVariablesInArgs(_G362,_G366,_G367,_G365).
unifEqHnf(_G536,_G537,_G538,_G539,_G540):-freeze(_G539,blocked_unifEqHnf(_G536,_G537,_G538,_G539,_G540)).
blocked_unifEqHnf(_G575,_G576,_G577,_G578,_G579):-var(_G576),!,user:bind(_G576,_G575,_G577,_G578,_G579).
blocked_unifEqHnf(_G635,'FAIL'(_G631),'FAIL'(_G631),_G638,_G638):-!.
blocked_unifEqHnf(_G674,_G675,_G676,_G677,_G678):-number(_G674),!, (_G674=_G675->_G676='Prelude.success',_G677=_G678;prim_failure(partcall(2,'Prelude.=:<=',[]),[_G674,_G675],_G676,_G677,_G678)).
blocked_unifEqHnf(_G755,_G756,_G757,_G758,_G759):-functor(_G755,_G762,_G763),functor(_G756,_G766,_G767),_G762==_G766,_G763==_G767,!,genUnifEqHnfBody(1,_G763,_G755,_G756,_G779),user:hnf(_G779,_G757,_G758,_G759).
blocked_unifEqHnf(_G840,_G841,_G842,_G843,_G844):-prim_failure(partcall(2,'Prelude.=:<=',[]),[_G840,_G841],_G842,_G843,_G844).
genUnifEqHnfBody(_G895,_G896,_G897,_G898,'Prelude.success'):-_G895>_G896,!.
genUnifEqHnfBody(_G943,_G944,_G945,_G946,'Prelude.=:<='(_G940,_G941)):-_G943=_G944,!,arg(_G943,_G945,_G940),arg(_G943,_G946,_G941).
genUnifEqHnfBody(_G1008,_G1009,_G1010,_G1011,'Prelude.&'('Prelude.=:<='(_G1002,_G1003),_G1006)):-arg(_G1008,_G1010,_G1002),arg(_G1008,_G1011,_G1003),_G1025 is _G1008+1,genUnifEqHnfBody(_G1025,_G1009,_G1010,_G1011,_G1006).
unifEqLinear(_G1188,_G1189,_G1190,_G1191,_G1192):-freeze(_G1191,blocked_unifEqLinear(_G1188,_G1189,_G1190,_G1191,_G1192)).
blocked_unifEqLinear(_G1227,_G1228,_G1229,_G1230,_G1231):-user:hnf(_G1227,_G1234,_G1230,_G1236),unifEqLinear1(_G1234,_G1228,_G1229,_G1236,_G1231).
unifEqLinear1(_G1398,_G1399,_G1400,_G1401,_G1402):-freeze(_G1401,blocked_unifEqLinear1(_G1398,_G1399,_G1400,_G1401,_G1402)).
blocked_unifEqLinear1(_G1437,_G1438,'Prelude.success',_G1440,_G1441):-var(_G1437),!,makeShare(_G1438,_G1437),_G1440=_G1441.
blocked_unifEqLinear1('FAIL'(_G379),_G382,'FAIL'(_G379),_G383,_G383):-!.
blocked_unifEqLinear1(_G412,_G413,_G414,_G415,_G416):-user:hnf(_G413,_G419,_G415,_G421),unifEqLinearHnf(_G412,_G419,_G414,_G421,_G416).
unifEqLinearHnf(_G589,_G590,_G591,_G592,_G593):-freeze(_G592,blocked_unifEqLinearHnf(_G589,_G590,_G591,_G592,_G593)).
blocked_unifEqLinearHnf(_G628,_G629,_G630,_G631,_G632):-var(_G629),!,user:nf(_G628,_G637,_G631,_G639),freeze(_G639, (isFail(_G637)->_G630=_G637,_G639=_G632;_G629=_G637,_G630='Prelude.success',_G639=_G632)).
blocked_unifEqLinearHnf(_G725,'FAIL'(_G721),'FAIL'(_G721),_G728,_G728):-!.
blocked_unifEqLinearHnf(_G764,_G765,_G766,_G767,_G768):-number(_G764),!, (_G764=_G765->_G766='Prelude.success',_G767=_G768;prim_failure(partcall(2,'Prelude.=:<<=',[]),[_G764,_G765],_G766,_G767,_G768)).
blocked_unifEqLinearHnf(_G845,_G846,_G847,_G848,_G849):-functor(_G845,_G852,_G853),functor(_G846,_G856,_G857),_G852==_G856,_G853==_G857,!,genUnifEqLinearHnfBody(1,_G853,_G845,_G846,_G869),user:hnf(_G869,_G847,_G848,_G849).
blocked_unifEqLinearHnf(_G930,_G931,_G932,_G933,_G934):-prim_failure(partcall(2,'Prelude.=:<<=',[]),[_G930,_G931],_G932,_G933,_G934).
genUnifEqLinearHnfBody(_G985,_G986,_G987,_G988,'Prelude.success'):-_G985>_G986,!.
genUnifEqLinearHnfBody(_G1033,_G1034,_G1035,_G1036,'Prelude.=:<<='(_G1030,_G1031)):-_G1033=_G1034,!,arg(_G1033,_G1035,_G1030),arg(_G1033,_G1036,_G1031).
genUnifEqLinearHnfBody(_G1098,_G1099,_G1100,_G1101,'Prelude.&'('Prelude.=:<<='(_G1092,_G1093),_G1096)):-arg(_G1098,_G1100,_G1092),arg(_G1098,_G1101,_G1093),_G1115 is _G1098+1,genUnifEqLinearHnfBody(_G1115,_G1099,_G1100,_G1101,_G1096).
prim_ifVar(_G1277,_G1278,_G1279,_G1280,_G1281,_G1282):-freeze(_G1281,blocked_prim_ifVar(_G1277,_G1278,_G1279,_G1280,_G1281,_G1282)).
blocked_prim_ifVar(_G1321,_G1322,_G1323,_G1324,_G1325,_G1326):-user:derefRoot(_G1321,_G1329), (var(_G1329)->user:hnf(_G1322,_G1324,_G1325,_G1326);user:hnf(_G1323,_G1324,_G1325,_G1326)).
