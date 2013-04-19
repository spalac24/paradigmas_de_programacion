:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
prim_profileReset('Prelude.()'):-checkPlProfiling,profile_reset([user:_G124]).
prim_profilePredicates(_G151,_G152):-checkPlProfiling,transSelection(_G151,_G155),profile_data([user:_G158],_G155,predicate,_G166),transPredData(_G166,_G152).
transPredData([],[]).
transPredData([_G219-0|_G223],_G226):-!,transPredData(_G223,_G226).
transPredData([ (user:_G255/_G256)-_G262|_G265],['Prelude.(,)'(_G267,_G262)|_G271]):-decodePrologName(_G255,_G277),atom2String(_G277,_G267),transPredData(_G265,_G271).
prim_profileClauses(_G312,_G313,_G314):-checkPlProfiling,transSelection(_G312,_G317),string2Atom(_G313,_G320),profile_data([user:_G320],_G317,clause,_G331),transClauseData(_G331,_G314).
transClauseData([],[]).
transClauseData([ (user:_G391/_G392/_G395)-_G401|_G404],['Prelude.(,)'(_G395,_G401)|_G410]):-transClauseData(_G404,_G410).
prim_profileView('Prelude.()'):-checkPlProfiling,use_module(library(gauge)),view([user:_G446]).
transSelection('PlProfileData.Calls',calls).
transSelection('PlProfileData.Backtracks',backtracks).
transSelection('PlProfileData.ChoicePoints',choice_points).
transSelection('PlProfileData.ExecTime',execution_time).
checkPlProfiling:-prolog_flag(compiling,_G56,_G56), (_G56=profiledcode->true;raise_exception('No in profiling mode, use ":set +plprofile"!')).
prim_getHnfDefinitions(_G85):-checkPlProfiling,lastload(_G87),atom_codes(_G89,_G87), (_G89=[]->write('ERROR: no program loaded for profiling'),nl,!,fail;true),prog2PrologFile(_G89,_G113),mainPrologFileName(_G113,_G116),see(_G116),repeat,read(_G120),isHnfClause(_G120),!,hnfClause2Info(_G120,_G125),hnfClauses2InfoList(2,_G128),seen,map2M(user:hnfInfo2Curry,[1/_G125|_G128],_G85).
hnfInfo2Curry(_G202/_G203,'Prelude.(,)'(_G202,_G206)):-atom2String(_G203,_G206).
hnfClauses2InfoList(_G235,[_G235/_G236|_G239]):-read(_G244),isHnfClause(_G244),!,hnfClause2Info(_G244,_G236),_G254 is _G235+1,hnfClauses2InfoList(_G254,_G239).
hnfClauses2InfoList(_G296,[]).
isHnfClause((:-hnf(_G314,_G315,_G316,_G317))):-!.
isHnfClause((hnf(_G340,_G341,_G342,_G343):-_G346)):-!.
isHnfClause(hnf(_G367,_G368,_G369,_G370)):-!.
hnfClause2Info((hnf(_G391,_G392,_G393,_G394):-_G397),'LOGVAR'):-var(_G391),!.
hnfClause2Info((hnf(_G428,_G429,_G430,_G431):-_G434),_G437):-!,functor(_G428,_G440,_G441),decodePrologName(_G440,_G437).
hnfClause2Info(hnf(_G473,_G474,_G475,_G476),'CONS').
