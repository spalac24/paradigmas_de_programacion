:-module(prim_dynamic,[prim_assertFact/2,prim_retractFact/2,prim_getDynamicKnowledge/1,prim_abortTransaction/1,prim_commitTransaction/2,prim_startTransaction/1,prim_isKnownAtTime/3,abortTransaction/0,initializeDynamic/0]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
:-use_module('../version').
:-use_module('../pakcsversion').
:-dynamic dynamicTime/1,dynamicVersion/3,insideTransaction/0.
dynamicTime(1).
setDynamicVersion(_G129,_G126/_G127,_G131):- (retract(dynamicVersion(_G129,_G126/_G127,_G138))->true;true),asserta(dynamicVersion(_G129,_G126/_G127,_G131)).
prim_assertFact('Dynamic.Dynamic'(_G185),'Prelude.()'):-retract(dynamicTime(_G190)),_G197 is _G190+1,assertz(dynamicTime(_G197)),_G185=..[_G204,_G207,_G210,_G213|_G214],_G228=..[_G204,_G197,0|_G214],assertz(user:_G228),logDynamicEvent(_G207,assertz,_G228).
prim_retractFact('Dynamic.Dynamic'(_G279),_G282):-_G279=..[_G284,_G287,_G290,_G293|_G294],_G308=..[_G284,_G302,_G305|_G294],reloadNewestDynamicVersionOf(_G284),retract(dynamicTime(_G313)),_G320 is _G313+1,assertz(dynamicTime(_G320)),call(user:_G308),isInTimeInterval(_G313,_G302,_G305),!,_G345=..[_G284,_G302,_G313|_G294],retract(user:_G308),!,logDynamicEvent(_G287,retract,_G308),assertz(user:_G345),_G282='Prelude.True'.
prim_retractFact(_G428,'Prelude.False').
prim_getDynamicKnowledge(_G446):-reloadNewestDynamicVersions,dynamicTime(_G448),term2partcall('Dynamic.isKnownAtTime'(_G448),1,_G446), (compileWithSharing(function)->makeShare(_G446,_G459);_G459=_G446).
prim_isKnownAtTime(_G3,'Dynamic.Dynamic'(_G1),'Prelude.success'):-_G1=..[_G7,_G10,_G13,_G16|_G17],_G31=..[_G7,_G25,_G28|_G17],call(user:_G31),isInTimeInterval(_G3,_G25,_G28).
isInTimeInterval(_G77,_G78,_G79):-_G77>=_G78, (_G79=0->true;_G77=<_G79).
logDynamicEvent('Dynamic.Temporary',_G125,_G126):-!.
logDynamicEvent('Dynamic.Persistent',_G154,_G155):-_G155=..[_G157,_G160,_G163|_G164],_G178=..[_G157,0,0|_G164],_G187=..[_G154,_G178],user:dynamicPredInfo(_G157/_G191,_G194),logDynamic(_G187,_G194).
logDynamic(_G239,_G240):-dir2tmplockfile(_G240,_G243),lockWithFileIfNotInTransaction(_G243),executeWithFinalize((dir2logfile(_G240,_G248),open(_G248,append,_G252),writeq(_G252,_G239),put_code(_G252,46),nl(_G252),flush_output(_G252),close(_G252),addChangePid(_G240)),unlockWithFileIfNotInTransaction(_G243)).
executeWithFinalize(_G321,_G322):-on_exception(_G326,_G321, (call(_G322),printError(_G326),!,fail)),!,call(_G322).
executeWithFinalize(_G370,_G371):-call(_G371),!,fail.
prim_startTransaction(_G402):- (insideTransaction->writeErr('ERROR: nested transactions not supported!'),nlErr,!,fail;true),assert(insideTransaction),on_exception(_G433,initTransaction(_G402), (unlockAllPersistentDynamics, (retract(insideTransation)->true;true),printError(_G433),fail)),!.
initTransaction(_G1):-lockAllPersistentDynamics,reloadNewestDynamicVersions,retract(dynamicTime(_G3)),_G1 is _G3+1,assertz(dynamicTime(_G1)),logAllDynamicPreds(startTransaction(_G1)).
prim_commitTransaction(_G53,'Prelude.()'):-logAllDynamicPreds(commitTransaction(_G53)),unlockAllPersistentDynamics, (retract(insideTransaction)->true;true).
prim_abortTransaction(_G95):-logAllDynamicPreds(abortTransaction),getDynamicDirs(_G99),map1partialM(prim_dynamic:addChangePid(0),_G99),unlockAllPersistentDynamics, (retract(insideTransaction)->true;true),!,fail.
getDynamicDirs(_G152):-findall(_G154,isDynamicDir(_G154),_G158),sort(_G158,_G152).
isDynamicDir(_G183):-user:dynamicPredInfo(_G185,_G183),\+_G183=''.
logAllDynamicPreds(_G216):-getDynamicDirs(_G218),map1partialM(prim_dynamic:logDynamic(_G216),_G218).
initializeDynamic:-user:dynamicPredInfo(_G248/_G249,_G252),initializeDynamic(_G252,_G248/_G249),fail.
initializeDynamic.
initializeDynamic('',_G287/_G288):-!,retractAllFacts(_G287/_G288).
initializeDynamic(_G325,_G322/_G323):-existsDirectory(_G325),!,checkDynSpec(_G325,_G322/_G323),setDynamicVersion(_G325,_G322/_G323,0),!.
initializeDynamic(_G376,_G377):-existsFile(_G376),!,appendAtom('ERROR: Directory "',_G376,_G383),appendAtom(_G383,'" for dynamic entries cannot be created!',_G387),writeErr(_G387),nlErr,!,fail.
initializeDynamic(_G436,_G433/_G434):-upDirectory(_G436,_G440), (existsDirectory(_G440)->createDynamicDir(_G436,_G433/_G434),setDynamicVersion(_G436,_G433/_G434,1);writeErr('>>> Warning: persistent dynamic data directory '),writeErr(_G436),writeErr(' not accessible!'),nlErr).
upDirectory('.',..):-!.
upDirectory(_G25,_G26):-atom_codes(_G25,_G29),append(_G34,[47|_G32],_G29),\+member(47,_G32),!,atom_codes(_G26,_G34).
upDirectory(_G79,'.').
checkDynSpec(_G100,_G97/_G98):-dir2specfile(_G100,_G104),existsFile(_G104),readPrologTermFile(_G104,_G109),dynamicSpec(_G97/_G98,_G109),!.
checkDynSpec(_G150,_G151):-appendAtom('ERROR: Directory "',_G150,_G155),appendAtom(_G155,'" contains no matching dynamic entries!',_G159),writeErr(_G159),nlErr,!,fail.
replayLogFile(_G202,_G199/_G200):-dir2tmplockfile(_G202,_G206),lockWithFileIfNotInTransaction(_G206),executeWithFinalize(replayLogFileWhenAlreadyLocked(_G202,_G199/_G200),unlockWithFileIfNotInTransaction(_G206)).
:-dynamic replayStatus/1.
replayLogFileWhenAlreadyLocked(_G260,_G257/_G258):- (pakcsrc(dynamicmessages,yes)->writeErr('>>> Restoring persistent dynamic data in directory \''),writeErr(_G260),writeErr('\'...'),nlErr;true),getRunTime(_G287),ensurePakcsVersion(_G260),dir2dbfile(_G260,_G292),dir2pofile(_G260,_G295),consultPrologorPOFile(_G292,_G295),dir2logfile(_G260,_G301),open(_G301,read,_G305), (atEndOfStream(_G305)->close(_G305);assert(replayStatus(unchanged)),repeat,on_exception(_G320,readAndReplay(_G305), ((printError(_G320)->true;true),writeErr('ERROR during reading of '),writeErr(_G301),writeErr(' (restored state might be incomplete)'),nlErr)),close(_G305),retract(replayStatus(_G352)),writeNewDatabaseIfChanged(_G257/_G258,_G260,_G352)),updateDynFactsIfNecessary(_G260,_G257/_G258),readDynamicVersion(_G260,_G388),setDynamicVersion(_G260,_G257/_G258,_G388),writeChangePids(_G260,[]), (pakcsrc(dynamicmessages,yes)->writeErr('>>> ...restored in '),getRunTime(_G405),_G410 is _G405-_G287,writeErr(_G410),writeErr(' msec'),nlErr;true),!.
ensurePakcsVersion(_G1):-readPAKCSVersion(_G1,_G4),_G4=pakcsVersion(_G6,_G7),prologMajor(_G12),_G12\=_G7,dir2pofile(_G1,_G18), (existsFile(_G18)->deleteFile(_G18);true),writePAKCSVersion(_G1),!.
ensurePakcsVersion(_G70).
isCurrentPakcsVersion(_G83):-readPAKCSVersion(_G83,_G86),compilerVersion(_G88),!, (_G86=compilerVersion(_G88)->writePAKCSVersion(_G83);_G86=pakcsVersion(_G88,_G101)).
readPAKCSVersion(_G135,_G136):-dir2pakcsversionfile(_G135,_G139),existsFile(_G139),readPrologTermFile(_G139,_G136).
writePAKCSVersion(_G173):-dir2pakcsversionfile(_G173,_G176),compilerVersion(_G178),prologMajor(_G180),writePrologTermFile(_G176,pakcsVersion(_G178,_G180)).
updateDynFactsIfNecessary(_G214,_G215):-isCurrentPakcsVersion(_G214),!.
updateDynFactsIfNecessary(_G246,_G243/_G244):-compilerVersion(_G249), (pakcsrc(dynamicmessages,yes)->writeErr('>>> Updating persistent dynamic data in directory \''),writeErr(_G246),writeErr('\' to compiler version '),writeErr(_G249),writeErr(...),nlErr;true),updateDynFacts(_G243/_G244),writeNewDatabaseIfChanged(_G243/_G244,_G246,changed),writePAKCSVersion(_G246).
updateDynFactsIfNecessary(_G332,_G333).
updateDynFacts(_G350/_G351):-functor(_G355,_G350,_G351),functor(_G359,_G350,_G351),retract(user:_G355),arg(1,_G355,_G370),arg(1,_G359,_G370),arg(2,_G355,_G378),arg(2,_G359,_G378),translateDynFactArgs(3,_G351,_G355,_G359),asserta(user:_G359),fail.
updateDynFacts(_G438).
translateDynFactArgs(_G451,_G452,_G453,_G454):-_G451>_G452,!.
translateDynFactArgs(_G1,_G2,_G3,_G4):-arg(_G1,_G3,_G8),translateDynFactArg(_G8,_G11),arg(_G1,_G4,_G11),_G20 is _G1+1,translateDynFactArgs(_G20,_G2,_G3,_G4).
translateDynFactArg(_G69,_G69):-var(_G69),!.
translateDynFactArg(_G98,_G98):-number(_G98),!.
translateDynFactArg('True','Prelude.True'):-!.
translateDynFactArg('False','Prelude.False'):-!.
translateDynFactArg('False','Prelude.False'):-!.
translateDynFactArg('Left','Prelude.Left'):-!.
translateDynFactArg('Right','Prelude.Right'):-!.
translateDynFactArg('Nothing','Prelude.Nothing'):-!.
translateDynFactArg('Just','Prelude.Just'):-!.
translateDynFactArg(success,'Prelude.success'):-!.
translateDynFactArg('LT','Prelude.LT'):-!.
translateDynFactArg('GT','Prelude.GT'):-!.
translateDynFactArg('EQ','Prelude.EQ'):-!.
translateDynFactArg(_G391,_G392):-atomic(_G391),atom_codes(_G391,[40|_G397]),!,append([80,114,101,108,117,100,101,46,40],_G397,_G431),atom_codes(_G392,_G431).
translateDynFactArg(_G1,_G2):-atom(_G1),atom_codes(_G1,[39|_G7]),!, (_G7=[_G12]->char_int(_G2,_G12);_G7=[78,85,76]->char_int(_G2,0);writeErr('INTERNAL ERROR in translateDynFactArg: unkown char'),nlErr).
translateDynFactArg(_G83,_G83):-atomic(_G83),!.
translateDynFactArg(_G112,_G113):-functor(_G112,_G116,_G117),translateDynFactArg(_G116,_G120),functor(_G113,_G120,_G117),translateDynFactArgs(1,_G117,_G112,_G113).
writeNewDatabaseIfChanged(_G161/_G162,_G165,changed):-!,dir2dbfile(_G165,_G169),dir2pofile(_G165,_G172),dir2logfile(_G165,_G175),appendAtom(_G169,'.bak',_G179),tell(_G179),writeq((:-dynamic _G161/_G162)),put_code(46),nl,listing(user:_G161/_G162),told,try_save_predicates(_G161/_G162,_G172),renameFile(_G179,_G169),appendAtom(_G175,'.bak',_G213),renameFile(_G175,_G213),appendAtom('touch ',_G175,_G220),shellCmd(_G220),readDynamicVersion(_G165,_G225),_G230 is _G225+1,writeDynamicVersion(_G165,_G230), (pakcsrc(dynamicmessages,yes)->writeErr('>>> ...new database constructed by merging old database and log file '),nlErr;true).
writeNewDatabaseIfChanged(_G335/_G336,_G339,_G340):-dir2logfile(_G339,_G343),appendAtom(_G343,'.bak',_G347),renameFile(_G343,_G347),appendAtom('touch ',_G343,_G354),shellCmd(_G354).
readAndReplay(_G395):-atEndOfStream(_G395),!.
readAndReplay(_G419):-read(_G419,_G422),replayEvent(_G419,_G422).
replayEvent(_G447,end_of_file):-!.
replayEvent(_G2,startTransaction(_G1)):-readAndReplayTransaction(_G2,_G1,[]),!,fail.
replayEvent(_G20,_G21):-retract(replayStatus(_G23)),assert(replayStatus(changed)),call(user:_G21),!,fail.
readAndReplayTransaction(_G69,_G70,_G71):-atEndOfStream(_G69),!, (pakcsrc(dynamicmessages,yes)->writeErr('>>> Warning: ignoring incomplete transaction '),writeErr(_G70),nlErr;true).
readAndReplayTransaction(_G125,_G126,_G127):-read(_G125,_G130), (_G130=commitTransaction(_G126)->callRevList(_G127);_G130=abortTransaction->true;readAndReplayTransaction(_G125,_G126,[_G130|_G127])),!.
callRevList([]).
callRevList([_G205|_G206]):-callRevList(_G206),retract(replayStatus(_G212)),assert(replayStatus(changed)),call(user:_G205).
startTransaction(_G251).
commitTransaction(_G264).
abortTransaction.
lockAllPersistentDynamics:-getDynamicDirs(_G283),map1M(prim_dynamic:lockDynamicDir,_G283).
lockDynamicDir(_G306):-dir2lockfile(_G306,_G309),lockWithFile(_G309).
lockWithFileIfNotInTransaction(_G333):-insideTransaction->true;lockWithFile(_G333).
lockWithFile(_G360):-appendAtom('lockfile -1 ',_G360,_G364), (existsFile(_G360),pakcsrc(dynamicmessages,yes)->writeErr('>>> Waiting for removing lock file \''),writeErr(_G360),writeErr('\'...'),nlErr;true),shellCmd(_G364),!.
unlockAllPersistentDynamics:-getDynamicDirs(_G423),map1M(prim_dynamic:unlockDynamicDir,_G423).
unlockDynamicDir(_G446):-dir2lockfile(_G446,_G449),unlockWithFile(_G449).
unlockWithFileIfNotInTransaction(_G473):-insideTransaction->true;unlockWithFile(_G473).
unlockWithFile(_G1):- (existsFile(_G1)->deleteFile(_G1);true),!.
reloadNewestDynamicVersions:-user:dynamicPredInfo(_G33/_G34,_G37),dynamicVersion(_G37,_G33/_G34,_G47),reloadNewestDynamicVersion(_G37,_G33/_G34,_G47),fail.
reloadNewestDynamicVersions.
reloadNewestDynamicVersionOf(_G83):-dynamicVersion(_G88,_G83/_G86,_G90),!,reloadNewestDynamicVersion(_G88,_G83/_G86,_G90).
reloadNewestDynamicVersionOf(_G122).
reloadNewestDynamicVersion(_G138,_G135/_G136,0):-!,retractAllFacts(_G135/_G136),replayLogFile(_G138,_G135/_G136),!.
reloadNewestDynamicVersion(_G190,_G187/_G188,_G192):-readDynamicVersion(_G190,_G195),\+_G192=_G195,!,retractAllFacts(_G187/_G188),replayLogFile(_G190,_G187/_G188),!.
reloadNewestDynamicVersion(_G253,_G254,_G255):-readChangePids(_G253,_G258),currentPID(_G260), (_G258=[];_G258=[_G260]),!.
reloadNewestDynamicVersion(_G311,_G308/_G309,_G313):-retractAllFacts(_G308/_G309),replayLogFile(_G311,_G308/_G309).
createDynamicDir(_G357,_G354/_G355):- (pakcsrc(dynamicmessages,yes)->writeErr('>>> Creating new persistent dynamic data in directory \''),writeErr(_G357),writeErr('\'...'),nlErr;true),appendAtom('mkdir -p ',_G357,_G386),shellCmd(_G386),dir2logfile(_G357,_G391),appendAtom('touch ',_G391,_G395),shellCmd(_G395),dir2dbfile(_G357,_G400),appendAtom('touch ',_G400,_G404),shellCmd(_G404),dir2specfile(_G357,_G409),dynamicSpec(_G354/_G355,_G415),writePrologTermFile(_G409,_G415),writePAKCSVersion(_G357).
readPrologTermFile(_G1,_G2):-open(_G1,read,_G6),read(_G6,_G2),close(_G6).
writePrologTermFile(_G37,_G38):-open(_G37,write,_G42),writeq(_G42,_G38),put_code(_G42,46),nl(_G42),flush_output(_G42),close(_G42).
readDynamicVersion(_G92,_G93):-dir2versionfile(_G92,_G96),existsFile(_G96),!,on_exception(_G106,readPrologTermFile(_G96,_G93),_G93=0).
readDynamicVersion(_G140,1).
writeDynamicVersion(_G158,_G159):-dir2versionfile(_G158,_G162),writePrologTermFile(_G162,_G159).
readChangePids(_G191,_G192):-dir2changepidsfile(_G191,_G195),existsFile(_G195),!,on_exception(_G208,readPrologTermFile(_G195,_G192),_G192=[0]).
readChangePids(_G242,[]).
writeChangePids(_G260,_G261):-dir2changepidsfile(_G260,_G264),writePrologTermFile(_G264,_G261).
addChangePid(_G293):-currentPID(_G295),addChangePid(_G295,_G293).
addChangePid(_G320,_G321):-readChangePids(_G321,_G324), (member(_G320,_G324)->true;writeChangePids(_G321,[_G320|_G324])),!.
dynamicSpec(_G368/_G369,dynamicPredicate(_G368,_G369)).
retractDeadDynamicFacts(_G392/_G393):-length(_G397,_G393),_G403=..[_G392|_G397],_G397=[_G406,_G409|_G410],dynamicTime(_G415),call(user:_G403),_G409>0,_G415>_G409,retract(user:_G403),fail.
retractDeadDynamicFacts(_G474).
dir2logfile(_G1,_G2):-appendAtom(_G1,'/eventlog',_G2).
dir2lockfile(_G26,_G27):-appendAtom(_G26,'/LOCK',_G27).
dir2tmplockfile(_G54,_G55):-appendAtom(_G54,'/LOCK',_G55).
dir2dbfile(_G82,_G83):-appendAtom(_G82,'/database.pl',_G83).
dir2pofile(_G110,_G111):-appendAtom(_G110,'/dbpred.po',_G111).
dir2specfile(_G138,_G139):-appendAtom(_G138,'/spec',_G139).
dir2pakcsversionfile(_G166,_G167):-appendAtom(_G166,'/pakcsversion',_G167).
dir2versionfile(_G194,_G195):-appendAtom(_G194,'/version',_G195).
dir2changepidsfile(_G222,_G223):-appendAtom(_G222,'/changepids',_G223).
