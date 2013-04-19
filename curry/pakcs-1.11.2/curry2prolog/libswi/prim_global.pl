:-module(prim_global,[initGlobalValue/4,prim_readGlobal/2,prim_writeGlobal/3]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
:-use_module(prim_readshowterm).
:-ensure_loaded(user:prim_standard).
initGlobalValue(_G1,'Global.Temporary',_G3,_G4):-evalToken(_G6),user:nf(_G3,_G4,_G6,_G11),user:waitUntilGround(_G4,_G11,_G18),_G29=..[_G1,_G26],user:retractClause(_G29,_G33),_G44=..[_G1,_G4],assertz(user:_G44),!.
initGlobalValue(_G104,'Global.Persistent'(_G102),_G106,_G107):-evalToken(_G109),user:nf(_G102,_G112,_G109,_G114),user:waitUntilGround(_G112,_G114,_G121),string2Atom(_G112,_G107),user:nf(_G106,_G130,_G121,_G132),user:waitUntilGround(_G130,_G132,_G139),_G150=..[_G104,_G147],user:retractClause(_G150,_G154),_G165=..[_G104,_G107],assertz(user:_G165), (existsFile(_G107)->true;writeGlobalFile(_G107,_G130)),!.
prim_readGlobal('Global.GlobalDef'(_G246,'Global.Temporary'),_G250):-_G258=..[_G246,_G250],call(user:_G258),!.
prim_readGlobal('Global.GlobalDef'(_G293,'Global.Persistent'),_G297):-_G305=..[_G293,_G302],call(user:_G305),readGlobalFile(_G302,_G297),!.
prim_writeGlobal('Global.GlobalDef'(_G346,'Global.Temporary'),_G350,'Prelude.()'):-_G359=..[_G346,_G356], (retract(user:_G359);user:retractClause(_G359,_G368)),_G382=..[_G346,_G350],assertz(user:_G382),!.
prim_writeGlobal('Global.GlobalDef'(_G427,'Global.Persistent'),_G431,'Prelude.()'):-_G440=..[_G427,_G437],call(user:_G440),writeGlobalFile(_G437,_G431),!.
readGlobalFile(_G1,_G2):-lockFileName(_G1,_G5),lockWithFile(_G5),open(_G1,read,_G11),readStreamLine(_G11,_G14),readTerm(_G14,qualified,_G18,_G2),close(_G11),unlockWithFile(_G5).
writeGlobalFile(_G64,_G65):-lockFileName(_G64,_G68),lockWithFile(_G68), (existsFile(_G64)->appendAtom(_G64,'.bak',_G76),renameFile(_G64,_G76);true),open(_G64,write,_G92),show_term(_G65,qualified,_G96,[]),writeChars(_G92,_G96),put_code(_G92,10),put_code(_G92,10),put_code(_G92,10),put_code(_G92,10),close(_G92),unlockWithFile(_G68).
lockFileName(_G172,_G173):-appendAtom(_G172,'.LOCK',_G173).
lockWithFile(_G200):-appendAtom('lockfile -1 ',_G200,_G204), (existsFile(_G200),pakcsrc(dynamicmessages,yes)->writeErr('>>> Waiting for removing lock file \''),writeErr(_G200),writeErr('\'...'),nlErr;true),shellCmd(_G204),!.
unlockWithFile(_G263):-deleteFile(_G263).
