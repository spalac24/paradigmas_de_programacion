:-module(prim_system,[prim_getCPUTime/1,prim_getElapsedTime/1,prim_getArgs/1,prim_getEnviron/2,prim_getHostname/1,prim_getPID/1,prim_getProgName/1,prim_system/2,prim_exitWith/2,prim_sleep/2,isWindows/1]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
prim_getCPUTime(_G92):-getRunTime(_G92).
prim_getElapsedTime(_G113):-getElapsedTime(_G113).
prim_getArgs(_G134):- (rtargs(_G136)->true;getProgramArgs(_G136)),map2M(basics:atom2String,_G136,_G134).
prim_getEnviron(_G173,_G174):-string2Atom(_G173,_G177), (getEnv(_G177,_G180)->atom2String(_G180,_G174);_G174=[]).
prim_getHostname(_G218):-getHostname(_G220),atom2String(_G220,_G218).
prim_getPID(_G245):-currentPID(_G245).
prim_getProgName(_G266):-user:currentModule(_G268),atom2String(_G268,_G266).
prim_system(_G296,_G297):-string2Atom(_G296,_G300),shellCmd(_G300,_G297).
prim_exitWith(_G329,_G330):-halt(_G329).
prim_sleep(_G355,'Prelude.()'):-sleepSeconds(_G355).
isWindows(_G381):-getEnv('COMSPEC',_G384)->_G381='Prelude.True';_G381='Prelude.False'.
