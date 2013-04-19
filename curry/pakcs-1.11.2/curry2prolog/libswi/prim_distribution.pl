:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../version').
:-use_module('../pakcsversion').
prim_curryCompiler(_G112):-atom2String(pakcs,_G112).
prim_curryCompilerMajorVersion(_G134):-compilerMajorVersion(_G134).
prim_curryCompilerMinorVersion(_G155):-compilerMinorVersion(_G155).
prim_curryCompilerRevisionVersion(_G176):-compilerRevisionVersion(_G176).
prim_curryRuntime(_G197):-prolog(_G199),atom2String(_G199,_G197).
prim_curryRuntimeMajorVersion(_G224):-prologMajorVersion(_G224).
prim_curryRuntimeMinorVersion(_G245):-prologMinorVersion(_G245).
prim_installDir(_G266):-getEnv('PAKCSHOME',_G269)->atom2String(_G269,_G266);raise_exception('Distribution.installDir: cannot determine installation directory!').
