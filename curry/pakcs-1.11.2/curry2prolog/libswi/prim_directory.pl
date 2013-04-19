:-module(prim_directory,[prim_doesFileExist/2,prim_doesDirectoryExist/2,prim_getModificationTime/2,prim_fileSize/2,prim_getCurrentDirectory/1,prim_setCurrentDirectory/2,prim_getDirectoryContents/2,prim_createDirectory/2,prim_removeFile/2,prim_removeDirectory/2,prim_renameFile/3,prim_renameDirectory/3]).
:-style_check(-singleton).
:-use_module('../prologbasics').
:-use_module('../basics').
prim_doesFileExist(_G98,_G99):-string2Atom(_G98,_G102), (existsFile(_G102)->_G99='Prelude.True';_G99='Prelude.False').
prim_doesDirectoryExist(_G142,_G143):-string2Atom(_G142,_G146), (existsDirectory(_G146)->_G143='Prelude.True';_G143='Prelude.False').
prim_getModificationTime(_G188,'Time.CTime'(_G186)):-string2Atom(_G188,_G192),fileModTime(_G192,_G186).
prim_fileSize(_G221,_G222):-string2Atom(_G221,_G225),fileSize(_G225,_G222).
prim_getCurrentDirectory(_G254):-workingDirectory(_G256),atom2String(_G256,_G254).
prim_setCurrentDirectory(_G281,'Prelude.()'):-string2Atom(_G281,_G285),setWorkingDirectory(_G285).
prim_getDirectoryContents(_G313,_G314):-string2Atom(_G313,_G317),directoryFiles(_G317,_G320),map2M(basics:atom2String,_G320,_G314).
prim_createDirectory(_G356,'Prelude.()'):-string2Atom(_G356,_G360),makeDirectory(_G360).
prim_removeFile(_G388,'Prelude.()'):-string2Atom(_G388,_G392),deleteFile(_G392).
prim_removeDirectory(_G420,'Prelude.()'):-string2Atom(_G420,_G424),deleteDirectory(_G424).
prim_renameFile(_G452,_G453,'Prelude.()'):-string2Atom(_G452,_G457),string2Atom(_G453,_G460),renameFile(_G457,_G460).
prim_renameDirectory(_G1,_G2,'Prelude.()'):-string2Atom(_G1,_G6),string2Atom(_G2,_G9),renameDirectory(_G6,_G9).
