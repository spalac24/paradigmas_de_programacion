

prim_doesFileExist ::  List Char -> (Debug (IO Bool)) 
prim_doesFileExist = 
  ioFunc1 "prim_doesFileExist" (\ _ -> coerce0 (getNextExtVal::Debug P.Bool))

prim_doesDirectoryExist ::  List Char -> (Debug (IO Bool)) 
prim_doesDirectoryExist = 
  ioFunc1 "prim_doesDirectoryExist" (\_ -> coerce0 (getNextExtVal::Debug P.Bool))

prim_fileSize ::  List Char -> (Debug (IO Int)) 
prim_fileSize = 
  ioFunc1 "prim_fileSize" (\_ -> getNextExtVal)


prim_getModificationTime ::  List Char -> (Debug (IO ClockTime)) 
prim_getModificationTime = 
  ioFunc1 "prim_getModificationTime" (\_ -> getNextExtVal)

getCurrentDirectory ::  Debug (IO (List Char)) 
getCurrentDirectory = 
  ioFunc0 "getCurrentDirectory" (coerce0 (getNextExtVal::Debug [P.Char]))

prim_getDirectoryContents ::  List Char -> Debug (IO (List (List Char)))
prim_getDirectoryContents = 
  ioFunc1 "prim_getDirectoryContents" (\_ -> coerce0 (getNextExtVal::Debug [[P.Char]]))

prim_createDirectory ::  List Char -> (Debug (IO T1)) 
prim_createDirectory = 
  ioFunc1 "prim_createDirectory" (\_ -> P.return (coerce ()))

prim_removeFile ::  List Char -> (Debug (IO T1)) 
prim_removeFile = 
  ioFunc1 "prim_removeFile" (\_ -> P.return (coerce ()))

prim_setCurrentDirectory ::  List Char -> (Debug (IO T1)) 
prim_setCurrentDirectory = 
  ioFunc1 "prim_setCurrentDirectory" (\_ -> P.return (coerce ()))

prim_removeDirectory ::  List Char -> (Debug (IO T1)) 
prim_removeDirectory = 
  ioFunc1 "prim_removeDirectory" (\_ -> P.return (coerce ()))

prim_renameFile ::  List Char -> List Char -> (Debug (IO T1)) 
prim_renameFile = 
  ioFunc2 "prim_renameFile" (\_ _ -> P.return (coerce ()))

prim_renameDirectory ::  List Char -> List Char -> (Debug (IO T1)) 
prim_renameDirectory = 
  ioFunc2 "prim_renameDirectory" (\_ _ -> P.return (coerce ()))

