module ExternalFunctionsDirectory where

import Curry
import CurryPrelude hiding (return)
import CurryTime

import System.Time
import System.Directory
import System.IO

prim_doesFileExist :: C_String -> Result (C_IO C_Bool)
prim_doesFileExist = ioFunc1 doesFileExist

prim_doesDirectoryExist :: C_String -> Result (C_IO C_Bool)
prim_doesDirectoryExist = ioFunc1 doesDirectoryExist

prim_fileSize :: C_String -> Result (C_IO C_Int)
prim_fileSize = ioFunc1 (\s->do h <- openFile s ReadMode 
                                i <- hFileSize h
                                hClose h
                                return i)

prim_getModificationTime :: C_String -> Result (C_IO C_ClockTime)
prim_getModificationTime = ioFunc1 getModificationTime

prim_getDirectoryContents :: C_String -> Result (C_IO (List C_String))
prim_getDirectoryContents = ioFunc1 getDirectoryContents

getCurrentDirectory :: Result (C_IO C_String)
getCurrentDirectory = ioFunc0 System.Directory.getCurrentDirectory

prim_createDirectory :: C_String -> Result (C_IO T0)
prim_createDirectory = ioFunc1 createDirectory

prim_removeFile :: C_String -> Result (C_IO T0)
prim_removeFile = ioFunc1 removeFile

prim_setCurrentDirectory :: C_String -> Result (C_IO T0)
prim_setCurrentDirectory = ioFunc1 setCurrentDirectory

prim_removeDirectory :: C_String -> Result (C_IO T0)
prim_removeDirectory = ioFunc1 removeDirectory

prim_renameFile :: C_String -> C_String -> Result (C_IO T0)
prim_renameFile = ioFunc2 renameFile

prim_renameDirectory :: C_String -> C_String -> Result (C_IO T0)
prim_renameDirectory = ioFunc2 renameDirectory