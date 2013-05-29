--- Library for accessing the directory structure of the
--- underlying operating system.
---
--- @author Michael Hanus
--- @version January 2013

module Directory
  ( doesFileExist, doesDirectoryExist, fileSize, getModificationTime
  , getCurrentDirectory, setCurrentDirectory
  , getDirectoryContents, createDirectory, createDirectoryIfMissing
  , removeDirectory, renameDirectory
  , getHomeDirectory, getTemporaryDirectory
  , removeFile, renameFile, copyFile
  ) where

import FilePath (FilePath, (</>), splitDirectories)
import List     (scanl1, last)
import System   (getEnviron, isWindows)
import Time     (ClockTime)


--- Returns true if the argument is the name of an existing file.
doesFileExist :: FilePath -> IO Bool
doesFileExist fname = prim_doesFileExist $## fname

prim_doesFileExist :: FilePath -> IO Bool
prim_doesFileExist external

--- Returns true if the argument is the name of an existing directory.
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist dir = prim_doesDirectoryExist $## dir

prim_doesDirectoryExist :: FilePath -> IO Bool
prim_doesDirectoryExist external

--- Returns the size of the file.
fileSize :: FilePath -> IO Int
fileSize fname = prim_fileSize $## fname

prim_fileSize :: FilePath -> IO Int
prim_fileSize external

--- Returns the modification time of the file.
getModificationTime :: FilePath -> IO ClockTime
getModificationTime fname = prim_getModificationTime $## fname

prim_getModificationTime :: FilePath -> IO ClockTime
prim_getModificationTime external

--- Returns the current working directory.
getCurrentDirectory :: IO FilePath
getCurrentDirectory external

--- Sets the current working directory.
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory dir = prim_setCurrentDirectory $## dir

prim_setCurrentDirectory :: FilePath -> IO ()
prim_setCurrentDirectory external

--- Returns the list of all entries in a directory.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents dir = prim_getDirectoryContents $## dir

prim_getDirectoryContents :: FilePath -> IO [FilePath]
prim_getDirectoryContents external

--- Creates a new directory with the given name.
createDirectory :: FilePath -> IO ()
createDirectory dir = prim_createDirectory $## dir

prim_createDirectory :: FilePath -> IO ()
prim_createDirectory external

--- Creates a new directory with the given name if it does not already exist.
--- If the first parameter is `True` it will also create all missing
--- parent directories.
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing createParents path
  = if createParents then createDirs parents
                     else createDirs [last parents]
 where
  parents = scanl1 (</>) $ splitDirectories $ path

  createDirs []     = done
  createDirs (d:ds) = do
    exists <- doesDirectoryExist d
    if exists then done else createDirectory d
    createDirs ds

--- Deletes a directory from the file system.
removeDirectory :: FilePath -> IO ()
removeDirectory dir = prim_removeDirectory $## dir

prim_removeDirectory :: FilePath -> IO ()
prim_removeDirectory external

--- Renames a directory.
renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory dir1 dir2 = (prim_renameDirectory $## dir1) $## dir2

prim_renameDirectory :: FilePath -> FilePath -> IO ()
prim_renameDirectory external

--- Return the home directory of the current user.
getHomeDirectory :: IO FilePath
getHomeDirectory = if isWindows
                      then getEnviron "USERPROFILE"
                      else getEnviron "HOME"

--- Return the temporary directory of the operating system.
getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = if isWindows then getEnviron "TMP" else return "/tmp"

--- Deletes a file from the file system.
removeFile :: FilePath -> IO ()
removeFile file = prim_removeFile $## file

prim_removeFile :: FilePath -> IO ()
prim_removeFile external

--- Renames a file.
renameFile :: FilePath -> FilePath -> IO ()
renameFile file1 file2 = (prim_renameFile $## file1) $## file2

prim_renameFile :: FilePath -> FilePath -> IO ()
prim_renameFile external

--- Copy the contents from one file to another file
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = readFile src >>= writeFile dest
