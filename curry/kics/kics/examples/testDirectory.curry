------------------------------------------------------------------------------
--- Some tests for library Directory
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testDirectory"
--- 
--- @author Michael Hanus
--- @version March 2008
------------------------------------------------------------------------------

import Assertion
import System
import Directory

testFile = AssertIO "test create/rename/delete file"
                    fileOps (True,False,True,False)
 where
  fileOps = do
    let fname = "xxx1234"
        fnamebak = fname++".bak"
    writeFile fname "test\n"
    ex1 <- doesFileExist fname
    renameFile fname fnamebak
    ex2 <- doesFileExist fname
    ex3 <- doesFileExist fnamebak
    removeFile fnamebak
    ex4 <- doesFileExist fnamebak
    return (ex1,ex2,ex3,ex4)

testDir = AssertIO "test create/rename/delete directory"
                   dirOps (True,False,True,False)
 where
  dirOps = do
    let dname = "xxx1111"
        dnamebak = dname++".bak"
    createDirectory dname
    ex1 <- doesDirectoryExist dname
    renameDirectory dname dnamebak
    ex2 <- doesDirectoryExist dname
    ex3 <- doesDirectoryExist dnamebak
    removeDirectory dnamebak
    ex4 <- doesDirectoryExist dnamebak
    return (ex1,ex2,ex3,ex4)

testGetSetDir = AssertIO "test to get and set directories"
                         dirOps (True,True,"x",False)
 where
  dirOps = do
    cdir <- getCurrentDirectory
    let dname = cdir++"/xxx2222"
    createDirectory dname
    ex1 <- doesDirectoryExist dname
    writeFile (dname++"/xxx") "x"
    setCurrentDirectory dname
    ex2 <- doesFileExist "xxx"
    cnt <- readFile "xxx"
    cnt==cnt `seq` removeFile "xxx"
    setCurrentDirectory cdir
    removeDirectory dname
    ex3 <- doesDirectoryExist dname
    return (ex1,ex2,cnt,ex3)
