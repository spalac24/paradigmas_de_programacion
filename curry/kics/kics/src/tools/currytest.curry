---------------------------------------------------------------------
-- A protocol GUI for the CurryTest tool:
--
-- The GUI process messages sent by running tests with the test tool
-- and summarizes the results in a GUI.
--
-- @author Michael Hanus
-- @version May 2006
---------------------------------------------------------------------

import Read
import Assertion
import System
import List
import FlatCurry
import IOExts -- use IORefs
import FileGoodies(stripSuffix)
import Distribution
import Directory (doesFileExist)

---------------------------------------------------------------------
-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
   a:as -> mapIO_ (testModule putStr "") (map stripSuffix (a:as))
   _ -> putStrLn $ "ERROR: Illegal arguments for currytest: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: currytest <module_names>"

-- Curry file types:
curryFileTypes = [("Curry Files",".curry"),
                  ("Literate Curry files",".lcurry")]

-- increment number text string:
incrText s = show (readInt s + 1)


---------------------------------------------------------------------------
-- Main function to test a module:
-- Arg 1: function for printing compilation messages
-- Arg 2: name of the GUI port (or empty string if no gui defined)
-- Arg 3: module name
testModule :: (String -> IO ()) -> String -> String -> IO ()
testModule prtmsg guiname modname = do
  prtmsg ("Loading module \""++modname++"\"...\n")
  prog_or_error <- tryReadFlatCurry modname
  testModuleIfPossible prtmsg guiname modname prog_or_error

testModuleIfPossible prtmsg guiname _ (Right errmsg) = do
  prtmsg ("ERROR: compilation not successful:\n\n"++errmsg++"\n")
  --showTestCompileError guiname
testModuleIfPossible prtmsg guiname modname (Left prog) =
  execTestFunctions prtmsg guiname modname (getTestFunctionNames prog)

execTestFunctions prtmsg guiname _ [] = do
  prtmsg "No test functions found.\n\n"
  --showTestEnd guiname
execTestFunctions prtmsg guiname modname (f:fs) = do
  print modname
  prtmsg ("Exported top-level test functions:\n"
            ++ concatMap (++" ") (f:fs) ++ "\n\n")
  pakcshome <- getEnviron "PAKCSHOME"
  let testgoal =
         "putStrLn \"\" >> putStrLn (take 60 (repeat (chr 61))) >> " ++
         "putStrLn (\"Testing module \\\""++modname++"\\\"...\") >> " ++
         concat
           (intersperse " `Assertion.seqStrActions` "
             (map (("Assertion.checkAssertion " ++
                    "return ") ++) (f:fs))) ++
         " >>= Assertion.writeAssertResult " ++
         " >> putStrLn (take 60 (repeat (chr 61)))" 
  --putStrLn testgoal
  system ("echo ' :a Assertion\n "++testgoal++" ' | " ++
          installDir++"/bin/kicsi -q "++modname ++ " 2>&1")
  done


-- Extract all test functions from a module:
getTestFunctionNames (Prog _ _ _ funs _) =
   map funcname . filter isExportedFunc . filter hasAssertType $ funs
 where
   isExportedFunc (Func _ _ vis _ _) = vis==Public

   funcname (Func (_,fname) _ _ _ _) = fname


hasAssertType (Func _ _ _ texp _) =
 case texp of
   TCons tc _ -> tc==("Assertion","Assertion")
   _          -> False


-- Tries to read a FlatCurry program.
-- Returns either (Left prog) (if reading was successful)
-- or (Right msg) where msg is the string of error messages from the parser
tryReadFlatCurry :: String -> IO (Either Prog String)
tryReadFlatCurry mname = do 
  pofile <- getPOFilename
  callFrontendWithParams FCY (setLogfile pofile defaultParams) mname
  exfcy <- doesFileExist (flatCurryFileName mname)
  if exfcy
   then
     do prog <- readFlatCurryFile (flatCurryFileName mname)
        system ("rm "++pofile)
        return (Left prog)
   else
     do msgs <- readFile pofile
        system ("rm "++pofile)
        return (Right msgs)
 where
   -- compute name for auxiliary file for parser outputs:
   getPOFilename =
     do pid <- getPID
        return ("/tmp/pakcsoutput_"++show pid)

 

