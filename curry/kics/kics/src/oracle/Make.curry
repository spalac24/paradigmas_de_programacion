-------------------------------------------------------
-- This module provides basic make functionality for 
-- curry programs. The provided actions traverse the
-- import tree and execute a given action only if 
-- necessary.
-------------------------------------------------------

module Make (
  ModuleName,
  Path,
  FileName,
  make, obsolete, unless) where

import FlatCurry
import FlatCurryGoodies (progImports)
import Distribution 
import FiniteMap
import IOExts
import Sort (leqString)
import FileGoodies (dirName)
import Time
import Directory

type ModuleName = String
type Path       = String
type FileName   = String

type TestAct a = Path -> ModuleName -> IO (Maybe a)
type ProgAct a = Path -> [a] -> Prog -> IO a

type Done a = IORef (FM String a)

--- calls act on each imported module transitively
--- if test returns Nothing.
make :: Bool -> ModuleName -> TestAct a -> ProgAct a -> IO ()
make quiet modu test act = do
  unless quiet $ putStrLn "ensuring existence of fcy/fint files..."
  callFrontendWithParams FCY (setQuiet True defaultParams) modu
  unless quiet $ putStrLn "...ensured"
  done <- newIORef (emptyFM (\ x y -> not (leqString x y)))
  workUpDependence done test act modu
  return ()

workUpDependence ::  Done a -> TestAct a -> ProgAct a -> ModuleName -> IO a
workUpDependence done test act modu = do
  fm <- readIORef done
  maybe (process done test act modu) return (lookupFM fm modu)

process ::  Done a -> TestAct a -> ProgAct a ->  ModuleName -> IO a
process done test act modu = do
  fn <- findFileInLoadPath (flatCurryFileName modu)
  imps <- fastReadImports fn >>= mapIO (workUpDependence done test act)
  let dir = dirName fn++"/"
  result <- test dir modu >>= 
            maybe (readFlatCurryFile fn >>= act dir imps) return 
  updateIORef done (\fm -> addToFM fm modu result)
  return result

--- a standard test if a given file is newer than a list of other files
--- if other files do not exist, the given file is assumed to be up-to-date
--- on up-to-date files a given action is performed
obsolete :: Bool -> (String -> String) -> [String -> String] 
         -> ([String] -> IO a) -> TestAct a
obsolete quiet f fs action dir modu = do
  let fn  = dir++f modu
      fns = map ((dir++).($modu)) fs
  ex <- doesFileExist fn
  if ex then do
               t  <- getModificationTime fn
               ns <- mapIO (isNewerThan t) fns
               if or ns
                 then do
                   unless quiet $ putStrLn $ "obsolete  : " ++ f modu
                   return Nothing
                 else do
                   unless quiet $ putStrLn $ "up-to-date: " ++ f modu   
                   action fns >>= return . Just
        else do unless quiet $ putStrLn ("missing   : "++ f modu) 
                return Nothing
 where
   isNewerThan t file = do 
     ex <- doesFileExist file
     if not ex then return False else do
      t' <- getModificationTime file
      return (compareClockTime t t'/=GT)

fastReadImports :: FileName -> IO [String]
fastReadImports fn = do 
  cont <- readFile fn
  return (strings (takeWhile (/=']') (dropWhile (/='[') cont)))

strings :: String -> [String]
strings [] = []
strings (c:cs) | c=='"' = case break (=='"') cs of
                            (s,_:rest) -> s : strings rest
               | otherwise = strings cs

updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef ref f = do
  x <- readIORef ref
  writeIORef ref (f x)

unless :: Bool -> IO () -> IO ()
unless True  _   = return ()
unless False act = act

  

  
  