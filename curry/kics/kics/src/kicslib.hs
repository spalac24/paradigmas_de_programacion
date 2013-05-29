import Config 
import System.Environment (getArgs)
import System.Exit
import System.Directory
import System.FilePath

main = do 
  home <- getEnv "HOME"
  ex <- doesFileExist (kicsrc home)
  (opts,_) <- readConfig 
  if not ex 
    then do 
     putStrLn ("Make sure, the paths in "++kicsrc home++" are correct")
     putStrLn "then run make again."
     dir <- getCurrentDirectory 
     makeScripts (dir++"/")
     exitWith (ExitFailure 1)
    else 
     putStr (toPathList $ pathWithSubdirs 
                    [joinPath [kicspath opts,"src"],
                     cmdLibpath opts,
                     joinPath [kicspath opts,"src","oracle"],
                     joinPath [frontend opts,"src"]])

makeScripts dir = do 
  writeFile "kghc" (script dir "  --make")
  writeFile "kghci" (script dir "i")

script dir s = 
  "#!/bin/sh\n\n"++
  "export KICS_LIB=`"++dir++"kicslib`\n\n"++
  "ghc"++s++" -fglasgow-exts -i$KICS_LIB $@"

