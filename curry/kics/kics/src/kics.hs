import Config
import CurryToHaskell
import SafeCalls
import System

-------------------------------
-- the kics compiler
-------------------------------

main :: IO ()
main = do 
  (opts,_) <- getOptions 
  if null (filename opts)
   then usage "no file to compile"
   else do
      safe (startCompilation opts) >>=
        maybe (error "error during compilaton") (\_ -> return ())
      let call = ghcCall opts{filename=inKicsSubdir "Main.hs"}
      if executable opts 
        then do 
              putStrLn ("compiling executable "++target opts) 
              if verbosity opts >= 3 then putStrLn call else return () 
              system call
        else return undefined
      return ()
