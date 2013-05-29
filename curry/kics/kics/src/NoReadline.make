module MyReadline (readline, addHistory, initializeReadline) where

import System.IO

initializeReadline :: IO ()
initializeReadline = 
  putStrLn "no readline support. Consider cabal install readline"

readline s = putStr s >> hFlush stdout >> getLine >>= return . Just

addHistory _ = return ()
