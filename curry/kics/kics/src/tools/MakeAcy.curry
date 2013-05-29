import Make (make)
import AbstractCurry (readCurry)
import System (getArgs)

main = do
  [m] <- getArgs
  make True m (\ _ m' -> putStr "ensure existence of acy file for " >> putStrLn m' 
                 >> readCurry m' >> return Nothing)
       (\ _ _ _ -> return ())