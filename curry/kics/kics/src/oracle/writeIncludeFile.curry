import FlatCurry
import FlatCurryGoodies
import System (getArgs)
import Distribution
import FileGoodies (dirName)
import Directory (doesFileExist)
import stricths
import Pretty
import PrettyStrict
import StyledText 

main = do
  args <- getArgs 
  case args of
    ("-f":args') -> mapIO_ (writeInclude True) args'
    _            -> mapIO_ (writeInclude False) args


writeInclude :: Bool -> String -> IO ()
writeInclude force m = do
  callFrontend FCY m
  fn <- findFileInLoadPath (m++".fcy")
  let inc = dirName fn++"/Strict"++m++".inc"
  ex <- doesFileExist inc
  if ex && not force
        then putStrLn ("file "++inc++" already exists")
        else readFlatCurryFile fn >>= \ prog ->
             writeFile inc $  header 
                           ++ (concatMap makeType $ 
                               filter isExternalType $ progTypes prog)
                           ++ (concatMap makeFunc $
                               filter isExternal $ progFuncs prog)

header = "\n\n"

makeFunc func = plain (funcTypeDeclDoc "" s t) ++"\n"++
  snd s++" = "++
  if isTCons r && tConsName r==("Prelude","IO")
  then if tConsArgs r/=[TCons ("Prelude","()") []]
       then "ioFunc"++show (funcArity func)++" \""++snd s
          ++"\" "++extVal (funcArity func)++"\n\n"
       else "ioFunc"++show (funcArity func)++" \""++snd s++"\" ("
          ++"Prelude.error \"" ++ snd s ++"\")\n\n"
  else "trace"++show (funcArity func)++" \""++snd s++"\" ("
     ++"Prelude.error \"" ++ snd s ++"\")\n\n"
  where
    t = liftResultType (funcArity func) (funcType func)
    s = funcName func
    r = resultType (funcType func) 

extVal n = if n==0 
           then "getNextExtVal"
           else "(\\"++concat (replicate n "_ ")++"-> getNextExtVal)"
         
makeType typ = "data "++s++args++" = "++s++" deriving (Show,Eq)\n"
             ++"instance ShowTerm ("++s++args++")\n\n"
  where
    s = snd (typeName typ)
    n = length (typeParams typ)
    args = concat [[' ',letters!!i] | i<-[1..n]]


letters = letters' 97 where letters' n = chr n:letters' (n+1)

plain = plainText . pretty 80