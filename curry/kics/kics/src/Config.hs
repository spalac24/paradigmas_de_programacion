module Config (module Config,module KicsSubdir) where

import System.FilePath

import InstallDir
import SafeCalls
import Char
import System.Environment (getEnvironment,getArgs)
import System.Directory hiding (executable)
import System.Time
import MetaProgramming.FlatCurry(readFlatCurry)
import Names
import KicsSubdir


getOptions :: IO (Options,State)
getOptions = do 
  (opts,state) <- readConfig
  args <- getArgs
  cupath <- getEnv "CURRYPATH"
  let parsed = parseOptions opts args
  parsedOpts <- either usage return parsed
  let addFiledir = case takeDirectory (filename opts) of "" -> id; dir -> (dir:)
      newOpts    = parsedOpts{userlibpath=  addFiledir $
                                         userlibpath parsedOpts
                                      ++ splitSearchPath cupath}
  return (newOpts,state)    



parseOptions :: Options -> [String] -> Either String Options
parseOptions opts ("-or":xs) = parseOptions (opts{cm=OrBased}) xs
parseOptions opts ("-ctc":xs) = parseOptions (opts{cm=CTC}) xs
parseOptions opts ("-main":x:xs) = parseOptions (opts{mainFunc=x}) xs
parseOptions opts ("-frontend":x:xs) = parseOptions (opts{frontend=x}) xs
parseOptions opts ("-kicspath":x:xs) = parseOptions (opts{kicspath=x}) xs
parseOptions opts ("-userlibpath":x:xs) = 
  parseOptions (opts{userlibpath=userlibpath opts ++ splitSearchPath x}) xs
parseOptions opts ("-nouserlibpath":xs) = parseOptions (opts{userlibpath=[]}) xs
parseOptions opts ("-ghc":x:xs) = parseOptions (opts{ghc=x}) xs
parseOptions opts ("-make":xs) = parseOptions (opts{make=True}) xs
parseOptions opts ("-nomake":xs) = parseOptions (opts{make=False}) xs
parseOptions opts ("-executable":xs) = parseOptions (opts{executable=True}) xs
parseOptions opts ("-noexecutable":xs) = parseOptions (opts{executable=False}) xs
parseOptions opts ("-q":xs) = parseOptions (opts{verbosity=0}) xs
parseOptions opts ("-v":i:xs) = parseOptions (opts{verbosity=read i}) xs
parseOptions opts ("-noforce":xs) = parseOptions (opts{force=False}) xs
parseOptions opts ("-force":xs) = parseOptions (opts{force=True}) xs
parseOptions opts ("-all":"df":xs) = parseOptions (opts{pm=All DF}) xs
parseOptions opts ("-all":"bf":xs) = parseOptions (opts{pm=All BF}) xs
parseOptions opts ("-st":xs) = parseOptions (opts{pm=ST}) xs
parseOptions opts ("-i":"df":xs) = parseOptions (opts{pm=Interactive DF}) xs
parseOptions opts ("-i":"bf":xs) = parseOptions (opts{pm=Interactive BF}) xs
parseOptions opts ("-o":x:xs) = parseOptions (opts{target=x}) xs
parseOptions opts ("-d":xs) = parseOptions (opts{debug=True,doNotUseInterface=True}) xs
parseOptions opts ("--debug":xs) = parseOptions opts ("-d":xs)
parseOptions opts ("--debugger":d:xs) = parseOptions opts{debugger=Just d} xs
parseOptions opts []    = Right opts
parseOptions opts [x]   = Right (opts{filename=x,mainModule=takeBaseName x})
parseOptions _    (x:_) = Left ("unrecognized option: "++x)

usage problem = do
  putStrLn problem
  putStrLn "Usage: kics [options] filename"
  putStrLn "option         | meaning"
  putStrLn "-or            | or based"
  putStrLn "-ctc           | switch to call time choice"
  putStrLn "-main          | name of main function "
  putStrLn "-frontend      | frontend binary"
  putStrLn "-kicspath      | path to kics compiler"
  putStrLn "-userlibpath   | path to curry libraries"
  putStrLn "-nouserlibpath | only standard curry libraries"
  putStrLn "-ghc           | path to ghc"
  putStrLn "-make          | chase imported modules"
  putStrLn "-nomake        | do not chase imported modules"
  putStrLn "-executable    | create executable"
  putStrLn "-noexecutable  | do not create executable"
  putStrLn "-v <n>         | set verbosity level to n, e.g., -v 3"
  putStrLn "-q             | scarce output"
  putStrLn "-force         | force recompilation"
  putStrLn "-noforce       | do not force recompilation"
  putStrLn "-all df        | print all solutions depth first"
  putStrLn "-all bf        | print all solutions breadth first"
  putStrLn "-st            | print solutions as search tree"
  putStrLn "-i df          | interactively show solutions depth first"
  putStrLn "-i bf          | interactively show solutions breadth first"
  putStrLn "-o             | name of output file"
  putStrLn "-d             | turn on debug mode"
  putStrLn "--debugger <n> | use debug tool <n>"
  error "compilation aborted"


data Options = Opts{ cm :: ChoiceMode,
                     filename, mainFunc, mainModule, target,
                     frontend, ghc, ghcOpts,
                     kicspath  :: String,
                     userlibpath, done :: [String],
                     verbosity :: Int,
                     make, executable, eval, 
                     force, debug, doNotUseInterface :: Bool, 
                     debugger :: Maybe String,
                     consUse :: ConsUse,
                     extCons,hasData :: Bool,
                     pm :: PresentationMode,
                     extData, extFuncs :: [String],
                     extInsts :: [(String,[ProvidedInstance])]} deriving Show

data ConsUse = DataDef | InstanceDef | FunctionDef deriving (Eq,Show)


cymake_call :: String
cymake_call = unpath [installDir,"bin","parsecurry"]


libpath :: Options -> [String]
libpath opts@Opts{userlibpath=up,kicspath=kp,filename=fn} 
  = --(case takeDirectory fn of "" -> id; dir -> ((dir++[pathSeparator]):))
    up ++ [unpath [kp,"src","lib",""]]


cmdLibpath :: Options -> String
cmdLibpath opts = toPathList (libpath opts)

currentModule :: Options -> String
currentModule opts = strip (filename opts)
  where
   strip s = case break isPathSeparator s of
               (s',[]) -> s'
               (_,_:s')  -> strip s'

hasExtData,hasExtInsts, hasExtFuncs :: Options -> Bool
hasExtData opts = 
  not (null (extData opts)) || any (elem Declaration . snd) (extInsts opts)
hasExtInsts opts = 
  not (null (filter (any (/=Declaration) . snd) (extInsts opts)))
hasExtFuncs opts = not (null (extFuncs opts))

defaultOpts curDir = Opts {cm=CTC,filename="", mainFunc= "main", mainModule="Main",
      target = "request",
      frontend=cymake_call,
      kicspath=installDir,
      userlibpath=[],
      ghc=ghc_call,
      ghcOpts=" -fglasgow-exts -fcontext-stack=50 ",
      done=[], 
      make=True, 
      executable=False, 
      verbosity=1,
      eval=True,
      force=False,
      debug=False,
      debugger = Nothing,
      doNotUseInterface=False,
      consUse=FunctionDef,
      extCons=False,
      hasData=False,
      pm=Interactive DF,
      extData=[],
      extInsts=[],
      extFuncs=[]}

kicsrc home = unpath [home,".kicsrc"]

data ChoiceMode = OrBased | CTC deriving (Eq,Read,Show)

data SearchMode = DF | BF 

instance Show SearchMode where
  show DF  = "depth first"
  show BF  = "breadth first"

data PresentationMode = First SearchMode
                      | All SearchMode 
                      | Interactive SearchMode
                      | ST 

instance Show PresentationMode where
  show (All x) = "all solutions "++show x
  show (Interactive x) = "interactive "++show x
  show (First x) = "first solution "++show x
  show ST  = "search tree"

data State = State {home,rts,cmdLineArgs :: String,
                    files :: [(Bool,String)],
                    time :: Bool} deriving Show

defaultState home = State {home=home,
                           rts=" -H400M ",
                           cmdLineArgs="",
                           files=[],
                           time=False}

readPMode s = readPM (words (map toLower s))
    where
      readPM ("interactive":ws) = Interactive (readSM ws)
      readPM ("all":"solutions":ws) = All (readSM ws)
      readPM ["search","tree"] = ST
      
      readSM ["depth","first"] = DF
      readSM ["breadth","first"] = BF

ghcCall :: Options -> String
ghcCall opts@Opts{filename=fn} = 
  callnorm (ghc opts
             ++makeGhc (make opts)
             ++" -i"++show (toPathList 
                             (pathWithSubdirs 
                                (unpath [installDir,"src"]:
                                 unpath [installDir,"src","oracle"]:
                                 libpath opts)))++" "
             ++kicsSubdirPathToFile
             ++linkOpts
             ++ghcOpts opts
             ++verboseGhc (verbosity opts >= 2) 
             ++ghcTarget opts
             ++" "++show fn)
      
  where
    linkOpts | debug opts = linkLib++" -L"++installDir++"/src/lib/ "
             | otherwise  = ""
    linkLib  | eval opts  = " -ldyncoracle "
             | otherwise  = " -lcoracle "

    verboseGhc True  = ""
    verboseGhc False = " -v0 "

    ghcTarget Opts{target=""} = ""
    ghcTarget Opts{target=t} = " -o "++show t

    makeGhc True = " --make "
    makeGhc False = ""

    kicsSubdirPathToFile = case takeDirectory fn of
                             "" -> ""
                             path -> " -i"++show (addKicsSubdir path)++" "


stricthsCall opts = 
  callnorm (installDir++"/bin/stricths --hs " 
             ++ ("-s"++mainModule opts++" ")
             ++ (if make  opts then "-m " else "")
             ++ (if force opts then "-f " else "")
             ++ (if verbosity opts < 2 then "-q " else "")
             ++ filename opts)

mkStrictCall opts = 
  callnorm (installDir++"/bin/mkstrict " 
             ++ (if verbosity opts < 2 then "--quiet " else "")
             ++ filename opts++" "            
          

             {-++ (if make  opts then "-m " else "")
             ++ (if force opts then "-f " else "")
             ++ filename opts-})

cyCall opts = callnorm $ frontend opts++" -e " ++
                         unwords (map (("-i"++) . show) (libpath opts))

callnorm s = unwords (words s) ++ " "

cymake opts = do
  safeSystem (verbosity opts >= 3) 
                         (cyCall opts ++ show (filename opts)
                             ++ if verbosity opts >= 3 then "" else " 1>/dev/null ")

prophecy opts = safeSystem (verbosity opts >= 4) $
   		      installDir++"/bin/prophecy " 
   		      ++ (if make  opts then " -m " else "")
   		      ++ (if force opts then " -f " else "")
   		      ++ (if verbosity opts < 2 then " -q " else "")
   		      ++ show (dropExtension $ filename opts)
   		      ++ if verbosity opts >= 4 then "" else " 1>/dev/null "
                

readConfig = do
   home <- getEnv "HOME"
   curDir <- getCurrentDirectory
   catch (readFile (kicsrc home) >>= getConfigs home) 
         (\_->do
                 let defaultsO = defaultOpts curDir
                     defaultsS = defaultState home
                 writeConfig defaultsO defaultsS
                 putStrLn ("The file "++kicsrc home++" has been written.")
                 putStrLn ("You might need to edit it.")
                 error "Please verify .kicsrc")

writeConfig opts state = do
  home <- getEnv "HOME"
  writeFile (kicsrc home)
    (wLibPath++wPM++wEval++wTime ++wRTS)
  where
    wLibPath  = setting 1 (\o-> toPathList $ case userlibpath o of
                            ".":path -> path
                            path     -> path)
    wPM       = setting 2 (show . pm)
    wEval     = setting 3 (show . eval)
    wTime     = inState 4 (show . time)
    wRTS      = inState 5 rts

    setting n f = entry n (f opts)
    inState n f = entry n (f state)
    entry n s   = (configs!!(n-1)) ++ "="++s++"\n\n"


mkTags = [kicspath,
          (toPathList . userlibpath),
          (show . pm)]

getConfigs home cfgs | cfgs == cfgs = do
  punkt <- getCurrentDirectory

  let readOpts = selOpts (entries cfgs)

      defaultsO = defaultOpts punkt  
      opts = defaultsO
            {cm           = OrBased,
             kicspath     = installDir,
             userlibpath  = let up = readSetting userlibpath splitSearchPath 1
                             in (punkt ++ [pathSeparator]) : up,
             pm           = readSetting pm readPMode 2,
             ghc          = ghc_call,                              
             frontend     = cymake_call,
             eval         = readSetting eval read 3,
             force = False}
      readSetting f r n = maybe (f defaultsO) r (readOpts!!(n-1))

      defaultsS = defaultState home
      state = defaultsS
               {time = readSSet time read 4,
                rts  = readSSet rts  id   5}
      readSSet f r n = maybe (f defaultsS) r (readOpts!!(n-1))

  return (opts,state)

entries s = equations (lines s)
  where
    equations [] = []
    equations (x:xs) = case break (=='=') x of
      (l,_:r) -> (l,r):equations xs
      _       -> equations xs

selOpts cfgs = map (selTag cfgs) configs

configs = 
 ["Libraries",
  "PresentationMode",
  "Eval",
  "Time",
  "RunTimeSettings"]

selTag [] _ = Nothing
selTag ((t,v):xs) s = 
  if map toLower t==map toLower s 
    then Just v
    else selTag xs s


paths s = case break (==':') s of
           ("","") -> []
           (w,"") -> [w]
           ("",_:ws) -> paths ws
           (w,_:ws) -> w : paths ws

getModTime fn = safeIO (do 
                   ex<-doesModuleExist fn
                   if ex then getModuleModTime fn else return (TOD 0 0))



safeReadFlat opts s = do
    fs <- safeIO (findFileInPath s (libpath opts))
    fn <- warning s (cmdLibpath opts) fs
    safeIOSeq (readFlatCurry fn)



warning fn path [] = fail ("module "++fn++" not found in path "++path)
warning _ _  (f:fs) = do
  mapM_ (safeIO . putStrLn) 
        (map (\f' -> "further file found (but ignored) "++f'
                   ++" taking "++f++" instead") fs)
  return f


----------------------------------------------
-- external definitions
----------------------------------------------

-- what is provided by external files

data ProvidedInstance = 
  Declaration | Show | Read | BaseCurry | Curry deriving (Eq,Ord,Read,Show)

data Provided = ForType String (Maybe [ProvidedInstance])
              | ForFunction String 
              | SomeFunctions
              deriving (Eq,Read,Show)

-- external specifications have to look like this:
-- fortype <typename> [definition|nodef] instances <instname>*
-- extfunc <funcname>

put :: Int -> Options -> String -> Safe IO ()
put i Opts{verbosity=j} s | i>j  = return ()
                          | i<=j = safeIO (putStrLn s)

readExternalSpec :: Options -> String -> Safe IO Options
readExternalSpec opts p = do
    specs <- safeIO $ findFileInPath 
                        (externalSpecName (p `withoutSubdir` currySubdir)) 
                        (libpath opts) 
    if null specs
      then return opts 
      else do
        spec <- warning "" "" specs >>= safeIO . readModule
        put 5 opts "reading external specification"
        let newOpts = foldr insertP opts (read spec)
        safeIO (seq newOpts (return ()))
        put 5 opts "external specification read"
        return newOpts
  where
    insertP SomeFunctions         opts = opts{extFuncs = ""     : extFuncs  opts}
    insertP (ForFunction f)       opts = opts{extFuncs = f      : extFuncs  opts}
    insertP (ForType t Nothing)   opts = opts{extData  = t      : extData   opts}
    insertP (ForType t (Just is)) opts = opts{extInsts = (t,is) : extInsts  opts}
    

baseName f = case reverse f of
  'y':'r':'r':'u':'c':'.':f'     -> reverse f'
  'y':'r':'r':'u':'c':'l':'.':f' -> reverse f'
  _ -> f

getEnv :: String -> IO String
getEnv s = getEnvironment >>= maybe (return "") return . lookup s