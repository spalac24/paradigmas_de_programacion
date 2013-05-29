module StrictSteps (
  Oracle, 

  traceFunCall, underscore, failure, trusted,

  traceWithStepfile,runWithStepfile, 

  Term, consTerm, consUnderscore, consFailed,
  eval, oneStep, ShowTerm(..), showTerm, 
  Convert(..), Generator(..),

  Func(..),Data(..),
  Debug,World(..),

  addConsole,
  addReadFile,addWrittenFile,addAppendedFile,
  getNextExtVal,app,unprim,toprim,

  mkOr,mkGenOr,childRef,genericCase,hnf,hnf',ghnf,nf,gnf,eqData,free,

  module PartCalls
  ) where


import Prelude hiding (catch,interact)
import System.IO.Unsafe
import DebuggerMonad 
import qualified DebuggerMonad as DM
import Control.Monad.Error 
import Control.Monad.State 
import System.IO hiding (interact)
import Term
import Data.IORef
import System.Process
import PartCalls
import Char (isDigit)
import AnsiCodes 
import Store
import FunLogStrict

hello= " ____    ____    _____   \n\
      \(  _ \\  (_  _)  (  _  )   Believe\n\
       \ ) _ <   _)(_    )(_)(    in\n\
       \(____/()(____)()(_____)() Oracles\n\
       \--------type ? for help----------"



------------------------------------------
-- some auxiliaries to manage the state
------------------------------------------

toggleVerbosity, toggleInspectMode :: DisplayMode -> DisplayMode
toggleVerbosity   m = m {verbose = not (verbose m)}
toggleInspectMode m = m {optionalResult = not (optionalResult m)}

pushPast :: Bool -> Debug ()
pushPast b = modify (\s -> s { past = push (past s) b })

-- each step in the debugger shifts one boolean
-- value from the future to the past.
-- we want to know what the shifted value was.
shift :: Debug Bool
shift = do 
  state <- get
  let (stack, entry) = pop (future state)
  put state { future = stack, 
              past = push (past state) entry}
  return entry
  

oneStep :: Debug ()
oneStep = eval (return ())

putCorrect :: Bool -> Debug ()
putCorrect b = do {st <- get; put st{correct=b}}

putSilent :: Bool -> Debug ()
putSilent b = do {st <- get; put st{interactive=not b,mainthread=not b}}

evalWith :: Convert a => (Bool -> Debug ()) -> Debug a -> Debug a
evalWith setter expr = do
            setter True
            r <- eval expr 
            setter False
            return r


-- function traceFunCall is the core of the debugging routine.
-- every function call in the program is instrumented with a 
-- call to this function.

traceFunCall :: ShowTerm a => Term -> Debug a -> Debug a
traceFunCall call expr = do
  printOrc
  st <- get
  isUnrated <- shift 
  if correct st || not (interactive st)
   then eval expr 
   else if isUnrated
        then do
              showCall call
              inspect <- getDisplayMode >>= return . optionalResult
              if inspect then inspector call expr
                         else stepper   call expr
        else evalWith putCorrect expr 
        



inspector :: ShowTerm a => Term -> Debug a -> Debug a
inspector call expr = do
  origState <- get
  result <- evalWith putSilent expr 
  showResult result
  nl
  let repaint = showCall call >> showResult result >> nl
  interact repaint
           [right repaint expr origState 
           ,wrong repaint result origState call (eval expr)
           ,stepBack (put origState >> showCall call >> inspector call expr)
           ,('i',(False,"toggle inspect mode",modifyDisplayMode toggleInspectMode >>
                                        put origState >>
                                        showCall call >>
                                        stepper call expr)) 
           ,('s',(True,"skip",return result))
           ,(' ',(False,"step into",put origState >> addHist ' ' >> eval expr))]
        

stepper :: ShowTerm a => Term -> Debug a -> Debug a
stepper call expr = do
  interact (nl >> showCall call)
           [('r',(False,"inspect result",inspector call expr))
           ,('i',(False,"toggle inspect mode",
                  modifyDisplayMode toggleInspectMode >>
                  inspector call expr)) 
           ,stepBack (stepper call expr)
           ,skip expr
           ,step call expr]


type Option a = (Char,(Bool,String,Debug a))

right :: Convert a => Debug () -> Debug a -> DebugState -> Option a
right repaint expr state = ('c', (False,"correct",do
  withColor green repaint
  put (state {past=push (fst (pop (past state))) False})
  addHist 'c'
  evalWith putCorrect expr))

wrong :: ShowTerm a => Debug () -> a -> DebugState -> Term 
                    -> Debug a -> Option a
wrong repaint result state call expr = ('w', (False,"wrong",do
  withColor red repaint
  put state
  addHist 'w'
  traceLoop expr
  closeGui
  throwError (BugReport {lhs=call,rhs=showTerm result})))

skip :: ShowTerm a => Debug a -> Option a
skip expr = ('s', (True,"skip",do
  st <- get
  put st{interactive=False,mainthread=True,correct=False}
  r <- eval expr 

  st <- get
  put st{interactive=True,mainthread=True,correct=False}
  showResult r

  --printOrc 

  nl
  return r))

step :: ShowTerm a => Term -> Debug a -> Option a
step call expr = (' ', (True,"step into",do
  nl
  r <- eval expr 
  showCall call
  showResult r
  nl
  return r))

stepBack :: Debug a -> Option a
stepBack noop = ('b', (False,"back",do 
  st <- get
  if null (history st) then noop 
    else do 
      nl
      puts "Showing the last 5 steps only..."
      nl
      modifyDisplayMode (\ dm -> dm{rewinding=True,
                                    rewind=reverse $ tail $ history st})
      throwError Back))


interact :: Debug () -> [Option a] -> Debug a
interact repaint men = do 
  input <- getRewindChar
  case lookup input menu of
    Nothing -> liftIO usage >> repaint >> interact repaint men
    Just (b,_, a) -> addToHistory b input a
   where 
      menu = men++standardOptions repaint men
      usage = do
        putStrLn "\nusage:"
        mapM_ (putStrLn . usageLine) menu
      usageLine (' ', (_,s,_)) = "  <SPACE>  "       ++ s
      usageLine (c,   (_,s,_)) = "  "++ c:"        " ++ s

standardOptions :: Debug () -> [Option a] -> [Option a]
standardOptions repaint menu = [
   ('v',(False,"toggle verbosity",   modifyDisplayMode toggleVerbosity >> 
                               repaint >>
                               interact repaint menu))
  ,('q', (False,"quit",closeGui >> throwError Quit))
  ,('d', (False,"set max depth", getDepth >>= \ d ->
                           modifyDisplayMode (setDepth d)>>
                           repaint >>
                           interact repaint menu))
  ]
  
addToHistory :: Bool -> Char -> Debug a -> Debug a
addToHistory False _ a = a
addToHistory True  c a = addHist c >> a

addHist :: Char -> Debug ()
addHist c = do
  st <- get
  put st{history=c:history st}

getRewindChar :: Debug Char
getRewindChar = do
  st <- getDisplayMode
  if rewinding st 
   then do
    case rewind st of
     []     -> do modifyDisplayMode (const st{rewinding=False})
                  liftIO getChar
     (c:cs) -> do modifyDisplayMode (const st{rewind=cs})
                  return c
   else liftIO getChar

nl :: Debug ()
nl = puts "\n"

getDepth :: Debug (Maybe Int)
getDepth = do
  d <- getDisplayMode  >>= return . depth
  liftIO $ do
    hSetEcho stdin True
    hSetBuffering stdin LineBuffering
    putStrLn "\nenter depth (just hit enter for no depth restriction)"
    l <- getLine
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    if null l then return Nothing else
      if all isDigit l then return (Just (read l))
      else return d


{- ---------------------------------------------------------

   traceWithStepfile - eine Orakeldatei laden und ein 
                       Programm mit dieser debuggen

   loadStepfile      - eine Orakeldatei laden

   traceProgram      - ein instrumentiertes Programm
                       mit einer übergebenen Orakelliste
                       debuggen

   traceFunCall      - einen Funktionsaufruf debuggen

--------------------------------------------------------- -}


{-
  loadStepfile laedt eine Orakelliste aus einer Datei
  make initial output and set terminal properties
-}
initState :: Bool -> String -> IO DebugState
initState isIO name
    = do file <- readFile (name++".steps")
         exts <- readFile (name++".ext")
         case reads file of
              [(oracle,_)] -> do
                 hSetBuffering stdout NoBuffering
                 hSetBuffering stdin NoBuffering
                 hSetEcho stdin False
                 banner
                 dmode <- newIORef (DisplayMode {verbose=False,
                                                 optionalResult=False,
                                                 depth=Just 30,
                                                 rewinding=False,
                                                 rewind=""})
                 return (DebugState
                           {interactive = True,
                            mainthread  = True,
                            spy         = False,
                            correct     = False,
                            oracle      = if isIO then fst $ pop $ fst $ pop oracle 
                                                  else oracle,
                            displayMode = dmode,
                            past        = emptyBoolStack,
                            future      = allTrue,
                            gui         = Nothing,
                            extValues   = split exts,
                            history     = "",
                            refCounter  = initRefCounterVal,
                            store       = emptyStore,
                            fontSettings= [black]})
              _       -> error $ "cannot load oracle file " ++ name ++ ".steps"


{-
  traceProgram verwendet die übergebenen Orakel-Informationen,
  um ein instrumentiertes Programm auszuführen. Das Programm
  wird interaktiv ausgeführt und dabei so lange wiederholt,
  bis alle Funktionsaufrufe bewertet sind oder ein Bug
  gefunden wurde. 
-}
traceProgram :: Debug a -> DebugState -> IO ()
traceProgram debugloop state = do
         bug <- runErrorT $ runStateT debugloop state
         report state debugloop bug


{-
  Die Auswertung eines Ausdrucks wird wiederholt, bis
  darin alle Funktionsaufrufe bewertet sind oder ein
  fehlerhafter Aufruf gefunden wurde.
-} 
traceLoop :: Debug a -> Debug a
traceLoop program
    = do state <- get
         put $ state { past=emptyBoolStack }
         r <- program 
         endState <- get
         dm <- getDisplayMode
         printOrc
         if sum (past endState) == 0 
          then closeGui >> return r
          else do 
                  c <- if not (rewinding dm) 
                        then liftIO $ putStrLn "end reached. press 'q' to abort \
                                               \or any other key to restart." >>
                                      getChar
                        else return ' '
                  if c=='q' 
                   then closeGui >> return r 
                   else do
                     resetGui 
                     put $ state { future = reverse (past endState),
                                   gui = gui endState, 
                                   history = history endState}
                     traceLoop program

traceWithStepfile :: ShowTerm a => String -> Debug a -> IO ()
traceWithStepfile name program = 
  initState False name >>= 
  traceProgram (traceLoop $ traceFunCall (consTerm "main" []) (program >>= nfDataT True))


runWithStepfile :: String -> IO' a -> IO ()
runWithStepfile name action = 
   initState True name >>= traceProgram (ioLoop action) 

ioLoop :: IO' a -> Debug a
ioLoop action = traceLoop (action >>= \ (Func _ f) -> f world)


-- for trusted applications
app :: (Convert a,Convert b) => Func a b -> a -> Debug b
app f x = hnf (\ f' -> eval (appl f')) f
   where
     appl (Func _ f') = f' x
     






{- ---------------------------------------------------------
   Nutzerinteraktion:

   - banner, report

   - showStep, showMenu
--------------------------------------------------------- -}


banner :: IO ()
banner = putStrLn hello >> putStrLn ""


report :: DebugState -> Debug a -> Either BugReport b -> IO ()
report state debugloop bug = do 
            hSetEcho stdin True
            hSetBuffering stdin LineBuffering
            case bug of
               Left Quit 
                   -> putStrLn "cancelled."
               Left (BugReport lhs rhs)
                 ->  do putStrLn "found bug in rule:"
                        putStrLn $ "  lhs = " ++ show (prettyTerm black 0 lhs)
                        putStrLn $ "  rhs = " ++ show (prettyTerm black 0 rhs)
               Left Back
                 -> do
                      hSetEcho stdin False
                      hSetBuffering stdin NoBuffering
                      traceProgram debugloop state{history=""}
               Right _ 
                   -> putStrLn "no bugs found."
          


showCall :: Term -> Debug ()
showCall = printTerm "" 

showResult :: ShowTerm a => a -> Debug ()
showResult res = printTerm "~>" (showTerm res)

printTerm :: String -> Term -> Debug ()
printTerm s term = unlessToOld $ do
      st <- get
      dm <- getDisplayMode
      let fs =fontSettings st
          smTerm = maybe term (restrict term) (depth dm)
      t <- cleanTerm smTerm 
      unless (null s) (puts " ">>withColor bgYellow (puts s)>>puts " ")
      puts (show (prettyTerm (head fs) 0 t))


withColor :: String -> Debug () -> Debug ()
withColor c act = setFont c >> act >> unsetFont

type IO' a = Debug (Func World a)

puts :: String -> Debug ()
puts s = unlessToOld (do
      st <- get
      let fonts=fontSettings st
      liftIO (putStr (head fonts))
      liftIO (putStr s)
      liftIO (putStr off))

unlessToOld :: Debug () -> Debug ()
unlessToOld act = do
  dm <- getDisplayMode
  unless (length (rewind dm)>5) act

---------------------------------------------------------------
-- saved values of external functions and their representation
---------------------------------------------------------------

liftDebug :: Debug () -> Debug ()
liftDebug act = do
  st <- get 
  if mainthread st then act else return ()


getGuiHandle :: Debug Handle
getGuiHandle = do
  st <- get 
  case gui st of
    Nothing -> do hs <- liftIO $ do
                    (h,_,_,ex) <- runInteractiveCommand "biotope" 
                    return (h,ex)
                  put (st{gui=Just hs})
                  return (fst hs)
    Just (h,ex)  -> do
                    mc <- liftIO $ getProcessExitCode ex
                    case mc of
                      Nothing -> return h
                      Just _  -> do
                                   liftIO $ putStrLn "restarting biotype. old\
                                                     \values are not available."
                                   put (st{gui=Nothing})
                                   getGuiHandle
closeGui :: Debug ()
closeGui = putIfAlive 'q' 

resetGui :: Debug ()
resetGui = putIfAlive '!'

putIfAlive :: Char -> Debug ()
putIfAlive c = do
  st <- get 
  maybe (return ()) 
        (\ (h,ex) -> liftIO $ getProcessExitCode ex >>=
                              maybe (hPutChar h c >> hFlush h)
                                    (\_ -> return ())) 
        (gui st)

addConsole :: Char -> Debug ()
addConsole c = liftDebug $ do 
  h <- getGuiHandle 
  liftIO $ hPutStr h ['c',c] >> hFlush h

addReadFile, addWrittenFile, addAppendedFile :: String -> String -> Debug ()
addReadFile     = addFile 'r'
addWrittenFile  = addFile 'w'
addAppendedFile = addFile 'a'

addFile :: Char -> String -> String -> Debug ()
addFile c fn cont = liftDebug $ do
  h <- getGuiHandle 
  liftIO $ do 
    hPutStrLn h (c:fn)
    hPutStrLn h (show (length cont))
    hPutStr h cont
    hFlush h

split :: String -> [String]
split s = case break (=='\n') s of
           ([],[])    -> []
           (n,_:vres) -> let (v,res) = splitAt (read n) vres in  
                         v:split res
           
getNextExtVal :: Read a => Debug a
getNextExtVal = do
  st <- get
  let vals = extValues st
  put (st{extValues=tail vals})
  return (read (head vals))
