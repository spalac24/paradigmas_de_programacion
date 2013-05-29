import GUI
import IO
import ReadAnswer
import IOExts
import Dequeue
import Read

hello= listToDeq 
       " ____    ____    _____   \n\
      \(  _ \\  (_  _)  (  _  )   Believe\n\
       \ ) _ <   _)(_    )(_)(    in\n\
       \(____/()(____)()(_____)() Oracles\n\
       \---------------------------------\n\n\n"

widget st lref ref cb fb act = Col [LeftAlign] [
  row [PlainButton [Text "Console",WRef cb,Active (act==Console),
                    Handler DefaultEvent (showConsole st)],
       PlainButton [Text "Files",Active (act==Files),WRef fb,
                       Handler DefaultEvent (showFile True st 0)]],
  row [ListBox [FillY,Width 0,WRef lref,
       Handler DefaultEvent (selectFile st)], 
       TextEditScroll [Text $ deqToList hello,WRef ref]]
  ]


main = do
  let cref,fb,lref,cb free
  stdinUnbuffered
  state <- newIORef (emptyState cref fb lref cb)
  runInitHandlesControlledGUI "B.I.O.tope -- the debugging environment"  
     (widget state lref cref cb fb Console,[readMessages state]) 
     (readMessages state stdin) [stdin]
    

---------------------------------------------------------------------------
-- the state and how to reset it
---------------------------------------------------------------------------

data ActiveWidget = Console | Files 

type Files = [(String,String)]
type State = {files :: Files, 
              console :: Queue Char,active :: ActiveWidget,
              listWidget,textWidget,
              consoleButton,fileButton :: WidgetRef}

emptyState r1 r2 r3 r4 = 
  {files=[],console=hello, active=Console,
   textWidget=r1,fileButton=r2,
   listWidget=r3,consoleButton=r4}


reset :: GuiPort -> IORef State -> IO [ReconfigureItem]
reset gp ref = do
  st <- readIORef ref
  writeIORef ref {files:=[],console:=hello,active:=Console|st}
  setValue (st->textWidget) (deqToList hello) gp
  return [WidgetConf (st->listWidget) (List []),
          acti False (st->fileButton),
          acti False (st->consoleButton)]


---------------------------------------------------------------------------
-- dynamically adding things to the state
---------------------------------------------------------------------------

addConsole ::  GuiPort -> IORef State -> Char -> IO [ReconfigureItem]
addConsole gp ref c = do
  st <- readIORef ref
  let newConsole = snoc c (st->console)
  writeIORef ref {console:=newConsole,active:=Console|st}
  case st->active of
    Console -> appendValue (st->textWidget) [c] gp >> 
               return []
    _       -> setValue (st->textWidget) (deqToList newConsole) gp >>
               return [activate st,acti False (st->consoleButton),
                       WidgetConf (st->listWidget) (List [])]
               

addFile :: Bool -> GuiPort -> IORef State -> (String,String) -> IO [ReconfigureItem]
addFile append gp ref f@(fn,cont) = do
  st <- readIORef ref
  let fs = st->files
  case break ((==tail fn) . tail . fst) fs of
    (_,[])    -> do
                   writeIORef ref {files:=st->files++[f] | st}
                   showFile True ref (length (st->files)) gp
    (xs,(_,yc):ys) -> if append 
                 then do
                   writeIORef ref {files:=xs++ys++[(fn,yc++cont)] | st}
                   showFile True ref (length (st->files) - 1) gp
                 else do 
                   writeIORef ref {files:=xs++ys++[f] | st}
                   showFile True ref (length (st->files) - 1) gp

---------------------------------------------------------------------------
-- showing content in text/list widget
---------------------------------------------------------------------------

selectFile :: IORef State -> GuiPort -> IO [ReconfigureItem]
selectFile ref gp = do
  st <- readIORef ref
  i  <- getValue (st->listWidget) gp
  showFile    False ref (readInt i) gp

showFile :: Bool -> IORef State -> Int -> GuiPort -> IO [ReconfigureItem]
showFile listchanged ref i gp = do
  st <- readIORef ref
  setValue (st->textWidget) (snd (st->files !! i)) gp
  writeIORef ref {active:=Files|st} 
  return (activate st:acti False (st->fileButton):
          if listchanged 
          then map (WidgetConf (st->listWidget)) 
               [List (map fst (st->files)),Text (show i)]
          else [])

showConsole :: IORef State ->  GuiPort -> IO [ReconfigureItem]
showConsole ref gp = do
  st <- readIORef ref
  setValue (st->textWidget) (deqToList (st->console)) gp 
  writeIORef ref {active:=Console|st} 
  return [activate st,acti False (st->consoleButton),
          WidgetConf (st->listWidget) (List [])]

activate :: State -> ReconfigureItem
activate st = acti True (case st -> active of
  Files   -> (st->fileButton)
  Console -> (st->consoleButton))


acti b ref = WidgetConf ref (Active b)

---------------------------------------------------------------------------
-- handling the messages from stdin
---------------------------------------------------------------------------

readMessages :: IORef State -> Handle -> GuiPort -> IO [ReconfigureItem]
readMessages ref h gp = do
  cmd <- hGetChar h
  case cmd of
    'c' -> do 
       c <- hGetChar h
       addConsole gp ref c
    'r' -> do
       l <- hGetLine h
       cont <- getFileCont h 
       addFile False gp ref ("R: "++l,cont)
    'w' -> do
       l <- hGetLine h
       cont <- getFileCont h 
       addFile False gp ref ("W: "++l,cont)
    'a' -> do
       l <- hGetLine h
       cont <- getFileCont h 
       addFile True gp ref ("W: "++l,cont)
    'q' -> exitGUI gp >> return []
    '!' -> reset gp ref
    _   -> return []
  
getFileCont :: Handle -> IO String
getFileCont h = do
  n <- hGetLine h
  hGetNO h (readInt n)

hGetNO ::  Handle -> Int -> IO String
hGetNO h i | i==0 = return []
	   | otherwise = do
                          c <- hGetChar h 
                          s <- hGetNO h (i-1)
                          return (c:s)


  
  