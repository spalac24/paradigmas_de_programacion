module ExternalFunctionsMeta where

import Curry 
import CurryPrelude hiding ( (==) , return , (>>=))
import InstancesMeta

import Data.Maybe ( catMaybes, isJust )
import System.Mem.Weak ( addFinalizer )
import Control.Concurrent
import System.IO.Unsafe ( unsafeInterleaveIO )
import Debug.Trace ( trace )
import Data.List

---------------------------------------------------------------------------------
-- test for free variable
---------------------------------------------------------------------------------

prim_isFree :: (Curry t0) => t0 -> Result (C_IO (C_Either t0 t0))
prim_isFree x _ = C_IO (\ _ -> case consKind x of
  Branching -> return (IOVal (if isGenerator (orRef x) 
                              then C_Left x 
                              else C_Right x))
  _         -> return (IOVal (C_Right x)))

---------------------------------------------------------------------------------
-- various normal forms in io monad
---------------------------------------------------------------------------------

-- yield head normal form with current state 
-- (including fetching and looking up variable bindings)
-- then apply continuation on it and make sure that you got a value
-- of type io before finally executing that action.

headNormalFormIO :: (Curry a,Curry b) => Prim (a -> Result (C_IO b)) -> a -> Result (C_IO b)
headNormalFormIO cont x _ = 
  C_IO (hnfCTC (\ x' st -> hnfCTC exec2 (apply cont x' st) st) x)

searchTree :: Curry a => a -> Result (C_SearchTree a)
searchTree = searchTr 

hnfIO  x _ = C_IO (hnfCTC  (\ x _ -> return (IOVal x)) x)
nfIO   x _ = C_IO (nfCTC   (\ x _ -> return (IOVal x)) x)
gnfIO  x _ = C_IO (ghnfCTC (\ x _ -> return (IOVal x)) x)
ghnfIO x _ = C_IO (ghnfCTC (\ x _ -> return (IOVal x)) x)

---------------------------------------------------------------------------------
-- rich search trees
---------------------------------------------------------------------------------

getRichSearchTree :: Curry a => a -> Result (C_IO (C_RichSearchTree a))
getRichSearchTree x _ = C_IO (\ state -> return (IOVal (richSearchTr x state)))

richSearchTree :: Curry a => a -> Result (C_RichSearchTree a)
richSearchTree = richSearchTr 

--inject :: Curry a => C_Context -> a -> C_RichSearchTree a
--inject (Context c) = richSearchTr c
 
richSearchTr :: Curry a => a -> Result (C_RichSearchTree a)
richSearchTr x state = 
  transVal (nfCTC (nfCTC (\ x _ -> x)) x state)
  where
    transVal x = case consKind x of
                   Val       -> C_RichValue x
                   Failed    -> C_RichFail (toCurry (exceptions x))
                   Branching -> transBranching (orRef x) (branches x)

    transBranching _  []         = C_RichFail (C_ErrorCall List)
    transBranching _  [x]        = transVal x
    transBranching r  xs@(_:_:_) = C_RichChoice (C_OrRef r)
                                            (fromHaskellList (map transVal xs))

instance ConvertCH C_Exception Exception where
  toCurry (ErrorCall s)        = C_ErrorCall (toCurry s)
  toCurry (PatternMatchFail s) = C_PatternMatchFail (toCurry s)
  toCurry (AssertionFailed s)  = C_AssertionFailed (toCurry s)
  toCurry (IOException s)      = C_IOException (toCurry s)
  toCurry PreludeFailed        = C_PreludeFailed

  fromCurry (C_ErrorCall s)        = ErrorCall (fromCurry s)
  fromCurry (C_PatternMatchFail s) = PatternMatchFail (fromCurry s)
  fromCurry (C_AssertionFailed s)  = AssertionFailed (fromCurry s)
  fromCurry (C_IOException s)      = IOException (fromCurry s)
  fromCurry C_PreludeFailed        = PreludeFailed

---------------------------------------------------------------------------------
-- parallel search
---------------------------------------------------------------------------------

parallelSearch :: Curry a => a -> Result (C_IO (List a))
parallelSearch v _ = C_IO (\state -> do
  chan <- newChan
  mvar <- newEmptyMVar
  qsem <- newMyQSem 0
  tid <- forkIO (searchThread qsem mvar chan 
                              (nfCTC (nfCTC (\ x _ -> x)) v state))
  putMVar mvar [tid]
  --addFinalizer res (stopSearch mvar2)
  res <- myGetChanContents qsem chan
  return (IOVal (fromHaskellList res)))


myGetChanContents :: Show a => MyQSem -> Chan (Maybe a) -> IO [a]
myGetChanContents qsem chan =
  unsafeInterleaveIO ( do
    decMyQSem qsem
    x <- readChan chan
    case x of
         Nothing -> return []
         Just y -> do
            xs <- myGetChanContents qsem chan
            return (y:xs) )


stopSearch :: MVar [ThreadId] -> IO ()
stopSearch mvar = do
  print "start"
  ids <- takeMVar mvar
  mapM_ killThread ids
  --putMVar mvar []


removeId :: MVar [ThreadId] -> Chan (Maybe a) -> ThreadId -> IO ()
removeId mvar chan tid = do
  ids <- takeMVar mvar
  let newids = delete tid ids
  case newids of
       [] -> writeChan chan Nothing -- >> putMVar mvar []
       _  -> putMVar mvar newids 


searchThread :: Curry a => MyQSem -> MVar [ThreadId] -> Chan (Maybe a) 
             -> a -> IO ()
searchThread qsem mvar chan x = do
  case consKind x of
    Val       -> incMyQSem qsem >> writeChan chan (Just x) >> terminate
    Failed    -> terminate
    Branching -> do
      --yield
      testMyQSem qsem
      let b:bs = branches x
      -- to prevent the threads from terminating till their Ids are registered
      ids <- takeMVar mvar  
      newIds <- mapM (forkIO . searchThread qsem mvar chan) bs
      putMVar mvar (newIds++ids)
      searchThread qsem mvar chan b
 where
  noThreads = do
    ids <- takeMVar mvar
    putStrLn ("noThreads: " ++ show (length ids))
    putMVar mvar ids
  terminate = do
    tid <- myThreadId
    removeId mvar chan tid

newtype MyQSem = MyQSem (MVar (Int, [MVar ()]))

-- |Build a new 'MyQSem'
newMyQSem :: Int -> IO MyQSem
newMyQSem init = do
   sem <- newMVar (init,[])
   return (MyQSem sem)


-- |Wait for a unit to become available
incMyQSem :: MyQSem -> IO ()
incMyQSem (MyQSem sem) = do
   (avail,blocked) <- takeMVar sem
   putMVar sem (avail+1,blocked)


-- |Signal that a unit of the 'MyQSem' is available
decMyQSem :: MyQSem -> IO ()
decMyQSem (MyQSem sem) = do
   (avail,blocked) <- takeMVar sem
   if avail>0 then putMVar sem (avail-1,blocked)
              else mapM_ (flip putMVar ()) blocked >> putMVar sem (avail-1,[]) 


testMyQSem :: MyQSem -> IO ()
testMyQSem (MyQSem sem) = do
   x@(avail,blocked) <- takeMVar sem
   if avail<0 then putMVar sem x
              else do
                block <- newEmptyMVar
                putMVar sem (avail,block:blocked)
                takeMVar block

-------------------------------
-- covering non-determinism
-------------------------------

cover :: Curry a => a -> Result a
cover x st = case consKind x of
              Branching -> branching (Curry.cover (orRef x)) 
                                     (map (flip ExternalFunctionsMeta.cover st) (branches x))
              _ -> x

-----------------------------------
-- encapsulate to head normal form
-----------------------------------

st :: Curry a => a -> Result (C_SearchTree a)
st x s = transVal (hnfCTC (\ x _ -> x) x s)
  where
    transVal x = case consKind x of
        Val       -> C_Value x
        Failed    -> C_Fail
        Branching -> let ref = orRef x in 
          if   isCovered ref
          then C_SearchTreeOr (uncover ref) (map (flip st s) (branches x))
          else C_Choice (fromHaskellList (map transVal (branches x)))

-----------------------------------
-- encapsulate to head normal form
-----------------------------------

richST :: Curry a => a -> Result (C_RichSearchTree a)
richST x s = transVal (hnfCTC (\ x _ -> x) x s)
  where
    transVal x = case consKind x of
        Val       -> C_RichValue x
        Failed    -> C_RichFail (toCurry (exceptions x))
        Branching -> let ref = orRef x in 
          if   isCovered ref
          then C_RichSearchTreeOr (uncover ref) 
                                  (map (flip richST s) (branches x))
          else C_RichChoice (C_OrRef (orRef x))
                        (fromHaskellList (map transVal (branches x)))

-----------------------------
-- the general question mark
-----------------------------

ors :: Curry a => List a -> Result a
ors xs _ = branching (error "Unsafe.ors") (toHaskellList xs)


-- temporarily added

prim_throw :: Curry a => C_Exception -> Result a
prim_throw e _ = Curry.failed (fromCurry e)
