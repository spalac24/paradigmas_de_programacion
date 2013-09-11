{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Curry_IOExts (C_IORef (..), d_C_execCmd, d_C_evalCmd, d_C_connectToCommand, d_C_readCompleteFile, d_C_updateFile, nd_C_updateFile, d_C_exclusiveIO, d_C_setAssoc, d_C_getAssoc, d_C_readIORef, d_C_writeIORef, d_C_modifyIORef, nd_C_modifyIORef, d_C_newIORef) where

import Basics
import qualified Curry_IO
import qualified Curry_Prelude
import qualified Curry_System
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO) -- for global associations
import System.Process     (readProcessWithExitCode, runInteractiveCommand)
import Control.Concurrent (forkIO)
import System.IO
import qualified Curry_Prelude as CP


d_C_execCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
d_C_execCmd x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_execCmd x1 x3250 x3500

d_C_evalCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_evalCmd x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs (acceptCs id) d_C_prim_evalCmd) x1 x3250 x3500) x2 x3250 x3500) x3 x3250 x3500

d_C_connectToCommand :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_connectToCommand x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_connectToCmd x1 x3250 x3500

d_C_readCompleteFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_readCompleteFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_OP_readCompleteFile_dot___hash_lambda1 x3250 x3500

d_OP_readCompleteFile_dot_f_dot_8 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List t0 -> t1 -> Cover -> ConstStore -> t1
d_OP_readCompleteFile_dot_f_dot_8 x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP_readCompleteFile_dot_f_dot_8 x4 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readCompleteFile_dot_f_dot_8 x1002 x2 x3250 x3500) (d_OP_readCompleteFile_dot_f_dot_8 x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readCompleteFile_dot_f_dot_8 z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readCompleteFile_dot_f_dot_8 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readCompleteFile_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_readCompleteFile_dot___hash_lambda1 x1 x3250 x3500 = d_OP_readCompleteFile_dot_f_dot_8 x1 (Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500

d_C_updateFile :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_updateFile x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readCompleteFile x2 x3250 x3500) (d_OP_updateFile_dot___hash_lambda2 x1 x2) x3250 x3500

nd_C_updateFile :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_updateFile x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_readCompleteFile x2 x3250 x3500) (wrapNX id (nd_OP_updateFile_dot___hash_lambda2 x1 x2)) x2000 x3250 x3500))

d_OP_updateFile_dot___hash_lambda2 :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateFile_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_writeFile x2 (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x3250 x3500

nd_OP_updateFile_dot___hash_lambda2 :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_updateFile_dot___hash_lambda2 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_writeFile x2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x3250 x3500))

d_C_exclusiveIO :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_exclusiveIO x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_catch (Curry_Prelude.d_OP_gt_gt_eq x2 (d_OP_exclusiveIO_dot___hash_lambda3 x1) x3250 x3500) (d_OP_exclusiveIO_dot___hash_lambda4 x1) x3250 x3500) x3250 x3500

d_OP_exclusiveIO_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_exclusiveIO_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x2 x3250 x3500) x3250 x3500

d_OP_exclusiveIO_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IOError -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_exclusiveIO_dot___hash_lambda4 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_ioError x2 x3250 x3500) x3250 x3500

d_C_setAssoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setAssoc x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_setAssoc) x1 x3250 x3500) x2 x3250 x3500

d_C_getAssoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getAssoc x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_getAssoc x1 x3250 x3500

d_C_readIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_readIORef x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_readIORef x1 x3250 x3500

d_C_writeIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeIORef x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_writeIORef) x1 x3250 x3500) x2 x3250 x3500

d_C_modifyIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> (t0 -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_modifyIORef x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readIORef x1 x3250 x3500) (Curry_Prelude.d_OP_dot (d_C_writeIORef x1) x2 x3250 x3500) x3250 x3500

nd_C_modifyIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> Func t0 t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_modifyIORef x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_readIORef x1 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id (d_C_writeIORef x1)) x2 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_prim_execCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
d_C_prim_execCmd x1 x3250 x3500 = external_d_C_prim_execCmd x1 x3250 x3500

d_C_prim_evalCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_prim_evalCmd x1 x2 x3 x3250 x3500 = external_d_C_prim_evalCmd x1 x2 x3 x3250 x3500

d_C_prim_connectToCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_prim_connectToCmd x1 x3250 x3500 = external_d_C_prim_connectToCmd x1 x3250 x3500

d_C_prim_setAssoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_setAssoc x1 x2 x3250 x3500 = external_d_C_prim_setAssoc x1 x2 x3250 x3500

d_C_prim_getAssoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_prim_getAssoc x1 x3250 x3500 = external_d_C_prim_getAssoc x1 x3250 x3500

d_C_newIORef :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_IORef t0)
d_C_newIORef x1 x3250 x3500 = external_d_C_newIORef x1 x3250 x3500

d_C_prim_readIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_prim_readIORef x1 x3250 x3500 = external_d_C_prim_readIORef x1 x3250 x3500

d_C_prim_writeIORef :: Curry_Prelude.Curry t0 => C_IORef t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_writeIORef x1 x2 x3250 x3500 = external_d_C_prim_writeIORef x1 x2 x3250 x3500
external_d_C_prim_execCmd :: CP.C_String -> Cover -> ConstStore 
  -> CP.C_IO (CP.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
external_d_C_prim_execCmd str _ _ = toCurry
  (\s -> do (h1,h2,h3,_) <- runInteractiveCommand s
            return (OneHandle h1, OneHandle h2, OneHandle h3)) str

external_d_C_prim_evalCmd :: CP.C_String -> CP.OP_List CP.C_String -> CP.C_String
  -> Cover -> ConstStore -> CP.C_IO (CP.OP_Tuple3 CP.C_Int CP.C_String CP.C_String)
external_d_C_prim_evalCmd cmd args input _ _
  = toCurry readProcessWithExitCode cmd args input

external_d_C_prim_connectToCmd :: CP.C_String -> Cover -> ConstStore
                               -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToCmd str _ _ = toCurry
  (\s -> do (hin,hout,herr,_) <- runInteractiveCommand s
            forkIO (forwardError herr)
            return (InOutHandle hout hin)) str

forwardError :: Handle -> IO ()
forwardError h = do
   eof <- hIsEOF h
   if eof then return ()
          else hGetLine h >>= hPutStrLn stderr >> forwardError h


-----------------------------------------------------------------------
-- Implementation of global associations as simple association lists
-- (could be later improved by a more efficient implementation, e.g., maps)

type Assocs = [(String,String)]

assocs :: IORef Assocs
assocs = unsafePerformIO (newIORef [])

external_d_C_prim_setAssoc :: CP.C_String -> CP.C_String -> Cover -> ConstStore
                           -> CP.C_IO CP.OP_Unit
external_d_C_prim_setAssoc str1 str2 _ _ = toCurry
  (\key val -> do as <- readIORef assocs
                  writeIORef assocs ((key,val):as)) str1 str2

external_d_C_prim_getAssoc :: CP.C_String -> Cover -> ConstStore
                           -> CP.C_IO (CP.C_Maybe (CP.C_String))
external_d_C_prim_getAssoc str _ _ = toCurry
  (\key -> do as <- readIORef assocs
              return (lookup key as)) str

-----------------------------------------------------------------------
-- Implementation of IORefs in Curry. Note that we store Curry values
-- (and not the corresponding Haskell values) in the Haskell IORefs
data C_IORef a
    = Choice_C_IORef Cover ID (C_IORef a) (C_IORef a)
    | Choices_C_IORef Cover ID ([C_IORef a])
    | Fail_C_IORef Cover FailInfo
    | Guard_C_IORef Cover  Constraints (C_IORef a)
    | C_IORef (IORef a)

instance Show (C_IORef a) where
  show = error "ERROR: no show for IORef"

instance Read (C_IORef a) where
  readsPrec = error "ERROR: no read for IORef"

instance NonDet (C_IORef a) where
  choiceCons = Choice_C_IORef
  choicesCons = Choices_C_IORef
  failCons = Fail_C_IORef
  guardCons = Guard_C_IORef
  try (Choice_C_IORef cd i x y) = tryChoice cd i x y
  try (Choices_C_IORef cd s xs) = tryChoices cd s xs
  try (Fail_C_IORef cd info) = Fail cd info
  try (Guard_C_IORef cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IORef  cd i x y)                 = f cd i x y
  match _ f _ _ _ _ (Choices_C_IORef cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IORef cd i@(FreeID _ _)     xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IORef _ i _)      =
    error ("IOExts.IORef.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_C_IORef cd info)                     = f cd info
  match _ _ _ _ f _ (Guard_C_IORef cd cs e)                    = f cd cs e
  match _ _ _ _ _ f x                                          = f x

instance Generable (C_IORef a) where
  generate _ _ = error "ERROR: no generator for IORef"

instance NormalForm (C_IORef a) where
  ($!!) cont ioref@(C_IORef _)            cd cs = cont ioref cd cs
  ($!!) cont (Choice_C_IORef d i io1 io2) cd cs = nfChoice cont d i io1 io2 cd cs
  ($!!) cont (Choices_C_IORef d i ios)    cd cs = nfChoices cont d  i ios cd cs
  ($!!) cont (Guard_C_IORef d c io)       cd cs 
    = guardCons d c ((cont $!! io) cd $! (addCs c cs))
  ($!!) _    (Fail_C_IORef d info)        _  _  = failCons d info
  ($##) cont io@(C_IORef _)               cd cs = cont io cd cs
  ($##) cont (Choice_C_IORef d i io1 io2) cd cs = gnfChoice cont d i io1 io2 cd cs
  ($##) cont (Choices_C_IORef d i ios)    cd cs = gnfChoices cont d i ios cd cs
  ($##) cont (Guard_C_IORef d c io)       cd cs 
    = guardCons d c ((cont $## io) cd $! (addCs c cs))
  ($##) _    (Fail_C_IORef d info)        cd cs = failCons d info
  searchNF _ cont ioref@(C_IORef _)        = cont ioref

instance Unifiable (C_IORef a) where
  (=.=) _ _ = error "(=.=) for C_IORef"
  (=.<=) _ _ = error "(=.<=) for C_IORef"
  bind cd i (Choice_C_IORef d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_IORef d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_IORef d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ (Fail_C_IORef cd info) = [Unsolvable info]
  bind cd i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ (bind cd i e)
  lazyBind cd i (Choice_C_IORef d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_IORef d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_IORef d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ (Fail_C_IORef cd info) = [Unsolvable info]
  lazyBind cd i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance CP.Curry a => CP.Curry (C_IORef a) where
  (=?=) = error "(==) is undefined for IORefs"
  (<?=) = error "(<=) is undefined for IORefs"

instance ConvertCurryHaskell (C_IORef a) (IORef a) where
  fromCurry (C_IORef r) = r
  fromCurry _           = error "IORef with no ground term occurred"
  toCurry r             = C_IORef r

external_d_C_newIORef :: CP.Curry a => a -> Cover -> ConstStore 
                      -> CP.C_IO (C_IORef a)
external_d_C_newIORef cv _ _ = toCurry (newIORef cv)

external_d_C_prim_readIORef :: CP.Curry a => C_IORef a -> Cover -> ConstStore
                            -> CP.C_IO a
external_d_C_prim_readIORef ref _ _ = fromIO (readIORef (fromCurry ref))

external_d_C_prim_writeIORef :: CP.Curry a => C_IORef a -> a
                             -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeIORef ref cv _ _ = toCurry (writeIORef (fromCurry ref) cv)

