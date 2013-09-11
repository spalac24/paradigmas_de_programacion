{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Curry_IO (C_Handle (..), C_IOMode (..), C_SeekMode (..), d_C_openFile, d_C_hClose, d_C_hFlush, d_C_hIsEOF, d_C_isEOF, d_C_hSeek, d_C_hWaitForInput, d_C_hWaitForInputs, d_C_hWaitForInputOrMsg, d_C_hWaitForInputsOrMsg, d_C_hReady, d_C_hGetChar, d_C_hGetLine, d_C_hGetContents, d_C_getContents, d_C_hPutChar, d_C_hPutStr, d_C_hPutStrLn, d_C_hPrint, nd_C_hPrint, d_C_hIsReadable, d_C_hIsWritable, d_C_stdin, d_C_stdout, d_C_stderr) where

import Basics
import qualified Curry_Prelude
import           Control.Concurrent
import qualified Control.Exception  as C (IOException, catch, throw)
import           Control.Monad           (zipWithM)
import           System.IO
import           System.IO.Error         (isEOFError)

import qualified Curry_Prelude as CP




data C_IOMode
     = C_ReadMode
     | C_WriteMode
     | C_AppendMode
     | Choice_C_IOMode Cover ID C_IOMode C_IOMode
     | Choices_C_IOMode Cover ID ([C_IOMode])
     | Fail_C_IOMode Cover FailInfo
     | Guard_C_IOMode Cover Constraints C_IOMode

instance Show C_IOMode where
  showsPrec d (Choice_C_IOMode cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_IOMode cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_IOMode cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_IOMode cd info) = showChar '!'
  showsPrec _ C_ReadMode = showString "ReadMode"
  showsPrec _ C_WriteMode = showString "WriteMode"
  showsPrec _ C_AppendMode = showString "AppendMode"


instance Read C_IOMode where
  readsPrec _ s = (readParen False (\r -> [ (C_ReadMode,r0) | (_,r0) <- readQualified "IO" "ReadMode" r]) s) ++ ((readParen False (\r -> [ (C_WriteMode,r0) | (_,r0) <- readQualified "IO" "WriteMode" r]) s) ++ (readParen False (\r -> [ (C_AppendMode,r0) | (_,r0) <- readQualified "IO" "AppendMode" r]) s))


instance NonDet C_IOMode where
  choiceCons = Choice_C_IOMode
  choicesCons = Choices_C_IOMode
  failCons = Fail_C_IOMode
  guardCons = Guard_C_IOMode
  try (Choice_C_IOMode cd i x y) = tryChoice cd i x y
  try (Choices_C_IOMode cd i xs) = tryChoices cd i xs
  try (Fail_C_IOMode cd info) = Fail cd info
  try (Guard_C_IOMode cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IOMode cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_IOMode cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IOMode cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IOMode cd i _) = error ("IO.IOMode.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_IOMode cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_IOMode cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_IOMode where
  generate s c = Choices_C_IOMode c (freeID [0,0,0] s) [C_ReadMode,C_WriteMode,C_AppendMode]


instance NormalForm C_IOMode where
  ($!!) cont C_ReadMode d cs = cont C_ReadMode d cs
  ($!!) cont C_WriteMode d cs = cont C_WriteMode d cs
  ($!!) cont C_AppendMode d cs = cont C_AppendMode d cs
  ($!!) cont (Choice_C_IOMode cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_IOMode cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_IOMode cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_IOMode cd info) _ _ = failCons cd info
  ($##) cont C_ReadMode d cs = cont C_ReadMode d cs
  ($##) cont C_WriteMode d cs = cont C_WriteMode d cs
  ($##) cont C_AppendMode d cs = cont C_AppendMode d cs
  ($##) cont (Choice_C_IOMode cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_IOMode cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_IOMode cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_IOMode cd info) _ _ = failCons cd info
  searchNF _ cont C_ReadMode = cont C_ReadMode
  searchNF _ cont C_WriteMode = cont C_WriteMode
  searchNF _ cont C_AppendMode = cont C_AppendMode
  searchNF _ _ x = error ("IO.IOMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_IOMode where
  (=.=) C_ReadMode C_ReadMode d cs = C_Success
  (=.=) C_WriteMode C_WriteMode d cs = C_Success
  (=.=) C_AppendMode C_AppendMode d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_ReadMode C_ReadMode d cs = C_Success
  (=.<=) C_WriteMode C_WriteMode d cs = C_Success
  (=.<=) C_AppendMode C_AppendMode d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_ReadMode = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_WriteMode = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_AppendMode = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_IOMode cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_IOMode cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_IOMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_IOMode cd i _) = error ("IO.IOMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_IOMode cd info) = [(Unsolvable info)]
  bind d i (Guard_C_IOMode cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_ReadMode = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_WriteMode = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_AppendMode = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_IOMode cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_IOMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_IOMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_IOMode cd i _) = error ("IO.IOMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_IOMode cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_IOMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_IOMode where
  (=?=) (Choice_C_IOMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_IOMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_IOMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_IOMode cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_IOMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_IOMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_IOMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_IOMode cd info) _ _ = failCons cd info
  (=?=) C_ReadMode C_ReadMode d cs = Curry_Prelude.C_True
  (=?=) C_WriteMode C_WriteMode d cs = Curry_Prelude.C_True
  (=?=) C_AppendMode C_AppendMode d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_IOMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_IOMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_IOMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_IOMode cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_IOMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_IOMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_IOMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_IOMode cd info) _ _ = failCons cd info
  (<?=) C_ReadMode C_ReadMode d cs = Curry_Prelude.C_True
  (<?=) C_ReadMode C_WriteMode _ _ = Curry_Prelude.C_True
  (<?=) C_ReadMode C_AppendMode _ _ = Curry_Prelude.C_True
  (<?=) C_WriteMode C_WriteMode d cs = Curry_Prelude.C_True
  (<?=) C_WriteMode C_AppendMode _ _ = Curry_Prelude.C_True
  (<?=) C_AppendMode C_AppendMode d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_SeekMode
     = C_AbsoluteSeek
     | C_RelativeSeek
     | C_SeekFromEnd
     | Choice_C_SeekMode Cover ID C_SeekMode C_SeekMode
     | Choices_C_SeekMode Cover ID ([C_SeekMode])
     | Fail_C_SeekMode Cover FailInfo
     | Guard_C_SeekMode Cover Constraints C_SeekMode

instance Show C_SeekMode where
  showsPrec d (Choice_C_SeekMode cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_SeekMode cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_SeekMode cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_SeekMode cd info) = showChar '!'
  showsPrec _ C_AbsoluteSeek = showString "AbsoluteSeek"
  showsPrec _ C_RelativeSeek = showString "RelativeSeek"
  showsPrec _ C_SeekFromEnd = showString "SeekFromEnd"


instance Read C_SeekMode where
  readsPrec _ s = (readParen False (\r -> [ (C_AbsoluteSeek,r0) | (_,r0) <- readQualified "IO" "AbsoluteSeek" r]) s) ++ ((readParen False (\r -> [ (C_RelativeSeek,r0) | (_,r0) <- readQualified "IO" "RelativeSeek" r]) s) ++ (readParen False (\r -> [ (C_SeekFromEnd,r0) | (_,r0) <- readQualified "IO" "SeekFromEnd" r]) s))


instance NonDet C_SeekMode where
  choiceCons = Choice_C_SeekMode
  choicesCons = Choices_C_SeekMode
  failCons = Fail_C_SeekMode
  guardCons = Guard_C_SeekMode
  try (Choice_C_SeekMode cd i x y) = tryChoice cd i x y
  try (Choices_C_SeekMode cd i xs) = tryChoices cd i xs
  try (Fail_C_SeekMode cd info) = Fail cd info
  try (Guard_C_SeekMode cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_SeekMode cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_SeekMode cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_SeekMode cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_SeekMode cd i _) = error ("IO.SeekMode.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_SeekMode cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_SeekMode cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_SeekMode where
  generate s c = Choices_C_SeekMode c (freeID [0,0,0] s) [C_AbsoluteSeek,C_RelativeSeek,C_SeekFromEnd]


instance NormalForm C_SeekMode where
  ($!!) cont C_AbsoluteSeek d cs = cont C_AbsoluteSeek d cs
  ($!!) cont C_RelativeSeek d cs = cont C_RelativeSeek d cs
  ($!!) cont C_SeekFromEnd d cs = cont C_SeekFromEnd d cs
  ($!!) cont (Choice_C_SeekMode cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_SeekMode cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_SeekMode cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_SeekMode cd info) _ _ = failCons cd info
  ($##) cont C_AbsoluteSeek d cs = cont C_AbsoluteSeek d cs
  ($##) cont C_RelativeSeek d cs = cont C_RelativeSeek d cs
  ($##) cont C_SeekFromEnd d cs = cont C_SeekFromEnd d cs
  ($##) cont (Choice_C_SeekMode cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_SeekMode cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_SeekMode cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_SeekMode cd info) _ _ = failCons cd info
  searchNF _ cont C_AbsoluteSeek = cont C_AbsoluteSeek
  searchNF _ cont C_RelativeSeek = cont C_RelativeSeek
  searchNF _ cont C_SeekFromEnd = cont C_SeekFromEnd
  searchNF _ _ x = error ("IO.SeekMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_SeekMode where
  (=.=) C_AbsoluteSeek C_AbsoluteSeek d cs = C_Success
  (=.=) C_RelativeSeek C_RelativeSeek d cs = C_Success
  (=.=) C_SeekFromEnd C_SeekFromEnd d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_AbsoluteSeek C_AbsoluteSeek d cs = C_Success
  (=.<=) C_RelativeSeek C_RelativeSeek d cs = C_Success
  (=.<=) C_SeekFromEnd C_SeekFromEnd d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_AbsoluteSeek = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_RelativeSeek = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_SeekFromEnd = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_SeekMode cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_SeekMode cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_SeekMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_SeekMode cd i _) = error ("IO.SeekMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_SeekMode cd info) = [(Unsolvable info)]
  bind d i (Guard_C_SeekMode cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_AbsoluteSeek = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_RelativeSeek = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_SeekFromEnd = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_SeekMode cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_SeekMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_SeekMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_SeekMode cd i _) = error ("IO.SeekMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_SeekMode cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_SeekMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_SeekMode where
  (=?=) (Choice_C_SeekMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_SeekMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_SeekMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_SeekMode cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_SeekMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_SeekMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_SeekMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_SeekMode cd info) _ _ = failCons cd info
  (=?=) C_AbsoluteSeek C_AbsoluteSeek d cs = Curry_Prelude.C_True
  (=?=) C_RelativeSeek C_RelativeSeek d cs = Curry_Prelude.C_True
  (=?=) C_SeekFromEnd C_SeekFromEnd d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SeekMode cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_SeekMode cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_SeekMode cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_SeekMode cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_SeekMode cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_SeekMode cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_SeekMode cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_SeekMode cd info) _ _ = failCons cd info
  (<?=) C_AbsoluteSeek C_AbsoluteSeek d cs = Curry_Prelude.C_True
  (<?=) C_AbsoluteSeek C_RelativeSeek _ _ = Curry_Prelude.C_True
  (<?=) C_AbsoluteSeek C_SeekFromEnd _ _ = Curry_Prelude.C_True
  (<?=) C_RelativeSeek C_RelativeSeek d cs = Curry_Prelude.C_True
  (<?=) C_RelativeSeek C_SeekFromEnd _ _ = Curry_Prelude.C_True
  (<?=) C_SeekFromEnd C_SeekFromEnd d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_openFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_IOMode -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Handle
d_C_openFile x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_openFile) x1 x3250 x3500) x2 x3250 x3500

d_C_hClose :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hClose x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hClose x1 x3250 x3500

d_C_hFlush :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hFlush x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hFlush x1 x3250 x3500

d_C_hIsEOF :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsEOF x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsEOF x1 x3250 x3500

d_C_isEOF :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_isEOF x3250 x3500 = d_C_hIsEOF (d_C_stdin x3250 x3500) x3250 x3500

d_C_hSeek :: C_Handle -> C_SeekMode -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hSeek x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs (acceptCs id) d_C_prim_hSeek) x1 x3250 x3500) x2 x3250 x3500) x3 x3250 x3500

d_C_hWaitForInput :: C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hWaitForInput x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_hWaitForInput) x1 x3250 x3500) x2 x3250 x3500

d_C_hWaitForInputs :: Curry_Prelude.OP_List C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_hWaitForInputs x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_hWaitForInputs) x1 x3250 x3500) x2 x3250 x3500

d_C_hWaitForInputOrMsg :: Curry_Prelude.Curry t0 => C_Handle -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either C_Handle (Curry_Prelude.OP_List t0))
d_C_hWaitForInputOrMsg x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hWaitForInputsOrMsg (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x2 x3250 x3500) (d_OP_hWaitForInputOrMsg_dot___hash_lambda1 x1) x3250 x3500

d_OP_hWaitForInputOrMsg_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => C_Handle -> Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either C_Handle (Curry_Prelude.OP_List t0))
d_OP_hWaitForInputOrMsg_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_C_either (d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 x1) (acceptCs id Curry_Prelude.C_Right) x2 x3250 x3500) x3250 x3500

d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.Curry t0 => C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Either C_Handle t0
d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.C_Left x1

d_C_hWaitForInputsOrMsg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_Handle -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t0))
d_C_hWaitForInputsOrMsg x1 x2 x3250 x3500 = Curry_Prelude.d_C_seq (Curry_Prelude.d_C_normalForm (Curry_Prelude.d_C_map Curry_Prelude.d_C_ensureNotFree (Curry_Prelude.d_C_ensureSpine x1 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_prim_hWaitForInputsOrMsg x1 x2 x3250 x3500) x3250 x3500

d_C_hReady :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hReady x1 x3250 x3500 = d_C_hWaitForInput x1 (Curry_Prelude.C_Int 0#) x3250 x3500

d_C_hGetChar :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Char
d_C_hGetChar x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hGetChar x1 x3250 x3500

d_C_hGetLine :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_hGetLine x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetChar x1 x3250 x3500) (d_OP_hGetLine_dot___hash_lambda3 x1) x3250 x3500

d_OP_hGetLine_dot___hash_lambda3 :: C_Handle -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetLine_dot___hash_lambda3 x1 x2 x3250 x3500 = d_OP__case_0 x2 x1 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\n'#) x3250 x3500) x3250 x3500

d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.C_Char -> C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetLine x2 x3250 x3500) (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x1002 x3250 x3500) (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3250 x3500

d_C_hGetContents :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_hGetContents x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hIsEOF x1 x3250 x3500) (d_OP_hGetContents_dot___hash_lambda6 x1) x3250 x3500

d_OP_hGetContents_dot___hash_lambda6 :: C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda6 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_hClose x1 x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetChar x1 x3250 x3500) (d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7 x1) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_hGetContents_dot___hash_lambda6 x1 x1002 x3250 x3500) (d_OP_hGetContents_dot___hash_lambda6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_hGetContents_dot___hash_lambda6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_hGetContents_dot___hash_lambda6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7 :: C_Handle -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetContents x1 x3250 x3500) (d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x2) x3250 x3500

d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda6_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3250 x3500

d_C_getContents :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getContents x3250 x3500 = d_C_hGetContents (d_C_stdin x3250 x3500) x3250 x3500

d_C_hPutChar :: C_Handle -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutChar x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_hPutChar) x1 x3250 x3500) x2 x3250 x3500

d_C_hPutStr :: C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStr x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutChar x1 x3 x3250 x3500) (d_C_hPutStr x1 x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hPutStr x1 x1002 x3250 x3500) (d_C_hPutStr x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hPutStr x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hPutStr x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hPutStrLn :: C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStrLn x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_hPutStr x1 x2 x3250 x3500) (d_C_hPutChar x1 (Curry_Prelude.C_Char '\n'#) x3250 x3500) x3250 x3500

d_C_hPrint :: Curry_Prelude.Curry t0 => C_Handle -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPrint x1 x3250 x3500 = Curry_Prelude.d_OP_dot (d_C_hPutStrLn x1) Curry_Prelude.d_C_show x3250 x3500

nd_C_hPrint :: Curry_Prelude.Curry t0 => C_Handle -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_hPrint x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (d_C_hPutStrLn x1)) (wrapDX id Curry_Prelude.d_C_show) x2000 x3250 x3500))

d_C_hIsReadable :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsReadable x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsReadable x1 x3250 x3500

d_C_hIsWritable :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsWritable x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsWritable x1 x3250 x3500

d_OP__case_0 :: Curry_Prelude.C_Char -> C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_0 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hIsEOF x1 x3250 x3500) (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x2 x1) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_stdin :: Cover -> ConstStore -> C_Handle
d_C_stdin x3250 x3500 = external_d_C_stdin x3250 x3500

d_C_stdout :: Cover -> ConstStore -> C_Handle
d_C_stdout x3250 x3500 = external_d_C_stdout x3250 x3500

d_C_stderr :: Cover -> ConstStore -> C_Handle
d_C_stderr x3250 x3500 = external_d_C_stderr x3250 x3500

d_C_prim_openFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_IOMode -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Handle
d_C_prim_openFile x1 x2 x3250 x3500 = external_d_C_prim_openFile x1 x2 x3250 x3500

d_C_prim_hClose :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hClose x1 x3250 x3500 = external_d_C_prim_hClose x1 x3250 x3500

d_C_prim_hFlush :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hFlush x1 x3250 x3500 = external_d_C_prim_hFlush x1 x3250 x3500

d_C_prim_hIsEOF :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsEOF x1 x3250 x3500 = external_d_C_prim_hIsEOF x1 x3250 x3500

d_C_prim_hSeek :: C_Handle -> C_SeekMode -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hSeek x1 x2 x3 x3250 x3500 = external_d_C_prim_hSeek x1 x2 x3 x3250 x3500

d_C_prim_hWaitForInput :: C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hWaitForInput x1 x2 x3250 x3500 = external_d_C_prim_hWaitForInput x1 x2 x3250 x3500

d_C_prim_hWaitForInputs :: Curry_Prelude.OP_List C_Handle -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_prim_hWaitForInputs x1 x2 x3250 x3500 = external_d_C_prim_hWaitForInputs x1 x2 x3250 x3500

d_C_prim_hWaitForInputsOrMsg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_Handle -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t0))
d_C_prim_hWaitForInputsOrMsg x1 x2 x3250 x3500 = external_d_C_prim_hWaitForInputsOrMsg x1 x2 x3250 x3500

d_C_prim_hGetChar :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Char
d_C_prim_hGetChar x1 x3250 x3500 = external_d_C_prim_hGetChar x1 x3250 x3500

d_C_prim_hPutChar :: C_Handle -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hPutChar x1 x2 x3250 x3500 = external_d_C_prim_hPutChar x1 x2 x3250 x3500

d_C_prim_hIsReadable :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsReadable x1 x3250 x3500 = external_d_C_prim_hIsReadable x1 x3250 x3500

d_C_prim_hIsWritable :: C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsWritable x1 x3250 x3500 = external_d_C_prim_hIsWritable x1 x3250 x3500
type C_Handle = PrimData CurryHandle

instance ConvertCurryHaskell C_IOMode IOMode where
  toCurry ReadMode   = C_ReadMode
  toCurry WriteMode  = C_WriteMode
  toCurry AppendMode = C_AppendMode

  fromCurry C_ReadMode   = ReadMode
  fromCurry C_WriteMode  = WriteMode
  fromCurry C_AppendMode = AppendMode
  fromCurry _            = error "IOMode data with no ground term occurred"

instance ConvertCurryHaskell C_SeekMode SeekMode where
  toCurry AbsoluteSeek = C_AbsoluteSeek
  toCurry RelativeSeek = C_RelativeSeek
  toCurry SeekFromEnd  = C_SeekFromEnd

  fromCurry C_AbsoluteSeek = AbsoluteSeek
  fromCurry C_RelativeSeek = RelativeSeek
  fromCurry C_SeekFromEnd  = SeekFromEnd
  fromCurry _            = error "SeekMode data with no ground term occurred"


external_d_C_stdin :: Cover -> ConstStore -> C_Handle
external_d_C_stdin _ _ = PrimData (OneHandle stdin)

external_d_C_stdout :: Cover -> ConstStore -> C_Handle
external_d_C_stdout _ _ = PrimData (OneHandle stdout)

external_d_C_stderr :: Cover -> ConstStore -> C_Handle
external_d_C_stderr _ _ = PrimData (OneHandle stderr)

external_d_C_prim_openFile :: CP.OP_List CP.C_Char -> C_IOMode
                           -> Cover -> ConstStore -> CP.C_IO C_Handle
external_d_C_prim_openFile fn  mode _ _ =
  toCurry (\s m -> openFile s m >>= return . OneHandle) fn mode

external_d_C_prim_hClose :: C_Handle -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hClose handle _ _ = toCurry
  (\ch -> case ch of OneHandle h       -> hClose h
                     InOutHandle h1 h2 -> hClose h1 >> hClose h2) handle

external_d_C_prim_hFlush :: C_Handle -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hFlush h _ _ = toCurry (hFlush . outputHandle) h

external_d_C_prim_hIsEOF :: C_Handle -> Cover -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsEOF h _ _ = toCurry (hIsEOF . inputHandle) h

external_d_C_prim_hSeek :: C_Handle -> C_SeekMode -> CP.C_Int
                        -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hSeek handle mode i _ _
  = toCurry (hSeek . inputHandle) handle mode i

external_d_C_prim_hWaitForInput :: C_Handle -> CP.C_Int -> Cover -> ConstStore  
                                -> CP.C_IO CP.C_Bool
external_d_C_prim_hWaitForInput handle timeout _ _
  = toCurry (myhWaitForInput . inputHandle) handle timeout

myhWaitForInput :: Handle -> Int -> IO Bool
myhWaitForInput h timeout = C.catch (hWaitForInput h timeout) handler
  where
  handler :: C.IOException -> IO Bool
  handler e = if isEOFError e then return False else C.throw e

external_d_C_prim_hWaitForInputs :: CP.OP_List C_Handle -> CP.C_Int
                                 -> Cover -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_hWaitForInputs hs i _ _ = toCurry selectHandle hs i

selectHandle :: [CurryHandle] -> Int -> IO Int
selectHandle handles timeout = do
  mvar <- newEmptyMVar
  threads <- zipWithM
              (\ i h -> forkIO (waitOnHandle (inputHandle h) i timeout mvar))
              [0 ..] handles
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] -> IO Int
inspectRes 0 _    _       = return (-1)
inspectRes n mvar threads = do
  res <- takeMVar mvar
  case res of
    Nothing -> inspectRes (n - 1) mvar threads
    Just v  -> mapM_ killThread threads >> return v

waitOnHandle :: Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v timeout mvar = do
  ready <- myhWaitForInput h timeout
  putMVar mvar (if ready then Just v else Nothing)

external_d_C_prim_hWaitForInputsOrMsg ::
 CP.Curry a => CP.OP_List C_Handle -> CP.OP_List a -> Cover -> ConstStore 
               -> CP.C_IO (CP.C_Either CP.C_Int (CP.OP_List a))
external_d_C_prim_hWaitForInputsOrMsg = error "hWaitForInputsOrMsg undefined"

external_d_C_prim_hGetChar :: C_Handle -> Cover -> ConstStore -> CP.C_IO CP.C_Char
external_d_C_prim_hGetChar h _ _ = toCurry (hGetChar . inputHandle) h

external_d_C_prim_hPutChar :: C_Handle -> CP.C_Char -> Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hPutChar h c _ _ = toCurry (hPutChar . outputHandle) h c

external_d_C_prim_hIsReadable :: C_Handle -> Cover -> ConstStore 
                              -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsReadable h _ _ = toCurry (hIsReadable . inputHandle) h

external_d_C_prim_hIsWritable :: C_Handle -> Cover -> ConstStore 
                              -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsWritable h _ _ = toCurry (hIsWritable . outputHandle) h

