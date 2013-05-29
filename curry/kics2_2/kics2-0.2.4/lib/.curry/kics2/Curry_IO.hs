{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Curry_IO (C_Handle (..), C_IOMode (..), C_SeekMode (..), d_C_openFile, d_C_hClose, d_C_hFlush, d_C_hIsEOF, d_C_isEOF, d_C_hSeek, d_C_hWaitForInput, d_C_hWaitForInputs, d_C_hWaitForInputOrMsg, d_C_hWaitForInputsOrMsg, d_C_hReady, d_C_hGetChar, d_C_hGetLine, d_C_hGetContents, d_C_getContents, d_C_hPutChar, d_C_hPutStr, d_C_hPutStrLn, d_C_hPrint, nd_C_hPrint, d_C_hIsReadable, d_C_hIsWritable, d_C_stdin, d_C_stdout, d_C_stderr) where

import Basics
import qualified Curry_Prelude
import System.IO
import Control.Concurrent
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
  generate s = Choices_C_IOMode defCover (freeID [0,0,0] s) [C_ReadMode,C_WriteMode,C_AppendMode]


instance NormalForm C_IOMode where
  ($!!) cont C_ReadMode cs = cont C_ReadMode cs
  ($!!) cont C_WriteMode cs = cont C_WriteMode cs
  ($!!) cont C_AppendMode cs = cont C_AppendMode cs
  ($!!) cont (Choice_C_IOMode cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_IOMode cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_IOMode cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_IOMode cd info) _ = failCons cd info
  ($##) cont C_ReadMode cs = cont C_ReadMode cs
  ($##) cont C_WriteMode cs = cont C_WriteMode cs
  ($##) cont C_AppendMode cs = cont C_AppendMode cs
  ($##) cont (Choice_C_IOMode cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_IOMode cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_IOMode cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_IOMode cd info) _ = failCons cd info
  searchNF _ cont C_ReadMode = cont C_ReadMode
  searchNF _ cont C_WriteMode = cont C_WriteMode
  searchNF _ cont C_AppendMode = cont C_AppendMode
  searchNF _ _ x = error ("IO.IOMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_IOMode where
  (=.=) C_ReadMode C_ReadMode cs = C_Success
  (=.=) C_WriteMode C_WriteMode cs = C_Success
  (=.=) C_AppendMode C_AppendMode cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_ReadMode C_ReadMode cs = C_Success
  (=.<=) C_WriteMode C_WriteMode cs = C_Success
  (=.<=) C_AppendMode C_AppendMode cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_ReadMode = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_WriteMode = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_AppendMode = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (Choice_C_IOMode cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_IOMode cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_IOMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_IOMode cd i _) = error ("IO.IOMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_IOMode cd info) = [(Unsolvable info)]
  bind i (Guard_C_IOMode cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_ReadMode = [(i :=: (ChooseN 0 0))]
  lazyBind i C_WriteMode = [(i :=: (ChooseN 1 0))]
  lazyBind i C_AppendMode = [(i :=: (ChooseN 2 0))]
  lazyBind i (Choice_C_IOMode cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_IOMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_IOMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_IOMode cd i _) = error ("IO.IOMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_IOMode cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_IOMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_IOMode where
  (=?=) (Choice_C_IOMode cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_IOMode cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_IOMode cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_IOMode cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_IOMode cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_IOMode cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_IOMode cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_IOMode cd info) _ = failCons cd info
  (=?=) C_ReadMode C_ReadMode cs = Curry_Prelude.C_True
  (=?=) C_WriteMode C_WriteMode cs = Curry_Prelude.C_True
  (=?=) C_AppendMode C_AppendMode cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_IOMode cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_IOMode cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_IOMode cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_IOMode cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_IOMode cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_IOMode cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_IOMode cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_IOMode cd info) _ = failCons cd info
  (<?=) C_ReadMode C_ReadMode cs = Curry_Prelude.C_True
  (<?=) C_ReadMode C_WriteMode _ = Curry_Prelude.C_True
  (<?=) C_ReadMode C_AppendMode _ = Curry_Prelude.C_True
  (<?=) C_WriteMode C_WriteMode cs = Curry_Prelude.C_True
  (<?=) C_WriteMode C_AppendMode _ = Curry_Prelude.C_True
  (<?=) C_AppendMode C_AppendMode cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_IOMode where
  cover C_ReadMode = C_ReadMode
  cover C_WriteMode = C_WriteMode
  cover C_AppendMode = C_AppendMode
  cover (Choice_C_IOMode cd i x y) = Choice_C_IOMode (incCover cd) i (cover x) (cover y)
  cover (Choices_C_IOMode cd i xs) = Choices_C_IOMode (incCover cd) i (map cover xs)
  cover (Fail_C_IOMode cd info) = Fail_C_IOMode (incCover cd) info
  cover (Guard_C_IOMode cd c e) = Guard_C_IOMode (incCover cd) c (cover e)


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
  generate s = Choices_C_SeekMode defCover (freeID [0,0,0] s) [C_AbsoluteSeek,C_RelativeSeek,C_SeekFromEnd]


instance NormalForm C_SeekMode where
  ($!!) cont C_AbsoluteSeek cs = cont C_AbsoluteSeek cs
  ($!!) cont C_RelativeSeek cs = cont C_RelativeSeek cs
  ($!!) cont C_SeekFromEnd cs = cont C_SeekFromEnd cs
  ($!!) cont (Choice_C_SeekMode cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_SeekMode cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_SeekMode cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_SeekMode cd info) _ = failCons cd info
  ($##) cont C_AbsoluteSeek cs = cont C_AbsoluteSeek cs
  ($##) cont C_RelativeSeek cs = cont C_RelativeSeek cs
  ($##) cont C_SeekFromEnd cs = cont C_SeekFromEnd cs
  ($##) cont (Choice_C_SeekMode cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_SeekMode cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_SeekMode cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_SeekMode cd info) _ = failCons cd info
  searchNF _ cont C_AbsoluteSeek = cont C_AbsoluteSeek
  searchNF _ cont C_RelativeSeek = cont C_RelativeSeek
  searchNF _ cont C_SeekFromEnd = cont C_SeekFromEnd
  searchNF _ _ x = error ("IO.SeekMode.searchNF: no constructor: " ++ (show x))


instance Unifiable C_SeekMode where
  (=.=) C_AbsoluteSeek C_AbsoluteSeek cs = C_Success
  (=.=) C_RelativeSeek C_RelativeSeek cs = C_Success
  (=.=) C_SeekFromEnd C_SeekFromEnd cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_AbsoluteSeek C_AbsoluteSeek cs = C_Success
  (=.<=) C_RelativeSeek C_RelativeSeek cs = C_Success
  (=.<=) C_SeekFromEnd C_SeekFromEnd cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_AbsoluteSeek = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_RelativeSeek = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_SeekFromEnd = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (Choice_C_SeekMode cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_SeekMode cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_SeekMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_SeekMode cd i _) = error ("IO.SeekMode.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_SeekMode cd info) = [(Unsolvable info)]
  bind i (Guard_C_SeekMode cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_AbsoluteSeek = [(i :=: (ChooseN 0 0))]
  lazyBind i C_RelativeSeek = [(i :=: (ChooseN 1 0))]
  lazyBind i C_SeekFromEnd = [(i :=: (ChooseN 2 0))]
  lazyBind i (Choice_C_SeekMode cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_SeekMode cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_SeekMode cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_SeekMode cd i _) = error ("IO.SeekMode.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_SeekMode cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_SeekMode cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_SeekMode where
  (=?=) (Choice_C_SeekMode cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_SeekMode cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_SeekMode cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_SeekMode cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_SeekMode cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_SeekMode cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_SeekMode cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_SeekMode cd info) _ = failCons cd info
  (=?=) C_AbsoluteSeek C_AbsoluteSeek cs = Curry_Prelude.C_True
  (=?=) C_RelativeSeek C_RelativeSeek cs = Curry_Prelude.C_True
  (=?=) C_SeekFromEnd C_SeekFromEnd cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SeekMode cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_SeekMode cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_SeekMode cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_SeekMode cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_SeekMode cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_SeekMode cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_SeekMode cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_SeekMode cd info) _ = failCons cd info
  (<?=) C_AbsoluteSeek C_AbsoluteSeek cs = Curry_Prelude.C_True
  (<?=) C_AbsoluteSeek C_RelativeSeek _ = Curry_Prelude.C_True
  (<?=) C_AbsoluteSeek C_SeekFromEnd _ = Curry_Prelude.C_True
  (<?=) C_RelativeSeek C_RelativeSeek cs = Curry_Prelude.C_True
  (<?=) C_RelativeSeek C_SeekFromEnd _ = Curry_Prelude.C_True
  (<?=) C_SeekFromEnd C_SeekFromEnd cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_SeekMode where
  cover C_AbsoluteSeek = C_AbsoluteSeek
  cover C_RelativeSeek = C_RelativeSeek
  cover C_SeekFromEnd = C_SeekFromEnd
  cover (Choice_C_SeekMode cd i x y) = Choice_C_SeekMode (incCover cd) i (cover x) (cover y)
  cover (Choices_C_SeekMode cd i xs) = Choices_C_SeekMode (incCover cd) i (map cover xs)
  cover (Fail_C_SeekMode cd info) = Fail_C_SeekMode (incCover cd) info
  cover (Guard_C_SeekMode cd c e) = Guard_C_SeekMode (incCover cd) c (cover e)


d_C_openFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_IOMode -> ConstStore -> Curry_Prelude.C_IO C_Handle
d_C_openFile x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_openFile) x1 x3500) x2 x3500

d_C_hClose :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hClose x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hClose x1 x3500

d_C_hFlush :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hFlush x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hFlush x1 x3500

d_C_hIsEOF :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsEOF x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsEOF x1 x3500

d_C_isEOF :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_isEOF x3500 = d_C_hIsEOF (d_C_stdin x3500) x3500

d_C_hSeek :: C_Handle -> C_SeekMode -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hSeek x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs (acceptCs id) d_C_prim_hSeek) x1 x3500) x2 x3500) x3 x3500

d_C_hWaitForInput :: C_Handle -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hWaitForInput x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_hWaitForInput) x1 x3500) x2 x3500

d_C_hWaitForInputs :: Curry_Prelude.OP_List C_Handle -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_hWaitForInputs x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_hWaitForInputs) x1 x3500) x2 x3500

d_C_hWaitForInputOrMsg :: Curry_Prelude.Curry t0 => C_Handle -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either C_Handle (Curry_Prelude.OP_List t0))
d_C_hWaitForInputOrMsg x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hWaitForInputsOrMsg (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x2 x3500) (d_OP_hWaitForInputOrMsg_dot___hash_lambda1 x1) x3500

d_OP_hWaitForInputOrMsg_dot___hash_lambda1 :: Curry_Prelude.Curry t52 => C_Handle -> Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t52) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either C_Handle (Curry_Prelude.OP_List t52))
d_OP_hWaitForInputOrMsg_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_C_either (d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 x1) (acceptCs id Curry_Prelude.C_Right) x2 x3500) x3500

d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.Curry t53 => C_Handle -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Either C_Handle t53
d_OP_hWaitForInputOrMsg_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3500 = Curry_Prelude.C_Left x1

d_C_hWaitForInputsOrMsg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_Handle -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t0))
d_C_hWaitForInputsOrMsg x1 x2 x3500 = Curry_Prelude.d_C_seq (Curry_Prelude.d_C_normalForm (Curry_Prelude.d_C_map Curry_Prelude.d_C_ensureNotFree (Curry_Prelude.d_C_ensureSpine x1 x3500) x3500) x3500) (d_C_prim_hWaitForInputsOrMsg x1 x2 x3500) x3500

d_C_hReady :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hReady x1 x3500 = d_C_hWaitForInput x1 (Curry_Prelude.C_Int 0#) x3500

d_C_hGetChar :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Char
d_C_hGetChar x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hGetChar x1 x3500

d_C_hGetLine :: C_Handle -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_hGetLine x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetChar x1 x3500) (d_OP_hGetLine_dot___hash_lambda3 x1) x3500

d_OP_hGetLine_dot___hash_lambda3 :: C_Handle -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetLine_dot___hash_lambda3 x1 x2 x3500 = d_OP__case_0 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\n'#) x3500) x3500

d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3500

d_C_hGetContents :: C_Handle -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_hGetContents x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hIsEOF x1 x3500) (d_OP_hGetContents_dot___hash_lambda5 x1) x3500

d_OP_hGetContents_dot___hash_lambda5 :: C_Handle -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda5 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_hClose x1 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetChar x1 x3500) (d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6 x1) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_hGetContents_dot___hash_lambda5 x1 x1002 x3500) (d_OP_hGetContents_dot___hash_lambda5 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_hGetContents_dot___hash_lambda5 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_hGetContents_dot___hash_lambda5 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6 :: C_Handle -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetContents x1 x3500) (d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x2) x3500

d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_hGetContents_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3500

d_C_getContents :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getContents x3500 = d_C_hGetContents (d_C_stdin x3500) x3500

d_C_hPutChar :: C_Handle -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutChar x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash (acceptCs id d_C_prim_hPutChar) x1 x3500) x2 x3500

d_C_hPutStr :: C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStr x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutChar x1 x3 x3500) (d_C_hPutStr x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hPutStr x1 x1002 x3500) (d_C_hPutStr x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hPutStr x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hPutStr x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hPutStrLn :: C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStrLn x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_hPutStr x1 x2 x3500) (d_C_hPutChar x1 (Curry_Prelude.C_Char '\n'#) x3500) x3500

d_C_hPrint :: Curry_Prelude.Curry t0 => C_Handle -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPrint x1 x3500 = Curry_Prelude.d_OP_dot (d_C_hPutStrLn x1) Curry_Prelude.d_C_show x3500

nd_C_hPrint :: Curry_Prelude.Curry t0 => C_Handle -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_hPrint x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (d_C_hPutStrLn x1)) (wrapDX id Curry_Prelude.d_C_show) x2000 x3500))

d_C_hIsReadable :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsReadable x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsReadable x1 x3500

d_C_hIsWritable :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_hIsWritable x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_hIsWritable x1 x3500

d_OP__case_0 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_hGetLine x1 x3500) (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x2) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x1002 x3500) (d_OP__case_0 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_hGetLine x1 x3500) (wrapDX id (d_OP_hGetLine_dot___hash_lambda3_dot___hash_lambda4 x2)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_stdin :: ConstStore -> C_Handle
d_C_stdin x3500 = external_d_C_stdin x3500

d_C_stdout :: ConstStore -> C_Handle
d_C_stdout x3500 = external_d_C_stdout x3500

d_C_stderr :: ConstStore -> C_Handle
d_C_stderr x3500 = external_d_C_stderr x3500

d_C_prim_openFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_IOMode -> ConstStore -> Curry_Prelude.C_IO C_Handle
d_C_prim_openFile x1 x2 x3500 = external_d_C_prim_openFile x1 x2 x3500

d_C_prim_hClose :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hClose x1 x3500 = external_d_C_prim_hClose x1 x3500

d_C_prim_hFlush :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hFlush x1 x3500 = external_d_C_prim_hFlush x1 x3500

d_C_prim_hIsEOF :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsEOF x1 x3500 = external_d_C_prim_hIsEOF x1 x3500

d_C_prim_hSeek :: C_Handle -> C_SeekMode -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hSeek x1 x2 x3 x3500 = external_d_C_prim_hSeek x1 x2 x3 x3500

d_C_prim_hWaitForInput :: C_Handle -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hWaitForInput x1 x2 x3500 = external_d_C_prim_hWaitForInput x1 x2 x3500

d_C_prim_hWaitForInputs :: Curry_Prelude.OP_List C_Handle -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_prim_hWaitForInputs x1 x2 x3500 = external_d_C_prim_hWaitForInputs x1 x2 x3500

d_C_prim_hWaitForInputsOrMsg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List C_Handle -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either Curry_Prelude.C_Int (Curry_Prelude.OP_List t0))
d_C_prim_hWaitForInputsOrMsg x1 x2 x3500 = external_d_C_prim_hWaitForInputsOrMsg x1 x2 x3500

d_C_prim_hGetChar :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Char
d_C_prim_hGetChar x1 x3500 = external_d_C_prim_hGetChar x1 x3500

d_C_prim_hPutChar :: C_Handle -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_hPutChar x1 x2 x3500 = external_d_C_prim_hPutChar x1 x2 x3500

d_C_prim_hIsReadable :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsReadable x1 x3500 = external_d_C_prim_hIsReadable x1 x3500

d_C_prim_hIsWritable :: C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_hIsWritable x1 x3500 = external_d_C_prim_hIsWritable x1 x3500
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


external_d_C_stdin :: ConstStore -> C_Handle
external_d_C_stdin _ = PrimData (OneHandle stdin)

external_d_C_stdout :: ConstStore -> C_Handle
external_d_C_stdout _ = PrimData (OneHandle stdout)

external_d_C_stderr :: ConstStore -> C_Handle
external_d_C_stderr _ = PrimData (OneHandle stderr)

external_d_C_prim_openFile :: CP.OP_List CP.C_Char -> C_IOMode
                           -> ConstStore -> CP.C_IO C_Handle
external_d_C_prim_openFile fn  mode _ =
  toCurry (\s m -> openFile s m >>= return . OneHandle) fn mode

external_d_C_prim_hClose :: C_Handle -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hClose handle _ = toCurry
  (\ch -> case ch of OneHandle h       -> hClose h
                     InOutHandle h1 h2 -> hClose h1 >> hClose h2) handle

external_d_C_prim_hFlush :: C_Handle -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hFlush h _ = toCurry (hFlush . outputHandle) h

external_d_C_prim_hIsEOF :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsEOF h _ = toCurry (hIsEOF . inputHandle) h

external_d_C_prim_hSeek :: C_Handle -> C_SeekMode -> CP.C_Int
                        -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hSeek handle mode i _ = toCurry (hSeek . inputHandle) handle mode i


external_d_C_prim_hWaitForInput :: C_Handle -> CP.C_Int -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hWaitForInput handle i _=
  toCurry (myhWaitForInput . inputHandle) handle i
  where

myhWaitForInput :: Handle -> Int -> IO Bool
myhWaitForInput h i =
  if i < 0
  then hIsEOF h >>= return . not
  else hWaitForInput h i


external_d_C_prim_hWaitForInputs :: CP.OP_List C_Handle -> CP.C_Int
                                 -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_hWaitForInputs hs i _ = toCurry selectHandle hs i

selectHandle :: [CurryHandle] -> Int -> IO Int
selectHandle handles t = do
  mvar <- newEmptyMVar
  threads <- mapM (\ (i,h) -> forkIO (waitOnHandle (inputHandle h) i t mvar))
                  (zip [0..] handles)
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] -> IO Int
inspectRes 0 _    _       = return (-1)
inspectRes n mvar threads = do
  res <- readMVar mvar
  case res of
    Nothing -> inspectRes (n-1) mvar threads
    Just v  -> mapM_ killThread threads >> return v

waitOnHandle :: Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v t mvar = do
   	    ready <- myhWaitForInput h t
  	    putMVar mvar (if ready then Just v else Nothing)

external_d_C_prim_hWaitForInputsOrMsg ::
 CP.Curry a => CP.OP_List C_Handle -> CP.OP_List a
            -> ConstStore -> CP.C_IO (CP.C_Either CP.C_Int (CP.OP_List a))
external_d_C_prim_hWaitForInputsOrMsg = error "hWaitForInputsOrMsg undefined"

external_d_C_prim_hGetChar :: C_Handle -> ConstStore -> CP.C_IO CP.C_Char
external_d_C_prim_hGetChar h _ = toCurry (hGetChar . inputHandle) h

external_d_C_prim_hPutChar :: C_Handle -> CP.C_Char -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_hPutChar h c _ = toCurry (hPutChar . outputHandle) h c

external_d_C_prim_hIsReadable :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsReadable h _ = toCurry (hIsReadable . inputHandle) h

external_d_C_prim_hIsWritable :: C_Handle -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_hIsWritable h _ = toCurry (hIsWritable . outputHandle) h

