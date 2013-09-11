{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE BangPatterns, MagicHash, MultiParamTypeClasses, ScopedTypeVariables #-}


module Curry_Prelude (Curry (..), OP_Unit (..), OP_List (..), OP_Tuple2 (..), OP_Tuple3 (..), OP_Tuple4 (..), OP_Tuple5 (..), OP_Tuple6 (..), OP_Tuple7 (..), OP_Tuple8 (..), OP_Tuple9 (..), OP_Tuple10 (..), OP_Tuple11 (..), OP_Tuple12 (..), OP_Tuple13 (..), OP_Tuple14 (..), OP_Tuple15 (..), C_Int (..), C_Float (..), C_Char (..), C_Bool (..), C_Ordering (..), C_Success (..), C_Maybe (..), C_Either (..), C_IO (..), C_IOError (..), C_String, d_OP_dot, nd_OP_dot, d_C_id, d_C_const, d_C_curry, nd_C_curry, d_C_uncurry, nd_C_uncurry, d_C_flip, nd_C_flip, d_C_until, nd_C_until, d_C_seq, d_C_ensureSpine, d_OP_dollar, nd_OP_dollar, d_OP_dollar_hash, nd_OP_dollar_hash, d_C_error, d_OP_ampersand_ampersand, d_OP_bar_bar, d_C_not, d_C_otherwise, d_C_if_then_else, d_OP_slash_eq, d_C_compare, d_OP_lt, d_OP_gt, d_OP_gt_eq, d_C_max, d_C_min, d_C_fst, d_C_snd, d_C_head, d_C_tail, d_C_null, d_OP_plus_plus, d_C_length, d_OP_bang_bang, d_C_map, nd_C_map, d_C_foldl, nd_C_foldl, d_C_foldl1, nd_C_foldl1, d_C_foldr, nd_C_foldr, d_C_foldr1, nd_C_foldr1, d_C_filter, nd_C_filter, d_C_zip, d_C_zip3, d_C_zipWith, nd_C_zipWith, d_C_zipWith3, nd_C_zipWith3, d_C_unzip, d_C_unzip3, d_C_concat, d_C_concatMap, nd_C_concatMap, d_C_iterate, nd_C_iterate, d_C_repeat, d_C_replicate, d_C_take, d_C_drop, d_C_splitAt, d_C_takeWhile, nd_C_takeWhile, d_C_dropWhile, nd_C_dropWhile, d_C_span, nd_C_span, d_C_break, nd_C_break, d_C_lines, d_C_unlines, d_C_words, d_C_unwords, d_C_reverse, nd_C_reverse, d_C_and, nd_C_and, d_C_or, nd_C_or, d_C_any, nd_C_any, d_C_all, nd_C_all, d_C_elem, nd_C_elem, d_C_notElem, nd_C_notElem, d_C_lookup, d_C_enumFrom, d_C_enumFromThen, d_C_enumFromTo, d_C_enumFromThenTo, d_C_ord, d_C_chr, d_C_negate, d_OP_ampersand_gt, d_C_maybe, nd_C_maybe, d_C_either, nd_C_either, d_OP_gt_gt, d_C_done, d_C_putChar, d_C_readFile, d_C_writeFile, d_C_appendFile, d_C_putStr, d_C_putStrLn, d_C_getLine, d_C_userError, d_C_ioError, d_C_showError, d_C_show, d_C_print, d_C_doSolve, d_C_sequenceIO, d_C_sequenceIO_, nd_C_sequenceIO_, d_C_mapIO, nd_C_mapIO, d_C_mapIO_, nd_C_mapIO_, d_C_foldIO, nd_C_foldIO, d_C_liftIO, nd_C_liftIO, nd_C_unknown, d_C_normalForm, d_C_groundNormalForm, d_C_ensureNotFree, d_OP_dollar_bang, nd_OP_dollar_bang, d_OP_dollar_bang_bang, nd_OP_dollar_bang_bang, d_OP_dollar_hash_hash, nd_OP_dollar_hash_hash, d_C_prim_error, d_C_failed, d_OP_eq_eq, d_OP_lt_eq, d_C_prim_ord, d_C_prim_chr, d_OP_plus, d_OP_minus, d_OP_star, d_C_div, d_C_mod, d_C_divMod, d_C_quot, d_C_rem, d_C_quotRem, d_C_negateFloat, d_OP_eq_colon_eq, d_C_success, d_OP_ampersand, d_OP_gt_gt_eq, nd_OP_gt_gt_eq, d_C_return, d_C_prim_putChar, d_C_getChar, d_C_prim_readFile, d_C_prim_writeFile, d_C_prim_appendFile, d_C_prim_ioError, d_C_catch, nd_C_catch, d_C_prim_show, nd_OP_qmark, d_C_apply, nd_C_apply, d_C_cond, d_OP_eq_colon_lt_eq) where

import Basics
import qualified Control.Exception as C

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import GHC.Exts (Int (I#), Int#, (==#), (/=#), (<#), (>#), (<=#))
import GHC.Exts ((+#), (-#), (*#), quotInt#, remInt#, negateInt#)
import GHC.Exts (Float (F#), Float#, eqFloat#, leFloat#, negateFloat#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#, ord#, chr#)
import System.IO

import Debug
import CurryException
import PrimTypes

-- ---------------------------------------------------------------------------
-- Externals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for Curry types



data OP_Unit
     = OP_Unit
     | Choice_OP_Unit Cover ID OP_Unit OP_Unit
     | Choices_OP_Unit Cover ID ([OP_Unit])
     | Fail_OP_Unit Cover FailInfo
     | Guard_OP_Unit Cover Constraints OP_Unit

instance Show OP_Unit where
  showsPrec d (Choice_OP_Unit cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Unit cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Unit cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Unit cd info) = showChar '!'
  showsPrec _ OP_Unit = showString "()"


instance Read OP_Unit where
  readsPrec _ s = readParen False (\r -> [ (OP_Unit,r0) | (_,r0) <- readQualified "Prelude" "()" r]) s


instance NonDet OP_Unit where
  choiceCons = Choice_OP_Unit
  choicesCons = Choices_OP_Unit
  failCons = Fail_OP_Unit
  guardCons = Guard_OP_Unit
  try (Choice_OP_Unit cd i x y) = tryChoice cd i x y
  try (Choices_OP_Unit cd i xs) = tryChoices cd i xs
  try (Fail_OP_Unit cd info) = Fail cd info
  try (Guard_OP_Unit cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Unit cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Unit cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Unit cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Unit cd i _) = error ("Prelude.().match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Unit cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Unit cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable OP_Unit where
  generate s c = Choices_OP_Unit c (freeID [0] s) [OP_Unit]


instance NormalForm OP_Unit where
  ($!!) cont OP_Unit d cs = cont OP_Unit d cs
  ($!!) cont (Choice_OP_Unit cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Unit cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Unit cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Unit cd info) _ _ = failCons cd info
  ($##) cont OP_Unit d cs = cont OP_Unit d cs
  ($##) cont (Choice_OP_Unit cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Unit cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Unit cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Unit cd info) _ _ = failCons cd info
  searchNF _ cont OP_Unit = cont OP_Unit
  searchNF _ _ x = error ("Prelude.().searchNF: no constructor: " ++ (show x))


instance Unifiable OP_Unit where
  (=.=) OP_Unit OP_Unit d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) OP_Unit OP_Unit d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i OP_Unit = ((i :=: (ChooseN 0 0)):(concat []))
  bind d i (Choice_OP_Unit cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Unit cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Unit cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Unit cd i _) = error ("Prelude.().bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Unit cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Unit cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i OP_Unit = [(i :=: (ChooseN 0 0))]
  lazyBind d i (Choice_OP_Unit cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Unit cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Unit cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Unit cd i _) = error ("Prelude.().lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Unit cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Unit cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry OP_Unit where
  (=?=) (Choice_OP_Unit cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Unit cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Unit cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Unit cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Unit cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Unit cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Unit cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Unit cd info) _ _ = failCons cd info
  (=?=) OP_Unit OP_Unit d cs = C_True
  (<?=) (Choice_OP_Unit cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Unit cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Unit cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Unit cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Unit cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Unit cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Unit cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Unit cd info) _ _ = failCons cd info
  (<?=) OP_Unit OP_Unit d cs = C_True


data OP_List t0
     = OP_List
     | OP_Cons t0 (OP_List t0)
     | Choice_OP_List Cover ID (OP_List t0) (OP_List t0)
     | Choices_OP_List Cover ID ([OP_List t0])
     | Fail_OP_List Cover FailInfo
     | Guard_OP_List Cover Constraints (OP_List t0)

instance Show t0 => Show (OP_List t0) where
  showsPrec = showsPrec4CurryList


instance Read t0 => Read (OP_List t0) where
  readsPrec d s = map readList (readsPrec d s)
   where
     readList (xs,s2) = (foldr OP_Cons OP_List xs,s2)


instance NonDet (OP_List t0) where
  choiceCons = Choice_OP_List
  choicesCons = Choices_OP_List
  failCons = Fail_OP_List
  guardCons = Guard_OP_List
  try (Choice_OP_List cd i x y) = tryChoice cd i x y
  try (Choices_OP_List cd i xs) = tryChoices cd i xs
  try (Fail_OP_List cd info) = Fail cd info
  try (Guard_OP_List cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_List cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_List cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_List cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_List cd i _) = error ("Prelude.[].match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_List cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_List cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (OP_List t0) where
  generate s c = Choices_OP_List c (freeID [0,2] s) [OP_List,(OP_Cons (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (OP_List t0) where
  ($!!) cont OP_List d cs = cont OP_List d cs
  ($!!) cont (OP_Cons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (OP_Cons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_List cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_List cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_List cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_List cd info) _ _ = failCons cd info
  ($##) cont OP_List d cs = cont OP_List d cs
  ($##) cont (OP_Cons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (OP_Cons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_List cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_List cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_List cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_List cd info) _ _ = failCons cd info
  searchNF _ cont OP_List = cont OP_List
  searchNF search cont (OP_Cons x1 x2) = search (\y1 -> search (\y2 -> cont (OP_Cons y1 y2)) x2) x1
  searchNF _ _ x = error ("Prelude.[].searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (OP_List t0) where
  (=.=) OP_List OP_List d cs = C_Success
  (=.=) (OP_Cons x1 x2) (OP_Cons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) OP_List OP_List d cs = C_Success
  (=.<=) (OP_Cons x1 x2) (OP_Cons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i OP_List = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (OP_Cons x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_OP_List cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_List cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_List cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_List cd i _) = error ("Prelude.[].bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_List cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_List cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i OP_List = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (OP_Cons x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_OP_List cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_List cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_List cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_List cd i _) = error ("Prelude.[].lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_List cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_List cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry t0 => Curry (OP_List t0) where
  (=?=) (Choice_OP_List cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_List cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_List cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_List cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_List cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_List cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_List cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_List cd info) _ _ = failCons cd info
  (=?=) OP_List OP_List d cs = C_True
  (=?=) (OP_Cons x1 x2) (OP_Cons y1 y2) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (((x2 =?= y2) d) cs) d cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_OP_List cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_List cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_List cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_List cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_List cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_List cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_List cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_List cd info) _ _ = failCons cd info
  (<?=) OP_List OP_List d cs = C_True
  (<?=) OP_List (OP_Cons _ _) _ _ = C_True
  (<?=) (OP_Cons x1 x2) (OP_Cons y1 y2) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (((x2 <?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = C_False


data OP_Tuple2 t0 t1
     = OP_Tuple2 t0 t1
     | Choice_OP_Tuple2 Cover ID (OP_Tuple2 t0 t1) (OP_Tuple2 t0 t1)
     | Choices_OP_Tuple2 Cover ID ([OP_Tuple2 t0 t1])
     | Fail_OP_Tuple2 Cover FailInfo
     | Guard_OP_Tuple2 Cover Constraints (OP_Tuple2 t0 t1)

instance (Show t0,Show t1) => Show (OP_Tuple2 t0 t1) where
  showsPrec d (Choice_OP_Tuple2 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple2 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple2 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple2 cd info) = showChar '!'
  showsPrec _ (OP_Tuple2 x1 x2) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . (showChar ')'))))


instance (Read t0,Read t1) => Read (OP_Tuple2 t0 t1) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2),s2) = (OP_Tuple2 x1 x2,s2)


instance NonDet (OP_Tuple2 t0 t1) where
  choiceCons = Choice_OP_Tuple2
  choicesCons = Choices_OP_Tuple2
  failCons = Fail_OP_Tuple2
  guardCons = Guard_OP_Tuple2
  try (Choice_OP_Tuple2 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple2 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple2 cd info) = Fail cd info
  try (Guard_OP_Tuple2 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple2 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple2 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple2 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple2 cd i _) = error ("Prelude.OP_Tuple2.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple2 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple2 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1) => Generable (OP_Tuple2 t0 t1) where
  generate s c = Choices_OP_Tuple2 c (freeID [2] s) [(OP_Tuple2 (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance (NormalForm t0,NormalForm t1) => NormalForm (OP_Tuple2 t0 t1) where
  ($!!) cont (OP_Tuple2 x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (OP_Tuple2 y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple2 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple2 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple2 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple2 x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (OP_Tuple2 y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple2 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple2 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple2 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple2 x1 x2) = search (\y1 -> search (\y2 -> cont (OP_Tuple2 y1 y2)) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple2.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1) => Unifiable (OP_Tuple2 t0 t1) where
  (=.=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple2 x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_OP_Tuple2 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple2 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple2 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple2 cd i _) = error ("Prelude.OP_Tuple2.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple2 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple2 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple2 x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_OP_Tuple2 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple2 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple2 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple2 cd i _) = error ("Prelude.OP_Tuple2.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple2 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple2 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1) => Curry (OP_Tuple2 t0 t1) where
  (=?=) (Choice_OP_Tuple2 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple2 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple2 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple2 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple2 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple2 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple2 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (((x2 =?= y2) d) cs) d cs
  (<?=) (Choice_OP_Tuple2 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple2 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple2 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple2 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple2 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple2 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple2 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (((x2 <?= y2) d) cs) d cs) d cs


data OP_Tuple3 t0 t1 t2
     = OP_Tuple3 t0 t1 t2
     | Choice_OP_Tuple3 Cover ID (OP_Tuple3 t0 t1 t2) (OP_Tuple3 t0 t1 t2)
     | Choices_OP_Tuple3 Cover ID ([OP_Tuple3 t0 t1 t2])
     | Fail_OP_Tuple3 Cover FailInfo
     | Guard_OP_Tuple3 Cover Constraints (OP_Tuple3 t0 t1 t2)

instance (Show t0,Show t1,Show t2) => Show (OP_Tuple3 t0 t1 t2) where
  showsPrec d (Choice_OP_Tuple3 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple3 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple3 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple3 cd info) = showChar '!'
  showsPrec _ (OP_Tuple3 x1 x2 x3) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . (showChar ')'))))))


instance (Read t0,Read t1,Read t2) => Read (OP_Tuple3 t0 t1 t2) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3),s2) = (OP_Tuple3 x1 x2 x3,s2)


instance NonDet (OP_Tuple3 t0 t1 t2) where
  choiceCons = Choice_OP_Tuple3
  choicesCons = Choices_OP_Tuple3
  failCons = Fail_OP_Tuple3
  guardCons = Guard_OP_Tuple3
  try (Choice_OP_Tuple3 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple3 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple3 cd info) = Fail cd info
  try (Guard_OP_Tuple3 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple3 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple3 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple3 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple3 cd i _) = error ("Prelude.OP_Tuple3.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple3 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple3 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2) => Generable (OP_Tuple3 t0 t1 t2) where
  generate s c = Choices_OP_Tuple3 c (freeID [3] s) [(OP_Tuple3 (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2) => NormalForm (OP_Tuple3 t0 t1 t2) where
  ($!!) cont (OP_Tuple3 x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (OP_Tuple3 y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple3 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple3 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple3 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple3 x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (OP_Tuple3 y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple3 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple3 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple3 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple3 x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (OP_Tuple3 y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple3.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2) => Unifiable (OP_Tuple3 t0 t1 t2) where
  (=.=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple3 x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_OP_Tuple3 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple3 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple3 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple3 cd i _) = error ("Prelude.OP_Tuple3.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple3 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple3 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple3 x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_OP_Tuple3 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple3 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple3 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple3 cd i _) = error ("Prelude.OP_Tuple3.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple3 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple3 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2) => Curry (OP_Tuple3 t0 t1 t2) where
  (=?=) (Choice_OP_Tuple3 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple3 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple3 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple3 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple3 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple3 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple3 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (((x3 =?= y3) d) cs) d cs) d cs
  (<?=) (Choice_OP_Tuple3 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple3 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple3 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple3 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple3 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple3 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple3 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (((x3 <?= y3) d) cs) d cs) d cs) d cs) d cs


data OP_Tuple4 t0 t1 t2 t3
     = OP_Tuple4 t0 t1 t2 t3
     | Choice_OP_Tuple4 Cover ID (OP_Tuple4 t0 t1 t2 t3) (OP_Tuple4 t0 t1 t2 t3)
     | Choices_OP_Tuple4 Cover ID ([OP_Tuple4 t0 t1 t2 t3])
     | Fail_OP_Tuple4 Cover FailInfo
     | Guard_OP_Tuple4 Cover Constraints (OP_Tuple4 t0 t1 t2 t3)

instance (Show t0,Show t1,Show t2,Show t3) => Show (OP_Tuple4 t0 t1 t2 t3) where
  showsPrec d (Choice_OP_Tuple4 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple4 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple4 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple4 cd info) = showChar '!'
  showsPrec _ (OP_Tuple4 x1 x2 x3 x4) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . (showChar ')'))))))))


instance (Read t0,Read t1,Read t2,Read t3) => Read (OP_Tuple4 t0 t1 t2 t3) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4),s2) = (OP_Tuple4 x1 x2 x3 x4,s2)


instance NonDet (OP_Tuple4 t0 t1 t2 t3) where
  choiceCons = Choice_OP_Tuple4
  choicesCons = Choices_OP_Tuple4
  failCons = Fail_OP_Tuple4
  guardCons = Guard_OP_Tuple4
  try (Choice_OP_Tuple4 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple4 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple4 cd info) = Fail cd info
  try (Guard_OP_Tuple4 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple4 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple4 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple4 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple4 cd i _) = error ("Prelude.OP_Tuple4.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple4 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple4 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3) => Generable (OP_Tuple4 t0 t1 t2 t3) where
  generate s c = Choices_OP_Tuple4 c (freeID [4] s) [(OP_Tuple4 (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3) => NormalForm (OP_Tuple4 t0 t1 t2 t3) where
  ($!!) cont (OP_Tuple4 x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (OP_Tuple4 y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple4 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple4 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple4 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple4 x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (OP_Tuple4 y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple4 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple4 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple4 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple4 x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (OP_Tuple4 y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple4.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3) => Unifiable (OP_Tuple4 t0 t1 t2 t3) where
  (=.=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple4 x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_OP_Tuple4 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple4 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple4 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple4 cd i _) = error ("Prelude.OP_Tuple4.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple4 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple4 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple4 x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_OP_Tuple4 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple4 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple4 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple4 cd i _) = error ("Prelude.OP_Tuple4.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple4 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple4 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3) => Curry (OP_Tuple4 t0 t1 t2 t3) where
  (=?=) (Choice_OP_Tuple4 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple4 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple4 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple4 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple4 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple4 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple4 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (((x4 =?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple4 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple4 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple4 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple4 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple4 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple4 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple4 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (((x4 <?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple5 t0 t1 t2 t3 t4
     = OP_Tuple5 t0 t1 t2 t3 t4
     | Choice_OP_Tuple5 Cover ID (OP_Tuple5 t0 t1 t2 t3 t4) (OP_Tuple5 t0 t1 t2 t3 t4)
     | Choices_OP_Tuple5 Cover ID ([OP_Tuple5 t0 t1 t2 t3 t4])
     | Fail_OP_Tuple5 Cover FailInfo
     | Guard_OP_Tuple5 Cover Constraints (OP_Tuple5 t0 t1 t2 t3 t4)

instance (Show t0,Show t1,Show t2,Show t3,Show t4) => Show (OP_Tuple5 t0 t1 t2 t3 t4) where
  showsPrec d (Choice_OP_Tuple5 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple5 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple5 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple5 cd info) = showChar '!'
  showsPrec _ (OP_Tuple5 x1 x2 x3 x4 x5) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . (showChar ')'))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4) => Read (OP_Tuple5 t0 t1 t2 t3 t4) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5),s2) = (OP_Tuple5 x1 x2 x3 x4 x5,s2)


instance NonDet (OP_Tuple5 t0 t1 t2 t3 t4) where
  choiceCons = Choice_OP_Tuple5
  choicesCons = Choices_OP_Tuple5
  failCons = Fail_OP_Tuple5
  guardCons = Guard_OP_Tuple5
  try (Choice_OP_Tuple5 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple5 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple5 cd info) = Fail cd info
  try (Guard_OP_Tuple5 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple5 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple5 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple5 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple5 cd i _) = error ("Prelude.OP_Tuple5.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple5 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple5 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4) => Generable (OP_Tuple5 t0 t1 t2 t3 t4) where
  generate s c = Choices_OP_Tuple5 c (freeID [5] s) [(OP_Tuple5 (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4) => NormalForm (OP_Tuple5 t0 t1 t2 t3 t4) where
  ($!!) cont (OP_Tuple5 x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (OP_Tuple5 y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple5 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple5 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple5 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple5 x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (OP_Tuple5 y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple5 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple5 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple5 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple5 x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (OP_Tuple5 y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple5.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4) => Unifiable (OP_Tuple5 t0 t1 t2 t3 t4) where
  (=.=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple5 x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_OP_Tuple5 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple5 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple5 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple5 cd i _) = error ("Prelude.OP_Tuple5.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple5 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple5 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple5 x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_OP_Tuple5 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple5 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple5 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple5 cd i _) = error ("Prelude.OP_Tuple5.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple5 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple5 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => Curry (OP_Tuple5 t0 t1 t2 t3 t4) where
  (=?=) (Choice_OP_Tuple5 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple5 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple5 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple5 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple5 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple5 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple5 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (((x5 =?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple5 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple5 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple5 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple5 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple5 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple5 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple5 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (((x5 <?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple6 t0 t1 t2 t3 t4 t5
     = OP_Tuple6 t0 t1 t2 t3 t4 t5
     | Choice_OP_Tuple6 Cover ID (OP_Tuple6 t0 t1 t2 t3 t4 t5) (OP_Tuple6 t0 t1 t2 t3 t4 t5)
     | Choices_OP_Tuple6 Cover ID ([OP_Tuple6 t0 t1 t2 t3 t4 t5])
     | Fail_OP_Tuple6 Cover FailInfo
     | Guard_OP_Tuple6 Cover Constraints (OP_Tuple6 t0 t1 t2 t3 t4 t5)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5) => Show (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  showsPrec d (Choice_OP_Tuple6 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple6 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple6 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple6 cd info) = showChar '!'
  showsPrec _ (OP_Tuple6 x1 x2 x3 x4 x5 x6) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . (showChar ')'))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5) => Read (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6),s2) = (OP_Tuple6 x1 x2 x3 x4 x5 x6,s2)


instance NonDet (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  choiceCons = Choice_OP_Tuple6
  choicesCons = Choices_OP_Tuple6
  failCons = Fail_OP_Tuple6
  guardCons = Guard_OP_Tuple6
  try (Choice_OP_Tuple6 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple6 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple6 cd info) = Fail cd info
  try (Guard_OP_Tuple6 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple6 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple6 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple6 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple6 cd i _) = error ("Prelude.OP_Tuple6.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple6 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple6 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5) => Generable (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  generate s c = Choices_OP_Tuple6 c (freeID [6] s) [(OP_Tuple6 (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5) => NormalForm (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  ($!!) cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple6 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple6 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple6 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple6 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple6 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple6 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple6.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5) => Unifiable (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  (=.=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple6 x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_OP_Tuple6 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple6 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple6 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple6 cd i _) = error ("Prelude.OP_Tuple6.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple6 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple6 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple6 x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_OP_Tuple6 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple6 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple6 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple6 cd i _) = error ("Prelude.OP_Tuple6.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple6 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple6 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5) => Curry (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  (=?=) (Choice_OP_Tuple6 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple6 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple6 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple6 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple6 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple6 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple6 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (((x6 =?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple6 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple6 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple6 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple6 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple6 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple6 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple6 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (((x6 <?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple7 t0 t1 t2 t3 t4 t5 t6
     = OP_Tuple7 t0 t1 t2 t3 t4 t5 t6
     | Choice_OP_Tuple7 Cover ID (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6)
     | Choices_OP_Tuple7 Cover ID ([OP_Tuple7 t0 t1 t2 t3 t4 t5 t6])
     | Fail_OP_Tuple7 Cover FailInfo
     | Guard_OP_Tuple7 Cover Constraints (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6) => Show (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  showsPrec d (Choice_OP_Tuple7 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple7 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple7 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple7 cd info) = showChar '!'
  showsPrec _ (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . (showChar ')'))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6) => Read (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7),s2) = (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7,s2)


instance NonDet (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  choiceCons = Choice_OP_Tuple7
  choicesCons = Choices_OP_Tuple7
  failCons = Fail_OP_Tuple7
  guardCons = Guard_OP_Tuple7
  try (Choice_OP_Tuple7 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple7 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple7 cd info) = Fail cd info
  try (Guard_OP_Tuple7 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple7 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple7 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple7 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple7 cd i _) = error ("Prelude.OP_Tuple7.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple7 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple7 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6) => Generable (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  generate s c = Choices_OP_Tuple7 c (freeID [7] s) [(OP_Tuple7 (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (rightSupply (leftSupply s))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6) => NormalForm (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  ($!!) cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple7 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple7 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple7 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple7 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple7 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple7 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7)) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple7.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6) => Unifiable (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  (=.=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & (((x7 =:= y7) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & (((x7 =:<= y7) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple7 x3 x4 x5 x6 x7 x8 x9) = ((i :=: (ChooseN 0 7)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (leftID (rightID (leftID i))) x5),(bind cd (rightID (rightID (leftID i))) x6),(bind cd (leftID (leftID (rightID i))) x7),(bind cd (rightID (leftID (rightID i))) x8),(bind cd (rightID (rightID i)) x9)]))
  bind d i (Choice_OP_Tuple7 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple7 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple7 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple7 cd i _) = error ("Prelude.OP_Tuple7.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple7 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple7 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple7 x3 x4 x5 x6 x7 x8 x9) = [(i :=: (ChooseN 0 7)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x6))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x7))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x8))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x9)))]
  lazyBind d i (Choice_OP_Tuple7 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple7 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple7 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple7 cd i _) = error ("Prelude.OP_Tuple7.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple7 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple7 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6) => Curry (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  (=?=) (Choice_OP_Tuple7 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple7 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple7 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple7 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple7 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple7 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple7 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (((x7 =?= y7) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple7 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple7 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple7 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple7 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple7 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple7 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple7 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (((x7 <?= y7) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7
     = OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7
     | Choice_OP_Tuple8 Cover ID (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7)
     | Choices_OP_Tuple8 Cover ID ([OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7])
     | Fail_OP_Tuple8 Cover FailInfo
     | Guard_OP_Tuple8 Cover Constraints (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7) => Show (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  showsPrec d (Choice_OP_Tuple8 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple8 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple8 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple8 cd info) = showChar '!'
  showsPrec _ (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . (showChar ')'))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7) => Read (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8),s2) = (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8,s2)


instance NonDet (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  choiceCons = Choice_OP_Tuple8
  choicesCons = Choices_OP_Tuple8
  failCons = Fail_OP_Tuple8
  guardCons = Guard_OP_Tuple8
  try (Choice_OP_Tuple8 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple8 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple8 cd info) = Fail cd info
  try (Guard_OP_Tuple8 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple8 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple8 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple8 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple8 cd i _) = error ("Prelude.OP_Tuple8.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple8 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple8 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7) => Generable (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  generate s c = Choices_OP_Tuple8 c (freeID [8] s) [(OP_Tuple8 (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (rightSupply (leftSupply s))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (rightSupply (rightSupply s))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7) => NormalForm (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  ($!!) cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple8 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple8 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple8 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple8 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple8 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple8 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8)) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple8.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7) => Unifiable (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  (=.=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & (((x8 =:= y8) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & (((x8 =:<= y8) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple8 x3 x4 x5 x6 x7 x8 x9 x10) = ((i :=: (ChooseN 0 8)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (leftID (rightID (leftID i))) x5),(bind cd (rightID (rightID (leftID i))) x6),(bind cd (leftID (leftID (rightID i))) x7),(bind cd (rightID (leftID (rightID i))) x8),(bind cd (leftID (rightID (rightID i))) x9),(bind cd (rightID (rightID (rightID i))) x10)]))
  bind d i (Choice_OP_Tuple8 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple8 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple8 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple8 cd i _) = error ("Prelude.OP_Tuple8.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple8 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple8 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple8 x3 x4 x5 x6 x7 x8 x9 x10) = [(i :=: (ChooseN 0 8)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x6))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x7))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x8))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (rightID i))) x9))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x10)))]
  lazyBind d i (Choice_OP_Tuple8 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple8 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple8 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple8 cd i _) = error ("Prelude.OP_Tuple8.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple8 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple8 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7) => Curry (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  (=?=) (Choice_OP_Tuple8 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple8 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple8 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple8 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple8 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple8 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple8 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (((x8 =?= y8) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple8 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple8 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple8 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple8 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple8 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple8 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple8 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (((x8 <?= y8) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8
     = OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8
     | Choice_OP_Tuple9 Cover ID (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8)
     | Choices_OP_Tuple9 Cover ID ([OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8])
     | Fail_OP_Tuple9 Cover FailInfo
     | Guard_OP_Tuple9 Cover Constraints (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8) => Show (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  showsPrec d (Choice_OP_Tuple9 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple9 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple9 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple9 cd info) = showChar '!'
  showsPrec _ (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . (showChar ')'))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8) => Read (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9),s2) = (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9,s2)


instance NonDet (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  choiceCons = Choice_OP_Tuple9
  choicesCons = Choices_OP_Tuple9
  failCons = Fail_OP_Tuple9
  guardCons = Guard_OP_Tuple9
  try (Choice_OP_Tuple9 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple9 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple9 cd info) = Fail cd info
  try (Guard_OP_Tuple9 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple9 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple9 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple9 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple9 cd i _) = error ("Prelude.OP_Tuple9.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple9 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple9 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8) => Generable (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  generate s c = Choices_OP_Tuple9 c (freeID [9] s) [(OP_Tuple9 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (rightSupply (leftSupply s))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (rightSupply (rightSupply s))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8) => NormalForm (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  ($!!) cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple9 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple9 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple9 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple9 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple9 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple9 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9)) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple9.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8) => Unifiable (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  (=.=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & (((x9 =:= y9) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & (((x9 =:<= y9) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple9 x3 x4 x5 x6 x7 x8 x9 x10 x11) = ((i :=: (ChooseN 0 9)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (rightID (leftID (leftID i))) x5),(bind cd (leftID (rightID (leftID i))) x6),(bind cd (rightID (rightID (leftID i))) x7),(bind cd (leftID (leftID (rightID i))) x8),(bind cd (rightID (leftID (rightID i))) x9),(bind cd (leftID (rightID (rightID i))) x10),(bind cd (rightID (rightID (rightID i))) x11)]))
  bind d i (Choice_OP_Tuple9 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple9 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple9 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple9 cd i _) = error ("Prelude.OP_Tuple9.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple9 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple9 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple9 x3 x4 x5 x6 x7 x8 x9 x10 x11) = [(i :=: (ChooseN 0 9)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x5))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID i))) x6))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x7))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x8))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x9))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (rightID i))) x10))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x11)))]
  lazyBind d i (Choice_OP_Tuple9 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple9 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple9 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple9 cd i _) = error ("Prelude.OP_Tuple9.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple9 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple9 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8) => Curry (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  (=?=) (Choice_OP_Tuple9 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple9 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple9 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple9 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple9 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple9 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple9 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (((x9 =?= y9) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple9 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple9 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple9 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple9 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple9 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple9 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple9 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (((x9 <?= y9) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9
     = OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9
     | Choice_OP_Tuple10 Cover ID (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9)
     | Choices_OP_Tuple10 Cover ID ([OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9])
     | Fail_OP_Tuple10 Cover FailInfo
     | Guard_OP_Tuple10 Cover Constraints (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9) => Show (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  showsPrec d (Choice_OP_Tuple10 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple10 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple10 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple10 cd info) = showChar '!'
  showsPrec _ (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . (showChar ')'))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9) => Read (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10),s2) = (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10,s2)


instance NonDet (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  choiceCons = Choice_OP_Tuple10
  choicesCons = Choices_OP_Tuple10
  failCons = Fail_OP_Tuple10
  guardCons = Guard_OP_Tuple10
  try (Choice_OP_Tuple10 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple10 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple10 cd info) = Fail cd info
  try (Guard_OP_Tuple10 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple10 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple10 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple10 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple10 cd i _) = error ("Prelude.OP_Tuple10.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple10 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple10 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9) => Generable (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  generate s c = Choices_OP_Tuple10 c (freeID [10] s) [(OP_Tuple10 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (rightSupply (leftSupply s))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (rightSupply (rightSupply s))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9) => NormalForm (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  ($!!) cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple10 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple10 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple10 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple10 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple10 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple10 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10)) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple10.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9) => Unifiable (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  (=.=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & (((x10 =:= y10) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & (((x10 =:<= y10) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple10 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = ((i :=: (ChooseN 0 10)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (rightID (leftID (leftID i))) x5),(bind cd (leftID (rightID (leftID i))) x6),(bind cd (rightID (rightID (leftID i))) x7),(bind cd (leftID (leftID (leftID (rightID i)))) x8),(bind cd (rightID (leftID (leftID (rightID i)))) x9),(bind cd (rightID (leftID (rightID i))) x10),(bind cd (leftID (rightID (rightID i))) x11),(bind cd (rightID (rightID (rightID i))) x12)]))
  bind d i (Choice_OP_Tuple10 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple10 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple10 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple10 cd i _) = error ("Prelude.OP_Tuple10.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple10 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple10 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple10 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = [(i :=: (ChooseN 0 10)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x5))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID i))) x6))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x7))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x8))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x10))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (rightID i))) x11))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x12)))]
  lazyBind d i (Choice_OP_Tuple10 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple10 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple10 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple10 cd i _) = error ("Prelude.OP_Tuple10.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple10 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple10 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9) => Curry (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  (=?=) (Choice_OP_Tuple10 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple10 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple10 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple10 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple10 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple10 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple10 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (((x10 =?= y10) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple10 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple10 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple10 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple10 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple10 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple10 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple10 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (((x10 <?= y10) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
     = OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
     | Choice_OP_Tuple11 Cover ID (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)
     | Choices_OP_Tuple11 Cover ID ([OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10])
     | Fail_OP_Tuple11 Cover FailInfo
     | Guard_OP_Tuple11 Cover Constraints (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10) => Show (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  showsPrec d (Choice_OP_Tuple11 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple11 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple11 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple11 cd info) = showChar '!'
  showsPrec _ (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . ((',':) . ((shows x11) . (showChar ')'))))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10) => Read (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11),s2) = (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11,s2)


instance NonDet (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  choiceCons = Choice_OP_Tuple11
  choicesCons = Choices_OP_Tuple11
  failCons = Fail_OP_Tuple11
  guardCons = Guard_OP_Tuple11
  try (Choice_OP_Tuple11 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple11 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple11 cd info) = Fail cd info
  try (Guard_OP_Tuple11 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple11 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple11 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple11 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple11 cd i _) = error ("Prelude.OP_Tuple11.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple11 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple11 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9,Generable t10) => Generable (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  generate s c = Choices_OP_Tuple11 c (freeID [11] s) [(OP_Tuple11 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (rightSupply (rightSupply s))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10) => NormalForm (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  ($!!) cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple11 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple11 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple11 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple11 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple11 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple11 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11)) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple11.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10) => Unifiable (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  (=.=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & (((x11 =:= y11) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & (((x11 =:<= y11) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple11 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = ((i :=: (ChooseN 0 11)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (rightID (leftID (leftID i))) x5),(bind cd (leftID (leftID (rightID (leftID i)))) x6),(bind cd (rightID (leftID (rightID (leftID i)))) x7),(bind cd (rightID (rightID (leftID i))) x8),(bind cd (leftID (leftID (leftID (rightID i)))) x9),(bind cd (rightID (leftID (leftID (rightID i)))) x10),(bind cd (rightID (leftID (rightID i))) x11),(bind cd (leftID (rightID (rightID i))) x12),(bind cd (rightID (rightID (rightID i))) x13)]))
  bind d i (Choice_OP_Tuple11 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple11 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple11 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple11 cd i _) = error ("Prelude.OP_Tuple11.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple11 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple11 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple11 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = [(i :=: (ChooseN 0 11)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x5))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID i)))) x6))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x7))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x8))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x11))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (rightID i))) x12))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x13)))]
  lazyBind d i (Choice_OP_Tuple11 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple11 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple11 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple11 cd i _) = error ("Prelude.OP_Tuple11.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple11 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple11 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10) => Curry (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  (=?=) (Choice_OP_Tuple11 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple11 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple11 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple11 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple11 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple11 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple11 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (((x11 =?= y11) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple11 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple11 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple11 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple11 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple11 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple11 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple11 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_bar_bar (d_OP_lt x10 y10 d cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (((x11 <?= y11) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11
     = OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11
     | Choice_OP_Tuple12 Cover ID (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)
     | Choices_OP_Tuple12 Cover ID ([OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11])
     | Fail_OP_Tuple12 Cover FailInfo
     | Guard_OP_Tuple12 Cover Constraints (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11) => Show (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  showsPrec d (Choice_OP_Tuple12 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple12 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple12 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple12 cd info) = showChar '!'
  showsPrec _ (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . ((',':) . ((shows x11) . ((',':) . ((shows x12) . (showChar ')'))))))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11) => Read (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12),s2) = (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12,s2)


instance NonDet (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  choiceCons = Choice_OP_Tuple12
  choicesCons = Choices_OP_Tuple12
  failCons = Fail_OP_Tuple12
  guardCons = Guard_OP_Tuple12
  try (Choice_OP_Tuple12 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple12 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple12 cd info) = Fail cd info
  try (Guard_OP_Tuple12 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple12 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple12 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple12 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple12 cd i _) = error ("Prelude.OP_Tuple12.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple12 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple12 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9,Generable t10,Generable t11) => Generable (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  generate s c = Choices_OP_Tuple12 c (freeID [12] s) [(OP_Tuple12 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11) => NormalForm (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  ($!!) cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs) $!! x12) d) cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple12 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple12 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple12 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs) $## x12) d) cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple12 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple12 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple12 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12)) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple12.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11) => Unifiable (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  (=.=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & ((((((x11 =:= y11) d) cs) & (((x12 =:= y12) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & ((((((x11 =:<= y11) d) cs) & (((x12 =:<= y12) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple12 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = ((i :=: (ChooseN 0 12)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (rightID (leftID (leftID i))) x5),(bind cd (leftID (leftID (rightID (leftID i)))) x6),(bind cd (rightID (leftID (rightID (leftID i)))) x7),(bind cd (rightID (rightID (leftID i))) x8),(bind cd (leftID (leftID (leftID (rightID i)))) x9),(bind cd (rightID (leftID (leftID (rightID i)))) x10),(bind cd (rightID (leftID (rightID i))) x11),(bind cd (leftID (leftID (rightID (rightID i)))) x12),(bind cd (rightID (leftID (rightID (rightID i)))) x13),(bind cd (rightID (rightID (rightID i))) x14)]))
  bind d i (Choice_OP_Tuple12 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple12 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple12 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple12 cd i _) = error ("Prelude.OP_Tuple12.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple12 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple12 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple12 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = [(i :=: (ChooseN 0 12)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x5))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID i)))) x6))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x7))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x8))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x11))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID i)))) x12))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID i)))) x13))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x14)))]
  lazyBind d i (Choice_OP_Tuple12 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple12 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple12 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple12 cd i _) = error ("Prelude.OP_Tuple12.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple12 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple12 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11) => Curry (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  (=?=) (Choice_OP_Tuple12 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple12 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple12 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple12 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple12 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple12 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple12 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (((x12 =?= y12) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple12 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple12 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple12 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple12 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple12 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple12 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple12 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_bar_bar (d_OP_lt x10 y10 d cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_bar_bar (d_OP_lt x11 y11 d cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (((x12 <?= y12) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12
     = OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12
     | Choice_OP_Tuple13 Cover ID (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)
     | Choices_OP_Tuple13 Cover ID ([OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12])
     | Fail_OP_Tuple13 Cover FailInfo
     | Guard_OP_Tuple13 Cover Constraints (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12) => Show (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  showsPrec d (Choice_OP_Tuple13 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple13 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple13 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple13 cd info) = showChar '!'
  showsPrec _ (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . ((',':) . ((shows x11) . ((',':) . ((shows x12) . ((',':) . ((shows x13) . (showChar ')'))))))))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12) => Read (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13),s2) = (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13,s2)


instance NonDet (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  choiceCons = Choice_OP_Tuple13
  choicesCons = Choices_OP_Tuple13
  failCons = Fail_OP_Tuple13
  guardCons = Guard_OP_Tuple13
  try (Choice_OP_Tuple13 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple13 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple13 cd info) = Fail cd info
  try (Guard_OP_Tuple13 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple13 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple13 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple13 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple13 cd i _) = error ("Prelude.OP_Tuple13.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple13 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple13 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9,Generable t10,Generable t11,Generable t12) => Generable (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  generate s c = Choices_OP_Tuple13 c (freeID [13] s) [(OP_Tuple13 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12) => NormalForm (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  ($!!) cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs) $!! x13) d) cs) $!! x12) d) cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple13 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple13 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple13 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs) $## x13) d) cs) $## x12) d) cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple13 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple13 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple13 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13)) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple13.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12) => Unifiable (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  (=.=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & ((((((x11 =:= y11) d) cs) & ((((((x12 =:= y12) d) cs) & (((x13 =:= y13) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & ((((((x11 =:<= y11) d) cs) & ((((((x12 =:<= y12) d) cs) & (((x13 =:<= y13) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple13 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = ((i :=: (ChooseN 0 13)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (leftID (rightID (leftID (leftID i)))) x5),(bind cd (rightID (rightID (leftID (leftID i)))) x6),(bind cd (leftID (leftID (rightID (leftID i)))) x7),(bind cd (rightID (leftID (rightID (leftID i)))) x8),(bind cd (rightID (rightID (leftID i))) x9),(bind cd (leftID (leftID (leftID (rightID i)))) x10),(bind cd (rightID (leftID (leftID (rightID i)))) x11),(bind cd (rightID (leftID (rightID i))) x12),(bind cd (leftID (leftID (rightID (rightID i)))) x13),(bind cd (rightID (leftID (rightID (rightID i)))) x14),(bind cd (rightID (rightID (rightID i))) x15)]))
  bind d i (Choice_OP_Tuple13 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple13 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple13 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple13 cd i _) = error ("Prelude.OP_Tuple13.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple13 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple13 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple13 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = [(i :=: (ChooseN 0 13)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID (leftID i)))) x5))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (leftID i)))) x6))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID i)))) x7))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x8))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x9))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x11))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x12))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID i)))) x13))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID i)))) x14))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x15)))]
  lazyBind d i (Choice_OP_Tuple13 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple13 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple13 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple13 cd i _) = error ("Prelude.OP_Tuple13.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple13 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple13 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12) => Curry (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  (=?=) (Choice_OP_Tuple13 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple13 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple13 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple13 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple13 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple13 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple13 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (((x13 =?= y13) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple13 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple13 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple13 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple13 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple13 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple13 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple13 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_bar_bar (d_OP_lt x10 y10 d cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_bar_bar (d_OP_lt x11 y11 d cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_bar_bar (d_OP_lt x12 y12 d cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (((x13 <?= y13) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
     = OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
     | Choice_OP_Tuple14 Cover ID (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)
     | Choices_OP_Tuple14 Cover ID ([OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13])
     | Fail_OP_Tuple14 Cover FailInfo
     | Guard_OP_Tuple14 Cover Constraints (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12,Show t13) => Show (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  showsPrec d (Choice_OP_Tuple14 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple14 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple14 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple14 cd info) = showChar '!'
  showsPrec _ (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . ((',':) . ((shows x11) . ((',':) . ((shows x12) . ((',':) . ((shows x13) . ((',':) . ((shows x14) . (showChar ')'))))))))))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12,Read t13) => Read (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14),s2) = (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14,s2)


instance NonDet (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  choiceCons = Choice_OP_Tuple14
  choicesCons = Choices_OP_Tuple14
  failCons = Fail_OP_Tuple14
  guardCons = Guard_OP_Tuple14
  try (Choice_OP_Tuple14 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple14 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple14 cd info) = Fail cd info
  try (Guard_OP_Tuple14 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple14 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple14 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple14 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple14 cd i _) = error ("Prelude.OP_Tuple14.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple14 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple14 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9,Generable t10,Generable t11,Generable t12,Generable t13) => Generable (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  generate s c = Choices_OP_Tuple14 c (freeID [14] s) [(OP_Tuple14 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (rightSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12,NormalForm t13) => NormalForm (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  ($!!) cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs) $!! x14) d) cs) $!! x13) d) cs) $!! x12) d) cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple14 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple14 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple14 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs) $## x14) d) cs) $## x13) d) cs) $## x12) d) cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple14 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple14 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple14 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> search (\y14 -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14)) x14) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple14.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12,Unifiable t13) => Unifiable (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  (=.=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & ((((((x11 =:= y11) d) cs) & ((((((x12 =:= y12) d) cs) & ((((((x13 =:= y13) d) cs) & (((x14 =:= y14) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & ((((((x11 =:<= y11) d) cs) & ((((((x12 =:<= y12) d) cs) & ((((((x13 =:<= y13) d) cs) & (((x14 =:<= y14) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple14 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) = ((i :=: (ChooseN 0 14)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (leftID (rightID (leftID (leftID i)))) x5),(bind cd (rightID (rightID (leftID (leftID i)))) x6),(bind cd (leftID (leftID (rightID (leftID i)))) x7),(bind cd (rightID (leftID (rightID (leftID i)))) x8),(bind cd (rightID (rightID (leftID i))) x9),(bind cd (leftID (leftID (leftID (rightID i)))) x10),(bind cd (rightID (leftID (leftID (rightID i)))) x11),(bind cd (leftID (rightID (leftID (rightID i)))) x12),(bind cd (rightID (rightID (leftID (rightID i)))) x13),(bind cd (leftID (leftID (rightID (rightID i)))) x14),(bind cd (rightID (leftID (rightID (rightID i)))) x15),(bind cd (rightID (rightID (rightID i))) x16)]))
  bind d i (Choice_OP_Tuple14 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple14 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple14 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple14 cd i _) = error ("Prelude.OP_Tuple14.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple14 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple14 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple14 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) = [(i :=: (ChooseN 0 14)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID (leftID i)))) x5))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (leftID i)))) x6))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID i)))) x7))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x8))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x9))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x11))),((leftID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID (rightID i)))) x12))),((rightID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (rightID i)))) x13))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID i)))) x14))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID i)))) x15))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x16)))]
  lazyBind d i (Choice_OP_Tuple14 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple14 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple14 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple14 cd i _) = error ("Prelude.OP_Tuple14.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple14 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple14 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13) => Curry (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  (=?=) (Choice_OP_Tuple14 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple14 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple14 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple14 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple14 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple14 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple14 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (d_OP_ampersand_ampersand (((x13 =?= y13) d) cs) (((x14 =?= y14) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple14 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple14 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple14 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple14 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple14 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple14 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple14 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_bar_bar (d_OP_lt x10 y10 d cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_bar_bar (d_OP_lt x11 y11 d cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_bar_bar (d_OP_lt x12 y12 d cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (d_OP_bar_bar (d_OP_lt x13 y13 d cs) (d_OP_ampersand_ampersand (((x13 =?= y13) d) cs) (((x14 <?= y14) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
     = OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
     | Choice_OP_Tuple15 Cover ID (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)
     | Choices_OP_Tuple15 Cover ID ([OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14])
     | Fail_OP_Tuple15 Cover FailInfo
     | Guard_OP_Tuple15 Cover Constraints (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14)

instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12,Show t13,Show t14) => Show (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  showsPrec d (Choice_OP_Tuple15 cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_OP_Tuple15 cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_OP_Tuple15 cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_OP_Tuple15 cd info) = showChar '!'
  showsPrec _ (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (showString "(") . ((shows x1) . ((',':) . ((shows x2) . ((',':) . ((shows x3) . ((',':) . ((shows x4) . ((',':) . ((shows x5) . ((',':) . ((shows x6) . ((',':) . ((shows x7) . ((',':) . ((shows x8) . ((',':) . ((shows x9) . ((',':) . ((shows x10) . ((',':) . ((shows x11) . ((',':) . ((shows x12) . ((',':) . ((shows x13) . ((',':) . ((shows x14) . ((',':) . ((shows x15) . (showChar ')'))))))))))))))))))))))))))))))


instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12,Read t13,Read t14) => Read (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  readsPrec d s = map readTup (readsPrec d s)
   where
     readTup ((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15),s2) = (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15,s2)


instance NonDet (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  choiceCons = Choice_OP_Tuple15
  choicesCons = Choices_OP_Tuple15
  failCons = Fail_OP_Tuple15
  guardCons = Guard_OP_Tuple15
  try (Choice_OP_Tuple15 cd i x y) = tryChoice cd i x y
  try (Choices_OP_Tuple15 cd i xs) = tryChoices cd i xs
  try (Fail_OP_Tuple15 cd info) = Fail cd info
  try (Guard_OP_Tuple15 cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_OP_Tuple15 cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_OP_Tuple15 cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_OP_Tuple15 cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_OP_Tuple15 cd i _) = error ("Prelude.OP_Tuple15.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_OP_Tuple15 cd info) = f cd info
  match _ _ _ _ f _ (Guard_OP_Tuple15 cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2,Generable t3,Generable t4,Generable t5,Generable t6,Generable t7,Generable t8,Generable t9,Generable t10,Generable t11,Generable t12,Generable t13,Generable t14) => Generable (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  generate s c = Choices_OP_Tuple15 c (freeID [15] s) [(OP_Tuple15 (generate (leftSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (leftSupply s)))) c) (generate (leftSupply (rightSupply (rightSupply (leftSupply s)))) c) (generate (rightSupply (rightSupply (rightSupply (leftSupply s)))) c) (generate (leftSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (rightSupply (leftSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (leftSupply (rightSupply s)))) c) (generate (leftSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (leftSupply (rightSupply (rightSupply s)))) c) (generate (rightSupply (rightSupply (rightSupply s))) c))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12,NormalForm t13,NormalForm t14) => NormalForm (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  ($!!) cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> (((\y15 d cs -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs) $!! x15) d) cs) $!! x14) d) cs) $!! x13) d) cs) $!! x12) d) cs) $!! x11) d) cs) $!! x10) d) cs) $!! x9) d) cs) $!! x8) d) cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_OP_Tuple15 cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_OP_Tuple15 cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_OP_Tuple15 cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  ($##) cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> (((\y8 d cs -> (((\y9 d cs -> (((\y10 d cs -> (((\y11 d cs -> (((\y12 d cs -> (((\y13 d cs -> (((\y14 d cs -> (((\y15 d cs -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs) $## x15) d) cs) $## x14) d) cs) $## x13) d) cs) $## x12) d) cs) $## x11) d) cs) $## x10) d) cs) $## x9) d) cs) $## x8) d) cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_OP_Tuple15 cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_OP_Tuple15 cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_OP_Tuple15 cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  searchNF search cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> search (\y14 -> search (\y15 -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)) x15) x14) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple15.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12,Unifiable t13,Unifiable t14) => Unifiable (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  (=.=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & ((((((x7 =:= y7) d) cs) & ((((((x8 =:= y8) d) cs) & ((((((x9 =:= y9) d) cs) & ((((((x10 =:= y10) d) cs) & ((((((x11 =:= y11) d) cs) & ((((((x12 =:= y12) d) cs) & ((((((x13 =:= y13) d) cs) & ((((((x14 =:= y14) d) cs) & (((x15 =:= y15) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & ((((((x7 =:<= y7) d) cs) & ((((((x8 =:<= y8) d) cs) & ((((((x9 =:<= y9) d) cs) & ((((((x10 =:<= y10) d) cs) & ((((((x11 =:<= y11) d) cs) & ((((((x12 =:<= y12) d) cs) & ((((((x13 =:<= y13) d) cs) & ((((((x14 =:<= y14) d) cs) & (((x15 =:<= y15) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (OP_Tuple15 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17) = ((i :=: (ChooseN 0 15)):(concat [(bind cd (leftID (leftID (leftID (leftID i)))) x3),(bind cd (rightID (leftID (leftID (leftID i)))) x4),(bind cd (leftID (rightID (leftID (leftID i)))) x5),(bind cd (rightID (rightID (leftID (leftID i)))) x6),(bind cd (leftID (leftID (rightID (leftID i)))) x7),(bind cd (rightID (leftID (rightID (leftID i)))) x8),(bind cd (leftID (rightID (rightID (leftID i)))) x9),(bind cd (rightID (rightID (rightID (leftID i)))) x10),(bind cd (leftID (leftID (leftID (rightID i)))) x11),(bind cd (rightID (leftID (leftID (rightID i)))) x12),(bind cd (leftID (rightID (leftID (rightID i)))) x13),(bind cd (rightID (rightID (leftID (rightID i)))) x14),(bind cd (leftID (leftID (rightID (rightID i)))) x15),(bind cd (rightID (leftID (rightID (rightID i)))) x16),(bind cd (rightID (rightID (rightID i))) x17)]))
  bind d i (Choice_OP_Tuple15 cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_OP_Tuple15 cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_OP_Tuple15 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_OP_Tuple15 cd i _) = error ("Prelude.OP_Tuple15.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_OP_Tuple15 cd info) = [(Unsolvable info)]
  bind d i (Guard_OP_Tuple15 cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (OP_Tuple15 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17) = [(i :=: (ChooseN 0 15)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (leftID i)))) x4))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID (leftID i)))) x5))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (leftID i)))) x6))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (leftID i)))) x7))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (leftID i)))) x8))),((leftID (rightID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (rightID (leftID i)))) x9))),((rightID (rightID (rightID (leftID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID (leftID i)))) x10))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID (rightID i)))) x11))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID (rightID i)))) x12))),((leftID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID (rightID i)))) x13))),((rightID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID (rightID i)))) x14))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID (rightID i)))) x15))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID (rightID i)))) x16))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (rightID i))) x17)))]
  lazyBind d i (Choice_OP_Tuple15 cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_OP_Tuple15 cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_OP_Tuple15 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_OP_Tuple15 cd i _) = error ("Prelude.OP_Tuple15.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_OP_Tuple15 cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_OP_Tuple15 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13,Curry t14) => Curry (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  (=?=) (Choice_OP_Tuple15 cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_OP_Tuple15 cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_OP_Tuple15 cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_OP_Tuple15 cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple15 cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_OP_Tuple15 cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_OP_Tuple15 cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  (=?=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs = d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (d_OP_ampersand_ampersand (((x13 =?= y13) d) cs) (d_OP_ampersand_ampersand (((x14 =?= y14) d) cs) (((x15 =?= y15) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_OP_Tuple15 cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_OP_Tuple15 cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_OP_Tuple15 cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_OP_Tuple15 cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple15 cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_OP_Tuple15 cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_OP_Tuple15 cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  (<?=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) d cs = d_OP_bar_bar (d_OP_lt x1 y1 d cs) (d_OP_ampersand_ampersand (((x1 =?= y1) d) cs) (d_OP_bar_bar (d_OP_lt x2 y2 d cs) (d_OP_ampersand_ampersand (((x2 =?= y2) d) cs) (d_OP_bar_bar (d_OP_lt x3 y3 d cs) (d_OP_ampersand_ampersand (((x3 =?= y3) d) cs) (d_OP_bar_bar (d_OP_lt x4 y4 d cs) (d_OP_ampersand_ampersand (((x4 =?= y4) d) cs) (d_OP_bar_bar (d_OP_lt x5 y5 d cs) (d_OP_ampersand_ampersand (((x5 =?= y5) d) cs) (d_OP_bar_bar (d_OP_lt x6 y6 d cs) (d_OP_ampersand_ampersand (((x6 =?= y6) d) cs) (d_OP_bar_bar (d_OP_lt x7 y7 d cs) (d_OP_ampersand_ampersand (((x7 =?= y7) d) cs) (d_OP_bar_bar (d_OP_lt x8 y8 d cs) (d_OP_ampersand_ampersand (((x8 =?= y8) d) cs) (d_OP_bar_bar (d_OP_lt x9 y9 d cs) (d_OP_ampersand_ampersand (((x9 =?= y9) d) cs) (d_OP_bar_bar (d_OP_lt x10 y10 d cs) (d_OP_ampersand_ampersand (((x10 =?= y10) d) cs) (d_OP_bar_bar (d_OP_lt x11 y11 d cs) (d_OP_ampersand_ampersand (((x11 =?= y11) d) cs) (d_OP_bar_bar (d_OP_lt x12 y12 d cs) (d_OP_ampersand_ampersand (((x12 =?= y12) d) cs) (d_OP_bar_bar (d_OP_lt x13 y13 d cs) (d_OP_ampersand_ampersand (((x13 =?= y13) d) cs) (d_OP_bar_bar (d_OP_lt x14 y14 d cs) (d_OP_ampersand_ampersand (((x14 =?= y14) d) cs) (((x15 <?= y15) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


type C_String = OP_List C_Char







data C_Bool
     = C_False
     | C_True
     | Choice_C_Bool Cover ID C_Bool C_Bool
     | Choices_C_Bool Cover ID ([C_Bool])
     | Fail_C_Bool Cover FailInfo
     | Guard_C_Bool Cover Constraints C_Bool

instance Show C_Bool where
  showsPrec d (Choice_C_Bool cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Bool cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Bool cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Bool cd info) = showChar '!'
  showsPrec _ C_False = showString "False"
  showsPrec _ C_True = showString "True"


instance Read C_Bool where
  readsPrec _ s = (readParen False (\r -> [ (C_False,r0) | (_,r0) <- readQualified "Prelude" "False" r]) s) ++ (readParen False (\r -> [ (C_True,r0) | (_,r0) <- readQualified "Prelude" "True" r]) s)


instance NonDet C_Bool where
  choiceCons = Choice_C_Bool
  choicesCons = Choices_C_Bool
  failCons = Fail_C_Bool
  guardCons = Guard_C_Bool
  try (Choice_C_Bool cd i x y) = tryChoice cd i x y
  try (Choices_C_Bool cd i xs) = tryChoices cd i xs
  try (Fail_C_Bool cd info) = Fail cd info
  try (Guard_C_Bool cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Bool cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Bool cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Bool cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Bool cd i _) = error ("Prelude.Bool.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Bool cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Bool cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Bool where
  generate s c = Choices_C_Bool c (freeID [0,0] s) [C_False,C_True]


instance NormalForm C_Bool where
  ($!!) cont C_False d cs = cont C_False d cs
  ($!!) cont C_True d cs = cont C_True d cs
  ($!!) cont (Choice_C_Bool cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Bool cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Bool cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  ($##) cont C_False d cs = cont C_False d cs
  ($##) cont C_True d cs = cont C_True d cs
  ($##) cont (Choice_C_Bool cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Bool cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Bool cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  searchNF _ cont C_False = cont C_False
  searchNF _ cont C_True = cont C_True
  searchNF _ _ x = error ("Prelude.Bool.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Bool where
  (=.=) C_False C_False d cs = C_Success
  (=.=) C_True C_True d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_False C_False d cs = C_Success
  (=.<=) C_True C_True d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_False = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_True = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_Bool cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Bool cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Bool cd i _) = error ("Prelude.Bool.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Bool cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Bool cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_False = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_True = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_Bool cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Bool cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Bool cd i _) = error ("Prelude.Bool.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Bool cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Bool cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry C_Bool where
  (=?=) (Choice_C_Bool cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_Bool cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_Bool cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_Bool cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Bool cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_Bool cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_Bool cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  (=?=) C_False C_False d cs = C_True
  (=?=) C_True C_True d cs = C_True
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_C_Bool cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_Bool cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_Bool cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_Bool cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Bool cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_Bool cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_Bool cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Bool cd info) _ _ = failCons cd info
  (<?=) C_False C_False d cs = C_True
  (<?=) C_False C_True _ _ = C_True
  (<?=) C_True C_True d cs = C_True
  (<?=) _ _ d _ = C_False


data C_Ordering
     = C_LT
     | C_EQ
     | C_GT
     | Choice_C_Ordering Cover ID C_Ordering C_Ordering
     | Choices_C_Ordering Cover ID ([C_Ordering])
     | Fail_C_Ordering Cover FailInfo
     | Guard_C_Ordering Cover Constraints C_Ordering

instance Show C_Ordering where
  showsPrec d (Choice_C_Ordering cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Ordering cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Ordering cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Ordering cd info) = showChar '!'
  showsPrec _ C_LT = showString "LT"
  showsPrec _ C_EQ = showString "EQ"
  showsPrec _ C_GT = showString "GT"


instance Read C_Ordering where
  readsPrec _ s = (readParen False (\r -> [ (C_LT,r0) | (_,r0) <- readQualified "Prelude" "LT" r]) s) ++ ((readParen False (\r -> [ (C_EQ,r0) | (_,r0) <- readQualified "Prelude" "EQ" r]) s) ++ (readParen False (\r -> [ (C_GT,r0) | (_,r0) <- readQualified "Prelude" "GT" r]) s))


instance NonDet C_Ordering where
  choiceCons = Choice_C_Ordering
  choicesCons = Choices_C_Ordering
  failCons = Fail_C_Ordering
  guardCons = Guard_C_Ordering
  try (Choice_C_Ordering cd i x y) = tryChoice cd i x y
  try (Choices_C_Ordering cd i xs) = tryChoices cd i xs
  try (Fail_C_Ordering cd info) = Fail cd info
  try (Guard_C_Ordering cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Ordering cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Ordering cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Ordering cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Ordering cd i _) = error ("Prelude.Ordering.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Ordering cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Ordering cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Ordering where
  generate s c = Choices_C_Ordering c (freeID [0,0,0] s) [C_LT,C_EQ,C_GT]


instance NormalForm C_Ordering where
  ($!!) cont C_LT d cs = cont C_LT d cs
  ($!!) cont C_EQ d cs = cont C_EQ d cs
  ($!!) cont C_GT d cs = cont C_GT d cs
  ($!!) cont (Choice_C_Ordering cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Ordering cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Ordering cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Ordering cd info) _ _ = failCons cd info
  ($##) cont C_LT d cs = cont C_LT d cs
  ($##) cont C_EQ d cs = cont C_EQ d cs
  ($##) cont C_GT d cs = cont C_GT d cs
  ($##) cont (Choice_C_Ordering cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Ordering cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Ordering cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Ordering cd info) _ _ = failCons cd info
  searchNF _ cont C_LT = cont C_LT
  searchNF _ cont C_EQ = cont C_EQ
  searchNF _ cont C_GT = cont C_GT
  searchNF _ _ x = error ("Prelude.Ordering.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Ordering where
  (=.=) C_LT C_LT d cs = C_Success
  (=.=) C_EQ C_EQ d cs = C_Success
  (=.=) C_GT C_GT d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_LT C_LT d cs = C_Success
  (=.<=) C_EQ C_EQ d cs = C_Success
  (=.<=) C_GT C_GT d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_LT = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_EQ = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_GT = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_Ordering cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Ordering cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Ordering cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Ordering cd i _) = error ("Prelude.Ordering.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Ordering cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Ordering cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_LT = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_EQ = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_GT = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Ordering cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Ordering cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Ordering cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Ordering cd i _) = error ("Prelude.Ordering.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Ordering cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Ordering cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry C_Ordering where
  (=?=) (Choice_C_Ordering cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_Ordering cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_Ordering cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_Ordering cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Ordering cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_Ordering cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_Ordering cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Ordering cd info) _ _ = failCons cd info
  (=?=) C_LT C_LT d cs = C_True
  (=?=) C_EQ C_EQ d cs = C_True
  (=?=) C_GT C_GT d cs = C_True
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_C_Ordering cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_Ordering cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_Ordering cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_Ordering cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Ordering cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_Ordering cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_Ordering cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Ordering cd info) _ _ = failCons cd info
  (<?=) C_LT C_LT d cs = C_True
  (<?=) C_LT C_EQ _ _ = C_True
  (<?=) C_LT C_GT _ _ = C_True
  (<?=) C_EQ C_EQ d cs = C_True
  (<?=) C_EQ C_GT _ _ = C_True
  (<?=) C_GT C_GT d cs = C_True
  (<?=) _ _ d _ = C_False




data C_Maybe t0
     = C_Nothing
     | C_Just t0
     | Choice_C_Maybe Cover ID (C_Maybe t0) (C_Maybe t0)
     | Choices_C_Maybe Cover ID ([C_Maybe t0])
     | Fail_C_Maybe Cover FailInfo
     | Guard_C_Maybe Cover Constraints (C_Maybe t0)

instance Show t0 => Show (C_Maybe t0) where
  showsPrec d (Choice_C_Maybe cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Maybe cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Maybe cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Maybe cd info) = showChar '!'
  showsPrec _ C_Nothing = showString "Nothing"
  showsPrec _ (C_Just x1) = (showString "(Just") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_Maybe t0) where
  readsPrec d s = (readParen False (\r -> [ (C_Nothing,r0) | (_,r0) <- readQualified "Prelude" "Nothing" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Just x1,r1) | (_,r0) <- readQualified "Prelude" "Just" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet (C_Maybe t0) where
  choiceCons = Choice_C_Maybe
  choicesCons = Choices_C_Maybe
  failCons = Fail_C_Maybe
  guardCons = Guard_C_Maybe
  try (Choice_C_Maybe cd i x y) = tryChoice cd i x y
  try (Choices_C_Maybe cd i xs) = tryChoices cd i xs
  try (Fail_C_Maybe cd info) = Fail cd info
  try (Guard_C_Maybe cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Maybe cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Maybe cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Maybe cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Maybe cd i _) = error ("Prelude.Maybe.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Maybe cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Maybe cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Maybe t0) where
  generate s c = Choices_C_Maybe c (freeID [0,1] s) [C_Nothing,(C_Just (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_Maybe t0) where
  ($!!) cont C_Nothing d cs = cont C_Nothing d cs
  ($!!) cont (C_Just x1) d cs = (((\y1 d cs -> cont (C_Just y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Maybe cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Maybe cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Maybe cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Maybe cd info) _ _ = failCons cd info
  ($##) cont C_Nothing d cs = cont C_Nothing d cs
  ($##) cont (C_Just x1) d cs = (((\y1 d cs -> cont (C_Just y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Maybe cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Maybe cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Maybe cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Maybe cd info) _ _ = failCons cd info
  searchNF _ cont C_Nothing = cont C_Nothing
  searchNF search cont (C_Just x1) = search (\y1 -> cont (C_Just y1)) x1
  searchNF _ _ x = error ("Prelude.Maybe.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Maybe t0) where
  (=.=) C_Nothing C_Nothing d cs = C_Success
  (=.=) (C_Just x1) (C_Just y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Nothing C_Nothing d cs = C_Success
  (=.<=) (C_Just x1) (C_Just y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Nothing = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_Just x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Maybe cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Maybe cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Maybe cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Maybe cd i _) = error ("Prelude.Maybe.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Maybe cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Maybe cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Nothing = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_Just x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Maybe cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Maybe cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Maybe cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Maybe cd i _) = error ("Prelude.Maybe.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Maybe cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Maybe cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry t0 => Curry (C_Maybe t0) where
  (=?=) (Choice_C_Maybe cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_Maybe cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_Maybe cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_Maybe cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Maybe cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_Maybe cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_Maybe cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Maybe cd info) _ _ = failCons cd info
  (=?=) C_Nothing C_Nothing d cs = C_True
  (=?=) (C_Just x1) (C_Just y1) d cs = ((x1 =?= y1) d) cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_C_Maybe cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_Maybe cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_Maybe cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_Maybe cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Maybe cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_Maybe cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_Maybe cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Maybe cd info) _ _ = failCons cd info
  (<?=) C_Nothing C_Nothing d cs = C_True
  (<?=) C_Nothing (C_Just _) _ _ = C_True
  (<?=) (C_Just x1) (C_Just y1) d cs = ((x1 <?= y1) d) cs
  (<?=) _ _ d _ = C_False


data C_Either t0 t1
     = C_Left t0
     | C_Right t1
     | Choice_C_Either Cover ID (C_Either t0 t1) (C_Either t0 t1)
     | Choices_C_Either Cover ID ([C_Either t0 t1])
     | Fail_C_Either Cover FailInfo
     | Guard_C_Either Cover Constraints (C_Either t0 t1)

instance (Show t0,Show t1) => Show (C_Either t0 t1) where
  showsPrec d (Choice_C_Either cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Either cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Either cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Either cd info) = showChar '!'
  showsPrec _ (C_Left x1) = (showString "(Left") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Right x1) = (showString "(Right") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance (Read t0,Read t1) => Read (C_Either t0 t1) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Left x1,r1) | (_,r0) <- readQualified "Prelude" "Left" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Right x1,r1) | (_,r0) <- readQualified "Prelude" "Right" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet (C_Either t0 t1) where
  choiceCons = Choice_C_Either
  choicesCons = Choices_C_Either
  failCons = Fail_C_Either
  guardCons = Guard_C_Either
  try (Choice_C_Either cd i x y) = tryChoice cd i x y
  try (Choices_C_Either cd i xs) = tryChoices cd i xs
  try (Fail_C_Either cd info) = Fail cd info
  try (Guard_C_Either cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Either cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Either cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Either cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Either cd i _) = error ("Prelude.Either.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Either cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Either cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1) => Generable (C_Either t0 t1) where
  generate s c = Choices_C_Either c (freeID [1,1] s) [(C_Left (generate (leftSupply s) c)),(C_Right (generate (leftSupply s) c))]


instance (NormalForm t0,NormalForm t1) => NormalForm (C_Either t0 t1) where
  ($!!) cont (C_Left x1) d cs = (((\y1 d cs -> cont (C_Left y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Right x1) d cs = (((\y1 d cs -> cont (C_Right y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Either cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Either cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Either cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Either cd info) _ _ = failCons cd info
  ($##) cont (C_Left x1) d cs = (((\y1 d cs -> cont (C_Left y1) d cs) $## x1) d) cs
  ($##) cont (C_Right x1) d cs = (((\y1 d cs -> cont (C_Right y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Either cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Either cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Either cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Either cd info) _ _ = failCons cd info
  searchNF search cont (C_Left x1) = search (\y1 -> cont (C_Left y1)) x1
  searchNF search cont (C_Right x1) = search (\y1 -> cont (C_Right y1)) x1
  searchNF _ _ x = error ("Prelude.Either.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1) => Unifiable (C_Either t0 t1) where
  (=.=) (C_Left x1) (C_Left y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Right x1) (C_Right y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Left x1) (C_Left y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Right x1) (C_Right y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Left x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Right x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Either cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Either cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Either cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Either cd i _) = error ("Prelude.Either.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Either cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Either cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Left x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Right x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Either cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Either cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Either cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Either cd i _) = error ("Prelude.Either.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Either cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Either cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance (Curry t0,Curry t1) => Curry (C_Either t0 t1) where
  (=?=) (Choice_C_Either cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_Either cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_Either cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_Either cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Either cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_Either cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_Either cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Either cd info) _ _ = failCons cd info
  (=?=) (C_Left x1) (C_Left y1) d cs = ((x1 =?= y1) d) cs
  (=?=) (C_Right x1) (C_Right y1) d cs = ((x1 =?= y1) d) cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_C_Either cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_Either cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_Either cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_Either cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Either cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_Either cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_Either cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Either cd info) _ _ = failCons cd info
  (<?=) (C_Left x1) (C_Left y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (C_Left _) (C_Right _) _ _ = C_True
  (<?=) (C_Right x1) (C_Right y1) d cs = ((x1 <?= y1) d) cs
  (<?=) _ _ d _ = C_False




data C_IOError
     = C_IOError (OP_List C_Char)
     | C_UserError (OP_List C_Char)
     | C_FailError (OP_List C_Char)
     | C_NondetError (OP_List C_Char)
     | Choice_C_IOError Cover ID C_IOError C_IOError
     | Choices_C_IOError Cover ID ([C_IOError])
     | Fail_C_IOError Cover FailInfo
     | Guard_C_IOError Cover Constraints C_IOError

instance Show C_IOError where
  showsPrec d (Choice_C_IOError cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_IOError cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_IOError cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_IOError cd info) = showChar '!'
  showsPrec _ (C_IOError x1) = (showString "(IOError") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_UserError x1) = (showString "(UserError") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FailError x1) = (showString "(FailError") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_NondetError x1) = (showString "(NondetError") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_IOError where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_IOError x1,r1) | (_,r0) <- readQualified "Prelude" "IOError" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_UserError x1,r1) | (_,r0) <- readQualified "Prelude" "UserError" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FailError x1,r1) | (_,r0) <- readQualified "Prelude" "FailError" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_NondetError x1,r1) | (_,r0) <- readQualified "Prelude" "NondetError" r, (x1,r1) <- readsPrec 11 r0]) s)))


instance NonDet C_IOError where
  choiceCons = Choice_C_IOError
  choicesCons = Choices_C_IOError
  failCons = Fail_C_IOError
  guardCons = Guard_C_IOError
  try (Choice_C_IOError cd i x y) = tryChoice cd i x y
  try (Choices_C_IOError cd i xs) = tryChoices cd i xs
  try (Fail_C_IOError cd info) = Fail cd info
  try (Guard_C_IOError cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IOError cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_IOError cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IOError cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IOError cd i _) = error ("Prelude.IOError.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_IOError cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_IOError cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_IOError where
  generate s c = Choices_C_IOError c (freeID [1,1,1,1] s) [(C_IOError (generate (leftSupply s) c)),(C_UserError (generate (leftSupply s) c)),(C_FailError (generate (leftSupply s) c)),(C_NondetError (generate (leftSupply s) c))]


instance NormalForm C_IOError where
  ($!!) cont (C_IOError x1) d cs = (((\y1 d cs -> cont (C_IOError y1) d cs) $!! x1) d) cs
  ($!!) cont (C_UserError x1) d cs = (((\y1 d cs -> cont (C_UserError y1) d cs) $!! x1) d) cs
  ($!!) cont (C_FailError x1) d cs = (((\y1 d cs -> cont (C_FailError y1) d cs) $!! x1) d) cs
  ($!!) cont (C_NondetError x1) d cs = (((\y1 d cs -> cont (C_NondetError y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_IOError cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_IOError cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_IOError cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_IOError cd info) _ _ = failCons cd info
  ($##) cont (C_IOError x1) d cs = (((\y1 d cs -> cont (C_IOError y1) d cs) $## x1) d) cs
  ($##) cont (C_UserError x1) d cs = (((\y1 d cs -> cont (C_UserError y1) d cs) $## x1) d) cs
  ($##) cont (C_FailError x1) d cs = (((\y1 d cs -> cont (C_FailError y1) d cs) $## x1) d) cs
  ($##) cont (C_NondetError x1) d cs = (((\y1 d cs -> cont (C_NondetError y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_IOError cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_IOError cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_IOError cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_IOError cd info) _ _ = failCons cd info
  searchNF search cont (C_IOError x1) = search (\y1 -> cont (C_IOError y1)) x1
  searchNF search cont (C_UserError x1) = search (\y1 -> cont (C_UserError y1)) x1
  searchNF search cont (C_FailError x1) = search (\y1 -> cont (C_FailError y1)) x1
  searchNF search cont (C_NondetError x1) = search (\y1 -> cont (C_NondetError y1)) x1
  searchNF _ _ x = error ("Prelude.IOError.searchNF: no constructor: " ++ (show x))


instance Unifiable C_IOError where
  (=.=) (C_IOError x1) (C_IOError y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_UserError x1) (C_UserError y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_FailError x1) (C_FailError y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_NondetError x1) (C_NondetError y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_IOError x1) (C_IOError y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_UserError x1) (C_UserError y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_FailError x1) (C_FailError y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_NondetError x1) (C_NondetError y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_IOError x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_UserError x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_FailError x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_NondetError x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_IOError cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_IOError cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_IOError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_IOError cd i _) = error ("Prelude.IOError.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_IOError cd info) = [(Unsolvable info)]
  bind d i (Guard_C_IOError cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_IOError x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_UserError x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FailError x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_NondetError x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_IOError cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_IOError cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_IOError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_IOError cd i _) = error ("Prelude.IOError.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_IOError cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_IOError cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry C_IOError where
  (=?=) (Choice_C_IOError cd i x y) z d cs = narrow cd i (((x =?= z) d) cs) (((y =?= z) d) cs)
  (=?=) (Choices_C_IOError cd i xs) y d cs = narrows cs cd i (\x -> ((x =?= y) d) cs) xs
  (=?=) (Guard_C_IOError cd c e) y d cs = guardCons cd c (((e =?= y) d) (addCs c cs))
  (=?=) (Fail_C_IOError cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_IOError cd i x y) d cs = narrow cd i (((z =?= x) d) cs) (((z =?= y) d) cs)
  (=?=) y (Choices_C_IOError cd i xs) d cs = narrows cs cd i (\x -> ((y =?= x) d) cs) xs
  (=?=) y (Guard_C_IOError cd c e) d cs = guardCons cd c (((y =?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_IOError cd info) _ _ = failCons cd info
  (=?=) (C_IOError x1) (C_IOError y1) d cs = ((x1 =?= y1) d) cs
  (=?=) (C_UserError x1) (C_UserError y1) d cs = ((x1 =?= y1) d) cs
  (=?=) (C_FailError x1) (C_FailError y1) d cs = ((x1 =?= y1) d) cs
  (=?=) (C_NondetError x1) (C_NondetError y1) d cs = ((x1 =?= y1) d) cs
  (=?=) _ _ d _ = C_False
  (<?=) (Choice_C_IOError cd i x y) z d cs = narrow cd i (((x <?= z) d) cs) (((y <?= z) d) cs)
  (<?=) (Choices_C_IOError cd i xs) y d cs = narrows cs cd i (\x -> ((x <?= y) d) cs) xs
  (<?=) (Guard_C_IOError cd c e) y d cs = guardCons cd c (((e <?= y) d) (addCs c cs))
  (<?=) (Fail_C_IOError cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_IOError cd i x y) d cs = narrow cd i (((z <?= x) d) cs) (((z <?= y) d) cs)
  (<?=) y (Choices_C_IOError cd i xs) d cs = narrows cs cd i (\x -> ((y <?= x) d) cs) xs
  (<?=) y (Guard_C_IOError cd c e) d cs = guardCons cd c (((y <?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_IOError cd info) _ _ = failCons cd info
  (<?=) (C_IOError x1) (C_IOError y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (C_IOError _) (C_UserError _) _ _ = C_True
  (<?=) (C_IOError _) (C_FailError _) _ _ = C_True
  (<?=) (C_IOError _) (C_NondetError _) _ _ = C_True
  (<?=) (C_UserError x1) (C_UserError y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (C_UserError _) (C_FailError _) _ _ = C_True
  (<?=) (C_UserError _) (C_NondetError _) _ _ = C_True
  (<?=) (C_FailError x1) (C_FailError y1) d cs = ((x1 <?= y1) d) cs
  (<?=) (C_FailError _) (C_NondetError _) _ _ = C_True
  (<?=) (C_NondetError x1) (C_NondetError y1) d cs = ((x1 <?= y1) d) cs
  (<?=) _ _ d _ = C_False


d_OP_dot :: (Curry t0,Curry t2,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> t2 -> Cover -> ConstStore -> t1
d_OP_dot x1 x2 x3250 x3500 = d_OP_dot_dot___hash_lambda1 x1 x2

nd_OP_dot :: (Curry t0,Curry t2,Curry t1) => Func t0 t1 -> Func t2 t0 -> IDSupply -> Cover -> ConstStore -> Func t2 t1
nd_OP_dot x1 x2 x3000 x3250 x3500 = wrapNX id (nd_OP_dot_dot___hash_lambda1 x1 x2)

d_OP_dot_dot___hash_lambda1 :: (Curry t0,Curry t2,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t0) -> t2 -> Cover -> ConstStore -> t1
d_OP_dot_dot___hash_lambda1 x1 x2 x3 x3250 x3500 = d_C_apply x1 (d_C_apply x2 x3 x3250 x3500) x3250 x3500

nd_OP_dot_dot___hash_lambda1 :: (Curry t0,Curry t2,Curry t1) => Func t0 t1 -> Func t2 t0 -> t2 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dot_dot___hash_lambda1 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_apply x1 (nd_C_apply x2 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_id :: Curry t0 => t0 -> Cover -> ConstStore -> t0
d_C_id x1 x3250 x3500 = x1

d_C_const :: (Curry t1,Curry t0) => t0 -> t1 -> Cover -> ConstStore -> t0
d_C_const x1 x2 x3250 x3500 = x1

d_C_curry :: (Curry t0,Curry t1,Curry t2) => (OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t2) -> t0 -> t1 -> Cover -> ConstStore -> t2
d_C_curry x1 x2 x3 x3250 x3500 = d_C_apply x1 (OP_Tuple2 x2 x3) x3250 x3500

nd_C_curry :: (Curry t0,Curry t1,Curry t2) => Func (OP_Tuple2 t0 t1) t2 -> t0 -> t1 -> IDSupply -> Cover -> ConstStore -> t2
nd_C_curry x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_apply x1 (OP_Tuple2 x2 x3) x2000 x3250 x3500))

d_C_uncurry :: (Curry t0,Curry t1,Curry t2) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t2
d_C_uncurry x1 x2 x3250 x3500 = case x2 of
     (OP_Tuple2 x3 x4) -> d_C_apply (d_C_apply x1 x3 x3250 x3500) x4 x3250 x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uncurry x1 x1002 x3250 x3500) (d_C_uncurry x1 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uncurry x1 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uncurry x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_uncurry :: (Curry t0,Curry t1,Curry t2) => Func t0 (Func t1 t2) -> OP_Tuple2 t0 t1 -> IDSupply -> Cover -> ConstStore -> t2
nd_C_uncurry x1 x2 x3000 x3250 x3500 = case x2 of
     (OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x3 x2000 x3250 x3500) x4 x2001 x3250 x3500)))))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_uncurry x1 x1002 x3000 x3250 x3500) (nd_C_uncurry x1 x1003 x3000 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_uncurry x1 z x3000 x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_uncurry x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_flip :: (Curry t1,Curry t0,Curry t2) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> t1 -> t0 -> Cover -> ConstStore -> t2
d_C_flip x1 x2 x3 x3250 x3500 = d_C_apply (d_C_apply x1 x3 x3250 x3500) x2 x3250 x3500

nd_C_flip :: (Curry t1,Curry t0,Curry t2) => Func t0 (Func t1 t2) -> t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t2
nd_C_flip x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x3 x2000 x3250 x3500) x2 x2001 x3250 x3500)))))

d_C_until :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> (t0 -> Cover -> ConstStore -> t0) -> t0 -> Cover -> ConstStore -> t0
d_C_until x1 x2 x3 x3250 x3500 = d_OP__case_35 x3 x1 x2 (d_C_apply x1 x3 x3250 x3500) x3250 x3500

nd_C_until :: Curry t0 => Func t0 C_Bool -> Func t0 t0 -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_until x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_35 x3 x1 x2 (nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_seq :: (Curry t0,Curry t1) => t0 -> t1 -> Cover -> ConstStore -> t1
d_C_seq x1 x2 x3250 x3500 = d_OP_dollar_bang (d_C_const x2) x1 x3250 x3500

d_C_ensureSpine :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_ensureSpine x1 x3250 x3500 = d_OP_ensureSpine_dot_ensureList_dot_20 (d_C_ensureNotFree x1 x3250 x3500) x3250 x3500

d_OP_ensureSpine_dot_ensureList_dot_20 :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_OP_ensureSpine_dot_ensureList_dot_20 x1 x3250 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> OP_Cons x2 (d_C_ensureSpine x3 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ensureSpine_dot_ensureList_dot_20 x1002 x3250 x3500) (d_OP_ensureSpine_dot_ensureList_dot_20 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ensureSpine_dot_ensureList_dot_20 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ensureSpine_dot_ensureList_dot_20 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_dollar :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_dollar x1 x2 x3250 x3500 = d_C_apply x1 x2 x3250 x3500

nd_OP_dollar :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dollar x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_apply x1 x2 x2000 x3250 x3500))

d_OP_dollar_hash :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_dollar_hash x1 x2 x3250 x3500 = d_OP_dollar_bang x1 (d_C_ensureNotFree x2 x3250 x3500) x3250 x3500

nd_OP_dollar_hash :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dollar_hash x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dollar_bang x1 (d_C_ensureNotFree x2 x3250 x3500) x2000 x3250 x3500))

d_C_error :: Curry t0 => OP_List C_Char -> Cover -> ConstStore -> t0
d_C_error x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_prim_error x1 x3250 x3500

d_OP_ampersand_ampersand :: C_Bool -> C_Bool -> Cover -> ConstStore -> C_Bool
d_OP_ampersand_ampersand x1 x2 x3250 x3500 = case x1 of
     C_True -> x2
     C_False -> C_False
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ampersand_ampersand x1002 x2 x3250 x3500) (d_OP_ampersand_ampersand x1003 x2 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ampersand_ampersand z x2 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ampersand_ampersand x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_bar_bar :: C_Bool -> C_Bool -> Cover -> ConstStore -> C_Bool
d_OP_bar_bar x1 x2 x3250 x3500 = case x1 of
     C_True -> C_True
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bar_bar x1002 x2 x3250 x3500) (d_OP_bar_bar x1003 x2 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bar_bar z x2 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bar_bar x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_not :: C_Bool -> Cover -> ConstStore -> C_Bool
d_C_not x1 x3250 x3500 = case x1 of
     C_True -> C_False
     C_False -> C_True
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_not x1002 x3250 x3500) (d_C_not x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_not z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_not x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_otherwise :: Cover -> ConstStore -> C_Bool
d_C_otherwise x3250 x3500 = C_True

d_C_if_then_else :: Curry t0 => C_Bool -> t0 -> t0 -> Cover -> ConstStore -> t0
d_C_if_then_else x1 x2 x3 x3250 x3500 = case x1 of
     C_True -> x2
     C_False -> x3
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_if_then_else x1002 x2 x3 x3250 x3500) (d_C_if_then_else x1003 x2 x3 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_if_then_else z x2 x3 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_if_then_else x1002 x2 x3 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_slash_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_slash_eq x1 x2 x3250 x3500 = d_C_not (d_OP_eq_eq x1 x2 x3250 x3500) x3250 x3500

d_C_compare :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Ordering
d_C_compare x1 x2 x3250 x3500 = d_OP__case_34 x2 x1 (d_OP_eq_eq x1 x2 x3250 x3500) x3250 x3500

d_OP_lt :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_lt x1 x2 x3250 x3500 = d_C_not (d_OP_lt_eq x2 x1 x3250 x3500) x3250 x3500

d_OP_gt :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_gt x1 x2 x3250 x3500 = d_C_not (d_OP_lt_eq x1 x2 x3250 x3500) x3250 x3500

d_OP_gt_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_gt_eq x1 x2 x3250 x3500 = d_C_not (d_OP_lt x1 x2 x3250 x3500) x3250 x3500

d_C_max :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> t0
d_C_max x1 x2 x3250 x3500 = d_OP__case_31 x2 x1 (d_OP_gt_eq x1 x2 x3250 x3500) x3250 x3500

d_C_min :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> t0
d_C_min x1 x2 x3250 x3500 = d_OP__case_30 x2 x1 (d_OP_lt_eq x1 x2 x3250 x3500) x3250 x3500

d_C_fst :: (Curry t1,Curry t0) => OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t0
d_C_fst x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fst x1002 x3250 x3500) (d_C_fst x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fst z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fst x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_snd :: (Curry t0,Curry t1) => OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t1
d_C_snd x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_snd x1002 x3250 x3500) (d_C_snd x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_snd z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_snd x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_head :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> t0
d_C_head x1 x3250 x3500 = case x1 of
     (OP_Cons x2 x3) -> x2
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_head x1002 x3250 x3500) (d_C_head x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_head z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_head x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_tail :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_tail x1 x3250 x3500 = case x1 of
     (OP_Cons x2 x3) -> x3
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tail x1002 x3250 x3500) (d_C_tail x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tail z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tail x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_null :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> C_Bool
d_C_null x1 x3250 x3500 = case x1 of
     OP_List -> C_True
     (OP_Cons x2 x3) -> C_False
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_null x1002 x3250 x3500) (d_C_null x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_null z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_null x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_plus_plus :: Curry t0 => OP_List t0 -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_OP_plus_plus x1 x2 x3250 x3500 = case x1 of
     OP_List -> x2
     (OP_Cons x3 x4) -> OP_Cons x3 (d_OP_plus_plus x4 x2 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_plus x1002 x2 x3250 x3500) (d_OP_plus_plus x1003 x2 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_plus z x2 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_plus x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_length :: Curry t0 => OP_List t0 -> Cover -> ConstStore -> C_Int
d_C_length x1 x3250 x3500 = d_OP_length_dot_len_dot_90 x1 (C_Int 0#) x3250 x3500

d_OP_length_dot_len_dot_90 :: Curry t0 => OP_List t0 -> C_Int -> Cover -> ConstStore -> C_Int
d_OP_length_dot_len_dot_90 x1 x2 x3250 x3500 = case x1 of
     OP_List -> x2
     (OP_Cons x3 x4) -> let
          x5 = d_OP_plus x2 (C_Int 1#) x3250 x3500
           in (d_OP_dollar_bang_bang (d_OP_length_dot_len_dot_90 x4) x5 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_length_dot_len_dot_90 x1002 x2 x3250 x3500) (d_OP_length_dot_len_dot_90 x1003 x2 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_length_dot_len_dot_90 z x2 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_length_dot_len_dot_90 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_bang_bang :: Curry t0 => OP_List t0 -> C_Int -> Cover -> ConstStore -> t0
d_OP_bang_bang x1 x2 x3250 x3500 = case x1 of
     (OP_Cons x3 x4) -> d_OP__case_29 x2 x4 x3 (d_OP_eq_eq x2 (C_Int 0#) x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bang_bang x1002 x2 x3250 x3500) (d_OP_bang_bang x1003 x2 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bang_bang z x2 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bang_bang x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_map :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> OP_List t0 -> Cover -> ConstStore -> OP_List t1
d_C_map x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> OP_Cons (d_C_apply x1 x3 x3250 x3500) (d_C_map x1 x4 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_map x1 x1002 x3250 x3500) (d_C_map x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_map x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_map x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_map :: (Curry t0,Curry t1) => Func t0 t1 -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> OP_List t1
nd_C_map x1 x2 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (OP_Cons (nd_C_apply x1 x3 x2000 x3250 x3500) (nd_C_map x1 x4 x2001 x3250 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_map x1 x1002 x3000 x3250 x3500) (nd_C_map x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_map x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_map x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foldl :: (Curry t1,Curry t0) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t0) -> t0 -> OP_List t1 -> Cover -> ConstStore -> t0
d_C_foldl x1 x2 x3 x3250 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> d_C_foldl x1 (d_C_apply (d_C_apply x1 x2 x3250 x3500) x4 x3250 x3500) x5 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldl x1 x2 x1002 x3250 x3500) (d_C_foldl x1 x2 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldl x1 x2 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldl x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_foldl :: (Curry t1,Curry t0) => Func t0 (Func t1 t0) -> t0 -> OP_List t1 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_foldl x1 x2 x3 x3000 x3250 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_foldl x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x2 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldl x1 x2 x1002 x3000 x3250 x3500) (nd_C_foldl x1 x2 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldl x1 x2 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldl x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foldl1 :: Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0) -> OP_List t0 -> Cover -> ConstStore -> t0
d_C_foldl1 x1 x2 x3250 x3500 = case x2 of
     (OP_Cons x3 x4) -> d_C_foldl x1 x3 x4 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldl1 x1 x1002 x3250 x3500) (d_C_foldl1 x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldl1 x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldl1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_foldl1 :: Curry t0 => Func t0 (Func t0 t0) -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_foldl1 x1 x2 x3000 x3250 x3500 = case x2 of
     (OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_foldl x1 x3 x4 x2000 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldl1 x1 x1002 x3000 x3250 x3500) (nd_C_foldl1 x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldl1 x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldl1 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foldr :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t1) -> t1 -> OP_List t0 -> Cover -> ConstStore -> t1
d_C_foldr x1 x2 x3 x3250 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> d_C_apply (d_C_apply x1 x4 x3250 x3500) (d_C_foldr x1 x2 x5 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldr x1 x2 x1002 x3250 x3500) (d_C_foldr x1 x2 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldr x1 x2 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldr x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_foldr :: (Curry t0,Curry t1) => Func t0 (Func t1 t1) -> t1 -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_foldr x1 x2 x3 x3000 x3250 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_C_apply (nd_C_apply x1 x4 x2000 x3250 x3500) (nd_C_foldr x1 x2 x5 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldr x1 x2 x1002 x3000 x3250 x3500) (nd_C_foldr x1 x2 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldr x1 x2 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldr x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_foldr1 :: Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0) -> OP_List t0 -> Cover -> ConstStore -> t0
d_C_foldr1 x1 x2 x3250 x3500 = case x2 of
     (OP_Cons x3 x4) -> d_OP__case_27 x1 x3 x4 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldr1 x1 x1002 x3250 x3500) (d_C_foldr1 x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldr1 x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldr1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_foldr1 :: Curry t0 => Func t0 (Func t0 t0) -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_foldr1 x1 x2 x3000 x3250 x3500 = case x2 of
     (OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x3 x4 x2000 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldr1 x1 x1002 x3000 x3250 x3500) (nd_C_foldr1 x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldr1 x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldr1 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_filter :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_filter x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_26 x3 x1 x4 (d_C_apply x1 x3 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_filter x1 x1002 x3250 x3500) (d_C_filter x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_filter x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_filter x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_filter :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_C_filter x1 x2 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_26 x3 x1 x4 (nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_filter x1 x1002 x3000 x3250 x3500) (nd_C_filter x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_filter x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_filter x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_zip :: (Curry t0,Curry t1) => OP_List t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List (OP_Tuple2 t0 t1)
d_C_zip x1 x2 x3250 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_25 x4 x3 x2 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zip x1002 x2 x3250 x3500) (d_C_zip x1003 x2 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zip z x2 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zip x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_zip3 :: (Curry t0,Curry t1,Curry t2) => OP_List t0 -> OP_List t1 -> OP_List t2 -> Cover -> ConstStore -> OP_List (OP_Tuple3 t0 t1 t2)
d_C_zip3 x1 x2 x3 x3250 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> d_OP__case_24 x3 x5 x4 x2 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zip3 x1002 x2 x3 x3250 x3500) (d_C_zip3 x1003 x2 x3 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zip3 z x2 x3 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zip3 x1002 x2 x3 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_zipWith :: (Curry t0,Curry t1,Curry t2) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> OP_List t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List t2
d_C_zipWith x1 x2 x3 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> d_OP__case_22 x5 x1 x4 x3 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zipWith x1 x1002 x3 x3250 x3500) (d_C_zipWith x1 x1003 x3 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zipWith x1 z x3 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zipWith x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_zipWith :: (Curry t0,Curry t1,Curry t2) => Func t0 (Func t1 t2) -> OP_List t0 -> OP_List t1 -> IDSupply -> Cover -> ConstStore -> OP_List t2
nd_C_zipWith x1 x2 x3 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x5 x1 x4 x3 x2000 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_zipWith x1 x1002 x3 x3000 x3250 x3500) (nd_C_zipWith x1 x1003 x3 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_zipWith x1 z x3 x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_zipWith x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2 -> Cover -> ConstStore -> t3) -> OP_List t0 -> OP_List t1 -> OP_List t2 -> Cover -> ConstStore -> OP_List t3
d_C_zipWith3 x1 x2 x3 x4 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> d_OP__case_21 x4 x6 x1 x5 x3 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zipWith3 x1 x1002 x3 x4 x3250 x3500) (d_C_zipWith3 x1 x1003 x3 x4 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zipWith3 x1 z x3 x4 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zipWith3 x1 x1002 x3 x4 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => Func t0 (Func t1 (Func t2 t3)) -> OP_List t0 -> OP_List t1 -> OP_List t2 -> IDSupply -> Cover -> ConstStore -> OP_List t3
nd_C_zipWith3 x1 x2 x3 x4 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x4 x6 x1 x5 x3 x2000 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_zipWith3 x1 x1002 x3 x4 x3000 x3250 x3500) (nd_C_zipWith3 x1 x1003 x3 x4 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_zipWith3 x1 z x3 x4 x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_zipWith3 x1 x1002 x3 x4 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unzip :: (Curry t0,Curry t1) => OP_List (OP_Tuple2 t0 t1) -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t1)
d_C_unzip x1 x3250 x3500 = case x1 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_19 x3 x2 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unzip x1002 x3250 x3500) (d_C_unzip x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unzip z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unzip x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unzip_dot___hash_selFP2_hash_xs :: (Curry t1,Curry t0) => OP_Tuple2 (OP_List t0) (OP_List t1) -> Cover -> ConstStore -> OP_List t0
d_OP_unzip_dot___hash_selFP2_hash_xs x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip_dot___hash_selFP2_hash_xs x1002 x3250 x3500) (d_OP_unzip_dot___hash_selFP2_hash_xs x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip_dot___hash_selFP2_hash_xs z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip_dot___hash_selFP2_hash_xs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unzip_dot___hash_selFP3_hash_ys :: (Curry t0,Curry t1) => OP_Tuple2 (OP_List t0) (OP_List t1) -> Cover -> ConstStore -> OP_List t1
d_OP_unzip_dot___hash_selFP3_hash_ys x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip_dot___hash_selFP3_hash_ys x1002 x3250 x3500) (d_OP_unzip_dot___hash_selFP3_hash_ys x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip_dot___hash_selFP3_hash_ys z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip_dot___hash_selFP3_hash_ys x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unzip3 :: (Curry t0,Curry t1,Curry t2) => OP_List (OP_Tuple3 t0 t1 t2) -> Cover -> ConstStore -> OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2)
d_C_unzip3 x1 x3250 x3500 = case x1 of
     OP_List -> OP_Tuple3 OP_List OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_18 x3 x2 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unzip3 x1002 x3250 x3500) (d_C_unzip3 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unzip3 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unzip3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unzip3_dot___hash_selFP5_hash_xs :: (Curry t1,Curry t2,Curry t0) => OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2) -> Cover -> ConstStore -> OP_List t0
d_OP_unzip3_dot___hash_selFP5_hash_xs x1 x3250 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x2
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP5_hash_xs x1002 x3250 x3500) (d_OP_unzip3_dot___hash_selFP5_hash_xs x1003 x3250 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP5_hash_xs z x3250 x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP5_hash_xs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unzip3_dot___hash_selFP6_hash_ys :: (Curry t0,Curry t2,Curry t1) => OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2) -> Cover -> ConstStore -> OP_List t1
d_OP_unzip3_dot___hash_selFP6_hash_ys x1 x3250 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x3
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP6_hash_ys x1002 x3250 x3500) (d_OP_unzip3_dot___hash_selFP6_hash_ys x1003 x3250 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP6_hash_ys z x3250 x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP6_hash_ys x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unzip3_dot___hash_selFP7_hash_zs :: (Curry t0,Curry t1,Curry t2) => OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2) -> Cover -> ConstStore -> OP_List t2
d_OP_unzip3_dot___hash_selFP7_hash_zs x1 x3250 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x4
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP7_hash_zs x1002 x3250 x3500) (d_OP_unzip3_dot___hash_selFP7_hash_zs x1003 x3250 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP7_hash_zs z x3250 x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP7_hash_zs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_concat :: Curry t0 => OP_List (OP_List t0) -> Cover -> ConstStore -> OP_List t0
d_C_concat x1 x3250 x3500 = d_C_foldr (acceptCs id d_OP_plus_plus) OP_List x1 x3250 x3500

d_C_concatMap :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> OP_List t1) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> OP_List t1
d_C_concatMap x1 x3250 x3500 = d_OP_dot d_C_concat (d_C_map x1) x3250 x3500

nd_C_concatMap :: (Curry t0,Curry t1) => Func t0 (OP_List t1) -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) (OP_List t1)
nd_C_concatMap x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dot (wrapDX id d_C_concat) (wrapNX id (nd_C_map x1)) x2000 x3250 x3500))

d_C_iterate :: Curry t0 => (t0 -> Cover -> ConstStore -> t0) -> t0 -> Cover -> ConstStore -> OP_List t0
d_C_iterate x1 x2 x3250 x3500 = OP_Cons x2 (d_C_iterate x1 (d_C_apply x1 x2 x3250 x3500) x3250 x3500)

nd_C_iterate :: Curry t0 => Func t0 t0 -> t0 -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_C_iterate x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (OP_Cons x2 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_iterate x1 (nd_C_apply x1 x2 x2000 x3250 x3500) x2001 x3250 x3500))))))

d_C_repeat :: Curry t0 => t0 -> Cover -> ConstStore -> OP_List t0
d_C_repeat x1 x3250 x3500 = OP_Cons x1 (d_C_repeat x1 x3250 x3500)

d_C_replicate :: Curry t0 => C_Int -> t0 -> Cover -> ConstStore -> OP_List t0
d_C_replicate x1 x2 x3250 x3500 = d_C_take x1 (d_C_repeat x2 x3250 x3500) x3250 x3500

d_C_take :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_take x1 x2 x3250 x3500 = d_OP__case_17 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3250 x3500) x3250 x3500

d_OP_take_dot_takep_dot_210 :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_OP_take_dot_takep_dot_210 x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> OP_Cons x3 (d_C_take (d_OP_minus x1 (C_Int 1#) x3250 x3500) x4 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_take_dot_takep_dot_210 x1 x1002 x3250 x3500) (d_OP_take_dot_takep_dot_210 x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_take_dot_takep_dot_210 x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_take_dot_takep_dot_210 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_drop :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_drop x1 x2 x3250 x3500 = d_OP__case_16 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3250 x3500) x3250 x3500

d_OP_drop_dot_dropp_dot_219 :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_OP_drop_dot_dropp_dot_219 x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_C_drop (d_OP_minus x1 (C_Int 1#) x3250 x3500) x4 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_drop_dot_dropp_dot_219 x1 x1002 x3250 x3500) (d_OP_drop_dot_dropp_dot_219 x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_drop_dot_dropp_dot_219 x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_drop_dot_dropp_dot_219 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_splitAt :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_splitAt x1 x2 x3250 x3500 = d_OP__case_15 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3250 x3500) x3250 x3500

d_OP_splitAt_dot_splitAtp_dot_229 :: Curry t0 => C_Int -> OP_List t0 -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_OP_splitAt_dot_splitAtp_dot_229 x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> let
          x5 = d_C_splitAt (d_OP_minus x1 (C_Int 1#) x3250 x3500) x4 x3250 x3500
          x6 = d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x5 x3250 x3500
          x7 = d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x5 x3250 x3500
           in (OP_Tuple2 (OP_Cons x3 x6) x7)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229 x1 x1002 x3250 x3500) (d_OP_splitAt_dot_splitAtp_dot_229 x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229 x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys :: Curry t0 => OP_Tuple2 (OP_List t0) (OP_List t0) -> Cover -> ConstStore -> OP_List t0
d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1002 x3250 x3500) (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs :: Curry t0 => OP_Tuple2 (OP_List t0) (OP_List t0) -> Cover -> ConstStore -> OP_List t0
d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1002 x3250 x3500) (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_takeWhile :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_takeWhile x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_14 x3 x1 x4 (d_C_apply x1 x3 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_takeWhile x1 x1002 x3250 x3500) (d_C_takeWhile x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_takeWhile x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_takeWhile x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_takeWhile :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_C_takeWhile x1 x2 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_14 x3 x1 x4 (nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_takeWhile x1 x1002 x3000 x3250 x3500) (nd_C_takeWhile x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_takeWhile x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_takeWhile x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dropWhile :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_dropWhile x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_13 x3 x1 x4 (d_C_apply x1 x3 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dropWhile x1 x1002 x3250 x3500) (d_C_dropWhile x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dropWhile x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dropWhile x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_dropWhile :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_C_dropWhile x1 x2 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_13 x3 x1 x4 (nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_dropWhile x1 x1002 x3000 x3250 x3500) (nd_C_dropWhile x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_dropWhile x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_dropWhile x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_span :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_span x1 x2 x3250 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> d_OP__case_12 x3 x1 x4 (d_C_apply x1 x3 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_span x1 x1002 x3250 x3500) (d_C_span x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_span x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_span x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_span :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
nd_C_span x1 x2 x3000 x3250 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_12 x3 x1 x4 (nd_C_apply x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_span x1 x1002 x3000 x3250 x3500) (nd_C_span x1 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_span x1 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_span x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_span_dot___hash_selFP12_hash_ys :: Curry t0 => OP_Tuple2 (OP_List t0) (OP_List t0) -> Cover -> ConstStore -> OP_List t0
d_OP_span_dot___hash_selFP12_hash_ys x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_span_dot___hash_selFP12_hash_ys x1002 x3250 x3500) (d_OP_span_dot___hash_selFP12_hash_ys x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_span_dot___hash_selFP12_hash_ys z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_span_dot___hash_selFP12_hash_ys x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_span_dot___hash_selFP13_hash_zs :: Curry t0 => OP_Tuple2 (OP_List t0) (OP_List t0) -> Cover -> ConstStore -> OP_List t0
d_OP_span_dot___hash_selFP13_hash_zs x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_span_dot___hash_selFP13_hash_zs x1002 x3250 x3500) (d_OP_span_dot___hash_selFP13_hash_zs x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_span_dot___hash_selFP13_hash_zs z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_span_dot___hash_selFP13_hash_zs x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_break :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_break x1 x3250 x3500 = d_C_span (d_OP_dot d_C_not x1 x3250 x3500)

nd_C_break :: Curry t0 => Func t0 C_Bool -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) (OP_Tuple2 (OP_List t0) (OP_List t0))
nd_C_break x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_span (nd_OP_dot (wrapDX id d_C_not) x1 x2000 x3250 x3500))))

d_C_lines :: OP_List C_Char -> Cover -> ConstStore -> OP_List (OP_List C_Char)
d_C_lines x1 x3250 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> let
          x4 = d_OP_lines_dot_splitline_dot_261 (OP_Cons x2 x3) x3250 x3500
          x5 = d_OP_lines_dot___hash_selFP18_hash_l x4 x3250 x3500
          x6 = d_OP_lines_dot___hash_selFP19_hash_xs_l x4 x3250 x3500
           in (OP_Cons x5 (d_C_lines x6 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lines x1002 x3250 x3500) (d_C_lines x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lines z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lines x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lines_dot_splitline_dot_261 :: OP_List C_Char -> Cover -> ConstStore -> OP_Tuple2 (OP_List C_Char) (OP_List C_Char)
d_OP_lines_dot_splitline_dot_261 x1 x3250 x3500 = case x1 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_10 x2 x3 (d_OP_eq_eq x2 (C_Char '\n'#) x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261 x1002 x3250 x3500) (d_OP_lines_dot_splitline_dot_261 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1002 x3250 x3500) (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1002 x3250 x3500) (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lines_dot___hash_selFP18_hash_l :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_lines_dot___hash_selFP18_hash_l x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot___hash_selFP18_hash_l x1002 x3250 x3500) (d_OP_lines_dot___hash_selFP18_hash_l x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot___hash_selFP18_hash_l z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot___hash_selFP18_hash_l x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lines_dot___hash_selFP19_hash_xs_l :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_lines_dot___hash_selFP19_hash_xs_l x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot___hash_selFP19_hash_xs_l x1002 x3250 x3500) (d_OP_lines_dot___hash_selFP19_hash_xs_l x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot___hash_selFP19_hash_xs_l z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot___hash_selFP19_hash_xs_l x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unlines :: OP_List (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_C_unlines x1 x3250 x3500 = d_C_apply (d_C_concatMap (d_C_flip (acceptCs id d_OP_plus_plus) (OP_Cons (C_Char '\n'#) OP_List)) x3250 x3500) x1 x3250 x3500

d_C_words :: OP_List C_Char -> Cover -> ConstStore -> OP_List (OP_List C_Char)
d_C_words x1 x3250 x3500 = let
     x2 = d_C_dropWhile d_OP_words_dot_isSpace_dot_273 x1 x3250 x3500
      in (d_OP__case_9 x2 (d_OP_eq_eq x2 OP_List x3250 x3500) x3250 x3500)

d_OP_words_dot_isSpace_dot_273 :: C_Char -> Cover -> ConstStore -> C_Bool
d_OP_words_dot_isSpace_dot_273 x1 x3250 x3500 = d_OP_bar_bar (d_OP_eq_eq x1 (C_Char ' '#) x3250 x3500) (d_OP_bar_bar (d_OP_eq_eq x1 (C_Char '\t'#) x3250 x3500) (d_OP_bar_bar (d_OP_eq_eq x1 (C_Char '\n'#) x3250 x3500) (d_OP_eq_eq x1 (C_Char '\r'#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_words_dot___hash_selFP21_hash_w :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_words_dot___hash_selFP21_hash_w x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_words_dot___hash_selFP21_hash_w x1002 x3250 x3500) (d_OP_words_dot___hash_selFP21_hash_w x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_words_dot___hash_selFP21_hash_w z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_words_dot___hash_selFP21_hash_w x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_words_dot___hash_selFP22_hash_s2 :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_OP_words_dot___hash_selFP22_hash_s2 x1 x3250 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_words_dot___hash_selFP22_hash_s2 x1002 x3250 x3500) (d_OP_words_dot___hash_selFP22_hash_s2 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_words_dot___hash_selFP22_hash_s2 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_words_dot___hash_selFP22_hash_s2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unwords :: OP_List (OP_List C_Char) -> Cover -> ConstStore -> OP_List C_Char
d_C_unwords x1 x3250 x3500 = d_OP__case_8 x1 (d_OP_eq_eq x1 OP_List x3250 x3500) x3250 x3500

d_OP_unwords_dot___hash_lambda3 :: OP_List C_Char -> OP_List C_Char -> Cover -> ConstStore -> OP_List C_Char
d_OP_unwords_dot___hash_lambda3 x1 x2 x3250 x3500 = d_OP_plus_plus x1 (OP_Cons (C_Char ' '#) x2) x3250 x3500

d_C_reverse :: Curry t0 => Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> OP_List t0
d_C_reverse x3250 x3500 = d_C_foldl (acceptCs id (d_C_flip (acceptCs (acceptCs id) OP_Cons))) OP_List

nd_C_reverse :: Curry t0 => IDSupply -> Cover -> ConstStore -> Func (OP_List t0) (OP_List t0)
nd_C_reverse x3000 x3250 x3500 = wrapNX id (nd_C_foldl (wrapDX (wrapNX id) (acceptCs id (nd_C_flip (wrapDX (wrapDX id) (acceptCs (acceptCs id) OP_Cons))))) OP_List)

d_C_and :: Cover -> ConstStore -> OP_List C_Bool -> Cover -> ConstStore -> C_Bool
d_C_and x3250 x3500 = d_C_foldr (acceptCs id d_OP_ampersand_ampersand) C_True

nd_C_and :: IDSupply -> Cover -> ConstStore -> Func (OP_List C_Bool) C_Bool
nd_C_and x3000 x3250 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_ampersand_ampersand)) C_True)

d_C_or :: Cover -> ConstStore -> OP_List C_Bool -> Cover -> ConstStore -> C_Bool
d_C_or x3250 x3500 = d_C_foldr (acceptCs id d_OP_bar_bar) C_False

nd_C_or :: IDSupply -> Cover -> ConstStore -> Func (OP_List C_Bool) C_Bool
nd_C_or x3000 x3250 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_bar_bar)) C_False)

d_C_any :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_Bool
d_C_any x1 x3250 x3500 = d_OP_dot (d_C_or x3250 x3500) (d_C_map x1) x3250 x3500

nd_C_any :: Curry t0 => Func t0 C_Bool -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_any x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_or x2000 x3250 x3500) (wrapNX id (nd_C_map x1)) x2001 x3250 x3500)))))

d_C_all :: Curry t0 => (t0 -> Cover -> ConstStore -> C_Bool) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_Bool
d_C_all x1 x3250 x3500 = d_OP_dot (d_C_and x3250 x3500) (d_C_map x1) x3250 x3500

nd_C_all :: Curry t0 => Func t0 C_Bool -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_all x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_and x2000 x3250 x3500) (wrapNX id (nd_C_map x1)) x2001 x3250 x3500)))))

d_C_elem :: Curry t0 => t0 -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_Bool
d_C_elem x1 x3250 x3500 = d_C_any (d_OP_eq_eq x1) x3250 x3500

nd_C_elem :: Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_elem x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_any (wrapDX id (d_OP_eq_eq x1)) x2000 x3250 x3500))

d_C_notElem :: Curry t0 => t0 -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_Bool
d_C_notElem x1 x3250 x3500 = d_C_all (d_OP_slash_eq x1) x3250 x3500

nd_C_notElem :: Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_notElem x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_all (wrapDX id (d_OP_slash_eq x1)) x2000 x3250 x3500))

d_C_lookup :: (Curry t0,Curry t1) => t0 -> OP_List (OP_Tuple2 t0 t1) -> Cover -> ConstStore -> C_Maybe t1
d_C_lookup x1 x2 x3250 x3500 = case x2 of
     OP_List -> C_Nothing
     (OP_Cons x3 x4) -> d_OP__case_7 x1 x4 x3 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookup x1 x1002 x3250 x3500) (d_C_lookup x1 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookup x1 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookup x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_enumFrom :: C_Int -> Cover -> ConstStore -> OP_List C_Int
d_C_enumFrom x1 x3250 x3500 = OP_Cons x1 (d_C_enumFrom (d_OP_plus x1 (C_Int 1#) x3250 x3500) x3250 x3500)

d_C_enumFromThen :: C_Int -> C_Int -> Cover -> ConstStore -> OP_List C_Int
d_C_enumFromThen x1 x2 x3250 x3500 = d_C_iterate (d_OP_plus (d_OP_minus x2 x1 x3250 x3500)) x1 x3250 x3500

d_C_enumFromTo :: C_Int -> C_Int -> Cover -> ConstStore -> OP_List C_Int
d_C_enumFromTo x1 x2 x3250 x3500 = d_OP__case_4 x2 x1 (d_OP_gt x1 x2 x3250 x3500) x3250 x3500

d_C_enumFromThenTo :: C_Int -> C_Int -> C_Int -> Cover -> ConstStore -> OP_List C_Int
d_C_enumFromThenTo x1 x2 x3 x3250 x3500 = d_C_takeWhile (d_OP_enumFromThenTo_dot_p_dot_311 x3 x1 x2) (d_C_enumFromThen x1 x2 x3250 x3500) x3250 x3500

d_OP_enumFromThenTo_dot_p_dot_311 :: C_Int -> C_Int -> C_Int -> C_Int -> Cover -> ConstStore -> C_Bool
d_OP_enumFromThenTo_dot_p_dot_311 x1 x2 x3 x4 x3250 x3500 = d_OP__case_3 x2 x3 x1 x4 (d_OP_gt_eq x3 x2 x3250 x3500) x3250 x3500

d_C_ord :: C_Char -> Cover -> ConstStore -> C_Int
d_C_ord x1 x3250 x3500 = d_OP_dollar_hash d_C_prim_ord x1 x3250 x3500

d_C_chr :: C_Int -> Cover -> ConstStore -> C_Char
d_C_chr x1 x3250 x3500 = d_OP__case_1 x1 (d_OP_gt_eq x1 (C_Int 0#) x3250 x3500) x3250 x3500

d_C_negate :: C_Int -> Cover -> ConstStore -> C_Int
d_C_negate x1 x3250 x3500 = d_OP_minus (C_Int 0#) x1 x3250 x3500

d_OP_ampersand_gt :: Curry t0 => C_Success -> t0 -> Cover -> ConstStore -> t0
d_OP_ampersand_gt x1 x2 x3250 x3500 = d_OP___cond_0__ampersand_gt x2 x1 x3250 x3500

d_OP___cond_0__ampersand_gt :: Curry t0 => t0 -> C_Success -> Cover -> ConstStore -> t0
d_OP___cond_0__ampersand_gt x1 x2 x3250 x3500 = case x2 of
     C_Success -> x1
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__ampersand_gt x1 x1002 x3250 x3500) (d_OP___cond_0__ampersand_gt x1 x1003 x3250 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__ampersand_gt x1 z x3250 x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__ampersand_gt x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_maybe :: (Curry t1,Curry t0) => t0 -> (t1 -> Cover -> ConstStore -> t0) -> C_Maybe t1 -> Cover -> ConstStore -> t0
d_C_maybe x1 x2 x3 x3250 x3500 = case x3 of
     C_Nothing -> x1
     (C_Just x4) -> d_C_apply x2 x4 x3250 x3500
     (Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maybe x1 x2 x1002 x3250 x3500) (d_C_maybe x1 x2 x1003 x3250 x3500)
     (Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maybe x1 x2 z x3250 x3500) x1002
     (Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maybe x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_maybe :: (Curry t1,Curry t0) => t0 -> Func t1 t0 -> C_Maybe t1 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_maybe x1 x2 x3 x3000 x3250 x3500 = case x3 of
     C_Nothing -> x1
     (C_Just x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x2 x4 x2000 x3250 x3500))
     (Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_maybe x1 x2 x1002 x3000 x3250 x3500) (nd_C_maybe x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_maybe x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_maybe x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_either :: (Curry t0,Curry t2,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t1) -> C_Either t0 t2 -> Cover -> ConstStore -> t1
d_C_either x1 x2 x3 x3250 x3500 = case x3 of
     (C_Left x4) -> d_C_apply x1 x4 x3250 x3500
     (C_Right x5) -> d_C_apply x2 x5 x3250 x3500
     (Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_either x1 x2 x1002 x3250 x3500) (d_C_either x1 x2 x1003 x3250 x3500)
     (Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_either x1 x2 z x3250 x3500) x1002
     (Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_either x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_either :: (Curry t0,Curry t2,Curry t1) => Func t0 t1 -> Func t2 t1 -> C_Either t0 t2 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_either x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (C_Left x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x1 x4 x2000 x3250 x3500))
     (C_Right x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x2 x5 x2000 x3250 x3500))
     (Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_either x1 x2 x1002 x3000 x3250 x3500) (nd_C_either x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_either x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_either x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_gt_gt :: (Curry t0,Curry t1) => C_IO t0 -> C_IO t1 -> Cover -> ConstStore -> C_IO t1
d_OP_gt_gt x1 x2 x3250 x3500 = d_OP_gt_gt_eq x1 (d_OP_gt_gt_dot___hash_lambda4 x2) x3250 x3500

d_OP_gt_gt_dot___hash_lambda4 :: (Curry t1,Curry t0) => C_IO t0 -> t1 -> Cover -> ConstStore -> C_IO t0
d_OP_gt_gt_dot___hash_lambda4 x1 x2 x3250 x3500 = x1

d_C_done :: Cover -> ConstStore -> C_IO OP_Unit
d_C_done x3250 x3500 = d_C_return OP_Unit x3250 x3500

d_C_putChar :: C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_putChar x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_prim_putChar x1 x3250 x3500

d_C_readFile :: OP_List C_Char -> Cover -> ConstStore -> C_IO (OP_List C_Char)
d_C_readFile x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_prim_readFile x1 x3250 x3500

d_C_writeFile :: OP_List C_Char -> OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_writeFile x1 x2 x3250 x3500 = d_OP_dollar_hash_hash (d_OP_dollar_hash_hash (acceptCs id d_C_prim_writeFile) x1 x3250 x3500) x2 x3250 x3500

d_C_appendFile :: OP_List C_Char -> OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_appendFile x1 x2 x3250 x3500 = d_OP_dollar_hash_hash (d_OP_dollar_hash_hash (acceptCs id d_C_prim_appendFile) x1 x3250 x3500) x2 x3250 x3500

d_C_putStr :: OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_putStr x1 x3250 x3500 = case x1 of
     OP_List -> d_C_done x3250 x3500
     (OP_Cons x2 x3) -> d_OP_gt_gt (d_C_putChar x2 x3250 x3500) (d_C_putStr x3 x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_putStr x1002 x3250 x3500) (d_C_putStr x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_putStr z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_putStr x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_putStrLn :: OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_putStrLn x1 x3250 x3500 = d_OP_gt_gt (d_C_putStr x1 x3250 x3500) (d_C_putChar (C_Char '\n'#) x3250 x3500) x3250 x3500

d_C_getLine :: Cover -> ConstStore -> C_IO (OP_List C_Char)
d_C_getLine x3250 x3500 = d_OP_gt_gt_eq (d_C_getChar x3250 x3500) d_OP_getLine_dot___hash_lambda5 x3250 x3500

d_OP_getLine_dot___hash_lambda5 :: C_Char -> Cover -> ConstStore -> C_IO (OP_List C_Char)
d_OP_getLine_dot___hash_lambda5 x1 x3250 x3500 = d_OP__case_0 x1 (d_OP_eq_eq x1 (C_Char '\n'#) x3250 x3500) x3250 x3500

d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 :: C_Char -> OP_List C_Char -> Cover -> ConstStore -> C_IO (OP_List C_Char)
d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3250 x3500 = d_C_return (OP_Cons x1 x2) x3250 x3500

d_C_userError :: OP_List C_Char -> Cover -> ConstStore -> C_IOError
d_C_userError x1 x3250 x3500 = C_UserError x1

d_C_ioError :: Curry t0 => C_IOError -> Cover -> ConstStore -> C_IO t0
d_C_ioError x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_prim_ioError x1 x3250 x3500

d_C_showError :: C_IOError -> Cover -> ConstStore -> OP_List C_Char
d_C_showError x1 x3250 x3500 = case x1 of
     (C_IOError x2) -> d_OP_plus_plus (OP_Cons (C_Char 'i'#) (OP_Cons (C_Char '/'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List))))))))))) x2 x3250 x3500
     (C_UserError x3) -> d_OP_plus_plus (OP_Cons (C_Char 'u'#) (OP_Cons (C_Char 's'#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))) x3 x3250 x3500
     (C_FailError x4) -> d_OP_plus_plus (OP_Cons (C_Char 'f'#) (OP_Cons (C_Char 'a'#) (OP_Cons (C_Char 'i'#) (OP_Cons (C_Char 'l'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))) x4 x3250 x3500
     (C_NondetError x5) -> d_OP_plus_plus (OP_Cons (C_Char 'n'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'n'#) (OP_Cons (C_Char 'd'#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 't'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))))) x5 x3250 x3500
     (Choice_C_IOError x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showError x1002 x3250 x3500) (d_C_showError x1003 x3250 x3500)
     (Choices_C_IOError x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showError z x3250 x3500) x1002
     (Guard_C_IOError x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showError x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_IOError x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_show :: Curry t0 => t0 -> Cover -> ConstStore -> OP_List C_Char
d_C_show x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_prim_show x1 x3250 x3500

d_C_print :: Curry t0 => t0 -> Cover -> ConstStore -> C_IO OP_Unit
d_C_print x1 x3250 x3500 = d_C_putStrLn (d_C_show x1 x3250 x3500) x3250 x3500

d_C_doSolve :: C_Success -> Cover -> ConstStore -> C_IO OP_Unit
d_C_doSolve x1 x3250 x3500 = d_OP___cond_0_doSolve x1 x3250 x3500

d_OP___cond_0_doSolve :: C_Success -> Cover -> ConstStore -> C_IO OP_Unit
d_OP___cond_0_doSolve x1 x3250 x3500 = case x1 of
     C_Success -> d_C_done x3250 x3500
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_doSolve x1002 x3250 x3500) (d_OP___cond_0_doSolve x1003 x3250 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_doSolve z x3250 x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_doSolve x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_sequenceIO :: Curry t0 => OP_List (C_IO t0) -> Cover -> ConstStore -> C_IO (OP_List t0)
d_C_sequenceIO x1 x3250 x3500 = case x1 of
     OP_List -> d_C_return OP_List x3250 x3500
     (OP_Cons x2 x3) -> d_OP_gt_gt_eq x2 (d_OP_sequenceIO_dot___hash_lambda7 x3) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sequenceIO x1002 x3250 x3500) (d_C_sequenceIO x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sequenceIO z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sequenceIO x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_sequenceIO_dot___hash_lambda7 :: Curry t0 => OP_List (C_IO t0) -> t0 -> Cover -> ConstStore -> C_IO (OP_List t0)
d_OP_sequenceIO_dot___hash_lambda7 x1 x2 x3250 x3500 = d_OP_gt_gt_eq (d_C_sequenceIO x1 x3250 x3500) (d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 x2) x3250 x3500

d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 :: Curry t0 => t0 -> OP_List t0 -> Cover -> ConstStore -> C_IO (OP_List t0)
d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3250 x3500 = d_C_return (OP_Cons x1 x2) x3250 x3500

d_C_sequenceIO_ :: Curry t0 => Cover -> ConstStore -> OP_List (C_IO t0) -> Cover -> ConstStore -> C_IO OP_Unit
d_C_sequenceIO_ x3250 x3500 = d_C_foldr (acceptCs id d_OP_gt_gt) (d_C_done x3250 x3500)

nd_C_sequenceIO_ :: Curry t0 => IDSupply -> Cover -> ConstStore -> Func (OP_List (C_IO t0)) (C_IO OP_Unit)
nd_C_sequenceIO_ x3000 x3250 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_gt_gt)) (d_C_done x3250 x3500))

d_C_mapIO :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> C_IO t1) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_IO (OP_List t1)
d_C_mapIO x1 x3250 x3500 = d_OP_dot d_C_sequenceIO (d_C_map x1) x3250 x3500

nd_C_mapIO :: (Curry t0,Curry t1) => Func t0 (C_IO t1) -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) (C_IO (OP_List t1))
nd_C_mapIO x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dot (wrapDX id d_C_sequenceIO) (wrapNX id (nd_C_map x1)) x2000 x3250 x3500))

d_C_mapIO_ :: (Curry t1,Curry t0) => (t0 -> Cover -> ConstStore -> C_IO t1) -> Cover -> ConstStore -> OP_List t0 -> Cover -> ConstStore -> C_IO OP_Unit
d_C_mapIO_ x1 x3250 x3500 = d_OP_dot (d_C_sequenceIO_ x3250 x3500) (d_C_map x1) x3250 x3500

nd_C_mapIO_ :: (Curry t1,Curry t0) => Func t0 (C_IO t1) -> IDSupply -> Cover -> ConstStore -> Func (OP_List t0) (C_IO OP_Unit)
nd_C_mapIO_ x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_sequenceIO_ x2000 x3250 x3500) (wrapNX id (nd_C_map x1)) x2001 x3250 x3500)))))

d_C_foldIO :: (Curry t1,Curry t0) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> C_IO t0) -> t0 -> OP_List t1 -> Cover -> ConstStore -> C_IO t0
d_C_foldIO x1 x2 x3 x3250 x3500 = case x3 of
     OP_List -> d_C_return x2 x3250 x3500
     (OP_Cons x4 x5) -> d_OP_gt_gt_eq (d_C_apply (d_C_apply x1 x2 x3250 x3500) x4 x3250 x3500) (d_OP_foldIO_dot___hash_lambda9 x1 x5) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldIO x1 x2 x1002 x3250 x3500) (d_C_foldIO x1 x2 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldIO x1 x2 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldIO x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_foldIO :: (Curry t1,Curry t0) => Func t0 (Func t1 (C_IO t0)) -> t0 -> OP_List t1 -> IDSupply -> Cover -> ConstStore -> C_IO t0
nd_C_foldIO x1 x2 x3 x3000 x3250 x3500 = case x3 of
     OP_List -> d_C_return x2 x3250 x3500
     (OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x2 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) (wrapNX id (nd_OP_foldIO_dot___hash_lambda9 x1 x5)) x2003 x3250 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldIO x1 x2 x1002 x3000 x3250 x3500) (nd_C_foldIO x1 x2 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldIO x1 x2 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldIO x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_foldIO_dot___hash_lambda9 :: (Curry t1,Curry t0) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> C_IO t0) -> OP_List t1 -> t0 -> Cover -> ConstStore -> C_IO t0
d_OP_foldIO_dot___hash_lambda9 x1 x2 x3 x3250 x3500 = d_C_foldIO x1 x3 x2 x3250 x3500

nd_OP_foldIO_dot___hash_lambda9 :: (Curry t1,Curry t0) => Func t0 (Func t1 (C_IO t0)) -> OP_List t1 -> t0 -> IDSupply -> Cover -> ConstStore -> C_IO t0
nd_OP_foldIO_dot___hash_lambda9 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_foldIO x1 x3 x2 x2000 x3250 x3500))

d_C_liftIO :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> C_IO t0 -> Cover -> ConstStore -> C_IO t1
d_C_liftIO x1 x2 x3250 x3500 = d_OP_gt_gt_eq x2 (d_OP_dot d_C_return x1 x3250 x3500) x3250 x3500

nd_C_liftIO :: (Curry t0,Curry t1) => Func t0 t1 -> C_IO t0 -> IDSupply -> Cover -> ConstStore -> C_IO t1
nd_C_liftIO x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_gt_gt_eq x2 (nd_OP_dot (wrapDX id d_C_return) x1 x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_C_unknown :: Curry t0 => IDSupply -> Cover -> ConstStore -> t0
nd_C_unknown x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x1 = generate x2000 x3250
           in x1))

d_C_normalForm :: Curry t0 => t0 -> Cover -> ConstStore -> t0
d_C_normalForm x1 x3250 x3500 = d_OP_dollar_bang_bang d_C_id x1 x3250 x3500

d_C_groundNormalForm :: Curry t0 => t0 -> Cover -> ConstStore -> t0
d_C_groundNormalForm x1 x3250 x3500 = d_OP_dollar_hash_hash d_C_id x1 x3250 x3500

d_OP__case_0 :: C_Char -> C_Bool -> Cover -> ConstStore -> C_IO (OP_List C_Char)
d_OP__case_0 x1 x2 x3250 x3500 = case x2 of
     C_True -> d_C_return OP_List x3250 x3500
     C_False -> d_OP_gt_gt_eq (d_C_getLine x3250 x3500) (d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 x1) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3250 x3500) (d_OP__case_0 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: C_Int -> C_Bool -> Cover -> ConstStore -> C_Char
d_OP__case_1 x1 x2 x3250 x3500 = case x2 of
     C_True -> d_OP_dollar_hash d_C_prim_chr x1 x3250 x3500
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3250 x3500) (d_OP__case_1 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: C_Int -> C_Int -> C_Int -> C_Int -> C_Bool -> Cover -> ConstStore -> C_Bool
d_OP__case_3 x2 x3 x1 x4 x5 x3250 x3500 = case x5 of
     C_True -> d_OP_lt_eq x4 x1 x3250 x3500
     C_False -> d_OP__case_2 x1 x4 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_3 x2 x3 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 x3 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_Int -> C_Int -> C_Bool -> Cover -> ConstStore -> C_Bool
d_OP__case_2 x1 x4 x5 x3250 x3500 = case x5 of
     C_True -> d_OP_gt_eq x4 x1 x3250 x3500
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x4 x1002 x3250 x3500) (d_OP__case_2 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: C_Int -> C_Int -> C_Bool -> Cover -> ConstStore -> OP_List C_Int
d_OP__case_4 x2 x1 x3 x3250 x3500 = case x3 of
     C_True -> OP_List
     C_False -> OP_Cons x1 (d_C_enumFromTo (d_OP_plus x1 (C_Int 1#) x3250 x3500) x2 x3250 x3500)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1 x1002 x3250 x3500) (d_OP__case_4 x2 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: (Curry t0,Curry t1) => t0 -> OP_List (OP_Tuple2 t0 t1) -> OP_Tuple2 t0 t1 -> Cover -> ConstStore -> C_Maybe t1
d_OP__case_7 x1 x4 x3 x3250 x3500 = case x3 of
     (OP_Tuple2 x5 x6) -> d_OP__case_6 x5 x1 x4 x6 (d_OP_eq_eq x1 x5 x3250 x3500) x3250 x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x4 x1002 x3250 x3500) (d_OP__case_7 x1 x4 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x4 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: (Curry t0,Curry t1) => t0 -> t0 -> OP_List (OP_Tuple2 t0 t1) -> t1 -> C_Bool -> Cover -> ConstStore -> C_Maybe t1
d_OP__case_6 x5 x1 x4 x6 x7 x3250 x3500 = case x7 of
     C_True -> C_Just x6
     C_False -> d_OP__case_5 x4 x1 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x5 x1 x4 x6 x1002 x3250 x3500) (d_OP__case_6 x5 x1 x4 x6 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x5 x1 x4 x6 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x5 x1 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: (Curry t0,Curry t1) => OP_List (OP_Tuple2 t0 t1) -> t0 -> C_Bool -> Cover -> ConstStore -> C_Maybe t1
d_OP__case_5 x4 x1 x5 x3250 x3500 = case x5 of
     C_True -> d_C_lookup x1 x4 x3250 x3500
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x4 x1 x1002 x3250 x3500) (d_OP__case_5 x4 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x4 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: OP_List (OP_List C_Char) -> C_Bool -> Cover -> ConstStore -> OP_List C_Char
d_OP__case_8 x1 x2 x3250 x3500 = case x2 of
     C_True -> OP_List
     C_False -> d_C_foldr1 (acceptCs id d_OP_unwords_dot___hash_lambda3) x1 x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3250 x3500) (d_OP__case_8 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: OP_List C_Char -> C_Bool -> Cover -> ConstStore -> OP_List (OP_List C_Char)
d_OP__case_9 x2 x6 x3250 x3500 = case x6 of
     C_True -> OP_List
     C_False -> let
          x3 = d_C_apply (d_C_break d_OP_words_dot_isSpace_dot_273 x3250 x3500) x2 x3250 x3500
          x4 = d_OP_words_dot___hash_selFP21_hash_w x3 x3250 x3500
          x5 = d_OP_words_dot___hash_selFP22_hash_s2 x3 x3250 x3500
           in (OP_Cons x4 (d_C_words x5 x3250 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x2 x1002 x3250 x3500) (d_OP__case_9 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: C_Char -> OP_List C_Char -> C_Bool -> Cover -> ConstStore -> OP_Tuple2 (OP_List C_Char) (OP_List C_Char)
d_OP__case_10 x2 x3 x7 x3250 x3500 = case x7 of
     C_True -> OP_Tuple2 OP_List x3
     C_False -> let
          x4 = d_OP_lines_dot_splitline_dot_261 x3 x3250 x3500
          x5 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x4 x3250 x3500
          x6 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x4 x3250 x3500
           in (OP_Tuple2 (OP_Cons x2 x5) x6)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x3 x1002 x3250 x3500) (d_OP__case_10 x2 x3 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 x3 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_OP__case_12 x3 x1 x4 x8 x3250 x3500 = case x8 of
     C_True -> let
          x5 = d_C_span x1 x4 x3250 x3500
          x6 = d_OP_span_dot___hash_selFP12_hash_ys x5 x3250 x3500
          x7 = d_OP_span_dot___hash_selFP13_hash_zs x5 x3250 x3500
           in (OP_Tuple2 (OP_Cons x3 x6) x7)
     C_False -> d_OP__case_11 x4 x3 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_12 x3 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x3 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry t0 => t0 -> Func t0 C_Bool -> OP_List t0 -> C_Bool -> IDSupply -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
nd_OP__case_12 x3 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x5 = nd_C_span x1 x4 x2000 x3250 x3500
               x6 = d_OP_span_dot___hash_selFP12_hash_ys x5 x3250 x3500
               x7 = d_OP_span_dot___hash_selFP13_hash_zs x5 x3250 x3500
                in (OP_Tuple2 (OP_Cons x3 x6) x7)))
     C_False -> d_OP__case_11 x4 x3 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_12 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry t0 => OP_List t0 -> t0 -> C_Bool -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_OP__case_11 x4 x3 x5 x3250 x3500 = case x5 of
     C_True -> OP_Tuple2 OP_List (OP_Cons x3 x4)
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x3 x1002 x3250 x3500) (d_OP__case_11 x4 x3 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x3 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_List t0
d_OP__case_13 x3 x1 x4 x5 x3250 x3500 = case x5 of
     C_True -> d_C_dropWhile x1 x4 x3250 x3500
     C_False -> OP_Cons x3 x4
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_13 x3 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x3 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry t0 => t0 -> Func t0 C_Bool -> OP_List t0 -> C_Bool -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_OP__case_13 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_dropWhile x1 x4 x2000 x3250 x3500))
     C_False -> OP_Cons x3 x4
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_13 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_List t0
d_OP__case_14 x3 x1 x4 x5 x3250 x3500 = case x5 of
     C_True -> OP_Cons x3 (d_C_takeWhile x1 x4 x3250 x3500)
     C_False -> OP_List
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_14 x3 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x3 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry t0 => t0 -> Func t0 C_Bool -> OP_List t0 -> C_Bool -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_OP__case_14 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (OP_Cons x3 (nd_C_takeWhile x1 x4 x2000 x3250 x3500)))
     C_False -> OP_List
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_14 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry t0 => C_Int -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_OP__case_15 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> OP_Tuple2 OP_List x2
     C_False -> d_OP_splitAt_dot_splitAtp_dot_229 x1 x2 x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x2 x1002 x3250 x3500) (d_OP__case_15 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry t0 => C_Int -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_List t0
d_OP__case_16 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> x2
     C_False -> d_OP_drop_dot_dropp_dot_219 x1 x2 x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x2 x1002 x3250 x3500) (d_OP__case_16 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry t0 => C_Int -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_List t0
d_OP__case_17 x1 x2 x3 x3250 x3500 = case x3 of
     C_True -> OP_List
     C_False -> d_OP_take_dot_takep_dot_210 x1 x2 x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x2 x1002 x3250 x3500) (d_OP__case_17 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: (Curry t0,Curry t1,Curry t2) => OP_List (OP_Tuple3 t0 t1 t2) -> OP_Tuple3 t0 t1 t2 -> Cover -> ConstStore -> OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2)
d_OP__case_18 x3 x2 x3250 x3500 = case x2 of
     (OP_Tuple3 x4 x5 x6) -> let
          x7 = d_C_unzip3 x3 x3250 x3500
          x8 = d_OP_unzip3_dot___hash_selFP5_hash_xs x7 x3250 x3500
          x9 = d_OP_unzip3_dot___hash_selFP6_hash_ys x7 x3250 x3500
          x10 = d_OP_unzip3_dot___hash_selFP7_hash_zs x7 x3250 x3500
           in (OP_Tuple3 (OP_Cons x4 x8) (OP_Cons x5 x9) (OP_Cons x6 x10))
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x3 x1002 x3250 x3500) (d_OP__case_18 x3 x1003 x3250 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x3 z x3250 x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: (Curry t0,Curry t1) => OP_List (OP_Tuple2 t0 t1) -> OP_Tuple2 t0 t1 -> Cover -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t1)
d_OP__case_19 x3 x2 x3250 x3500 = case x2 of
     (OP_Tuple2 x4 x5) -> let
          x6 = d_C_unzip x3 x3250 x3500
          x7 = d_OP_unzip_dot___hash_selFP2_hash_xs x6 x3250 x3500
          x8 = d_OP_unzip_dot___hash_selFP3_hash_ys x6 x3250 x3500
           in (OP_Tuple2 (OP_Cons x4 x7) (OP_Cons x5 x8))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x3 x1002 x3250 x3500) (d_OP__case_19 x3 x1003 x3250 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x3 z x3250 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: (Curry t2,Curry t0,Curry t1,Curry t3) => OP_List t2 -> OP_List t0 -> (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2 -> Cover -> ConstStore -> t3) -> t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List t3
d_OP__case_21 x4 x6 x1 x5 x3 x3250 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x7 x8) -> d_OP__case_20 x8 x6 x1 x7 x5 x4 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x4 x6 x1 x5 x1002 x3250 x3500) (d_OP__case_21 x4 x6 x1 x5 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x4 x6 x1 x5 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x4 x6 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_21 :: (Curry t2,Curry t0,Curry t1,Curry t3) => OP_List t2 -> OP_List t0 -> Func t0 (Func t1 (Func t2 t3)) -> t0 -> OP_List t1 -> IDSupply -> Cover -> ConstStore -> OP_List t3
nd_OP__case_21 x4 x6 x1 x5 x3 x3000 x3250 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x8 x6 x1 x7 x5 x4 x2000 x3250 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x4 x6 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_21 x4 x6 x1 x5 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x4 x6 x1 x5 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x4 x6 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: (Curry t1,Curry t0,Curry t2,Curry t3) => OP_List t1 -> OP_List t0 -> (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2 -> Cover -> ConstStore -> t3) -> t1 -> t0 -> OP_List t2 -> Cover -> ConstStore -> OP_List t3
d_OP__case_20 x8 x6 x1 x7 x5 x4 x3250 x3500 = case x4 of
     OP_List -> OP_List
     (OP_Cons x9 x10) -> OP_Cons (d_C_apply (d_C_apply (d_C_apply x1 x5 x3250 x3500) x7 x3250 x3500) x9 x3250 x3500) (d_C_zipWith3 x1 x6 x8 x10 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x8 x6 x1 x7 x5 x1002 x3250 x3500) (d_OP__case_20 x8 x6 x1 x7 x5 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x8 x6 x1 x7 x5 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x8 x6 x1 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_20 :: (Curry t1,Curry t0,Curry t2,Curry t3) => OP_List t1 -> OP_List t0 -> Func t0 (Func t1 (Func t2 t3)) -> t1 -> t0 -> OP_List t2 -> IDSupply -> Cover -> ConstStore -> OP_List t3
nd_OP__case_20 x8 x6 x1 x7 x5 x4 x3000 x3250 x3500 = case x4 of
     OP_List -> OP_List
     (OP_Cons x9 x10) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2004 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2004 (seq x2005 (OP_Cons (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x5 x2000 x3250 x3500) x7 x2001 x3250 x3500)))) x9 x2003 x3250 x3500)))) (nd_C_zipWith3 x1 x6 x8 x10 x2005 x3250 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x8 x6 x1 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_20 x8 x6 x1 x7 x5 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x8 x6 x1 x7 x5 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x8 x6 x1 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: (Curry t0,Curry t1,Curry t2) => OP_List t0 -> (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List t2
d_OP__case_22 x5 x1 x4 x3 x3250 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> OP_Cons (d_C_apply (d_C_apply x1 x4 x3250 x3500) x6 x3250 x3500) (d_C_zipWith x1 x5 x7 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x5 x1 x4 x1002 x3250 x3500) (d_OP__case_22 x5 x1 x4 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x5 x1 x4 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x5 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_22 :: (Curry t0,Curry t1,Curry t2) => OP_List t0 -> Func t0 (Func t1 t2) -> t0 -> OP_List t1 -> IDSupply -> Cover -> ConstStore -> OP_List t2
nd_OP__case_22 x5 x1 x4 x3 x3000 x3250 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (OP_Cons (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x4 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) (nd_C_zipWith x1 x5 x7 x2003 x3250 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x5 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_22 x5 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x5 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x5 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: (Curry t0,Curry t1,Curry t2) => OP_List t2 -> OP_List t0 -> t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List (OP_Tuple3 t0 t1 t2)
d_OP__case_24 x3 x5 x4 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> d_OP__case_23 x7 x5 x6 x4 x3 x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x3 x5 x4 x1002 x3250 x3500) (d_OP__case_24 x3 x5 x4 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x3 x5 x4 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x3 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: (Curry t0,Curry t1,Curry t2) => OP_List t1 -> OP_List t0 -> t1 -> t0 -> OP_List t2 -> Cover -> ConstStore -> OP_List (OP_Tuple3 t0 t1 t2)
d_OP__case_23 x7 x5 x6 x4 x3 x3250 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x8 x9) -> OP_Cons (OP_Tuple3 x4 x6 x8) (d_C_zip3 x5 x7 x9 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x7 x5 x6 x4 x1002 x3250 x3500) (d_OP__case_23 x7 x5 x6 x4 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x7 x5 x6 x4 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x7 x5 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: (Curry t0,Curry t1) => OP_List t0 -> t0 -> OP_List t1 -> Cover -> ConstStore -> OP_List (OP_Tuple2 t0 t1)
d_OP__case_25 x4 x3 x2 x3250 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> OP_Cons (OP_Tuple2 x3 x5) (d_C_zip x4 x6 x3250 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x4 x3 x1002 x3250 x3500) (d_OP__case_25 x4 x3 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x4 x3 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> C_Bool) -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> OP_List t0
d_OP__case_26 x3 x1 x4 x5 x3250 x3500 = case x5 of
     C_True -> OP_Cons x3 (d_C_filter x1 x4 x3250 x3500)
     C_False -> d_C_filter x1 x4 x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_26 x3 x1 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x3 x1 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_26 :: Curry t0 => t0 -> Func t0 C_Bool -> OP_List t0 -> C_Bool -> IDSupply -> Cover -> ConstStore -> OP_List t0
nd_OP__case_26 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (OP_Cons x3 (nd_C_filter x1 x4 x2000 x3250 x3500)))
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_filter x1 x4 x2000 x3250 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_26 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0) -> t0 -> OP_List t0 -> Cover -> ConstStore -> t0
d_OP__case_27 x1 x3 x4 x3250 x3500 = case x4 of
     OP_List -> x3
     (OP_Cons x5 x6) -> d_C_apply (d_C_apply x1 x3 x3250 x3500) (d_C_foldr1 x1 (OP_Cons x5 x6) x3250 x3500) x3250 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x3 x1002 x3250 x3500) (d_OP__case_27 x1 x3 x1003 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x3 z x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_27 :: Curry t0 => Func t0 (Func t0 t0) -> t0 -> OP_List t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_OP__case_27 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     OP_List -> x3
     (OP_Cons x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_C_apply (nd_C_apply x1 x3 x2000 x3250 x3500) (nd_C_foldr1 x1 (OP_Cons x5 x6) x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_27 x1 x3 x1003 x3000 x3250 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x3 z x3000 x3250 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry t0 => C_Int -> OP_List t0 -> t0 -> C_Bool -> Cover -> ConstStore -> t0
d_OP__case_29 x2 x4 x3 x5 x3250 x3500 = case x5 of
     C_True -> x3
     C_False -> d_OP__case_28 x2 x4 (d_OP_gt x2 (C_Int 0#) x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x2 x4 x3 x1002 x3250 x3500) (d_OP__case_29 x2 x4 x3 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x2 x4 x3 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x2 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry t0 => C_Int -> OP_List t0 -> C_Bool -> Cover -> ConstStore -> t0
d_OP__case_28 x2 x4 x5 x3250 x3500 = case x5 of
     C_True -> d_OP_bang_bang x4 (d_OP_minus x2 (C_Int 1#) x3250 x3500) x3250 x3500
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x4 x1002 x3250 x3500) (d_OP__case_28 x2 x4 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 x4 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry t0 => t0 -> t0 -> C_Bool -> Cover -> ConstStore -> t0
d_OP__case_30 x2 x1 x3 x3250 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x2 x1 x1002 x3250 x3500) (d_OP__case_30 x2 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x2 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry t0 => t0 -> t0 -> C_Bool -> Cover -> ConstStore -> t0
d_OP__case_31 x2 x1 x3 x3250 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x2 x1 x1002 x3250 x3500) (d_OP__case_31 x2 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x2 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry t0 => t0 -> t0 -> C_Bool -> Cover -> ConstStore -> C_Ordering
d_OP__case_34 x2 x1 x3 x3250 x3500 = case x3 of
     C_True -> C_EQ
     C_False -> d_OP__case_33 x2 x1 (d_OP_lt_eq x1 x2 x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x2 x1 x1002 x3250 x3500) (d_OP__case_34 x2 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x2 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry t0 => t0 -> t0 -> C_Bool -> Cover -> ConstStore -> C_Ordering
d_OP__case_33 x2 x1 x3 x3250 x3500 = case x3 of
     C_True -> C_LT
     C_False -> d_OP__case_32 (d_C_otherwise x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x2 x1 x1002 x3250 x3500) (d_OP__case_33 x2 x1 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x2 x1 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: C_Bool -> Cover -> ConstStore -> C_Ordering
d_OP__case_32 x1 x3250 x3500 = case x1 of
     C_True -> C_GT
     C_False -> d_C_failed x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3250 x3500) (d_OP__case_32 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> C_Bool) -> (t0 -> Cover -> ConstStore -> t0) -> C_Bool -> Cover -> ConstStore -> t0
d_OP__case_35 x3 x1 x2 x4 x3250 x3500 = case x4 of
     C_True -> x3
     C_False -> d_C_until x1 x2 (d_C_apply x2 x3 x3250 x3500) x3250 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_35 x3 x1 x2 x1003 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x3 x1 x2 z x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_35 :: Curry t0 => t0 -> Func t0 C_Bool -> Func t0 t0 -> C_Bool -> IDSupply -> Cover -> ConstStore -> t0
nd_OP__case_35 x3 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     C_True -> x3
     C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_until x1 x2 (nd_C_apply x2 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_35 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ensureNotFree :: Curry t0 => t0 -> Cover -> ConstStore -> t0
d_C_ensureNotFree x1 x3250 x3500 = external_d_C_ensureNotFree x1 x3250 x3500

d_OP_dollar_bang :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_dollar_bang x1 x2 x3250 x3500 = external_d_OP_dollar_bang x1 x2 x3250 x3500

nd_OP_dollar_bang :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dollar_bang x1 x2 x3000 x3250 x3500 = external_nd_OP_dollar_bang x1 x2 x3000 x3250 x3500

d_OP_dollar_bang_bang :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_dollar_bang_bang x1 x2 x3250 x3500 = external_d_OP_dollar_bang_bang x1 x2 x3250 x3500

nd_OP_dollar_bang_bang :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dollar_bang_bang x1 x2 x3000 x3250 x3500 = external_nd_OP_dollar_bang_bang x1 x2 x3000 x3250 x3500

d_OP_dollar_hash_hash :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_dollar_hash_hash x1 x2 x3250 x3500 = external_d_OP_dollar_hash_hash x1 x2 x3250 x3500

nd_OP_dollar_hash_hash :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_OP_dollar_hash_hash x1 x2 x3000 x3250 x3500 = external_nd_OP_dollar_hash_hash x1 x2 x3000 x3250 x3500

d_C_prim_error :: Curry t0 => OP_List C_Char -> Cover -> ConstStore -> t0
d_C_prim_error x1 x3250 x3500 = external_d_C_prim_error x1 x3250 x3500

d_C_failed :: Curry t0 => Cover -> ConstStore -> t0
d_C_failed x3250 x3500 = external_d_C_failed x3250 x3500

d_OP_eq_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_eq_eq x1 x2 x3250 x3500 = external_d_OP_eq_eq x1 x2 x3250 x3500

d_OP_lt_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Bool
d_OP_lt_eq x1 x2 x3250 x3500 = external_d_OP_lt_eq x1 x2 x3250 x3500

d_C_prim_ord :: C_Char -> Cover -> ConstStore -> C_Int
d_C_prim_ord x1 x3250 x3500 = external_d_C_prim_ord x1 x3250 x3500

d_C_prim_chr :: C_Int -> Cover -> ConstStore -> C_Char
d_C_prim_chr x1 x3250 x3500 = external_d_C_prim_chr x1 x3250 x3500

d_OP_plus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_OP_plus x1 x2 x3250 x3500 = external_d_OP_plus x1 x2 x3250 x3500

d_OP_minus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_OP_minus x1 x2 x3250 x3500 = external_d_OP_minus x1 x2 x3250 x3500

d_OP_star :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_OP_star x1 x2 x3250 x3500 = external_d_OP_star x1 x2 x3250 x3500

d_C_div :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_C_div x1 x2 x3250 x3500 = external_d_C_div x1 x2 x3250 x3500

d_C_mod :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_C_mod x1 x2 x3250 x3500 = external_d_C_mod x1 x2 x3250 x3500

d_C_divMod :: C_Int -> C_Int -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
d_C_divMod x1 x2 x3250 x3500 = external_d_C_divMod x1 x2 x3250 x3500

d_C_quot :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_C_quot x1 x2 x3250 x3500 = external_d_C_quot x1 x2 x3250 x3500

d_C_rem :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
d_C_rem x1 x2 x3250 x3500 = external_d_C_rem x1 x2 x3250 x3500

d_C_quotRem :: C_Int -> C_Int -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
d_C_quotRem x1 x2 x3250 x3500 = external_d_C_quotRem x1 x2 x3250 x3500

d_C_negateFloat :: C_Float -> Cover -> ConstStore -> C_Float
d_C_negateFloat x1 x3250 x3500 = external_d_C_negateFloat x1 x3250 x3500

d_OP_eq_colon_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Success
d_OP_eq_colon_eq x1 x2 x3250 x3500 = external_d_OP_eq_colon_eq x1 x2 x3250 x3500

d_C_success :: Cover -> ConstStore -> C_Success
d_C_success x3250 x3500 = external_d_C_success x3250 x3500

d_OP_ampersand :: C_Success -> C_Success -> Cover -> ConstStore -> C_Success
d_OP_ampersand x1 x2 x3250 x3500 = external_d_OP_ampersand x1 x2 x3250 x3500

d_OP_gt_gt_eq :: (Curry t0,Curry t1) => C_IO t0 -> (t0 -> Cover -> ConstStore -> C_IO t1) -> Cover -> ConstStore -> C_IO t1
d_OP_gt_gt_eq x1 x2 x3250 x3500 = external_d_OP_gt_gt_eq x1 x2 x3250 x3500

nd_OP_gt_gt_eq :: (Curry t0,Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> Cover -> ConstStore -> C_IO t1
nd_OP_gt_gt_eq x1 x2 x3000 x3250 x3500 = external_nd_OP_gt_gt_eq x1 x2 x3000 x3250 x3500

d_C_return :: Curry t0 => t0 -> Cover -> ConstStore -> C_IO t0
d_C_return x1 x3250 x3500 = external_d_C_return x1 x3250 x3500

d_C_prim_putChar :: C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_prim_putChar x1 x3250 x3500 = external_d_C_prim_putChar x1 x3250 x3500

d_C_getChar :: Cover -> ConstStore -> C_IO C_Char
d_C_getChar x3250 x3500 = external_d_C_getChar x3250 x3500

d_C_prim_readFile :: OP_List C_Char -> Cover -> ConstStore -> C_IO (OP_List C_Char)
d_C_prim_readFile x1 x3250 x3500 = external_d_C_prim_readFile x1 x3250 x3500

d_C_prim_writeFile :: OP_List C_Char -> OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_prim_writeFile x1 x2 x3250 x3500 = external_d_C_prim_writeFile x1 x2 x3250 x3500

d_C_prim_appendFile :: OP_List C_Char -> OP_List C_Char -> Cover -> ConstStore -> C_IO OP_Unit
d_C_prim_appendFile x1 x2 x3250 x3500 = external_d_C_prim_appendFile x1 x2 x3250 x3500

d_C_prim_ioError :: Curry t0 => C_IOError -> Cover -> ConstStore -> C_IO t0
d_C_prim_ioError x1 x3250 x3500 = external_d_C_prim_ioError x1 x3250 x3500

d_C_catch :: Curry t0 => C_IO t0 -> (C_IOError -> Cover -> ConstStore -> C_IO t0) -> Cover -> ConstStore -> C_IO t0
d_C_catch x1 x2 x3250 x3500 = external_d_C_catch x1 x2 x3250 x3500

nd_C_catch :: Curry t0 => C_IO t0 -> Func C_IOError (C_IO t0) -> IDSupply -> Cover -> ConstStore -> C_IO t0
nd_C_catch x1 x2 x3000 x3250 x3500 = external_nd_C_catch x1 x2 x3000 x3250 x3500

d_C_prim_show :: Curry t0 => t0 -> Cover -> ConstStore -> OP_List C_Char
d_C_prim_show x1 x3250 x3500 = external_d_C_prim_show x1 x3250 x3500

nd_OP_qmark :: Curry t0 => t0 -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_OP_qmark x1 x2 x3000 x3250 x3500 = external_nd_OP_qmark x1 x2 x3000 x3250 x3500

d_C_apply :: (Curry t0,Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t1
d_C_apply x1 x2 x3250 x3500 = external_d_C_apply x1 x2 x3250 x3500

nd_C_apply :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_apply x1 x2 x3000 x3250 x3500 = external_nd_C_apply x1 x2 x3000 x3250 x3500

d_C_cond :: Curry t0 => C_Success -> t0 -> Cover -> ConstStore -> t0
d_C_cond x1 x2 x3250 x3500 = external_d_C_cond x1 x2 x3250 x3500

d_OP_eq_colon_lt_eq :: Curry t0 => t0 -> t0 -> Cover -> ConstStore -> C_Success
d_OP_eq_colon_lt_eq x1 x2 x3250 x3500 = external_d_OP_eq_colon_lt_eq x1 x2 x3250 x3500
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a)
      => Curry a where
  -- implementation of strict equalit (==) for a data type
  (=?=) :: a -> a -> Cover -> ConstStore -> C_Bool
  (=?=) = error "(==) is undefined"
  -- implementation of less-or-equal (<=) for a data type
  (<?=) :: a -> a -> Cover -> ConstStore -> C_Bool
  (<?=) = error "(<=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(==) is undefined for primitive data"
  (<?=) = error "(<=) is undefined for primitive data"


-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry_Prelude.Curry C_Success where
  (=?=) (Choice_C_Success cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Success cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Success cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Success cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Success cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Success cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Success cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Success cd info) _ _ = failCons cd info
  (=?=) C_Success C_Success d cs = Curry_Prelude.C_True
  (<?=) (Choice_C_Success cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Success cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Success cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Success cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Success cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Success cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Success cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Success cd info) _ _ = failCons cd info
  (<?=) C_Success C_Success d cs = Curry_Prelude.C_True  
-- END GENERATED FROM PrimTypes.curry


instance (Curry t0,Curry t1) => Curry (Func t0 t1) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

instance Curry t0 => Curry (C_IO t0) where
  (=?=) = error "(==) is undefined for I/O actions"
  (<?=) = error "(<=) is undefined for I/O actions"


instance NonDet b => Curry (a -> b) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Int#
     | C_CurryInt BinInt
     | Choice_C_Int Cover ID C_Int C_Int
     | Choices_C_Int Cover ID ([C_Int])
     | Fail_C_Int Cover FailInfo
     | Guard_C_Int Cover Constraints C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Int cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Int cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Int _ _) = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Int: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> shows x1
    Choices_BinInt _ _ _  -> shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (I# (curryint2primint gnfBinInt))

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int cd i x y) = tryChoice cd i x y
  try (Choices_C_Int cd i xs) = tryChoices cd i xs
  try (Fail_C_Int cd info) = Fail cd info
  try (Guard_C_Int cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Int cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Int cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Int cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Int _  i _) = error ("Prelude.Int.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Int cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Int cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Int where
  generate s cd = Choices_C_Int cd (freeID [1] s) [C_CurryInt (generate (leftSupply s) cd)]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cd cs = cont x cd cs
  ($!!) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cd cs
  ($!!) cont (Choice_C_Int d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Int d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Int cd info) _ _ = failCons cd info
  ($##) cont x@(C_Int _) cd cs = cont x cd cs
  ($##) cont (C_CurryInt x1) cd cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cd cs
  ($##) cont (Choice_C_Int d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Int d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Int d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Int d info) _ _ = failCons d info
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) cd _  = if (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:= y1) cd cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:= (primint2curryint y1)) cd cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:= y1) cd cs
  (=.=) _               _               cd _  = Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_Int      y1) cd _ = if (x1 ==# y1) then C_Success else Fail_C_Success cd defFailInfo
  (=.<=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =:<= y1) cd cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =:<= (primint2curryint y1)) cd cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =:<= y1) cd cs
  (=.<=) _ _ cd _= Fail_C_Success cd defFailInfo
  bind cd i (C_Int      x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primint2curryint x2)
  bind cd i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind cd (leftID i) x2
  bind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Int d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ _ (Fail_C_Int _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Int _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primint2curryint x2))]
  lazyBind cd i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x2)]
  lazyBind cd i (Choice_C_Int d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Int d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Int d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Int _ i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Int _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Int _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry_Prelude.Curry C_Int where
  (=?=) (Choice_C_Int d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Int d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Int d c x) y cd cs = guardCons d c ((x =?= y) cd $! (addCs c cs))
  (=?=) (Fail_C_Int d info) _ _ _ = failCons d info
  (=?=) z (Choice_C_Int d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Int d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Int d c x) cd cs = guardCons d c ((y =?= x) cd $! (addCs c cs))
  (=?=) _ (Fail_C_Int d info) _ _ = failCons d info
  (=?=) (C_Int      x1) (C_Int      y1) _ _ = toCurry (x1 ==# y1)
  (=?=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) =?= y1) cd cs
  (=?=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 =?= (primint2curryint y1)) cd cs
  (=?=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 =?= y1) cd cs
  (<?=) (Choice_C_Int d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Int d i xs) y cd cs = narrows cs d i (\x -> (x<?= y) cd cs) xs
  (<?=) (Guard_C_Int d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Int d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Int d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Int d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Int d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Int d info) _ _ = failCons d info
  (<?=) (C_Int      x1) (C_Int      y1) _ _ = toCurry (x1 <=# y1)
  (<?=) (C_Int      x1) (C_CurryInt y1) cd cs = ((primint2curryint x1) `d_C_lteqInteger` y1) cd cs
  (<?=) (C_CurryInt x1) (C_Int      y1) cd cs = (x1 `d_C_lteqInteger` (primint2curryint y1)) cd cs
  (<?=) (C_CurryInt x1) (C_CurryInt y1) cd cs = (x1 `d_C_lteqInteger` y1) cd cs
-- END GENERATED FROM PrimTypes.curry

primint2curryint :: Int# -> BinInt
primint2curryint n
  | n <#  0#  = Neg (primint2currynat (negateInt# n))
  | n ==# 0#  = Zero
  | otherwise = Pos (primint2currynat n)

primint2currynat :: Int# -> Nat
primint2currynat n
  | n ==# 1#                = IHi
  | (n `remInt#` 2#) ==# 0# = O (primint2currynat (n `quotInt#` 2#))
  | otherwise               = I (primint2currynat (n `quotInt#` 2#))

currynat2primint :: Nat -> Int#
currynat2primint IHi   = 1#
currynat2primint (O n) = 2# *# currynat2primint n
currynat2primint (I n) = 2# *# currynat2primint n +# 1#
currynat2primint _ = error "KiCS2 error: Prelude.currynat2primint: no ground term"

curryint2primint :: BinInt -> Int#
curryint2primint Zero    = 0#
curryint2primint (Pos n) = currynat2primint n
curryint2primint (Neg n) = negateInt# (currynat2primint n)
curryint2primint _ = error "KiCS2 error: Prelude.curryint2primint: no ground term"

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = C_Float Float#
     | Choice_C_Float Cover ID C_Float C_Float
     | Choices_C_Float Cover ID ([C_Float])
     | Fail_C_Float Cover FailInfo
     | Guard_C_Float Cover (Constraints) C_Float

instance Show C_Float where
  showsPrec d (Choice_C_Float cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Float cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Float cd c e) = showsGuard d cd c e
  showsPrec d (Fail_C_Float _ _) = showChar '!'
  showsPrec d (C_Float x1) = shows (F# x1)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s) where readFloat (F# f, s) = (C_Float f, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  choicesCons = Choices_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float cd i x y) = tryChoice cd i x y
  try (Choices_C_Float cd i xs) = tryChoices cd i xs
  try (Fail_C_Float cd info) = Fail cd info
  try (Guard_C_Float cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Float cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Float cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Float cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Float cd i@(ChoiceID _) _) = error ("Prelude.Float.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Float cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Float cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Float where
  generate = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) cd cs = cont x cd cs
  ($!!) cont (Choice_C_Float d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Float d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Float d info) _ _ = failCons d info
  ($##) cont x@(C_Float _) cd cs = cont x cd cs
  ($##) cont (Choice_C_Float d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Float d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Float d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Float d info) _ _ = failCons d info
  searchNF search cont x@(C_Float _) = cont x
  searchNF _ _ x = error ("Prelude.Float.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Float where
  (=.=) _ _ cd _  = Fail_C_Success cd defFailInfo
  (=.<=) _ _ cd _ = Fail_C_Success cd defFailInfo
  bind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Float d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  bind cd i (Guard_C_Float _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (Choice_C_Float d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Float d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Float d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Float _ i _) = error ("Prelude.Float.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Float _ info) = [Unsolvable info]
  lazyBind cd  i (Guard_C_Float _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Float d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Float d c x) y cd cs = guardCons d c ((x =?= y) cd  $! (addCs c cs))
  (=?=) (Fail_C_Float d info) _ _ _= failCons d info
  (=?=) z (Choice_C_Float d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Float d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Float d c x) cd cs = guardCons d c ((y =?= x) cd  $! (addCs c cs))
  (=?=) _ (Fail_C_Float d info) _ _ = failCons d info
  (=?=) (C_Float x1) (C_Float y1) _ _ = toCurry (x1 `eqFloat#` y1)
  (<?=) (Choice_C_Float d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Float d i xs) y cd cs = narrows cs d i (\x -> (x <?= y) cd cs) xs
  (<?=) (Guard_C_Float d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Float d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Float d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Float d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Float d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Float d info) _ _ = failCons d info
  (<?=) (C_Float x1) (C_Float y1) _ _ = toCurry (x1 `leFloat#` y1)

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = C_Char Char#
     | CurryChar BinInt
     | Choice_C_Char Cover ID C_Char C_Char
     | Choices_C_Char Cover ID ([C_Char])
     | Fail_C_Char Cover FailInfo
     | Guard_C_Char Cover (Constraints) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Char cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Char cd c e) = showsGuard d d c e
  showsPrec d (Fail_C_Char _ _) = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))
  showsPrec d (CurryChar x1) = case ((\x _ _ -> x) $## x1) (error "Show C_Char: nesting depth used") emptyCs of
    Choice_BinInt _ _ _ _ -> showString "chr " . shows x1
    Choices_BinInt _ _ _  -> showString "chr " . shows x1
    Fail_BinInt _ _       -> shows x1
    Guard_BinInt _ _ _    -> shows x1
    gnfBinInt             -> shows (C# (curryChar2primChar gnfBinInt))

  showList cs = showList (map convert cs)
   where
    convert (C_Char c) = C# c
    convert (CurryChar c) = C# (curryChar2primChar c)

instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)

  readList s = map readString (readList s) where readString (cs, s) = (map (\(C# c) -> C_Char c) cs, s)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  choicesCons = Choices_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char cd i x y) = tryChoice cd i x y
  try (Choices_C_Char cd i xs) = tryChoices cd i xs
  try (Fail_C_Char cd info) = Fail cd info
  try (Guard_C_Char cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Char cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Char cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Char cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Char cd i _) = error ("Prelude.Char.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Char cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Char cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Char where
  generate s cd = Choices_C_Char cd (freeID [1] s) [CurryChar (generate (leftSupply s) cd)]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) cd cs = cont x cd cs
  ($!!) cont (CurryChar x) cd cs = ((cont . CurryChar) $!! x) cd cs
  ($!!) cont (Choice_C_Char d i x y) cd cs = nfChoice cont d i x y cd cs
  ($!!) cont (Choices_C_Char d i xs) cd cs = nfChoices cont d i xs cd cs
  ($!!) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $!! x) cd $! (addCs c cs))
  ($!!) _ (Fail_C_Char d info) _ _ = failCons d info
  ($##) cont x@(C_Char _) cd cs = cont x cd cs
  ($##) cont (CurryChar x) cd cs = ((cont . CurryChar) $## x) cd cs
  ($##) cont (Choice_C_Char d i x y) cd cs = gnfChoice cont d i x y cd cs
  ($##) cont (Choices_C_Char d i xs) cd cs = gnfChoices cont d i xs cd cs
  ($##) cont (Guard_C_Char d c x) cd cs = guardCons d c ((cont $## x) cd $! (addCs c cs))
  ($##) _ (Fail_C_Char d info) _ _ = failCons d info
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x
  searchNF _ _ x = error ("Prelude.Char.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) cd _ | x1 `eqChar#` x2 = C_Success
                                                | otherwise = Fail_C_Success cd defFailInfo
  (=.=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:= x2) cd cs
  (=.=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:= primChar2CurryChar x2) cd cs
  (=.=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:= x2) cd cs
  (=.=) _                 _                cd _  = Fail_C_Success cd  defFailInfo
  (=.<=) (C_Char       x1) (C_Char      x2) cd _ | x1 `eqChar#` x2 = C_Success
                                                 | otherwise = Fail_C_Success cd defFailInfo
  (=.<=) (C_Char       x1) (CurryChar x2)   cd cs = (primChar2CurryChar x1 =:<= x2) cd cs
  (=.<=) (CurryChar  x1) (C_Char      x2)   cd cs = (x1 =:<= primChar2CurryChar x2) cd cs
  (=.<=) (CurryChar x1)    (CurryChar   x2) cd cs = (x1 =:<= x2) cd cs
  (=.<=) _                 _                cd _  = Fail_C_Success cd defFailInfo
  bind cd i (C_Char    x) = (i :=: ChooseN 0 1) : bind cd (leftID i) (primChar2CurryChar x)
  bind cd i (CurryChar x) = (i :=: ChooseN 0 1) : bind cd (leftID i) x
  bind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (bind cd i l) (bind cd i r))]
  bind cd i (Choices_C_Char d j@(FreeID _ _) xs) = bindOrNarrow cd i d j xs
  bind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (bind cd i) xs))]
  bind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.bind: Choices with ChoiceID: " ++ (show c))
  bind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  bind cd  i (Guard_C_Char _ cs e) = getConstrList cs ++ (bind cd i e)
  lazyBind cd i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) (primChar2CurryChar x))]
  lazyBind cd i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind cd (leftID i) x)]
  lazyBind cd i (Choice_C_Char d j l r) = [(ConstraintChoice d j (lazyBind cd i l) (lazyBind cd i r))]
  lazyBind cd i (Choices_C_Char d j@(FreeID _ _) xs) = lazyBindOrNarrow cd i d j xs
  lazyBind cd i (Choices_C_Char d j@(NarrowedID _ _) xs) = [(ConstraintChoices d j (map (lazyBind cd i) xs))]
  lazyBind _  _ c@(Choices_C_Char _ i _) = error ("Prelude.Char.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _  _ (Fail_C_Char _ info) = [Unsolvable info]
  lazyBind cd i (Guard_C_Char _ cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind cd i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char d i x y) z cd cs = narrow d i ((x =?= z) cd cs) ((y =?= z) cd cs)
  (=?=) (Choices_C_Char d i xs) y cd cs = narrows cs d i (\x -> (x =?= y) cd cs) xs
  (=?=) (Guard_C_Char d c x) y cd cs = guardCons d c ((x =?= y) cd $! (addCs c cs))
  (=?=) (Fail_C_Char d info) _ _ _ = failCons d info
  (=?=) z (Choice_C_Char d i x y) cd cs = narrow d i ((z =?= x) cd cs) ((z =?= y) cd cs)
  (=?=) y (Choices_C_Char d i xs) cd cs = narrows cs d i (\x -> (y =?= x) cd cs) xs
  (=?=) y (Guard_C_Char d c x) cd cs = guardCons d c ((y =?= x) cd $! (addCs c cs))
  (=?=) _ (Fail_C_Char d info) _ _ = failCons d info
  (=?=) (C_Char x1) (C_Char y1) _ _ = toCurry (x1 `eqChar#` y1)
  (=?=) (C_Char      x1) (CurryChar y1) cd cs = ((primChar2CurryChar x1) =?= y1) cd cs
  (=?=) (CurryChar x1) (C_Char      y1) cd cs = (x1 =?= (primChar2CurryChar y1)) cd cs
  (=?=) (CurryChar x1) (CurryChar y1) cd cs = (x1 =?= y1) cd cs
  (<?=) (Choice_C_Char d i x y) z cd cs = narrow d i ((x <?= z) cd cs) ((y <?= z) cd cs)
  (<?=) (Choices_C_Char d i xs) y cd cs = narrows cs d i (\x -> (x <?= y) cd cs) xs
  (<?=) (Guard_C_Char d c x) y cd cs = guardCons d c ((x <?= y) cd $! (addCs c cs))
  (<?=) (Fail_C_Char d info) _ _ _ = failCons d info
  (<?=) z (Choice_C_Char d i x y) cd cs = narrow d i ((z <?= x) cd cs) ((z <?= y) cd cs)
  (<?=) y (Choices_C_Char d i xs) cd cs = narrows cs d i (\x -> (y <?= x) cd cs) xs
  (<?=) y (Guard_C_Char d c x) cd cs = guardCons d c ((y <?= x) cd $! (addCs c cs))
  (<?=) _ (Fail_C_Char d info) _ _ = failCons d info
  (<?=) (C_Char x1) (C_Char y1) _ _ = toCurry (x1 `leChar#` y1)
  (<?=) (C_Char      x1) (CurryChar y1) cd cs = ((primChar2CurryChar x1) `d_C_lteqInteger` y1) cd cs
  (<?=) (CurryChar x1) (C_Char      y1) cd cs = (x1 `d_C_lteqInteger` (primChar2CurryChar y1)) cd cs
  (<?=) (CurryChar x1) (CurryChar y1) cd cs = (x1 `d_C_lteqInteger` y1) cd cs


primChar2CurryChar :: Char# -> BinInt
primChar2CurryChar c = primint2curryint (ord# c)

curryChar2primChar :: BinInt -> Char#
curryChar2primChar c = chr# (curryint2primint c)
-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i)      = I# i
  fromCurry (C_CurryInt i) = I# (curryint2primint i)
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)
   where
    int2C_Int (I# c) = C_Int c

  fromCurry (C_Int      i) = toInteger (I# i)
  fromCurry (C_CurryInt i) = toInteger (I# (curryint2primint i))
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Float Float where
  toCurry (F# f) = C_Float f

  fromCurry (C_Float f) = F# f
  fromCurry _           = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char    c) = C# c
  fromCurry (CurryChar c) = C# (curryChar2primChar c)
  fromCurry _             = error "KiCS2 error: Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "KiCS2 error: List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "KiCS2 error: Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "KiCS2 error: Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "KiCS2 error: Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "KiCS2 error: Maybe data with no ground term occurred"

--fromOrdering :: Ordering -> C_Ordering
--fromOrdering LT = C_LT
--fromOrdering EQ = C_EQ
--fromOrdering GT = C_GT


-- ---------------------------------------------------------------------------
-- Auxiliary operations for showing lists
-- ---------------------------------------------------------------------------

showsPrec4CurryList :: Show a => Int -> OP_List a -> ShowS
showsPrec4CurryList d cl =
  if isStandardCurryList cl
  then showsPrec d (clist2hlist cl)
  else showChar '(' . showsPrecRaw d cl . showChar ')'
 where
  isStandardCurryList OP_List = True
  isStandardCurryList (OP_Cons _ xs) = isStandardCurryList xs
  isStandardCurryList _ = False

  clist2hlist OP_List = []
  clist2hlist (OP_Cons x xs) = x : clist2hlist xs

  showsPrecRaw d (Choice_OP_List cd i x y) = showsChoice d cd i x y
  showsPrecRaw d (Choices_OP_List cd i xs) = showsChoices d cd i xs
  showsPrecRaw d (Guard_OP_List cd c e) = showsGuard d cd c e
  showsPrecRaw d (Fail_OP_List _ _) = showChar '!'
  showsPrecRaw d OP_List = showString "[]"
  showsPrecRaw d (OP_Cons x xs) =
    showParen (d > 5) (showsPrec 6 x . showChar ':' . showsPrecRaw 5 xs)


--- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

external_d_C_ensureNotFree :: Curry a => a -> Cover -> ConstStore -> a
external_d_C_ensureNotFree x cd cs =
  case try x of
    Choice d i a b  -> choiceCons d i (external_d_C_ensureNotFree a cd cs)
                                      (external_d_C_ensureNotFree b cd cs)
    Narrowed d i xs -> choicesCons d i (map (\x -> external_d_C_ensureNotFree x cd cs) xs)
    Free d i xs     -> narrows cs d i (\x -> external_d_C_ensureNotFree x cd cs) xs
    Guard d c e     -> guardCons d c (external_d_C_ensureNotFree e cd $! (addCs c cs))
    _               -> x

external_d_C_failed :: NonDet a => Cover -> ConstStore -> a
external_d_C_failed cd _ = failCons cd defFailInfo

external_d_OP_eq_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Bool
external_d_OP_lt_eq = (<?=)

-- characters

external_d_C_prim_ord :: C_Char -> Cover -> ConstStore -> C_Int
external_d_C_prim_ord (C_Char c)    _ _ = C_Int (ord# c)
external_d_C_prim_ord (CurryChar c) _ _ = C_CurryInt c

external_d_C_prim_chr :: C_Int -> Cover -> ConstStore -> C_Char
external_d_C_prim_chr (C_Int i)      _ _ = C_Char (chr# i)
external_d_C_prim_chr (C_CurryInt i) _ _ = CurryChar i

-- int arithmetics

external_d_OP_plus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) _ _  = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_OP_plus_hash` y) cd cs)
external_d_OP_plus (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_OP_plus_hash` (primint2curryint y)) cd cs)
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_OP_plus_hash` y) cd cs)
external_d_OP_plus x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_plus` b) cd2 cs2)) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_OP_minus :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) _ _  = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_OP_minus_hash` y) cd cs)
external_d_OP_minus (C_CurryInt x) (C_Int y)      cd cs = C_CurryInt ((x `d_OP_minus_hash` (primint2curryint y)) cd cs)
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_OP_minus_hash` y) cd cs)
external_d_OP_minus x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_minus` b) cd2 cs2 )) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_OP_star :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) _ _  = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_OP_star_hash` y) cd cs)
external_d_OP_star (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_OP_star_hash` (primint2curryint y)) cd cs)
external_d_OP_star (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_OP_star_hash` y) cd cs)
external_d_OP_star x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_OP_star` b) cd2 cs2)) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quot :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_C_Int cd defFailInfo
  | otherwise = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_C_quotInteger` y) cd cs)
external_d_C_quot (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_C_quotInteger` (primint2curryint y)) cd cs)
external_d_C_quot (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_C_quotInteger` y) cd cs)
external_d_C_quot x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quot` b) cd2 cs2 )) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_rem :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_C_Int cd defFailInfo
  | otherwise = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_C_remInteger` y) cd cs)
external_d_C_rem (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_C_remInteger` (primint2curryint y)) cd cs)
external_d_C_rem (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_C_remInteger` y) cd cs)
external_d_C_rem x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_rem` b) cd2 cs2)) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_quotRem :: C_Int -> C_Int -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_OP_Tuple2 cd defFailInfo
  | otherwise = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) cd cs = (mkIntTuple `d_dollar_bang` (((primint2curryint x) `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem (C_CurryInt x) (C_Int      y) cd cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) cd cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` y) cd cs)) cd cs
external_d_C_quotRem x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_quotRem` b) cd2 cs2)) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

external_d_C_div :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_div (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_C_Int cd defFailInfo
  | otherwise = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_C_divInteger` y) cd cs)
external_d_C_div (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_C_divInteger` (primint2curryint y)) cd cs)
external_d_C_div (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_C_divInteger` y) cd cs)
external_d_C_div x y cd cs = ((\a cd1 cs1-> ((\b cd2 cs2-> ((a `external_d_C_div` b) cd2 cs2)) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

-- PrimOp taken from GHC.Base
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | (x# ># 0#) && (y# <# 0#) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | (x# <# 0#) && (y# ># 0#) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                = x# `quotInt#` y#

external_d_C_mod :: C_Int -> C_Int -> Cover -> ConstStore -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_C_Int cd defFailInfo
  | otherwise = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) cd cs = C_CurryInt (((primint2curryint x) `d_C_modInteger` y) cd cs)
external_d_C_mod (C_CurryInt x) (C_Int      y) cd cs = C_CurryInt ((x `d_C_modInteger` (primint2curryint y)) cd cs)
external_d_C_mod (C_CurryInt x) (C_CurryInt y) cd cs = C_CurryInt ((x `d_C_modInteger` y) cd cs)
external_d_C_mod x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_mod` b)) cd2 cs2) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

-- PrimOp taken from GHC.Base
modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | (x# ># 0#) && (y# <# 0#) ||
      (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
    | otherwise                   = r#
    where
    !r# = x# `remInt#` y#

-- TODO: $! instead of $#?
external_d_C_divMod :: C_Int -> C_Int ->  Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y) cd _
  | y ==# 0#  = Fail_OP_Tuple2 cd defFailInfo
  | otherwise = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) cd cs = (mkIntTuple `d_OP_dollar_hash` (((primint2curryint x) `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod (C_CurryInt x) (C_Int      y) cd cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` (primint2curryint y)) cd cs)) cd cs
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) cd cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` y) cd cs)) cd cs
external_d_C_divMod x y cd cs = ((\a cd1 cs1 -> ((\b cd2 cs2 -> ((a `external_d_C_divMod` b) cd2 cs2 )) `d_OP_dollar_hash` y) cd1 cs1) `d_OP_dollar_hash` x) cd cs

mkIntTuple :: OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) _ _ = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> Cover -> ConstStore -> C_Float
external_d_C_negateFloat (C_Float x) _ _ = C_Float (negateFloat# x)
external_d_C_negateFloat x cd cs          = (external_d_C_negateFloat `d_OP_dollar_hash` x) cd cs

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> Cover -> ConstStore -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: Cover -> ConstStore -> C_Success
external_d_C_success _ _ = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> Cover -> ConstStore -> C_Success
external_d_OP_ampersand = (&)

-- IO stuff

external_d_C_return :: a -> Cover -> ConstStore -> C_IO a
external_d_C_return a _ _ = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_putChar c _ _ = toCurry putChar c

external_d_C_getChar :: Cover -> ConstStore -> C_IO C_Char
external_d_C_getChar _ _ = toCurry getChar

external_d_C_prim_readFile :: C_String -> Cover -> ConstStore -> C_IO C_String
external_d_C_prim_readFile s _ _ = toCurry readFile s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_writeFile s1 s2 _ _ = toCurry writeFile s1 s2

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String -> Cover -> ConstStore -> C_IO OP_Unit
external_d_C_prim_appendFile s1 s2 _ _ = toCurry appendFile s1 s2

external_d_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> Cover -> ConstStore -> C_IO t1) -> Cover -> ConstStore -> C_IO t1
external_d_OP_gt_gt_eq m f cd cs = fromIO $ do
  x <- toIO m cs
  cs1 <- lookupGlobalCs
  let cs2 = combineCs cs cs1
  toIO  (f x cd cs2) cs2

external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> Cover -> ConstStore -> C_IO t1
external_nd_OP_gt_gt_eq m f s cd cs = fromIO $ do
 x <- toIO m cs
 cs1 <- lookupGlobalCs
 let cs2 = combineCs cs cs1
 toIO (nd_apply f x s cd cs2) cs2

-- Exception handling

instance ConvertCurryHaskell C_IOError CurryException where
  toCurry (IOException     s) = C_IOError     (toCurry s)
  toCurry (UserException   s) = C_UserError   (toCurry s)
  toCurry (FailException   s) = C_FailError   (toCurry s)
  toCurry (NondetException s) = C_NondetError (toCurry s)

  fromCurry (C_IOError     s) = IOException     $ fromCurry s
  fromCurry (C_UserError   s) = UserException   $ fromCurry s
  fromCurry (C_FailError   s) = FailException   $ fromCurry s
  fromCurry (C_NondetError s) = NondetException $ fromCurry s
  fromCurry _                 = internalError "non-deterministic IOError"

external_d_C_prim_error :: C_String -> Cover -> ConstStore -> a
external_d_C_prim_error s _ _ = C.throw $ UserException (fromCurry s)

external_d_C_prim_ioError :: C_IOError -> Cover -> ConstStore -> C_IO a
external_d_C_prim_ioError e _ _ = C.throw $ (fromCurry e :: CurryException)

external_d_C_catch :: C_IO a -> (C_IOError -> Cover -> ConstStore -> C_IO a) -> Cover -> ConstStore -> C_IO a
external_d_C_catch act hndl cd cs = fromIO $ C.catches (toIO act cs) handlers
  where handlers = exceptionHandlers cs (\e -> hndl e cd cs)

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> Cover -> ConstStore -> C_IO a
external_nd_C_catch act hndl s cd cs = fromIO $ C.catches (toIO act cs) handlers
  where handlers = exceptionHandlers cs (\e -> nd_apply hndl e s cd cs)

exceptionHandlers :: ConstStore -> (C_IOError -> C_IO a) -> [C.Handler a]
exceptionHandlers cs hndl =
  [ C.Handler (\ (e :: CurryException) -> toIO (hndl $ toCurry         e) cs)
  , C.Handler (\ (e ::  C.IOException) -> toIO (hndl $ fromIOException e) cs)
  ] where fromIOException = toCurry . IOException . show

-- other stuff

external_d_C_prim_show :: Show a => a -> Cover -> ConstStore -> C_String
external_d_C_prim_show a _ _ = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> Cover -> ConstStore -> a
external_d_C_cond succ a cd cs = ((\_ _ _ -> a) `d_OP_dollar_hash` succ) cd cs

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> Cover -> ConstStore -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> Cover -> ConstStore -> a
external_nd_OP_qmark x y ids cd _ = let i = thisID ids in i `seq` choiceCons cd i x y

-- External HO
-- -----------

external_d_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_bang_bang f x s cd cs = ((\y cd1 cs1-> nd_apply f y s cd1 cs1) $!! x) cd cs

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_OP_dollar_hash_hash f x s cd cs = ((\y cd1 cs1 -> nd_apply f y s cd1 cs1) $## x) cd cs

external_d_C_apply :: (a -> Cover -> ConstStore -> b) -> a -> Cover -> ConstStore -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> Cover -> ConstStore -> b
external_nd_C_apply = nd_apply



-- Encapsulated search
-- -------------------

-- external_d_C_try :: (a -> Success) -> [a -> Success]
external_d_C_try = error "Prelude.external_d_C_try called"

-- external_nd_C_try :: Func a Success -> [Func a Success]
external_nd_C_try = error "Prelude.external_nd_C_try called"

-- Functions on Integer and Nat added from PrimTypes
-- -------------------------------------------------
instance Curry_Prelude.Curry Nat where
  (=?=) (Choice_Nat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_Nat  cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_Nat  cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_Nat  cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_Nat  cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_Nat  cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_Nat  cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_Nat  cd info) _ _ = failCons cd info
  (=?=) IHi IHi d cs = Curry_Prelude.C_True
  (=?=) (O x1) (O y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (I x1) (I y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_Nat  cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_Nat  cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_Nat  cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_Nat  cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_Nat  cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_Nat  cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_Nat  cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_Nat  cd info) _ _ = failCons cd info
  (<?=) IHi IHi d cs = Curry_Prelude.C_True
  (<?=) IHi (O _) _ _ = Curry_Prelude.C_True
  (<?=) IHi (I _) _ _ = Curry_Prelude.C_True
  (<?=) (O x1) (O y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (O _) (I _) _ _ = Curry_Prelude.C_True
  (<?=) (I x1) (I y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


instance Curry_Prelude.Curry BinInt where
  (=?=) (Choice_BinInt cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_BinInt cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_BinInt cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_BinInt cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_BinInt cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_BinInt cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_BinInt cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_BinInt cd info) _ _ = failCons cd info
  (=?=) (Neg x1) (Neg y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) Zero Zero d cs = Curry_Prelude.C_True
  (=?=) (Pos x1) (Pos y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_BinInt cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_BinInt cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_BinInt cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_BinInt cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_BinInt cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_BinInt cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_BinInt cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_BinInt cd info) _ _ = failCons cd info
  (<?=) (Neg x1) (Neg y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (Neg _) Zero _ _ = Curry_Prelude.C_True
  (<?=) (Neg _) (Pos _) _ _ = Curry_Prelude.C_True
  (<?=) Zero Zero d cs = Curry_Prelude.C_True
  (<?=) Zero (Pos _) _ _ = Curry_Prelude.C_True
  (<?=) (Pos x1) (Pos y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_cmpNat :: Nat  -> Nat  -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpNat x1 x2 x3250 x3500 = case x1 of
     IHi -> d_OP__casePT_33 x2 x3250 x3500
     (O x5) -> d_OP__casePT_32 x5 x2 x3250 x3500
     (I x8) -> d_OP__casePT_30 x8 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpNat x1002 x2 x3250 x3500) (d_C_cmpNat x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpNat z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpNat x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_succ :: Nat  -> Cover -> ConstStore -> Nat 
d_C_succ x1 x3250 x3500 = case x1 of
     IHi -> O IHi
     (O x2) -> I x2
     (I x3) -> O (d_C_succ x3 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_succ x1002 x3250 x3500) (d_C_succ x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_succ z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_succ x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pred :: Nat  -> Cover -> ConstStore -> Nat 
d_C_pred x1 x3250 x3500 = case x1 of
     IHi -> Curry_Prelude.d_C_failed x3250 x3500
     (O x2) -> d_OP__casePT_28 x2 x3250 x3500
     (I x5) -> O x5
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pred x1002 x3250 x3500) (d_C_pred x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pred z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pred x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_plus_caret :: Nat  -> Nat  -> Cover -> ConstStore -> Nat 
d_OP_plus_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> d_C_succ x2 x3250 x3500
     (O x3) -> d_OP__casePT_27 x3 x2 x3250 x3500
     (I x6) -> d_OP__casePT_26 x6 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_caret x1002 x2 x3250 x3500) (d_OP_plus_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_minus_caret :: Nat  -> Nat  -> Cover -> ConstStore -> BinInt
d_OP_minus_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> d_C_inc (Neg x2) x3250 x3500
     (O x3) -> d_OP__casePT_25 x1 x3 x2 x3250 x3500
     (I x6) -> d_OP__casePT_24 x6 x2 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_caret x1002 x2 x3250 x3500) (d_OP_minus_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mult2 :: BinInt -> Cover -> ConstStore -> BinInt
d_C_mult2 x1 x3250 x3500 = case x1 of
     (Pos x2) -> Pos (O x2)
     Zero -> Zero
     (Neg x3) -> Neg (O x3)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mult2 x1002 x3250 x3500) (d_C_mult2 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mult2 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mult2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_star_caret :: Nat  -> Nat  -> Cover -> ConstStore -> Nat 
d_OP_star_caret x1 x2 x3250 x3500 = case x1 of
     IHi -> x2
     (O x3) -> O (d_OP_star_caret x3 x2 x3250 x3500)
     (I x4) -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2 x3250 x3500)) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_caret x1002 x2 x3250 x3500) (d_OP_star_caret x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_caret z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_caret x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_div2 :: Nat  -> Cover -> ConstStore -> Nat 
d_C_div2 x1 x3250 x3500 = case x1 of
     IHi -> Curry_Prelude.d_C_failed x3250 x3500
     (O x2) -> x2
     (I x3) -> x3
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_div2 x1002 x3250 x3500) (d_C_div2 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_div2 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_div2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mod2 :: Nat  -> Cover -> ConstStore -> BinInt
d_C_mod2 x1 x3250 x3500 = case x1 of
     IHi -> Pos IHi
     (O x2) -> Zero
     (I x3) -> Pos IHi
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mod2 x1002 x3250 x3500) (d_C_mod2 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mod2 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mod2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quotRemNat :: Nat  -> Nat  -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 x3250 x3500 = d_OP__casePT_23 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 IHi x3250 x3500) x3250 x3500

d_OP_quotRemNat_dot_shift_dot_104 :: Nat  -> Nat  -> Cover -> ConstStore -> Nat 
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 x3250 x3500 = case x1 of
     (O x3) -> O x2
     (I x4) -> I x2
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3250 x3500) (d_OP_quotRemNat_dot_shift_dot_104 x1003 x2 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemNat_dot_shift_dot_104 z x2 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lteqInteger :: BinInt -> BinInt -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_lteqInteger x1 x2 x3250 x3500 = Curry_Prelude.d_OP_slash_eq (d_C_cmpInteger x1 x2 x3250 x3500) Curry_Prelude.C_GT x3250 x3500

d_C_cmpInteger :: BinInt -> BinInt -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpInteger x1 x2 x3250 x3500 = case x1 of
     Zero -> d_OP__casePT_14 x2 x3250 x3500
     (Pos x5) -> d_OP__casePT_13 x5 x2 x3250 x3500
     (Neg x8) -> d_OP__casePT_12 x8 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpInteger x1002 x2 x3250 x3500) (d_C_cmpInteger x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpInteger z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpInteger x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_neg :: BinInt -> Cover -> ConstStore -> BinInt
d_C_neg x1 x3250 x3500 = case x1 of
     Zero -> Zero
     (Pos x2) -> Neg x2
     (Neg x3) -> Pos x3
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_neg x1002 x3250 x3500) (d_C_neg x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_neg z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_neg x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_inc :: BinInt -> Cover -> ConstStore -> BinInt
d_C_inc x1 x3250 x3500 = case x1 of
     Zero -> Pos IHi
     (Pos x2) -> Pos (d_C_succ x2 x3250 x3500)
     (Neg x3) -> d_OP__casePT_11 x3 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_inc x1002 x3250 x3500) (d_C_inc x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_inc z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_inc x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dec :: BinInt -> Cover -> ConstStore -> BinInt
d_C_dec x1 x3250 x3500 = case x1 of
     Zero -> Neg IHi
     (Pos x2) -> d_OP__casePT_10 x2 x3250 x3500
     (Neg x5) -> Neg (d_C_succ x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dec x1002 x3250 x3500) (d_C_dec x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dec z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dec x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_plus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_plus_hash x1 x2 x3250 x3500 = case x1 of
     Zero -> x2
     (Pos x3) -> d_OP__casePT_9 x1 x3 x2 x3250 x3500
     (Neg x6) -> d_OP__casePT_8 x1 x6 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_hash x1002 x2 x3250 x3500) (d_OP_plus_hash x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_hash z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_hash x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_minus_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_minus_hash x1 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x3) -> d_OP_plus_hash x1 (Neg x3) x3250 x3500
     (Neg x4) -> d_OP_plus_hash x1 (Pos x4) x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_hash x1 x1002 x3250 x3500) (d_OP_minus_hash x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_hash x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_hash x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_star_hash :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_OP_star_hash x1 x2 x3250 x3500 = case x1 of
     Zero -> Zero
     (Pos x3) -> d_OP__casePT_7 x3 x2 x3250 x3500
     (Neg x6) -> d_OP__casePT_6 x6 x2 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_hash x1002 x2 x3250 x3500) (d_OP_star_hash x1003 x2 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_hash z x2 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_hash x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quotRemInteger :: BinInt -> BinInt -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3250 x3500
     (Pos x3) -> d_OP__casePT_5 x3 x1 x3250 x3500
     (Neg x9) -> d_OP__casePT_4 x9 x1 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_quotRemInteger x1 x1002 x3250 x3500) (d_C_quotRemInteger x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_quotRemInteger x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_quotRemInteger x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP2_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP2_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP3_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP3_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP5_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP5_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP6_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP6_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP8_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP8_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quotRemInteger_dot___hash_selFP9_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3250 x3500) (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP9_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_divModInteger :: BinInt -> BinInt -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3250 x3500
     (Pos x3) -> d_OP__casePT_3 x3 x1 x3250 x3500
     (Neg x11) -> d_OP__casePT_1 x11 x1 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_divModInteger x1 x1002 x3250 x3500) (d_C_divModInteger x1 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_divModInteger x1 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_divModInteger x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP11_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP11_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP11_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP11_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP12_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP12_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP12_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP12_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP14_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP14_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP14_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP14_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP15_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP15_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP15_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP15_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP17_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP17_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP17_hash_d x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP17_hash_d z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_divModInteger_dot___hash_selFP18_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> Cover -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP18_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3250 x3500) (d_OP_divModInteger_dot___hash_selFP18_hash_m x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP18_hash_m z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_divInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_divInteger x1 x2 x3250 x3500 = Curry_Prelude.d_C_fst (d_C_divModInteger x1 x2 x3250 x3500) x3250 x3500

d_C_modInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_modInteger x1 x2 x3250 x3500 = Curry_Prelude.d_C_snd (d_C_divModInteger x1 x2 x3250 x3500) x3250 x3500

d_C_quotInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_quotInteger x1 x2 x3250 x3500 = Curry_Prelude.d_C_fst (d_C_quotRemInteger x1 x2 x3250 x3500) x3250 x3500

d_C_remInteger :: BinInt -> BinInt -> Cover -> ConstStore -> BinInt
d_C_remInteger x1 x2 x3250 x3500 = Curry_Prelude.d_C_snd (d_C_quotRemInteger x1 x2 x3250 x3500) x3250 x3500

d_OP__casePT_1 x11 x1 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x13 = d_C_quotRemNat x12 x11 x3250 x3500
          x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3250 x3500
          x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3250 x3500
           in (d_OP__casePT_0 x11 x14 x15 x3250 x3500)
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3250 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3250 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_1 x11 x1002 x3250 x3500) (d_OP__casePT_1 x11 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_1 x11 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_1 x11 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_1 x11 x1 x3000 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = d_C_quotRemNat x12 x11 x3250 x3500
               x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3250 x3500
               x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3250 x3500
                in (nd_OP__casePT_0 x11 x14 x15 x2000 x3250 x3500)))
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3250 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3250 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_1 x11 x1002 x3000 x3250 x3500) (nd_OP__casePT_1 x11 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_1 x11 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_1 x11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_0 x11 x14 x15 x3250 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3250 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_0 x11 x14 x1002 x3250 x3500) (d_OP__casePT_0 x11 x14 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_0 x11 x14 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_0 x11 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_0 x11 x14 x15 x3000 x3250 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3250 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3250 x3500) x3250 x3500) (d_OP_minus_hash x15 (Pos x11) x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_0 x11 x14 x1002 x3000 x3250 x3500) (nd_OP__casePT_0 x11 x14 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_0 x11 x14 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_0 x11 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_3 x3 x1 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3250 x3500
          x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3250 x3500
           in (d_OP__casePT_2 x3 x7 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_3 x3 x1002 x3250 x3500) (d_OP__casePT_3 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_3 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_3 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_3 x3 x1 x3000 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = d_C_quotRemNat x5 x3 x3250 x3500
               x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3250 x3500
               x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3250 x3500
                in (nd_OP__casePT_2 x3 x7 x8 x2000 x3250 x3500)))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_3 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_3 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_3 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_3 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_2 x3 x7 x8 x3250 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3250 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_2 x3 x7 x1002 x3250 x3500) (d_OP__casePT_2 x3 x7 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_2 x3 x7 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_2 x3 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_2 x3 x7 x8 x3000 x3250 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3250 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3250 x3500) x3250 x3500) (d_OP_minus_hash (Pos x3) x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_2 x3 x7 x1002 x3000 x3250 x3500) (nd_OP__casePT_2 x3 x7 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_2 x3 x7 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_2 x3 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_4 x9 x1 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3250 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3250 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3250 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3250 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3250 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_4 x9 x1002 x3250 x3500) (d_OP__casePT_4 x9 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_4 x9 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_4 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_4 x9 x1 x3000 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3250 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3250 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3250 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3250 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3250 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_4 x9 x1002 x3000 x3250 x3500) (nd_OP__casePT_4 x9 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_4 x9 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_4 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_5 x3 x1 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3250 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3250 x3500) (d_C_neg x8 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_5 x3 x1002 x3250 x3500) (d_OP__casePT_5 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_5 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_5 x3 x1 x3000 x3250 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3250 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3250 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3250 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3250 x3500) (d_C_neg x8 x3250 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_5 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_5 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_5 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_5 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_6 x6 x2 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3250 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_6 x6 x1002 x3250 x3500) (d_OP__casePT_6 x6 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_6 x6 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_6 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_6 x6 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3250 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_6 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_6 x6 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_6 x6 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_6 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_7 x3 x2 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3250 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_7 x3 x1002 x3250 x3500) (d_OP__casePT_7 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_7 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_7 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_7 x3 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3250 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_7 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_7 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_7 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_7 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_8 x1 x6 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3250 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_8 x1 x6 x1002 x3250 x3500) (d_OP__casePT_8 x1 x6 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_8 x1 x6 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_8 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_8 x1 x6 x2 x3000 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3250 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3250 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_8 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_8 x1 x6 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_8 x1 x6 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_8 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_9 x1 x3 x2 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3250 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_9 x1 x3 x1002 x3250 x3500) (d_OP__casePT_9 x1 x3 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_9 x1 x3 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_9 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_9 x1 x3 x2 x3000 x3250 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3250 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_9 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_9 x1 x3 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_9 x1 x3 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_9 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_10 x2 x3250 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3250 x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_10 x1002 x3250 x3500) (d_OP__casePT_10 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_10 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_10 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_10 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3250 x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_10 x1002 x3000 x3250 x3500) (nd_OP__casePT_10 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_10 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_10 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_11 x3 x3250 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3250 x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_11 x1002 x3250 x3500) (d_OP__casePT_11 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_11 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_11 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_11 x3 x3000 x3250 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3250 x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_11 x1002 x3000 x3250 x3500) (nd_OP__casePT_11 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_11 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_11 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_12 x8 x2 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_12 x8 x1002 x3250 x3500) (d_OP__casePT_12 x8 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_12 x8 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_12 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_12 x8 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_12 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_12 x8 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_12 x8 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_12 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_13 x5 x2 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_13 x5 x1002 x3250 x3500) (d_OP__casePT_13 x5 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_13 x5 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_13 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_13 x5 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_13 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_13 x5 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_13 x5 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_13 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_14 x2 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_14 x1002 x3250 x3500) (d_OP__casePT_14 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_14 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_14 x2 x3000 x3250 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_14 x1002 x3000 x3250 x3500) (nd_OP__casePT_14 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_14 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_23 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> d_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_23 x1 x2 x1002 x3250 x3500) (d_OP__casePT_23 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_23 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_23 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_23 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_23 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_23 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_23 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_23 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_22 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> d_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_22 x1 x2 x1002 x3250 x3500) (d_OP__casePT_22 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_22 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_22 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_22 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_22 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_22 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_22 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_22 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_21 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_21 x1 x2 x1002 x3250 x3500) (d_OP__casePT_21 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_21 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_21 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_21 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_21 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_21 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_21 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_21 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_20 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> d_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_20 x1 x2 x1002 x3250 x3500) (d_OP__casePT_20 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_20 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_20 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_20 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3250 x3500) x2 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_20 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_20 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_20 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_20 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_19 x1 x2 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__casePT_18 x1 x2 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_19 x1 x2 x1002 x3250 x3500) (d_OP__casePT_19 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_19 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_19 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_19 x1 x2 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_18 x1 x2 x4 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_19 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__casePT_19 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_19 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_19 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_18 x1 x2 x4 x3 x3250 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3250 x3500)
     (Pos x5) -> d_OP__casePT_17 x1 x2 x5 x4 x3250 x3500
     (Neg x12) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_18 x1 x2 x4 x1002 x3250 x3500) (d_OP__casePT_18 x1 x2 x4 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_18 x1 x2 x4 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_18 x1 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_18 x1 x2 x4 x3 x3000 x3250 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3250 x3500)
     (Pos x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_17 x1 x2 x5 x4 x2000 x3250 x3500))
     (Neg x12) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3250 x3500) (nd_OP__casePT_18 x1 x2 x4 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_18 x1 x2 x4 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_17 x1 x2 x5 x4 x3250 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3250 x3500)
     (Pos x6) -> d_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Neg x11) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_17 x1 x2 x5 x1002 x3250 x3500) (d_OP__casePT_17 x1 x2 x5 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_17 x1 x2 x5 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_17 x1 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_17 x1 x2 x5 x4 x3000 x3250 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3250 x3500)
     (Pos x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3250 x3500) x2 x3250 x3500) x2000 x3250 x3500))
     (Neg x11) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_17 x1 x2 x5 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_17 x1 x2 x5 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_16 x1 x2 x5 x6 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__casePT_15 x5 x8 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_16 x1 x2 x5 x6 x1002 x3250 x3500) (d_OP__casePT_16 x1 x2 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_16 x1 x2 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_16 x1 x2 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_16 x1 x2 x5 x6 x9 x3000 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_15 x5 x8 x7 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_16 x1 x2 x5 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_16 x1 x2 x5 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_15 x5 x8 x7 x3250 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3250 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_15 x5 x8 x1002 x3250 x3500) (d_OP__casePT_15 x5 x8 x1003 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_15 x5 x8 z x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_15 x5 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_15 x5 x8 x7 x3000 x3250 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3250 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_15 x5 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_15 x5 x8 x1003 x3000 x3250 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_15 x5 x8 z x3000 x3250 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_15 x5 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_24 x6 x2 x3250 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3250 x3500) x3250 x3500) x3250 x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_24 x6 x1002 x3250 x3500) (d_OP__casePT_24 x6 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_24 x6 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_24 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_24 x6 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3250 x3500) x3250 x3500) x3250 x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_24 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_24 x6 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_24 x6 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_24 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_25 x1 x3 x2 x3250 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3250 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3250 x3500) x3250 x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_25 x1 x3 x1002 x3250 x3500) (d_OP__casePT_25 x1 x3 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_25 x1 x3 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_25 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_25 x1 x3 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3250 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3250 x3500) x3250 x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_25 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_25 x1 x3 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_25 x1 x3 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_25 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_26 x6 x2 x3250 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3250 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3250 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3250 x3500) x8 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_26 x6 x1002 x3250 x3500) (d_OP__casePT_26 x6 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_26 x6 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_26 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_26 x6 x2 x3000 x3250 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3250 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3250 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3250 x3500) x8 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_26 x6 x1002 x3000 x3250 x3500) (nd_OP__casePT_26 x6 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_26 x6 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_26 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_27 x3 x2 x3250 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3250 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_27 x3 x1002 x3250 x3500) (d_OP__casePT_27 x3 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_27 x3 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_27 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_27 x3 x2 x3000 x3250 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3250 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3250 x3500)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_27 x3 x1002 x3000 x3250 x3500) (nd_OP__casePT_27 x3 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_27 x3 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_27 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_28 x2 x3250 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3250 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_28 x1002 x3250 x3500) (d_OP__casePT_28 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_28 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_28 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_28 x2 x3000 x3250 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3250 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_28 x1002 x3000 x3250 x3500) (nd_OP__casePT_28 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_28 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_28 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_30 x8 x2 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x9) -> d_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3250 x3500) x3250 x3500
     (I x10) -> d_C_cmpNat x8 x10 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_30 x8 x1002 x3250 x3500) (d_OP__casePT_30 x8 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_30 x8 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_30 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_30 x8 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3250 x3500) x2000 x3250 x3500))
     (I x10) -> d_C_cmpNat x8 x10 x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_30 x8 x1002 x3000 x3250 x3500) (nd_OP__casePT_30 x8 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_30 x8 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_30 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_29 x8 x9 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_29 x8 x9 x1002 x3250 x3500) (d_OP__casePT_29 x8 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_29 x8 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_29 x8 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_29 x8 x9 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_29 x8 x9 x1002 x3000 x3250 x3500) (nd_OP__casePT_29 x8 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_29 x8 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_29 x8 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_32 x5 x2 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (I x7) -> d_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3250 x3500) x3250 x3500
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_32 x5 x1002 x3250 x3500) (d_OP__casePT_32 x5 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_32 x5 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_32 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_32 x5 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3250 x3500
     (I x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3250 x3500) x2000 x3250 x3500))
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_32 x5 x1002 x3000 x3250 x3500) (nd_OP__casePT_32 x5 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_32 x5 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_32 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_31 x5 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_31 x5 x7 x1002 x3250 x3500) (d_OP__casePT_31 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_31 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_31 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_31 x5 x7 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_31 x5 x7 x1002 x3000 x3250 x3500) (nd_OP__casePT_31 x5 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_31 x5 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_31 x5 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__casePT_33 x2 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_33 x1002 x3250 x3500) (d_OP__casePT_33 x1003 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_33 z x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_33 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__casePT_33 x2 x3000 x3250 x3500 = case x2 of
     IHi -> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat  x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_33 x1002 x3000 x3250 x3500) (nd_OP__casePT_33 x1003 x3000 x3250 x3500)
     (Choices_Nat  x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_33 z x3000 x3250 x3500) x1002
     (Guard_Nat  x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_33 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_Nat  x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

