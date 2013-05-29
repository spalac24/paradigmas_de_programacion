{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE BangPatterns, MagicHash, MultiParamTypeClasses, ScopedTypeVariables #-}


module Curry_Prelude (Curry (..), OP_Unit (..), OP_List (..), OP_Tuple2 (..), OP_Tuple3 (..), OP_Tuple4 (..), OP_Tuple5 (..), OP_Tuple6 (..), OP_Tuple7 (..), OP_Tuple8 (..), OP_Tuple9 (..), OP_Tuple10 (..), OP_Tuple11 (..), OP_Tuple12 (..), OP_Tuple13 (..), OP_Tuple14 (..), OP_Tuple15 (..), C_Int (..), C_Float (..), C_Char (..), C_Bool (..), C_Ordering (..), C_Success (..), C_Maybe (..), C_Either (..), C_IO (..), C_IOError (..), C_String, d_OP_dot, nd_OP_dot, d_C_id, d_C_const, d_C_curry, nd_C_curry, d_C_uncurry, nd_C_uncurry, d_C_flip, nd_C_flip, d_C_until, nd_C_until, d_C_seq, d_C_ensureSpine, d_OP_dollar, nd_OP_dollar, d_OP_dollar_hash, nd_OP_dollar_hash, d_C_error, d_OP_ampersand_ampersand, d_OP_bar_bar, d_C_not, d_C_otherwise, d_C_if_then_else, d_OP_slash_eq, d_C_compare, d_OP_lt, d_OP_gt, d_OP_gt_eq, d_C_max, d_C_min, d_C_fst, d_C_snd, d_C_head, d_C_tail, d_C_null, d_OP_plus_plus, d_C_length, d_OP_bang_bang, d_C_map, nd_C_map, d_C_foldl, nd_C_foldl, d_C_foldl1, nd_C_foldl1, d_C_foldr, nd_C_foldr, d_C_foldr1, nd_C_foldr1, d_C_filter, nd_C_filter, d_C_zip, d_C_zip3, d_C_zipWith, nd_C_zipWith, d_C_zipWith3, nd_C_zipWith3, d_C_unzip, d_C_unzip3, d_C_concat, d_C_concatMap, nd_C_concatMap, d_C_iterate, nd_C_iterate, d_C_repeat, d_C_replicate, d_C_take, d_C_drop, d_C_splitAt, d_C_takeWhile, nd_C_takeWhile, d_C_dropWhile, nd_C_dropWhile, d_C_span, nd_C_span, d_C_break, nd_C_break, d_C_lines, d_C_unlines, d_C_words, d_C_unwords, d_C_reverse, nd_C_reverse, d_C_and, nd_C_and, d_C_or, nd_C_or, d_C_any, nd_C_any, d_C_all, nd_C_all, d_C_elem, nd_C_elem, d_C_notElem, nd_C_notElem, d_C_lookup, d_C_enumFrom, d_C_enumFromThen, d_C_enumFromTo, d_C_enumFromThenTo, d_C_ord, d_C_chr, d_C_negate, d_OP_ampersand_gt, d_C_maybe, nd_C_maybe, d_C_either, nd_C_either, d_OP_gt_gt, d_C_done, d_C_putChar, d_C_readFile, d_C_writeFile, d_C_appendFile, d_C_putStr, d_C_putStrLn, d_C_getLine, d_C_userError, d_C_ioError, d_C_showError, d_C_show, d_C_print, d_C_doSolve, d_C_sequenceIO, d_C_sequenceIO_, nd_C_sequenceIO_, d_C_mapIO, nd_C_mapIO, d_C_mapIO_, nd_C_mapIO_, nd_C_unknown, nd_C_getSomeValue, d_C_inject, nd_C_inject, d_C_solveAll, nd_C_solveAll, d_C_solveAll2, nd_C_solveAll2, d_C_once, nd_C_once, nd_C_best, nd_C_findall, nd_C_findfirst, nd_C_browse, nd_C_browseList, nd_C_unpack, d_C_normalForm, d_C_groundNormalForm, d_C_ensureNotFree, d_OP_dollar_bang, nd_OP_dollar_bang, d_OP_dollar_bang_bang, nd_OP_dollar_bang_bang, d_OP_dollar_hash_hash, nd_OP_dollar_hash_hash, d_C_prim_error, d_C_failed, d_OP_eq_eq, d_OP_lt_eq, d_C_prim_ord, d_C_prim_chr, d_OP_plus, d_OP_minus, d_OP_star, d_C_div, d_C_mod, d_C_divMod, d_C_quot, d_C_rem, d_C_quotRem, d_C_negateFloat, d_OP_eq_colon_eq, d_C_success, d_OP_ampersand, d_OP_gt_gt_eq, nd_OP_gt_gt_eq, d_C_return, d_C_prim_putChar, d_C_getChar, d_C_prim_readFile, d_C_prim_writeFile, d_C_prim_appendFile, d_C_prim_ioError, d_C_catch, nd_C_catch, d_C_prim_show, nd_OP_qmark, d_C_try, nd_C_try, d_C_apply, nd_C_apply, d_C_cond, d_OP_eq_colon_lt_eq) where

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
  generate s = Choices_OP_Unit defCover (freeID [0] s) [OP_Unit]


instance NormalForm OP_Unit where
  ($!!) cont OP_Unit cs = cont OP_Unit cs
  ($!!) cont (Choice_OP_Unit cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Unit cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Unit cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Unit cd info) _ = failCons cd info
  ($##) cont OP_Unit cs = cont OP_Unit cs
  ($##) cont (Choice_OP_Unit cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Unit cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Unit cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Unit cd info) _ = failCons cd info
  searchNF _ cont OP_Unit = cont OP_Unit
  searchNF _ _ x = error ("Prelude.().searchNF: no constructor: " ++ (show x))


instance Unifiable OP_Unit where
  (=.=) OP_Unit OP_Unit cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) OP_Unit OP_Unit cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i OP_Unit = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_OP_Unit cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Unit cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Unit cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Unit cd i _) = error ("Prelude.().bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Unit cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Unit cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i OP_Unit = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_OP_Unit cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Unit cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Unit cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Unit cd i _) = error ("Prelude.().lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Unit cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Unit cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry OP_Unit where
  (=?=) (Choice_OP_Unit cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Unit cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Unit cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Unit cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Unit cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Unit cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Unit cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Unit cd info) _ = failCons cd info
  (=?=) OP_Unit OP_Unit cs = C_True
  (<?=) (Choice_OP_Unit cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Unit cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Unit cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Unit cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Unit cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Unit cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Unit cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Unit cd info) _ = failCons cd info
  (<?=) OP_Unit OP_Unit cs = C_True


instance Coverable OP_Unit where
  cover OP_Unit = OP_Unit
  cover (Choice_OP_Unit cd i x y) = Choice_OP_Unit (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Unit cd i xs) = Choices_OP_Unit (incCover cd) i (map cover xs)
  cover (Fail_OP_Unit cd info) = Fail_OP_Unit (incCover cd) info
  cover (Guard_OP_Unit cd c e) = Guard_OP_Unit (incCover cd) c (cover e)


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
  generate s = Choices_OP_List defCover (freeID [0,2] s) [OP_List,(OP_Cons (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm t0 => NormalForm (OP_List t0) where
  ($!!) cont OP_List cs = cont OP_List cs
  ($!!) cont (OP_Cons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (OP_Cons y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_List cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_List cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_List cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_List cd info) _ = failCons cd info
  ($##) cont OP_List cs = cont OP_List cs
  ($##) cont (OP_Cons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (OP_Cons y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_List cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_List cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_List cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_List cd info) _ = failCons cd info
  searchNF _ cont OP_List = cont OP_List
  searchNF search cont (OP_Cons x1 x2) = search (\y1 -> search (\y2 -> cont (OP_Cons y1 y2)) x2) x1
  searchNF _ _ x = error ("Prelude.[].searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (OP_List t0) where
  (=.=) OP_List OP_List cs = C_Success
  (=.=) (OP_Cons x1 x2) (OP_Cons y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) OP_List OP_List cs = C_Success
  (=.<=) (OP_Cons x1 x2) (OP_Cons y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i OP_List = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (OP_Cons x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_OP_List cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_List cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_List cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_List cd i _) = error ("Prelude.[].bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_List cd info) = [(Unsolvable info)]
  bind i (Guard_OP_List cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i OP_List = [(i :=: (ChooseN 0 0))]
  lazyBind i (OP_Cons x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_OP_List cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_List cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_List cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_List cd i _) = error ("Prelude.[].lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_List cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_List cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry t0 => Curry (OP_List t0) where
  (=?=) (Choice_OP_List cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_List cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_List cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_List cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_List cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_List cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_List cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_List cd info) _ = failCons cd info
  (=?=) OP_List OP_List cs = C_True
  (=?=) (OP_Cons x1 x2) (OP_Cons y1 y2) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) ((x2 =?= y2) cs) cs
  (=?=) _ _ _ = C_False
  (<?=) (Choice_OP_List cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_List cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_List cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_List cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_List cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_List cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_List cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_List cd info) _ = failCons cd info
  (<?=) OP_List OP_List cs = C_True
  (<?=) OP_List (OP_Cons _ _) _ = C_True
  (<?=) (OP_Cons x1 x2) (OP_Cons y1 y2) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) ((x2 <?= y2) cs) cs) cs
  (<?=) _ _ _ = C_False


instance Coverable t0 => Coverable (OP_List t0) where
  cover OP_List = OP_List
  cover (OP_Cons x1 x2) = OP_Cons (cover x1) (cover x2)
  cover (Choice_OP_List cd i x y) = Choice_OP_List (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_List cd i xs) = Choices_OP_List (incCover cd) i (map cover xs)
  cover (Fail_OP_List cd info) = Fail_OP_List (incCover cd) info
  cover (Guard_OP_List cd c e) = Guard_OP_List (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple2 defCover (freeID [2] s) [(OP_Tuple2 (generate (leftSupply s)) (generate (rightSupply s)))]


instance (NormalForm t0,NormalForm t1) => NormalForm (OP_Tuple2 t0 t1) where
  ($!!) cont (OP_Tuple2 x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (OP_Tuple2 y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple2 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple2 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple2 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple2 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple2 x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (OP_Tuple2 y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple2 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple2 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple2 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple2 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple2 x1 x2) = search (\y1 -> search (\y2 -> cont (OP_Tuple2 y1 y2)) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple2.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1) => Unifiable (OP_Tuple2 t0 t1) where
  (=.=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple2 x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_OP_Tuple2 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple2 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple2 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple2 cd i _) = error ("Prelude.OP_Tuple2.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple2 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple2 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple2 x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_OP_Tuple2 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple2 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple2 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple2 cd i _) = error ("Prelude.OP_Tuple2.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple2 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple2 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1) => Curry (OP_Tuple2 t0 t1) where
  (=?=) (Choice_OP_Tuple2 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple2 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple2 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple2 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple2 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple2 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple2 cd info) _ = failCons cd info
  (=?=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) ((x2 =?= y2) cs) cs
  (<?=) (Choice_OP_Tuple2 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple2 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple2 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple2 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple2 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple2 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple2 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple2 cd info) _ = failCons cd info
  (<?=) (OP_Tuple2 x1 x2) (OP_Tuple2 y1 y2) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) ((x2 <?= y2) cs) cs) cs


instance (Coverable t0,Coverable t1) => Coverable (OP_Tuple2 t0 t1) where
  cover (OP_Tuple2 x1 x2) = OP_Tuple2 (cover x1) (cover x2)
  cover (Choice_OP_Tuple2 cd i x y) = Choice_OP_Tuple2 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple2 cd i xs) = Choices_OP_Tuple2 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple2 cd info) = Fail_OP_Tuple2 (incCover cd) info
  cover (Guard_OP_Tuple2 cd c e) = Guard_OP_Tuple2 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple3 defCover (freeID [3] s) [(OP_Tuple3 (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance (NormalForm t0,NormalForm t1,NormalForm t2) => NormalForm (OP_Tuple3 t0 t1 t2) where
  ($!!) cont (OP_Tuple3 x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (OP_Tuple3 y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple3 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple3 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple3 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple3 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple3 x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (OP_Tuple3 y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple3 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple3 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple3 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple3 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple3 x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (OP_Tuple3 y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple3.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2) => Unifiable (OP_Tuple3 t0 t1 t2) where
  (=.=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple3 x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_OP_Tuple3 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple3 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple3 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple3 cd i _) = error ("Prelude.OP_Tuple3.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple3 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple3 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple3 x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_OP_Tuple3 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple3 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple3 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple3 cd i _) = error ("Prelude.OP_Tuple3.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple3 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple3 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2) => Curry (OP_Tuple3 t0 t1 t2) where
  (=?=) (Choice_OP_Tuple3 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple3 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple3 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple3 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple3 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple3 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple3 cd info) _ = failCons cd info
  (=?=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) ((x3 =?= y3) cs) cs) cs
  (<?=) (Choice_OP_Tuple3 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple3 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple3 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple3 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple3 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple3 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple3 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple3 cd info) _ = failCons cd info
  (<?=) (OP_Tuple3 x1 x2 x3) (OP_Tuple3 y1 y2 y3) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) ((x3 <?= y3) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2) => Coverable (OP_Tuple3 t0 t1 t2) where
  cover (OP_Tuple3 x1 x2 x3) = OP_Tuple3 (cover x1) (cover x2) (cover x3)
  cover (Choice_OP_Tuple3 cd i x y) = Choice_OP_Tuple3 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple3 cd i xs) = Choices_OP_Tuple3 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple3 cd info) = Fail_OP_Tuple3 (incCover cd) info
  cover (Guard_OP_Tuple3 cd c e) = Guard_OP_Tuple3 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple4 defCover (freeID [4] s) [(OP_Tuple4 (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3) => NormalForm (OP_Tuple4 t0 t1 t2 t3) where
  ($!!) cont (OP_Tuple4 x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (OP_Tuple4 y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple4 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple4 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple4 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple4 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple4 x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (OP_Tuple4 y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple4 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple4 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple4 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple4 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple4 x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (OP_Tuple4 y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple4.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3) => Unifiable (OP_Tuple4 t0 t1 t2 t3) where
  (=.=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple4 x2 x3 x4 x5) = ((i :=: (ChooseN 0 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (Choice_OP_Tuple4 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple4 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple4 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple4 cd i _) = error ("Prelude.OP_Tuple4.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple4 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple4 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple4 x2 x3 x4 x5) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (Choice_OP_Tuple4 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple4 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple4 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple4 cd i _) = error ("Prelude.OP_Tuple4.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple4 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple4 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3) => Curry (OP_Tuple4 t0 t1 t2 t3) where
  (=?=) (Choice_OP_Tuple4 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple4 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple4 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple4 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple4 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple4 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple4 cd info) _ = failCons cd info
  (=?=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) ((x4 =?= y4) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple4 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple4 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple4 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple4 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple4 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple4 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple4 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple4 cd info) _ = failCons cd info
  (<?=) (OP_Tuple4 x1 x2 x3 x4) (OP_Tuple4 y1 y2 y3 y4) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) ((x4 <?= y4) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3) => Coverable (OP_Tuple4 t0 t1 t2 t3) where
  cover (OP_Tuple4 x1 x2 x3 x4) = OP_Tuple4 (cover x1) (cover x2) (cover x3) (cover x4)
  cover (Choice_OP_Tuple4 cd i x y) = Choice_OP_Tuple4 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple4 cd i xs) = Choices_OP_Tuple4 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple4 cd info) = Fail_OP_Tuple4 (incCover cd) info
  cover (Guard_OP_Tuple4 cd c e) = Guard_OP_Tuple4 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple5 defCover (freeID [5] s) [(OP_Tuple5 (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4) => NormalForm (OP_Tuple5 t0 t1 t2 t3 t4) where
  ($!!) cont (OP_Tuple5 x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (OP_Tuple5 y1 y2 y3 y4 y5) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple5 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple5 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple5 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple5 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple5 x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (OP_Tuple5 y1 y2 y3 y4 y5) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple5 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple5 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple5 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple5 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple5 x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (OP_Tuple5 y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple5.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4) => Unifiable (OP_Tuple5 t0 t1 t2 t3 t4) where
  (=.=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((x5 =:= y5) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((x5 =:<= y5) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple5 x2 x3 x4 x5 x6) = ((i :=: (ChooseN 0 5)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (rightID i)) x5),(bind (rightID (rightID i)) x6)]))
  bind i (Choice_OP_Tuple5 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple5 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple5 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple5 cd i _) = error ("Prelude.OP_Tuple5.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple5 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple5 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple5 x2 x3 x4 x5 x6) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x6)))]
  lazyBind i (Choice_OP_Tuple5 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple5 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple5 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple5 cd i _) = error ("Prelude.OP_Tuple5.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple5 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple5 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => Curry (OP_Tuple5 t0 t1 t2 t3 t4) where
  (=?=) (Choice_OP_Tuple5 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple5 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple5 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple5 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple5 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple5 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple5 cd info) _ = failCons cd info
  (=?=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) ((x5 =?= y5) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple5 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple5 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple5 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple5 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple5 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple5 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple5 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple5 cd info) _ = failCons cd info
  (<?=) (OP_Tuple5 x1 x2 x3 x4 x5) (OP_Tuple5 y1 y2 y3 y4 y5) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) ((x5 <?= y5) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4) => Coverable (OP_Tuple5 t0 t1 t2 t3 t4) where
  cover (OP_Tuple5 x1 x2 x3 x4 x5) = OP_Tuple5 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5)
  cover (Choice_OP_Tuple5 cd i x y) = Choice_OP_Tuple5 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple5 cd i xs) = Choices_OP_Tuple5 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple5 cd info) = Fail_OP_Tuple5 (incCover cd) info
  cover (Guard_OP_Tuple5 cd c e) = Guard_OP_Tuple5 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple6 defCover (freeID [6] s) [(OP_Tuple6 (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (leftSupply (rightSupply s)))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (rightSupply (rightSupply s))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5) => NormalForm (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  ($!!) cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple6 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple6 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple6 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple6 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple6 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple6 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple6 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple6 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple6 x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (OP_Tuple6 y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple6.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5) => Unifiable (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  (=.=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((x6 =:= y6) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((x6 =:<= y6) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple6 x2 x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 6)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (leftID (rightID i))) x5),(bind (rightID (leftID (rightID i))) x6),(bind (rightID (rightID i)) x7)]))
  bind i (Choice_OP_Tuple6 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple6 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple6 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple6 cd i _) = error ("Prelude.OP_Tuple6.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple6 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple6 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple6 x2 x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind (leftID (leftID (rightID i))) x5))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x7)))]
  lazyBind i (Choice_OP_Tuple6 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple6 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple6 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple6 cd i _) = error ("Prelude.OP_Tuple6.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple6 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple6 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5) => Curry (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  (=?=) (Choice_OP_Tuple6 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple6 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple6 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple6 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple6 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple6 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple6 cd info) _ = failCons cd info
  (=?=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) ((x6 =?= y6) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple6 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple6 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple6 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple6 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple6 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple6 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple6 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple6 cd info) _ = failCons cd info
  (<?=) (OP_Tuple6 x1 x2 x3 x4 x5 x6) (OP_Tuple6 y1 y2 y3 y4 y5 y6) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) ((x6 <?= y6) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5) => Coverable (OP_Tuple6 t0 t1 t2 t3 t4 t5) where
  cover (OP_Tuple6 x1 x2 x3 x4 x5 x6) = OP_Tuple6 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6)
  cover (Choice_OP_Tuple6 cd i x y) = Choice_OP_Tuple6 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple6 cd i xs) = Choices_OP_Tuple6 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple6 cd info) = Fail_OP_Tuple6 (incCover cd) info
  cover (Guard_OP_Tuple6 cd c e) = Guard_OP_Tuple6 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple7 defCover (freeID [7] s) [(OP_Tuple7 (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (rightSupply (leftSupply s)))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply s)))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (rightSupply (rightSupply s))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6) => NormalForm (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  ($!!) cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple7 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple7 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple7 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple7 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple7 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple7 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple7 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple7 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> cont (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7)) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple7.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6) => Unifiable (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  (=.=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((x7 =:= y7) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((x7 =:<= y7) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple7 x2 x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 7)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (leftID (rightID (leftID i))) x4),(bind (rightID (rightID (leftID i))) x5),(bind (leftID (leftID (rightID i))) x6),(bind (rightID (leftID (rightID i))) x7),(bind (rightID (rightID i)) x8)]))
  bind i (Choice_OP_Tuple7 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple7 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple7 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple7 cd i _) = error ("Prelude.OP_Tuple7.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple7 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple7 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple7 x2 x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 7)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind (leftID (rightID (leftID i))) x4))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x8)))]
  lazyBind i (Choice_OP_Tuple7 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple7 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple7 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple7 cd i _) = error ("Prelude.OP_Tuple7.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple7 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple7 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6) => Curry (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  (=?=) (Choice_OP_Tuple7 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple7 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple7 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple7 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple7 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple7 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple7 cd info) _ = failCons cd info
  (=?=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) ((x7 =?= y7) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple7 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple7 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple7 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple7 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple7 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple7 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple7 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple7 cd info) _ = failCons cd info
  (<?=) (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) (OP_Tuple7 y1 y2 y3 y4 y5 y6 y7) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) ((x7 <?= y7) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6) => Coverable (OP_Tuple7 t0 t1 t2 t3 t4 t5 t6) where
  cover (OP_Tuple7 x1 x2 x3 x4 x5 x6 x7) = OP_Tuple7 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7)
  cover (Choice_OP_Tuple7 cd i x y) = Choice_OP_Tuple7 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple7 cd i xs) = Choices_OP_Tuple7 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple7 cd info) = Fail_OP_Tuple7 (incCover cd) info
  cover (Guard_OP_Tuple7 cd c e) = Guard_OP_Tuple7 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple8 defCover (freeID [8] s) [(OP_Tuple8 (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (rightSupply (leftSupply s)))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply s)))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (rightSupply (rightSupply s)))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7) => NormalForm (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  ($!!) cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple8 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple8 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple8 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple8 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple8 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple8 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple8 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple8 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> cont (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8)) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple8.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7) => Unifiable (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  (=.=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((x8 =:= y8) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((x8 =:<= y8) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple8 x2 x3 x4 x5 x6 x7 x8 x9) = ((i :=: (ChooseN 0 8)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (leftID (rightID (leftID i))) x4),(bind (rightID (rightID (leftID i))) x5),(bind (leftID (leftID (rightID i))) x6),(bind (rightID (leftID (rightID i))) x7),(bind (leftID (rightID (rightID i))) x8),(bind (rightID (rightID (rightID i))) x9)]))
  bind i (Choice_OP_Tuple8 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple8 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple8 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple8 cd i _) = error ("Prelude.OP_Tuple8.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple8 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple8 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple8 x2 x3 x4 x5 x6 x7 x8 x9) = [(i :=: (ChooseN 0 8)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind (leftID (rightID (leftID i))) x4))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x7))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind (leftID (rightID (rightID i))) x8))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x9)))]
  lazyBind i (Choice_OP_Tuple8 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple8 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple8 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple8 cd i _) = error ("Prelude.OP_Tuple8.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple8 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple8 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7) => Curry (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  (=?=) (Choice_OP_Tuple8 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple8 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple8 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple8 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple8 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple8 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple8 cd info) _ = failCons cd info
  (=?=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) ((x8 =?= y8) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple8 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple8 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple8 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple8 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple8 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple8 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple8 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple8 cd info) _ = failCons cd info
  (<?=) (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) (OP_Tuple8 y1 y2 y3 y4 y5 y6 y7 y8) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) ((x8 <?= y8) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7) => Coverable (OP_Tuple8 t0 t1 t2 t3 t4 t5 t6 t7) where
  cover (OP_Tuple8 x1 x2 x3 x4 x5 x6 x7 x8) = OP_Tuple8 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8)
  cover (Choice_OP_Tuple8 cd i x y) = Choice_OP_Tuple8 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple8 cd i xs) = Choices_OP_Tuple8 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple8 cd info) = Fail_OP_Tuple8 (incCover cd) info
  cover (Guard_OP_Tuple8 cd c e) = Guard_OP_Tuple8 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple9 defCover (freeID [9] s) [(OP_Tuple9 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (rightSupply (leftSupply s)))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply s)))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (rightSupply (rightSupply s)))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8) => NormalForm (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  ($!!) cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple9 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple9 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple9 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple9 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple9 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple9 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple9 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple9 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> cont (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9)) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple9.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8) => Unifiable (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  (=.=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((x9 =:= y9) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((x9 =:<= y9) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple9 x2 x3 x4 x5 x6 x7 x8 x9 x10) = ((i :=: (ChooseN 0 9)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (rightID (leftID (leftID i))) x4),(bind (leftID (rightID (leftID i))) x5),(bind (rightID (rightID (leftID i))) x6),(bind (leftID (leftID (rightID i))) x7),(bind (rightID (leftID (rightID i))) x8),(bind (leftID (rightID (rightID i))) x9),(bind (rightID (rightID (rightID i))) x10)]))
  bind i (Choice_OP_Tuple9 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple9 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple9 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple9 cd i _) = error ("Prelude.OP_Tuple9.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple9 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple9 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple9 x2 x3 x4 x5 x6 x7 x8 x9 x10) = [(i :=: (ChooseN 0 9)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x6))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind (leftID (leftID (rightID i))) x7))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x8))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind (leftID (rightID (rightID i))) x9))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x10)))]
  lazyBind i (Choice_OP_Tuple9 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple9 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple9 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple9 cd i _) = error ("Prelude.OP_Tuple9.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple9 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple9 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8) => Curry (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  (=?=) (Choice_OP_Tuple9 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple9 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple9 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple9 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple9 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple9 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple9 cd info) _ = failCons cd info
  (=?=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) ((x9 =?= y9) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple9 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple9 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple9 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple9 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple9 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple9 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple9 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple9 cd info) _ = failCons cd info
  (<?=) (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (OP_Tuple9 y1 y2 y3 y4 y5 y6 y7 y8 y9) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) ((x9 <?= y9) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8) => Coverable (OP_Tuple9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  cover (OP_Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = OP_Tuple9 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9)
  cover (Choice_OP_Tuple9 cd i x y) = Choice_OP_Tuple9 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple9 cd i xs) = Choices_OP_Tuple9 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple9 cd info) = Fail_OP_Tuple9 (incCover cd) info
  cover (Guard_OP_Tuple9 cd c e) = Guard_OP_Tuple9 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple10 defCover (freeID [10] s) [(OP_Tuple10 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (rightSupply (leftSupply s)))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (rightSupply (rightSupply s)))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9) => NormalForm (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  ($!!) cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple10 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple10 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple10 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple10 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple10 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple10 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple10 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple10 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> cont (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10)) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple10.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9) => Unifiable (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  (=.=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((x10 =:= y10) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((x10 =:<= y10) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple10 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = ((i :=: (ChooseN 0 10)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (rightID (leftID (leftID i))) x4),(bind (leftID (rightID (leftID i))) x5),(bind (rightID (rightID (leftID i))) x6),(bind (leftID (leftID (leftID (rightID i)))) x7),(bind (rightID (leftID (leftID (rightID i)))) x8),(bind (rightID (leftID (rightID i))) x9),(bind (leftID (rightID (rightID i))) x10),(bind (rightID (rightID (rightID i))) x11)]))
  bind i (Choice_OP_Tuple10 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple10 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple10 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple10 cd i _) = error ("Prelude.OP_Tuple10.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple10 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple10 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple10 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = [(i :=: (ChooseN 0 10)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x6))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x7))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x8))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x9))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind (leftID (rightID (rightID i))) x10))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x11)))]
  lazyBind i (Choice_OP_Tuple10 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple10 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple10 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple10 cd i _) = error ("Prelude.OP_Tuple10.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple10 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple10 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9) => Curry (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  (=?=) (Choice_OP_Tuple10 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple10 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple10 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple10 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple10 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple10 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple10 cd info) _ = failCons cd info
  (=?=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) ((x10 =?= y10) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple10 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple10 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple10 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple10 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple10 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple10 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple10 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple10 cd info) _ = failCons cd info
  (<?=) (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (OP_Tuple10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) ((x10 <?= y10) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9) => Coverable (OP_Tuple10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  cover (OP_Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = OP_Tuple10 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10)
  cover (Choice_OP_Tuple10 cd i x y) = Choice_OP_Tuple10 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple10 cd i xs) = Choices_OP_Tuple10 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple10 cd info) = Fail_OP_Tuple10 (incCover cd) info
  cover (Guard_OP_Tuple10 cd c e) = Guard_OP_Tuple10 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple11 defCover (freeID [11] s) [(OP_Tuple11 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (rightSupply (rightSupply s)))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10) => NormalForm (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  ($!!) cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs) $!! x11) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple11 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple11 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple11 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple11 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs) $## x11) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple11 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple11 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple11 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple11 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> cont (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11)) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple11.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10) => Unifiable (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  (=.=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((((x10 =:= y10) cs) & ((x11 =:= y11) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((((x10 =:<= y10) cs) & ((x11 =:<= y11) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple11 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = ((i :=: (ChooseN 0 11)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (rightID (leftID (leftID i))) x4),(bind (leftID (leftID (rightID (leftID i)))) x5),(bind (rightID (leftID (rightID (leftID i)))) x6),(bind (rightID (rightID (leftID i))) x7),(bind (leftID (leftID (leftID (rightID i)))) x8),(bind (rightID (leftID (leftID (rightID i)))) x9),(bind (rightID (leftID (rightID i))) x10),(bind (leftID (rightID (rightID i))) x11),(bind (rightID (rightID (rightID i))) x12)]))
  bind i (Choice_OP_Tuple11 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple11 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple11 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple11 cd i _) = error ("Prelude.OP_Tuple11.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple11 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple11 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple11 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = [(i :=: (ChooseN 0 11)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x4))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (leftID i)))) x5))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (leftID i)))) x6))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x7))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x8))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x10))),((leftID (rightID (rightID i))) :=: (LazyBind (lazyBind (leftID (rightID (rightID i))) x11))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x12)))]
  lazyBind i (Choice_OP_Tuple11 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple11 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple11 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple11 cd i _) = error ("Prelude.OP_Tuple11.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple11 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple11 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10) => Curry (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  (=?=) (Choice_OP_Tuple11 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple11 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple11 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple11 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple11 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple11 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple11 cd info) _ = failCons cd info
  (=?=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) ((x11 =?= y11) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple11 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple11 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple11 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple11 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple11 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple11 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple11 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple11 cd info) _ = failCons cd info
  (<?=) (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (OP_Tuple11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_bar_bar (d_OP_lt x10 y10 cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) ((x11 <?= y11) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9,Coverable t10) => Coverable (OP_Tuple11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  cover (OP_Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = OP_Tuple11 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10) (cover x11)
  cover (Choice_OP_Tuple11 cd i x y) = Choice_OP_Tuple11 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple11 cd i xs) = Choices_OP_Tuple11 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple11 cd info) = Fail_OP_Tuple11 (incCover cd) info
  cover (Guard_OP_Tuple11 cd c e) = Guard_OP_Tuple11 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple12 defCover (freeID [12] s) [(OP_Tuple12 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (leftSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11) => NormalForm (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  ($!!) cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs) $!! x12) cs) $!! x11) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple12 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple12 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple12 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple12 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs) $## x12) cs) $## x11) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple12 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple12 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple12 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple12 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> cont (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12)) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple12.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11) => Unifiable (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  (=.=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((((x10 =:= y10) cs) & ((((x11 =:= y11) cs) & ((x12 =:= y12) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((((x10 =:<= y10) cs) & ((((x11 =:<= y11) cs) & ((x12 =:<= y12) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple12 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = ((i :=: (ChooseN 0 12)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (rightID (leftID (leftID i))) x4),(bind (leftID (leftID (rightID (leftID i)))) x5),(bind (rightID (leftID (rightID (leftID i)))) x6),(bind (rightID (rightID (leftID i))) x7),(bind (leftID (leftID (leftID (rightID i)))) x8),(bind (rightID (leftID (leftID (rightID i)))) x9),(bind (rightID (leftID (rightID i))) x10),(bind (leftID (leftID (rightID (rightID i)))) x11),(bind (rightID (leftID (rightID (rightID i)))) x12),(bind (rightID (rightID (rightID i))) x13)]))
  bind i (Choice_OP_Tuple12 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple12 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple12 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple12 cd i _) = error ("Prelude.OP_Tuple12.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple12 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple12 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple12 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = [(i :=: (ChooseN 0 12)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x4))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (leftID i)))) x5))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (leftID i)))) x6))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x7))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x8))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x10))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (rightID i)))) x11))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (rightID i)))) x12))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x13)))]
  lazyBind i (Choice_OP_Tuple12 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple12 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple12 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple12 cd i _) = error ("Prelude.OP_Tuple12.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple12 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple12 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11) => Curry (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  (=?=) (Choice_OP_Tuple12 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple12 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple12 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple12 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple12 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple12 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple12 cd info) _ = failCons cd info
  (=?=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) ((x12 =?= y12) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple12 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple12 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple12 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple12 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple12 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple12 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple12 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple12 cd info) _ = failCons cd info
  (<?=) (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (OP_Tuple12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_bar_bar (d_OP_lt x10 y10 cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_bar_bar (d_OP_lt x11 y11 cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) ((x12 <?= y12) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9,Coverable t10,Coverable t11) => Coverable (OP_Tuple12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  cover (OP_Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = OP_Tuple12 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10) (cover x11) (cover x12)
  cover (Choice_OP_Tuple12 cd i x y) = Choice_OP_Tuple12 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple12 cd i xs) = Choices_OP_Tuple12 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple12 cd info) = Fail_OP_Tuple12 (incCover cd) info
  cover (Guard_OP_Tuple12 cd c e) = Guard_OP_Tuple12 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple13 defCover (freeID [13] s) [(OP_Tuple13 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (leftSupply (rightSupply (leftSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply (leftSupply s))))) (generate (leftSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply s)))) (generate (leftSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12) => NormalForm (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  ($!!) cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs) $!! x13) cs) $!! x12) cs) $!! x11) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple13 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple13 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple13 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple13 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs) $## x13) cs) $## x12) cs) $## x11) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple13 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple13 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple13 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple13 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> cont (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13)) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple13.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12) => Unifiable (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  (=.=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((((x10 =:= y10) cs) & ((((x11 =:= y11) cs) & ((((x12 =:= y12) cs) & ((x13 =:= y13) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((((x10 =:<= y10) cs) & ((((x11 =:<= y11) cs) & ((((x12 =:<= y12) cs) & ((x13 =:<= y13) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple13 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = ((i :=: (ChooseN 0 13)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (leftID (rightID (leftID (leftID i)))) x4),(bind (rightID (rightID (leftID (leftID i)))) x5),(bind (leftID (leftID (rightID (leftID i)))) x6),(bind (rightID (leftID (rightID (leftID i)))) x7),(bind (rightID (rightID (leftID i))) x8),(bind (leftID (leftID (leftID (rightID i)))) x9),(bind (rightID (leftID (leftID (rightID i)))) x10),(bind (rightID (leftID (rightID i))) x11),(bind (leftID (leftID (rightID (rightID i)))) x12),(bind (rightID (leftID (rightID (rightID i)))) x13),(bind (rightID (rightID (rightID i))) x14)]))
  bind i (Choice_OP_Tuple13 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple13 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple13 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple13 cd i _) = error ("Prelude.OP_Tuple13.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple13 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple13 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple13 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = [(i :=: (ChooseN 0 13)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (rightID (leftID (leftID i)))) x4))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (rightID (leftID (leftID i)))) x5))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (leftID i)))) x6))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (leftID i)))) x7))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x8))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind (rightID (leftID (rightID i))) x11))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (rightID i)))) x12))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (rightID i)))) x13))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x14)))]
  lazyBind i (Choice_OP_Tuple13 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple13 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple13 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple13 cd i _) = error ("Prelude.OP_Tuple13.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple13 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple13 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12) => Curry (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  (=?=) (Choice_OP_Tuple13 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple13 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple13 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple13 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple13 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple13 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple13 cd info) _ = failCons cd info
  (=?=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) ((x13 =?= y13) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple13 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple13 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple13 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple13 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple13 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple13 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple13 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple13 cd info) _ = failCons cd info
  (<?=) (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (OP_Tuple13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_bar_bar (d_OP_lt x10 y10 cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_bar_bar (d_OP_lt x11 y11 cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_bar_bar (d_OP_lt x12 y12 cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) ((x13 <?= y13) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9,Coverable t10,Coverable t11,Coverable t12) => Coverable (OP_Tuple13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  cover (OP_Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = OP_Tuple13 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10) (cover x11) (cover x12) (cover x13)
  cover (Choice_OP_Tuple13 cd i x y) = Choice_OP_Tuple13 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple13 cd i xs) = Choices_OP_Tuple13 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple13 cd info) = Fail_OP_Tuple13 (incCover cd) info
  cover (Guard_OP_Tuple13 cd c e) = Guard_OP_Tuple13 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple14 defCover (freeID [14] s) [(OP_Tuple14 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (leftSupply (rightSupply (leftSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply (leftSupply s))))) (generate (leftSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply s)))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (leftSupply (rightSupply (leftSupply (rightSupply s))))) (generate (rightSupply (rightSupply (leftSupply (rightSupply s))))) (generate (leftSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12,NormalForm t13) => NormalForm (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  ($!!) cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> ((\y14 cs -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs) $!! x14) cs) $!! x13) cs) $!! x12) cs) $!! x11) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple14 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple14 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple14 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple14 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> ((\y14 cs -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs) $## x14) cs) $## x13) cs) $## x12) cs) $## x11) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple14 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple14 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple14 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple14 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> search (\y14 -> cont (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14)) x14) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple14.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12,Unifiable t13) => Unifiable (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  (=.=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((((x10 =:= y10) cs) & ((((x11 =:= y11) cs) & ((((x12 =:= y12) cs) & ((((x13 =:= y13) cs) & ((x14 =:= y14) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((((x10 =:<= y10) cs) & ((((x11 =:<= y11) cs) & ((((x12 =:<= y12) cs) & ((((x13 =:<= y13) cs) & ((x14 =:<= y14) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple14 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = ((i :=: (ChooseN 0 14)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (leftID (rightID (leftID (leftID i)))) x4),(bind (rightID (rightID (leftID (leftID i)))) x5),(bind (leftID (leftID (rightID (leftID i)))) x6),(bind (rightID (leftID (rightID (leftID i)))) x7),(bind (rightID (rightID (leftID i))) x8),(bind (leftID (leftID (leftID (rightID i)))) x9),(bind (rightID (leftID (leftID (rightID i)))) x10),(bind (leftID (rightID (leftID (rightID i)))) x11),(bind (rightID (rightID (leftID (rightID i)))) x12),(bind (leftID (leftID (rightID (rightID i)))) x13),(bind (rightID (leftID (rightID (rightID i)))) x14),(bind (rightID (rightID (rightID i))) x15)]))
  bind i (Choice_OP_Tuple14 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple14 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple14 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple14 cd i _) = error ("Prelude.OP_Tuple14.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple14 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple14 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple14 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = [(i :=: (ChooseN 0 14)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (rightID (leftID (leftID i)))) x4))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (rightID (leftID (leftID i)))) x5))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (leftID i)))) x6))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (leftID i)))) x7))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind (rightID (rightID (leftID i))) x8))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x9))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x10))),((leftID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (rightID (leftID (rightID i)))) x11))),((rightID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (rightID (leftID (rightID i)))) x12))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (rightID i)))) x13))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (rightID i)))) x14))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x15)))]
  lazyBind i (Choice_OP_Tuple14 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple14 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple14 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple14 cd i _) = error ("Prelude.OP_Tuple14.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple14 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple14 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13) => Curry (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  (=?=) (Choice_OP_Tuple14 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple14 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple14 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple14 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple14 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple14 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple14 cd info) _ = failCons cd info
  (=?=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) (d_OP_ampersand_ampersand ((x13 =?= y13) cs) ((x14 =?= y14) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple14 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple14 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple14 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple14 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple14 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple14 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple14 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple14 cd info) _ = failCons cd info
  (<?=) (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (OP_Tuple14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_bar_bar (d_OP_lt x10 y10 cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_bar_bar (d_OP_lt x11 y11 cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_bar_bar (d_OP_lt x12 y12 cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) (d_OP_bar_bar (d_OP_lt x13 y13 cs) (d_OP_ampersand_ampersand ((x13 =?= y13) cs) ((x14 <?= y14) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9,Coverable t10,Coverable t11,Coverable t12,Coverable t13) => Coverable (OP_Tuple14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  cover (OP_Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = OP_Tuple14 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10) (cover x11) (cover x12) (cover x13) (cover x14)
  cover (Choice_OP_Tuple14 cd i x y) = Choice_OP_Tuple14 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple14 cd i xs) = Choices_OP_Tuple14 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple14 cd info) = Fail_OP_Tuple14 (incCover cd) info
  cover (Guard_OP_Tuple14 cd c e) = Guard_OP_Tuple14 (incCover cd) c (cover e)


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
  generate s = Choices_OP_Tuple15 defCover (freeID [15] s) [(OP_Tuple15 (generate (leftSupply (leftSupply (leftSupply (leftSupply s))))) (generate (rightSupply (leftSupply (leftSupply (leftSupply s))))) (generate (leftSupply (rightSupply (leftSupply (leftSupply s))))) (generate (rightSupply (rightSupply (leftSupply (leftSupply s))))) (generate (leftSupply (leftSupply (rightSupply (leftSupply s))))) (generate (rightSupply (leftSupply (rightSupply (leftSupply s))))) (generate (leftSupply (rightSupply (rightSupply (leftSupply s))))) (generate (rightSupply (rightSupply (rightSupply (leftSupply s))))) (generate (leftSupply (leftSupply (leftSupply (rightSupply s))))) (generate (rightSupply (leftSupply (leftSupply (rightSupply s))))) (generate (leftSupply (rightSupply (leftSupply (rightSupply s))))) (generate (rightSupply (rightSupply (leftSupply (rightSupply s))))) (generate (leftSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (leftSupply (rightSupply (rightSupply s))))) (generate (rightSupply (rightSupply (rightSupply s)))))]


instance (NormalForm t0,NormalForm t1,NormalForm t2,NormalForm t3,NormalForm t4,NormalForm t5,NormalForm t6,NormalForm t7,NormalForm t8,NormalForm t9,NormalForm t10,NormalForm t11,NormalForm t12,NormalForm t13,NormalForm t14) => NormalForm (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  ($!!) cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> ((\y14 cs -> ((\y15 cs -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs) $!! x15) cs) $!! x14) cs) $!! x13) cs) $!! x12) cs) $!! x11) cs) $!! x10) cs) $!! x9) cs) $!! x8) cs) $!! x7) cs) $!! x6) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_OP_Tuple15 cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_OP_Tuple15 cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_OP_Tuple15 cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_OP_Tuple15 cd info) _ = failCons cd info
  ($##) cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> ((\y6 cs -> ((\y7 cs -> ((\y8 cs -> ((\y9 cs -> ((\y10 cs -> ((\y11 cs -> ((\y12 cs -> ((\y13 cs -> ((\y14 cs -> ((\y15 cs -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs) $## x15) cs) $## x14) cs) $## x13) cs) $## x12) cs) $## x11) cs) $## x10) cs) $## x9) cs) $## x8) cs) $## x7) cs) $## x6) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_OP_Tuple15 cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_OP_Tuple15 cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_OP_Tuple15 cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_OP_Tuple15 cd info) _ = failCons cd info
  searchNF search cont (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> search (\y8 -> search (\y9 -> search (\y10 -> search (\y11 -> search (\y12 -> search (\y13 -> search (\y14 -> search (\y15 -> cont (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)) x15) x14) x13) x12) x11) x10) x9) x8) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Prelude.OP_Tuple15.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2,Unifiable t3,Unifiable t4,Unifiable t5,Unifiable t6,Unifiable t7,Unifiable t8,Unifiable t9,Unifiable t10,Unifiable t11,Unifiable t12,Unifiable t13,Unifiable t14) => Unifiable (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  (=.=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((((x5 =:= y5) cs) & ((((x6 =:= y6) cs) & ((((x7 =:= y7) cs) & ((((x8 =:= y8) cs) & ((((x9 =:= y9) cs) & ((((x10 =:= y10) cs) & ((((x11 =:= y11) cs) & ((((x12 =:= y12) cs) & ((((x13 =:= y13) cs) & ((((x14 =:= y14) cs) & ((x15 =:= y15) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((((x5 =:<= y5) cs) & ((((x6 =:<= y6) cs) & ((((x7 =:<= y7) cs) & ((((x8 =:<= y8) cs) & ((((x9 =:<= y9) cs) & ((((x10 =:<= y10) cs) & ((((x11 =:<= y11) cs) & ((((x12 =:<= y12) cs) & ((((x13 =:<= y13) cs) & ((((x14 =:<= y14) cs) & ((x15 =:<= y15) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (OP_Tuple15 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) = ((i :=: (ChooseN 0 15)):(concat [(bind (leftID (leftID (leftID (leftID i)))) x2),(bind (rightID (leftID (leftID (leftID i)))) x3),(bind (leftID (rightID (leftID (leftID i)))) x4),(bind (rightID (rightID (leftID (leftID i)))) x5),(bind (leftID (leftID (rightID (leftID i)))) x6),(bind (rightID (leftID (rightID (leftID i)))) x7),(bind (leftID (rightID (rightID (leftID i)))) x8),(bind (rightID (rightID (rightID (leftID i)))) x9),(bind (leftID (leftID (leftID (rightID i)))) x10),(bind (rightID (leftID (leftID (rightID i)))) x11),(bind (leftID (rightID (leftID (rightID i)))) x12),(bind (rightID (rightID (leftID (rightID i)))) x13),(bind (leftID (leftID (rightID (rightID i)))) x14),(bind (rightID (leftID (rightID (rightID i)))) x15),(bind (rightID (rightID (rightID i))) x16)]))
  bind i (Choice_OP_Tuple15 cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_OP_Tuple15 cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_OP_Tuple15 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_OP_Tuple15 cd i _) = error ("Prelude.OP_Tuple15.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_OP_Tuple15 cd info) = [(Unsolvable info)]
  bind i (Guard_OP_Tuple15 cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (OP_Tuple15 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) = [(i :=: (ChooseN 0 15)),((leftID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (leftID i)))) x2))),((rightID (leftID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (leftID i)))) x3))),((leftID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (leftID (rightID (leftID (leftID i)))) x4))),((rightID (rightID (leftID (leftID i)))) :=: (LazyBind (lazyBind (rightID (rightID (leftID (leftID i)))) x5))),((leftID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (leftID i)))) x6))),((rightID (leftID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (leftID i)))) x7))),((leftID (rightID (rightID (leftID i)))) :=: (LazyBind (lazyBind (leftID (rightID (rightID (leftID i)))) x8))),((rightID (rightID (rightID (leftID i)))) :=: (LazyBind (lazyBind (rightID (rightID (rightID (leftID i)))) x9))),((leftID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (leftID (rightID i)))) x10))),((rightID (leftID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (leftID (rightID i)))) x11))),((leftID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind (leftID (rightID (leftID (rightID i)))) x12))),((rightID (rightID (leftID (rightID i)))) :=: (LazyBind (lazyBind (rightID (rightID (leftID (rightID i)))) x13))),((leftID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (leftID (leftID (rightID (rightID i)))) x14))),((rightID (leftID (rightID (rightID i)))) :=: (LazyBind (lazyBind (rightID (leftID (rightID (rightID i)))) x15))),((rightID (rightID (rightID i))) :=: (LazyBind (lazyBind (rightID (rightID (rightID i))) x16)))]
  lazyBind i (Choice_OP_Tuple15 cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_OP_Tuple15 cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_OP_Tuple15 cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_OP_Tuple15 cd i _) = error ("Prelude.OP_Tuple15.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_OP_Tuple15 cd info) = [(Unsolvable info)]
  lazyBind i (Guard_OP_Tuple15 cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13,Curry t14) => Curry (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  (=?=) (Choice_OP_Tuple15 cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_OP_Tuple15 cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_OP_Tuple15 cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  (=?=) z (Choice_OP_Tuple15 cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_OP_Tuple15 cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_OP_Tuple15 cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_OP_Tuple15 cd info) _ = failCons cd info
  (=?=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs = d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) (d_OP_ampersand_ampersand ((x13 =?= y13) cs) (d_OP_ampersand_ampersand ((x14 =?= y14) cs) ((x15 =?= y15) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (Choice_OP_Tuple15 cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_OP_Tuple15 cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_OP_Tuple15 cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_OP_Tuple15 cd info) _ _ = failCons cd info
  (<?=) z (Choice_OP_Tuple15 cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_OP_Tuple15 cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_OP_Tuple15 cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_OP_Tuple15 cd info) _ = failCons cd info
  (<?=) (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (OP_Tuple15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) cs = d_OP_bar_bar (d_OP_lt x1 y1 cs) (d_OP_ampersand_ampersand ((x1 =?= y1) cs) (d_OP_bar_bar (d_OP_lt x2 y2 cs) (d_OP_ampersand_ampersand ((x2 =?= y2) cs) (d_OP_bar_bar (d_OP_lt x3 y3 cs) (d_OP_ampersand_ampersand ((x3 =?= y3) cs) (d_OP_bar_bar (d_OP_lt x4 y4 cs) (d_OP_ampersand_ampersand ((x4 =?= y4) cs) (d_OP_bar_bar (d_OP_lt x5 y5 cs) (d_OP_ampersand_ampersand ((x5 =?= y5) cs) (d_OP_bar_bar (d_OP_lt x6 y6 cs) (d_OP_ampersand_ampersand ((x6 =?= y6) cs) (d_OP_bar_bar (d_OP_lt x7 y7 cs) (d_OP_ampersand_ampersand ((x7 =?= y7) cs) (d_OP_bar_bar (d_OP_lt x8 y8 cs) (d_OP_ampersand_ampersand ((x8 =?= y8) cs) (d_OP_bar_bar (d_OP_lt x9 y9 cs) (d_OP_ampersand_ampersand ((x9 =?= y9) cs) (d_OP_bar_bar (d_OP_lt x10 y10 cs) (d_OP_ampersand_ampersand ((x10 =?= y10) cs) (d_OP_bar_bar (d_OP_lt x11 y11 cs) (d_OP_ampersand_ampersand ((x11 =?= y11) cs) (d_OP_bar_bar (d_OP_lt x12 y12 cs) (d_OP_ampersand_ampersand ((x12 =?= y12) cs) (d_OP_bar_bar (d_OP_lt x13 y13 cs) (d_OP_ampersand_ampersand ((x13 =?= y13) cs) (d_OP_bar_bar (d_OP_lt x14 y14 cs) (d_OP_ampersand_ampersand ((x14 =?= y14) cs) ((x15 <?= y15) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2,Coverable t3,Coverable t4,Coverable t5,Coverable t6,Coverable t7,Coverable t8,Coverable t9,Coverable t10,Coverable t11,Coverable t12,Coverable t13,Coverable t14) => Coverable (OP_Tuple15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  cover (OP_Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = OP_Tuple15 (cover x1) (cover x2) (cover x3) (cover x4) (cover x5) (cover x6) (cover x7) (cover x8) (cover x9) (cover x10) (cover x11) (cover x12) (cover x13) (cover x14) (cover x15)
  cover (Choice_OP_Tuple15 cd i x y) = Choice_OP_Tuple15 (incCover cd) i (cover x) (cover y)
  cover (Choices_OP_Tuple15 cd i xs) = Choices_OP_Tuple15 (incCover cd) i (map cover xs)
  cover (Fail_OP_Tuple15 cd info) = Fail_OP_Tuple15 (incCover cd) info
  cover (Guard_OP_Tuple15 cd c e) = Guard_OP_Tuple15 (incCover cd) c (cover e)


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
  generate s = Choices_C_Bool defCover (freeID [0,0] s) [C_False,C_True]


instance NormalForm C_Bool where
  ($!!) cont C_False cs = cont C_False cs
  ($!!) cont C_True cs = cont C_True cs
  ($!!) cont (Choice_C_Bool cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Bool cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Bool cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Bool cd info) _ = failCons cd info
  ($##) cont C_False cs = cont C_False cs
  ($##) cont C_True cs = cont C_True cs
  ($##) cont (Choice_C_Bool cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Bool cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Bool cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Bool cd info) _ = failCons cd info
  searchNF _ cont C_False = cont C_False
  searchNF _ cont C_True = cont C_True
  searchNF _ _ x = error ("Prelude.Bool.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Bool where
  (=.=) C_False C_False cs = C_Success
  (=.=) C_True C_True cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_False C_False cs = C_Success
  (=.<=) C_True C_True cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_False = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_True = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_Bool cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Bool cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Bool cd i _) = error ("Prelude.Bool.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Bool cd info) = [(Unsolvable info)]
  bind i (Guard_C_Bool cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_False = [(i :=: (ChooseN 0 0))]
  lazyBind i C_True = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_Bool cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Bool cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Bool cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Bool cd i _) = error ("Prelude.Bool.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Bool cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Bool cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry C_Bool where
  (=?=) (Choice_C_Bool cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Bool cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Bool cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_C_Bool cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Bool cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Bool cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Bool cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_C_Bool cd info) _ = failCons cd info
  (=?=) C_False C_False cs = C_True
  (=?=) C_True C_True cs = C_True
  (=?=) _ _ _ = C_False
  (<?=) (Choice_C_Bool cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Bool cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Bool cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_C_Bool cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Bool cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Bool cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Bool cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_C_Bool cd info) _ = failCons cd info
  (<?=) C_False C_False cs = C_True
  (<?=) C_False C_True _ = C_True
  (<?=) C_True C_True cs = C_True
  (<?=) _ _ _ = C_False


instance Coverable C_Bool where
  cover C_False = C_False
  cover C_True = C_True
  cover (Choice_C_Bool cd i x y) = Choice_C_Bool (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Bool cd i xs) = Choices_C_Bool (incCover cd) i (map cover xs)
  cover (Fail_C_Bool cd info) = Fail_C_Bool (incCover cd) info
  cover (Guard_C_Bool cd c e) = Guard_C_Bool (incCover cd) c (cover e)


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
  generate s = Choices_C_Ordering defCover (freeID [0,0,0] s) [C_LT,C_EQ,C_GT]


instance NormalForm C_Ordering where
  ($!!) cont C_LT cs = cont C_LT cs
  ($!!) cont C_EQ cs = cont C_EQ cs
  ($!!) cont C_GT cs = cont C_GT cs
  ($!!) cont (Choice_C_Ordering cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Ordering cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Ordering cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Ordering cd info) _ = failCons cd info
  ($##) cont C_LT cs = cont C_LT cs
  ($##) cont C_EQ cs = cont C_EQ cs
  ($##) cont C_GT cs = cont C_GT cs
  ($##) cont (Choice_C_Ordering cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Ordering cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Ordering cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Ordering cd info) _ = failCons cd info
  searchNF _ cont C_LT = cont C_LT
  searchNF _ cont C_EQ = cont C_EQ
  searchNF _ cont C_GT = cont C_GT
  searchNF _ _ x = error ("Prelude.Ordering.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Ordering where
  (=.=) C_LT C_LT cs = C_Success
  (=.=) C_EQ C_EQ cs = C_Success
  (=.=) C_GT C_GT cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_LT C_LT cs = C_Success
  (=.<=) C_EQ C_EQ cs = C_Success
  (=.<=) C_GT C_GT cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_LT = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_EQ = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_GT = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (Choice_C_Ordering cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Ordering cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Ordering cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Ordering cd i _) = error ("Prelude.Ordering.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Ordering cd info) = [(Unsolvable info)]
  bind i (Guard_C_Ordering cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_LT = [(i :=: (ChooseN 0 0))]
  lazyBind i C_EQ = [(i :=: (ChooseN 1 0))]
  lazyBind i C_GT = [(i :=: (ChooseN 2 0))]
  lazyBind i (Choice_C_Ordering cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Ordering cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Ordering cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Ordering cd i _) = error ("Prelude.Ordering.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Ordering cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Ordering cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry C_Ordering where
  (=?=) (Choice_C_Ordering cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Ordering cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Ordering cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_C_Ordering cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Ordering cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Ordering cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Ordering cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_C_Ordering cd info) _ = failCons cd info
  (=?=) C_LT C_LT cs = C_True
  (=?=) C_EQ C_EQ cs = C_True
  (=?=) C_GT C_GT cs = C_True
  (=?=) _ _ _ = C_False
  (<?=) (Choice_C_Ordering cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Ordering cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Ordering cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_C_Ordering cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Ordering cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Ordering cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Ordering cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_C_Ordering cd info) _ = failCons cd info
  (<?=) C_LT C_LT cs = C_True
  (<?=) C_LT C_EQ _ = C_True
  (<?=) C_LT C_GT _ = C_True
  (<?=) C_EQ C_EQ cs = C_True
  (<?=) C_EQ C_GT _ = C_True
  (<?=) C_GT C_GT cs = C_True
  (<?=) _ _ _ = C_False


instance Coverable C_Ordering where
  cover C_LT = C_LT
  cover C_EQ = C_EQ
  cover C_GT = C_GT
  cover (Choice_C_Ordering cd i x y) = Choice_C_Ordering (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Ordering cd i xs) = Choices_C_Ordering (incCover cd) i (map cover xs)
  cover (Fail_C_Ordering cd info) = Fail_C_Ordering (incCover cd) info
  cover (Guard_C_Ordering cd c e) = Guard_C_Ordering (incCover cd) c (cover e)




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
  generate s = Choices_C_Maybe defCover (freeID [0,1] s) [C_Nothing,(C_Just (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_Maybe t0) where
  ($!!) cont C_Nothing cs = cont C_Nothing cs
  ($!!) cont (C_Just x1) cs = ((\y1 cs -> cont (C_Just y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Maybe cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Maybe cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Maybe cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Maybe cd info) _ = failCons cd info
  ($##) cont C_Nothing cs = cont C_Nothing cs
  ($##) cont (C_Just x1) cs = ((\y1 cs -> cont (C_Just y1) cs) $## x1) cs
  ($##) cont (Choice_C_Maybe cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Maybe cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Maybe cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Maybe cd info) _ = failCons cd info
  searchNF _ cont C_Nothing = cont C_Nothing
  searchNF search cont (C_Just x1) = search (\y1 -> cont (C_Just y1)) x1
  searchNF _ _ x = error ("Prelude.Maybe.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Maybe t0) where
  (=.=) C_Nothing C_Nothing cs = C_Success
  (=.=) (C_Just x1) (C_Just y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Nothing C_Nothing cs = C_Success
  (=.<=) (C_Just x1) (C_Just y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Nothing = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_Just x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Maybe cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Maybe cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Maybe cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Maybe cd i _) = error ("Prelude.Maybe.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Maybe cd info) = [(Unsolvable info)]
  bind i (Guard_C_Maybe cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Nothing = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_Just x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Maybe cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Maybe cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Maybe cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Maybe cd i _) = error ("Prelude.Maybe.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Maybe cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Maybe cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry t0 => Curry (C_Maybe t0) where
  (=?=) (Choice_C_Maybe cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Maybe cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Maybe cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_C_Maybe cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Maybe cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Maybe cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Maybe cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_C_Maybe cd info) _ = failCons cd info
  (=?=) C_Nothing C_Nothing cs = C_True
  (=?=) (C_Just x1) (C_Just y1) cs = (x1 =?= y1) cs
  (=?=) _ _ _ = C_False
  (<?=) (Choice_C_Maybe cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Maybe cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Maybe cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_C_Maybe cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Maybe cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Maybe cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Maybe cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_C_Maybe cd info) _ = failCons cd info
  (<?=) C_Nothing C_Nothing cs = C_True
  (<?=) C_Nothing (C_Just _) _ = C_True
  (<?=) (C_Just x1) (C_Just y1) cs = (x1 <?= y1) cs
  (<?=) _ _ _ = C_False


instance Coverable t0 => Coverable (C_Maybe t0) where
  cover C_Nothing = C_Nothing
  cover (C_Just x1) = C_Just (cover x1)
  cover (Choice_C_Maybe cd i x y) = Choice_C_Maybe (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Maybe cd i xs) = Choices_C_Maybe (incCover cd) i (map cover xs)
  cover (Fail_C_Maybe cd info) = Fail_C_Maybe (incCover cd) info
  cover (Guard_C_Maybe cd c e) = Guard_C_Maybe (incCover cd) c (cover e)


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
  generate s = Choices_C_Either defCover (freeID [1,1] s) [(C_Left (generate (leftSupply s))),(C_Right (generate (leftSupply s)))]


instance (NormalForm t0,NormalForm t1) => NormalForm (C_Either t0 t1) where
  ($!!) cont (C_Left x1) cs = ((\y1 cs -> cont (C_Left y1) cs) $!! x1) cs
  ($!!) cont (C_Right x1) cs = ((\y1 cs -> cont (C_Right y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Either cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Either cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Either cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Either cd info) _ = failCons cd info
  ($##) cont (C_Left x1) cs = ((\y1 cs -> cont (C_Left y1) cs) $## x1) cs
  ($##) cont (C_Right x1) cs = ((\y1 cs -> cont (C_Right y1) cs) $## x1) cs
  ($##) cont (Choice_C_Either cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Either cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Either cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Either cd info) _ = failCons cd info
  searchNF search cont (C_Left x1) = search (\y1 -> cont (C_Left y1)) x1
  searchNF search cont (C_Right x1) = search (\y1 -> cont (C_Right y1)) x1
  searchNF _ _ x = error ("Prelude.Either.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1) => Unifiable (C_Either t0 t1) where
  (=.=) (C_Left x1) (C_Left y1) cs = (x1 =:= y1) cs
  (=.=) (C_Right x1) (C_Right y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Left x1) (C_Left y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Right x1) (C_Right y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Left x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Right x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Either cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Either cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Either cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Either cd i _) = error ("Prelude.Either.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Either cd info) = [(Unsolvable info)]
  bind i (Guard_C_Either cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Left x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Right x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Either cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Either cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Either cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Either cd i _) = error ("Prelude.Either.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Either cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Either cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry t0,Curry t1) => Curry (C_Either t0 t1) where
  (=?=) (Choice_C_Either cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Either cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Either cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_C_Either cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Either cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Either cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Either cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_C_Either cd info) _ = failCons cd info
  (=?=) (C_Left x1) (C_Left y1) cs = (x1 =?= y1) cs
  (=?=) (C_Right x1) (C_Right y1) cs = (x1 =?= y1) cs
  (=?=) _ _ _ = C_False
  (<?=) (Choice_C_Either cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Either cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Either cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_C_Either cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Either cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Either cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Either cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_C_Either cd info) _ = failCons cd info
  (<?=) (C_Left x1) (C_Left y1) cs = (x1 <?= y1) cs
  (<?=) (C_Left _) (C_Right _) _ = C_True
  (<?=) (C_Right x1) (C_Right y1) cs = (x1 <?= y1) cs
  (<?=) _ _ _ = C_False


instance (Coverable t0,Coverable t1) => Coverable (C_Either t0 t1) where
  cover (C_Left x1) = C_Left (cover x1)
  cover (C_Right x1) = C_Right (cover x1)
  cover (Choice_C_Either cd i x y) = Choice_C_Either (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Either cd i xs) = Choices_C_Either (incCover cd) i (map cover xs)
  cover (Fail_C_Either cd info) = Fail_C_Either (incCover cd) info
  cover (Guard_C_Either cd c e) = Guard_C_Either (incCover cd) c (cover e)




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
  generate s = Choices_C_IOError defCover (freeID [1,1,1,1] s) [(C_IOError (generate (leftSupply s))),(C_UserError (generate (leftSupply s))),(C_FailError (generate (leftSupply s))),(C_NondetError (generate (leftSupply s)))]


instance NormalForm C_IOError where
  ($!!) cont (C_IOError x1) cs = ((\y1 cs -> cont (C_IOError y1) cs) $!! x1) cs
  ($!!) cont (C_UserError x1) cs = ((\y1 cs -> cont (C_UserError y1) cs) $!! x1) cs
  ($!!) cont (C_FailError x1) cs = ((\y1 cs -> cont (C_FailError y1) cs) $!! x1) cs
  ($!!) cont (C_NondetError x1) cs = ((\y1 cs -> cont (C_NondetError y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_IOError cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_IOError cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_IOError cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_IOError cd info) _ = failCons cd info
  ($##) cont (C_IOError x1) cs = ((\y1 cs -> cont (C_IOError y1) cs) $## x1) cs
  ($##) cont (C_UserError x1) cs = ((\y1 cs -> cont (C_UserError y1) cs) $## x1) cs
  ($##) cont (C_FailError x1) cs = ((\y1 cs -> cont (C_FailError y1) cs) $## x1) cs
  ($##) cont (C_NondetError x1) cs = ((\y1 cs -> cont (C_NondetError y1) cs) $## x1) cs
  ($##) cont (Choice_C_IOError cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_IOError cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_IOError cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_IOError cd info) _ = failCons cd info
  searchNF search cont (C_IOError x1) = search (\y1 -> cont (C_IOError y1)) x1
  searchNF search cont (C_UserError x1) = search (\y1 -> cont (C_UserError y1)) x1
  searchNF search cont (C_FailError x1) = search (\y1 -> cont (C_FailError y1)) x1
  searchNF search cont (C_NondetError x1) = search (\y1 -> cont (C_NondetError y1)) x1
  searchNF _ _ x = error ("Prelude.IOError.searchNF: no constructor: " ++ (show x))


instance Unifiable C_IOError where
  (=.=) (C_IOError x1) (C_IOError y1) cs = (x1 =:= y1) cs
  (=.=) (C_UserError x1) (C_UserError y1) cs = (x1 =:= y1) cs
  (=.=) (C_FailError x1) (C_FailError y1) cs = (x1 =:= y1) cs
  (=.=) (C_NondetError x1) (C_NondetError y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_IOError x1) (C_IOError y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_UserError x1) (C_UserError y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_FailError x1) (C_FailError y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_NondetError x1) (C_NondetError y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_IOError x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_UserError x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_FailError x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_NondetError x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_IOError cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_IOError cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_IOError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_IOError cd i _) = error ("Prelude.IOError.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_IOError cd info) = [(Unsolvable info)]
  bind i (Guard_C_IOError cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_IOError x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_UserError x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_FailError x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_NondetError x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_IOError cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_IOError cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_IOError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_IOError cd i _) = error ("Prelude.IOError.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_IOError cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_IOError cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry C_IOError where
  (=?=) (Choice_C_IOError cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_IOError cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_IOError cd c e) y cs = guardCons cd c ((e =?= y) (addCs c cs))
  (=?=) (Fail_C_IOError cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_IOError cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_IOError cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_IOError cd c e) cs = guardCons cd c ((y =?= e) (addCs c cs))
  (=?=) _ (Fail_C_IOError cd info) _ = failCons cd info
  (=?=) (C_IOError x1) (C_IOError y1) cs = (x1 =?= y1) cs
  (=?=) (C_UserError x1) (C_UserError y1) cs = (x1 =?= y1) cs
  (=?=) (C_FailError x1) (C_FailError y1) cs = (x1 =?= y1) cs
  (=?=) (C_NondetError x1) (C_NondetError y1) cs = (x1 =?= y1) cs
  (=?=) _ _ _ = C_False
  (<?=) (Choice_C_IOError cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_IOError cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_IOError cd c e) y cs = guardCons cd c ((e <?= y) (addCs c cs))
  (<?=) (Fail_C_IOError cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_IOError cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_IOError cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_IOError cd c e) cs = guardCons cd c ((y <?= e) (addCs c cs))
  (<?=) _ (Fail_C_IOError cd info) _ = failCons cd info
  (<?=) (C_IOError x1) (C_IOError y1) cs = (x1 <?= y1) cs
  (<?=) (C_IOError _) (C_UserError _) _ = C_True
  (<?=) (C_IOError _) (C_FailError _) _ = C_True
  (<?=) (C_IOError _) (C_NondetError _) _ = C_True
  (<?=) (C_UserError x1) (C_UserError y1) cs = (x1 <?= y1) cs
  (<?=) (C_UserError _) (C_FailError _) _ = C_True
  (<?=) (C_UserError _) (C_NondetError _) _ = C_True
  (<?=) (C_FailError x1) (C_FailError y1) cs = (x1 <?= y1) cs
  (<?=) (C_FailError _) (C_NondetError _) _ = C_True
  (<?=) (C_NondetError x1) (C_NondetError y1) cs = (x1 <?= y1) cs
  (<?=) _ _ _ = C_False


instance Coverable C_IOError where
  cover (C_IOError x1) = C_IOError (cover x1)
  cover (C_UserError x1) = C_UserError (cover x1)
  cover (C_FailError x1) = C_FailError (cover x1)
  cover (C_NondetError x1) = C_NondetError (cover x1)
  cover (Choice_C_IOError cd i x y) = Choice_C_IOError (incCover cd) i (cover x) (cover y)
  cover (Choices_C_IOError cd i xs) = Choices_C_IOError (incCover cd) i (map cover xs)
  cover (Fail_C_IOError cd info) = Fail_C_IOError (incCover cd) info
  cover (Guard_C_IOError cd c e) = Guard_C_IOError (incCover cd) c (cover e)


d_OP_dot :: (Curry t0,Curry t2,Curry t1) => (t0 -> ConstStore -> t1) -> (t2 -> ConstStore -> t0) -> ConstStore -> t2 -> ConstStore -> t1
d_OP_dot x1 x2 x3500 = d_OP_dot_dot___hash_lambda1 x1 x2

nd_OP_dot :: (Curry t0,Curry t2,Curry t1) => Func t0 t1 -> Func t2 t0 -> IDSupply -> ConstStore -> Func t2 t1
nd_OP_dot x1 x2 x3000 x3500 = wrapNX id (nd_OP_dot_dot___hash_lambda1 x1 x2)

d_OP_dot_dot___hash_lambda1 :: (Curry t8,Curry t6,Curry t10) => (t8 -> ConstStore -> t10) -> (t6 -> ConstStore -> t8) -> t6 -> ConstStore -> t10
d_OP_dot_dot___hash_lambda1 x1 x2 x3 x3500 = d_C_apply x1 (d_C_apply x2 x3 x3500) x3500

nd_OP_dot_dot___hash_lambda1 :: (Curry t8,Curry t6,Curry t10) => Func t8 t10 -> Func t6 t8 -> t6 -> IDSupply -> ConstStore -> t10
nd_OP_dot_dot___hash_lambda1 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_apply x1 (nd_C_apply x2 x3 x2000 x3500) x2001 x3500)))))

d_C_id :: Curry t0 => t0 -> ConstStore -> t0
d_C_id x1 x3500 = x1

d_C_const :: (Curry t1,Curry t0) => t0 -> t1 -> ConstStore -> t0
d_C_const x1 x2 x3500 = x1

d_C_curry :: (Curry t0,Curry t1,Curry t2) => (OP_Tuple2 t0 t1 -> ConstStore -> t2) -> t0 -> t1 -> ConstStore -> t2
d_C_curry x1 x2 x3 x3500 = d_C_apply x1 (OP_Tuple2 x2 x3) x3500

nd_C_curry :: (Curry t0,Curry t1,Curry t2) => Func (OP_Tuple2 t0 t1) t2 -> t0 -> t1 -> IDSupply -> ConstStore -> t2
nd_C_curry x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_apply x1 (OP_Tuple2 x2 x3) x2000 x3500))

d_C_uncurry :: (Curry t0,Curry t1,Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> t2) -> OP_Tuple2 t0 t1 -> ConstStore -> t2
d_C_uncurry x1 x2 x3500 = case x2 of
     (OP_Tuple2 x3 x4) -> d_C_apply (d_C_apply x1 x3 x3500) x4 x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_uncurry x1 x1002 x3500) (d_C_uncurry x1 x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_uncurry x1 z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_uncurry x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_uncurry :: (Curry t0,Curry t1,Curry t2) => Func t0 (Func t1 t2) -> OP_Tuple2 t0 t1 -> IDSupply -> ConstStore -> t2
nd_C_uncurry x1 x2 x3000 x3500 = case x2 of
     (OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x3 x2000 x3500) x4 x2001 x3500)))))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_uncurry x1 x1002 x3000 x3500) (nd_C_uncurry x1 x1003 x3000 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_uncurry x1 z x3000 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_uncurry x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_flip :: (Curry t1,Curry t0,Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> t2) -> t1 -> t0 -> ConstStore -> t2
d_C_flip x1 x2 x3 x3500 = d_C_apply (d_C_apply x1 x3 x3500) x2 x3500

nd_C_flip :: (Curry t1,Curry t0,Curry t2) => Func t0 (Func t1 t2) -> t1 -> t0 -> IDSupply -> ConstStore -> t2
nd_C_flip x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x3 x2000 x3500) x2 x2001 x3500)))))

d_C_until :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> (t0 -> ConstStore -> t0) -> t0 -> ConstStore -> t0
d_C_until x1 x2 x3 x3500 = d_OP__case_42 x1 x2 x3 (d_C_apply x1 x3 x3500) x3500

nd_C_until :: Curry t0 => Func t0 C_Bool -> Func t0 t0 -> t0 -> IDSupply -> ConstStore -> t0
nd_C_until x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_42 x1 x2 x3 (nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))

d_C_seq :: (Curry t0,Curry t1) => t0 -> t1 -> ConstStore -> t1
d_C_seq x1 x2 x3500 = d_OP_dollar_bang (d_C_const x2) x1 x3500

d_C_ensureSpine :: Curry t0 => OP_List t0 -> ConstStore -> OP_List t0
d_C_ensureSpine x1 x3500 = d_OP_ensureSpine_dot_ensureList_dot_20 (d_C_ensureNotFree x1 x3500) x3500

d_OP_ensureSpine_dot_ensureList_dot_20 :: Curry t0 => OP_List t0 -> ConstStore -> OP_List t0
d_OP_ensureSpine_dot_ensureList_dot_20 x1 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> OP_Cons x2 (d_C_ensureSpine x3 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ensureSpine_dot_ensureList_dot_20 x1002 x3500) (d_OP_ensureSpine_dot_ensureList_dot_20 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ensureSpine_dot_ensureList_dot_20 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ensureSpine_dot_ensureList_dot_20 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_dollar :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_OP_dollar x1 x2 x3500 = d_C_apply x1 x2 x3500

nd_OP_dollar :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_OP_dollar x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_apply x1 x2 x2000 x3500))

d_OP_dollar_hash :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_OP_dollar_hash x1 x2 x3500 = d_OP_dollar_bang x1 (d_C_ensureNotFree x2 x3500) x3500

nd_OP_dollar_hash :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_OP_dollar_hash x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dollar_bang x1 (d_C_ensureNotFree x2 x3500) x2000 x3500))

d_C_error :: Curry t0 => OP_List C_Char -> ConstStore -> t0
d_C_error x1 x3500 = d_OP_dollar_hash_hash d_C_prim_error x1 x3500

d_OP_ampersand_ampersand :: C_Bool -> C_Bool -> ConstStore -> C_Bool
d_OP_ampersand_ampersand x1 x2 x3500 = case x1 of
     C_True -> x2
     C_False -> C_False
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_ampersand_ampersand x1002 x2 x3500) (d_OP_ampersand_ampersand x1003 x2 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_ampersand_ampersand z x2 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_ampersand_ampersand x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_bar_bar :: C_Bool -> C_Bool -> ConstStore -> C_Bool
d_OP_bar_bar x1 x2 x3500 = case x1 of
     C_True -> C_True
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bar_bar x1002 x2 x3500) (d_OP_bar_bar x1003 x2 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bar_bar z x2 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bar_bar x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_not :: C_Bool -> ConstStore -> C_Bool
d_C_not x1 x3500 = case x1 of
     C_True -> C_False
     C_False -> C_True
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_not x1002 x3500) (d_C_not x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_not z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_not x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_otherwise :: ConstStore -> C_Bool
d_C_otherwise x3500 = C_True

d_C_if_then_else :: Curry t0 => C_Bool -> t0 -> t0 -> ConstStore -> t0
d_C_if_then_else x1 x2 x3 x3500 = case x1 of
     C_True -> x2
     C_False -> x3
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_if_then_else x1002 x2 x3 x3500) (d_C_if_then_else x1003 x2 x3 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_if_then_else z x2 x3 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_if_then_else x1002 x2 x3) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_slash_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_slash_eq x1 x2 x3500 = d_C_not (d_OP_eq_eq x1 x2 x3500) x3500

d_C_compare :: Curry t0 => t0 -> t0 -> ConstStore -> C_Ordering
d_C_compare x1 x2 x3500 = d_OP__case_41 x1 x2 (d_OP_eq_eq x1 x2 x3500) x3500

d_OP_lt :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_lt x1 x2 x3500 = d_C_not (d_OP_lt_eq x2 x1 x3500) x3500

d_OP_gt :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_gt x1 x2 x3500 = d_C_not (d_OP_lt_eq x1 x2 x3500) x3500

d_OP_gt_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_gt_eq x1 x2 x3500 = d_C_not (d_OP_lt x1 x2 x3500) x3500

d_C_max :: Curry t0 => t0 -> t0 -> ConstStore -> t0
d_C_max x1 x2 x3500 = d_OP__case_38 x1 x2 (d_OP_gt_eq x1 x2 x3500) x3500

d_C_min :: Curry t0 => t0 -> t0 -> ConstStore -> t0
d_C_min x1 x2 x3500 = d_OP__case_37 x1 x2 (d_OP_lt_eq x1 x2 x3500) x3500

d_C_fst :: (Curry t1,Curry t0) => OP_Tuple2 t0 t1 -> ConstStore -> t0
d_C_fst x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fst x1002 x3500) (d_C_fst x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fst z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fst x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_snd :: (Curry t0,Curry t1) => OP_Tuple2 t0 t1 -> ConstStore -> t1
d_C_snd x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_snd x1002 x3500) (d_C_snd x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_snd z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_snd x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_head :: Curry t0 => OP_List t0 -> ConstStore -> t0
d_C_head x1 x3500 = case x1 of
     (OP_Cons x2 x3) -> x2
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_head x1002 x3500) (d_C_head x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_head z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_head x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tail :: Curry t0 => OP_List t0 -> ConstStore -> OP_List t0
d_C_tail x1 x3500 = case x1 of
     (OP_Cons x2 x3) -> x3
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tail x1002 x3500) (d_C_tail x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tail z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tail x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_null :: Curry t0 => OP_List t0 -> ConstStore -> C_Bool
d_C_null x1 x3500 = case x1 of
     OP_List -> C_True
     (OP_Cons x2 x3) -> C_False
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_null x1002 x3500) (d_C_null x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_null z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_null x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_plus_plus :: Curry t0 => OP_List t0 -> OP_List t0 -> ConstStore -> OP_List t0
d_OP_plus_plus x1 x2 x3500 = case x1 of
     OP_List -> x2
     (OP_Cons x3 x4) -> OP_Cons x3 (d_OP_plus_plus x4 x2 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_plus x1002 x2 x3500) (d_OP_plus_plus x1003 x2 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_plus z x2 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_plus x1002 x2) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_length :: Curry t0 => OP_List t0 -> ConstStore -> C_Int
d_C_length x1 x3500 = d_OP_length_dot_len_dot_90 x1 (C_Int 0#) x3500

d_OP_length_dot_len_dot_90 :: Curry t0 => OP_List t0 -> C_Int -> ConstStore -> C_Int
d_OP_length_dot_len_dot_90 x1 x2 x3500 = case x1 of
     OP_List -> x2
     (OP_Cons x3 x4) -> let
          x5 = d_OP_plus x2 (C_Int 1#) x3500
           in (d_OP_dollar_bang_bang (d_OP_length_dot_len_dot_90 x4) x5 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_length_dot_len_dot_90 x1002 x2 x3500) (d_OP_length_dot_len_dot_90 x1003 x2 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_length_dot_len_dot_90 z x2 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_length_dot_len_dot_90 x1002 x2) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_bang_bang :: Curry t0 => OP_List t0 -> C_Int -> ConstStore -> t0
d_OP_bang_bang x1 x2 x3500 = case x1 of
     (OP_Cons x3 x4) -> d_OP__case_36 x2 x3 x4 (d_OP_eq_eq x2 (C_Int 0#) x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bang_bang x1002 x2 x3500) (d_OP_bang_bang x1003 x2 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bang_bang z x2 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bang_bang x1002 x2) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_map :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> OP_List t0 -> ConstStore -> OP_List t1
d_C_map x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> OP_Cons (d_C_apply x1 x3 x3500) (d_C_map x1 x4 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_map x1 x1002 x3500) (d_C_map x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_map x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_map x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_map :: (Curry t0,Curry t1) => Func t0 t1 -> OP_List t0 -> IDSupply -> ConstStore -> OP_List t1
nd_C_map x1 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (OP_Cons (nd_C_apply x1 x3 x2000 x3500) (nd_C_map x1 x4 x2001 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_map x1 x1002 x3000 x3500) (nd_C_map x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_map x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_map x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_foldl :: (Curry t1,Curry t0) => (t0 -> ConstStore -> t1 -> ConstStore -> t0) -> t0 -> OP_List t1 -> ConstStore -> t0
d_C_foldl x1 x2 x3 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> d_C_foldl x1 (d_C_apply (d_C_apply x1 x2 x3500) x4 x3500) x5 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldl x1 x2 x1002 x3500) (d_C_foldl x1 x2 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldl x1 x2 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldl x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldl :: (Curry t1,Curry t0) => Func t0 (Func t1 t0) -> t0 -> OP_List t1 -> IDSupply -> ConstStore -> t0
nd_C_foldl x1 x2 x3 x3000 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_foldl x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))) x5 x2003 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldl x1 x2 x1002 x3000 x3500) (nd_C_foldl x1 x2 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldl x1 x2 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldl x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_foldl1 :: Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> t0) -> OP_List t0 -> ConstStore -> t0
d_C_foldl1 x1 x2 x3500 = case x2 of
     (OP_Cons x3 x4) -> d_C_foldl x1 x3 x4 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldl1 x1 x1002 x3500) (d_C_foldl1 x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldl1 x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldl1 x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldl1 :: Curry t0 => Func t0 (Func t0 t0) -> OP_List t0 -> IDSupply -> ConstStore -> t0
nd_C_foldl1 x1 x2 x3000 x3500 = case x2 of
     (OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_foldl x1 x3 x4 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldl1 x1 x1002 x3000 x3500) (nd_C_foldl1 x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldl1 x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldl1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_foldr :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1 -> ConstStore -> t1) -> t1 -> OP_List t0 -> ConstStore -> t1
d_C_foldr x1 x2 x3 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> d_C_apply (d_C_apply x1 x4 x3500) (d_C_foldr x1 x2 x5 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldr x1 x2 x1002 x3500) (d_C_foldr x1 x2 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldr x1 x2 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldr x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldr :: (Curry t0,Curry t1) => Func t0 (Func t1 t1) -> t1 -> OP_List t0 -> IDSupply -> ConstStore -> t1
nd_C_foldr x1 x2 x3 x3000 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_C_apply (nd_C_apply x1 x4 x2000 x3500) (nd_C_foldr x1 x2 x5 x2001 x3500) x2002 x3500))))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldr x1 x2 x1002 x3000 x3500) (nd_C_foldr x1 x2 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldr x1 x2 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldr x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_foldr1 :: Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> t0) -> OP_List t0 -> ConstStore -> t0
d_C_foldr1 x1 x2 x3500 = case x2 of
     (OP_Cons x3 x4) -> d_OP__case_34 x1 x3 x4 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldr1 x1 x1002 x3500) (d_C_foldr1 x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldr1 x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldr1 x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldr1 :: Curry t0 => Func t0 (Func t0 t0) -> OP_List t0 -> IDSupply -> ConstStore -> t0
nd_C_foldr1 x1 x2 x3000 x3500 = case x2 of
     (OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x3 x4 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldr1 x1 x1002 x3000 x3500) (nd_C_foldr1 x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldr1 x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldr1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_filter :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> OP_List t0 -> ConstStore -> OP_List t0
d_C_filter x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_33 x1 x3 x4 (d_C_apply x1 x3 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_filter x1 x1002 x3500) (d_C_filter x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_filter x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_filter x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_filter :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> ConstStore -> OP_List t0
nd_C_filter x1 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_33 x1 x3 x4 (nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_filter x1 x1002 x3000 x3500) (nd_C_filter x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_filter x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_filter x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_zip :: (Curry t0,Curry t1) => OP_List t0 -> OP_List t1 -> ConstStore -> OP_List (OP_Tuple2 t0 t1)
d_C_zip x1 x2 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_32 x3 x4 x2 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zip x1002 x2 x3500) (d_C_zip x1003 x2 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zip z x2 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zip x1002 x2) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_zip3 :: (Curry t0,Curry t1,Curry t2) => OP_List t0 -> OP_List t1 -> OP_List t2 -> ConstStore -> OP_List (OP_Tuple3 t0 t1 t2)
d_C_zip3 x1 x2 x3 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> d_OP__case_31 x3 x4 x5 x2 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zip3 x1002 x2 x3 x3500) (d_C_zip3 x1003 x2 x3 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zip3 z x2 x3 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zip3 x1002 x2 x3) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_zipWith :: (Curry t0,Curry t1,Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> t2) -> OP_List t0 -> OP_List t1 -> ConstStore -> OP_List t2
d_C_zipWith x1 x2 x3 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> d_OP__case_29 x1 x4 x5 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zipWith x1 x1002 x3 x3500) (d_C_zipWith x1 x1003 x3 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zipWith x1 z x3 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zipWith x1 x1002 x3) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_zipWith :: (Curry t0,Curry t1,Curry t2) => Func t0 (Func t1 t2) -> OP_List t0 -> OP_List t1 -> IDSupply -> ConstStore -> OP_List t2
nd_C_zipWith x1 x2 x3 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x1 x4 x5 x3 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_zipWith x1 x1002 x3 x3000 x3500) (nd_C_zipWith x1 x1003 x3 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_zipWith x1 z x3 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_zipWith x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3) -> OP_List t0 -> OP_List t1 -> OP_List t2 -> ConstStore -> OP_List t3
d_C_zipWith3 x1 x2 x3 x4 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> d_OP__case_28 x1 x4 x5 x6 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_zipWith3 x1 x1002 x3 x4 x3500) (d_C_zipWith3 x1 x1003 x3 x4 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_zipWith3 x1 z x3 x4 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_zipWith3 x1 x1002 x3 x4) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => Func t0 (Func t1 (Func t2 t3)) -> OP_List t0 -> OP_List t1 -> OP_List t2 -> IDSupply -> ConstStore -> OP_List t3
nd_C_zipWith3 x1 x2 x3 x4 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x1 x4 x5 x6 x3 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_zipWith3 x1 x1002 x3 x4 x3000 x3500) (nd_C_zipWith3 x1 x1003 x3 x4 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_zipWith3 x1 z x3 x4 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_zipWith3 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unzip :: (Curry t0,Curry t1) => OP_List (OP_Tuple2 t0 t1) -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t1)
d_C_unzip x1 x3500 = case x1 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_26 x3 x2 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unzip x1002 x3500) (d_C_unzip x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unzip z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unzip x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unzip_dot___hash_selFP2_hash_xs :: (Curry t396,Curry t395) => OP_Tuple2 (OP_List t395) (OP_List t396) -> ConstStore -> OP_List t395
d_OP_unzip_dot___hash_selFP2_hash_xs x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip_dot___hash_selFP2_hash_xs x1002 x3500) (d_OP_unzip_dot___hash_selFP2_hash_xs x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip_dot___hash_selFP2_hash_xs z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip_dot___hash_selFP2_hash_xs x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unzip_dot___hash_selFP3_hash_ys :: (Curry t395,Curry t396) => OP_Tuple2 (OP_List t395) (OP_List t396) -> ConstStore -> OP_List t396
d_OP_unzip_dot___hash_selFP3_hash_ys x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip_dot___hash_selFP3_hash_ys x1002 x3500) (d_OP_unzip_dot___hash_selFP3_hash_ys x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip_dot___hash_selFP3_hash_ys z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip_dot___hash_selFP3_hash_ys x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unzip3 :: (Curry t0,Curry t1,Curry t2) => OP_List (OP_Tuple3 t0 t1 t2) -> ConstStore -> OP_Tuple3 (OP_List t0) (OP_List t1) (OP_List t2)
d_C_unzip3 x1 x3500 = case x1 of
     OP_List -> OP_Tuple3 OP_List OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_25 x3 x2 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unzip3 x1002 x3500) (d_C_unzip3 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unzip3 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unzip3 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unzip3_dot___hash_selFP5_hash_xs :: (Curry t413,Curry t414,Curry t412) => OP_Tuple3 (OP_List t412) (OP_List t413) (OP_List t414) -> ConstStore -> OP_List t412
d_OP_unzip3_dot___hash_selFP5_hash_xs x1 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x2
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP5_hash_xs x1002 x3500) (d_OP_unzip3_dot___hash_selFP5_hash_xs x1003 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP5_hash_xs z x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP5_hash_xs x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unzip3_dot___hash_selFP6_hash_ys :: (Curry t412,Curry t414,Curry t413) => OP_Tuple3 (OP_List t412) (OP_List t413) (OP_List t414) -> ConstStore -> OP_List t413
d_OP_unzip3_dot___hash_selFP6_hash_ys x1 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x3
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP6_hash_ys x1002 x3500) (d_OP_unzip3_dot___hash_selFP6_hash_ys x1003 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP6_hash_ys z x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP6_hash_ys x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unzip3_dot___hash_selFP7_hash_zs :: (Curry t412,Curry t413,Curry t414) => OP_Tuple3 (OP_List t412) (OP_List t413) (OP_List t414) -> ConstStore -> OP_List t414
d_OP_unzip3_dot___hash_selFP7_hash_zs x1 x3500 = case x1 of
     (OP_Tuple3 x2 x3 x4) -> x4
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unzip3_dot___hash_selFP7_hash_zs x1002 x3500) (d_OP_unzip3_dot___hash_selFP7_hash_zs x1003 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unzip3_dot___hash_selFP7_hash_zs z x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unzip3_dot___hash_selFP7_hash_zs x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_concat :: Curry t0 => OP_List (OP_List t0) -> ConstStore -> OP_List t0
d_C_concat x1 x3500 = d_C_foldr (acceptCs id d_OP_plus_plus) OP_List x1 x3500

d_C_concatMap :: (Curry t0,Curry t1) => (t0 -> ConstStore -> OP_List t1) -> ConstStore -> OP_List t0 -> ConstStore -> OP_List t1
d_C_concatMap x1 x3500 = d_OP_dot d_C_concat (d_C_map x1) x3500

nd_C_concatMap :: (Curry t0,Curry t1) => Func t0 (OP_List t1) -> IDSupply -> ConstStore -> Func (OP_List t0) (OP_List t1)
nd_C_concatMap x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dot (wrapDX id d_C_concat) (wrapNX id (nd_C_map x1)) x2000 x3500))

d_C_iterate :: Curry t0 => (t0 -> ConstStore -> t0) -> t0 -> ConstStore -> OP_List t0
d_C_iterate x1 x2 x3500 = OP_Cons x2 (d_C_iterate x1 (d_C_apply x1 x2 x3500) x3500)

nd_C_iterate :: Curry t0 => Func t0 t0 -> t0 -> IDSupply -> ConstStore -> OP_List t0
nd_C_iterate x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (OP_Cons x2 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_iterate x1 (nd_C_apply x1 x2 x2000 x3500) x2001 x3500))))))

d_C_repeat :: Curry t0 => t0 -> ConstStore -> OP_List t0
d_C_repeat x1 x3500 = OP_Cons x1 (d_C_repeat x1 x3500)

d_C_replicate :: Curry t0 => C_Int -> t0 -> ConstStore -> OP_List t0
d_C_replicate x1 x2 x3500 = d_C_take x1 (d_C_repeat x2 x3500) x3500

d_C_take :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_List t0
d_C_take x1 x2 x3500 = d_OP__case_24 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3500) x3500

d_OP_take_dot_takep_dot_210 :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_List t0
d_OP_take_dot_takep_dot_210 x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> OP_Cons x3 (d_C_take (d_OP_minus x1 (C_Int 1#) x3500) x4 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_take_dot_takep_dot_210 x1 x1002 x3500) (d_OP_take_dot_takep_dot_210 x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_take_dot_takep_dot_210 x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_take_dot_takep_dot_210 x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_drop :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_List t0
d_C_drop x1 x2 x3500 = d_OP__case_23 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3500) x3500

d_OP_drop_dot_dropp_dot_219 :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_List t0
d_OP_drop_dot_dropp_dot_219 x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_C_drop (d_OP_minus x1 (C_Int 1#) x3500) x4 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_drop_dot_dropp_dot_219 x1 x1002 x3500) (d_OP_drop_dot_dropp_dot_219 x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_drop_dot_dropp_dot_219 x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_drop_dot_dropp_dot_219 x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitAt :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_splitAt x1 x2 x3500 = d_OP__case_22 x1 x2 (d_OP_lt_eq x1 (C_Int 0#) x3500) x3500

d_OP_splitAt_dot_splitAtp_dot_229 :: Curry t0 => C_Int -> OP_List t0 -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_OP_splitAt_dot_splitAtp_dot_229 x1 x2 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> let
          x5 = d_C_splitAt (d_OP_minus x1 (C_Int 1#) x3500) x4 x3500
          x6 = d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x5 x3500
          x7 = d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x5 x3500
           in (OP_Tuple2 (OP_Cons x3 x6) x7)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229 x1 x1002 x3500) (d_OP_splitAt_dot_splitAtp_dot_229 x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229 x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229 x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys :: Curry t498 => OP_Tuple2 (OP_List t498) (OP_List t498) -> ConstStore -> OP_List t498
d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1002 x3500) (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP9_hash_ys x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs :: Curry t498 => OP_Tuple2 (OP_List t498) (OP_List t498) -> ConstStore -> OP_List t498
d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1002 x3500) (d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAt_dot_splitAtp_dot_229_dot___hash_selFP10_hash_zs x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_takeWhile :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> OP_List t0 -> ConstStore -> OP_List t0
d_C_takeWhile x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_21 x1 x3 x4 (d_C_apply x1 x3 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_takeWhile x1 x1002 x3500) (d_C_takeWhile x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_takeWhile x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_takeWhile x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_takeWhile :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> ConstStore -> OP_List t0
nd_C_takeWhile x1 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_21 x1 x3 x4 (nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_takeWhile x1 x1002 x3000 x3500) (nd_C_takeWhile x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_takeWhile x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_takeWhile x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dropWhile :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> OP_List t0 -> ConstStore -> OP_List t0
d_C_dropWhile x1 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> d_OP__case_20 x1 x3 x4 (d_C_apply x1 x3 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dropWhile x1 x1002 x3500) (d_C_dropWhile x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dropWhile x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dropWhile x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_dropWhile :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> ConstStore -> OP_List t0
nd_C_dropWhile x1 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_20 x1 x3 x4 (nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_dropWhile x1 x1002 x3000 x3500) (nd_C_dropWhile x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_dropWhile x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_dropWhile x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_span :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> OP_List t0 -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_span x1 x2 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> d_OP__case_19 x1 x3 x4 (d_C_apply x1 x3 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_span x1 x1002 x3500) (d_C_span x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_span x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_span x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_span :: Curry t0 => Func t0 C_Bool -> OP_List t0 -> IDSupply -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
nd_C_span x1 x2 x3000 x3500 = case x2 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_19 x1 x3 x4 (nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_span x1 x1002 x3000 x3500) (nd_C_span x1 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_span x1 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_span x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_span_dot___hash_selFP12_hash_ys :: Curry t550 => OP_Tuple2 (OP_List t550) (OP_List t550) -> ConstStore -> OP_List t550
d_OP_span_dot___hash_selFP12_hash_ys x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_span_dot___hash_selFP12_hash_ys x1002 x3500) (d_OP_span_dot___hash_selFP12_hash_ys x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_span_dot___hash_selFP12_hash_ys z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_span_dot___hash_selFP12_hash_ys x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_span_dot___hash_selFP13_hash_zs :: Curry t550 => OP_Tuple2 (OP_List t550) (OP_List t550) -> ConstStore -> OP_List t550
d_OP_span_dot___hash_selFP13_hash_zs x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_span_dot___hash_selFP13_hash_zs x1002 x3500) (d_OP_span_dot___hash_selFP13_hash_zs x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_span_dot___hash_selFP13_hash_zs z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_span_dot___hash_selFP13_hash_zs x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_break :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> ConstStore -> OP_List t0 -> ConstStore -> OP_Tuple2 (OP_List t0) (OP_List t0)
d_C_break x1 x3500 = d_C_span (d_OP_dot d_C_not x1 x3500)

nd_C_break :: Curry t0 => Func t0 C_Bool -> IDSupply -> ConstStore -> Func (OP_List t0) (OP_Tuple2 (OP_List t0) (OP_List t0))
nd_C_break x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_span (nd_OP_dot (wrapDX id d_C_not) x1 x2000 x3500))))

d_C_lines :: OP_List C_Char -> ConstStore -> OP_List (OP_List C_Char)
d_C_lines x1 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> let
          x4 = d_OP_lines_dot_splitline_dot_261 (OP_Cons x2 x3) x3500
          x5 = d_OP_lines_dot___hash_selFP18_hash_l x4 x3500
          x6 = d_OP_lines_dot___hash_selFP19_hash_xs_l x4 x3500
           in (OP_Cons x5 (d_C_lines x6 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lines x1002 x3500) (d_C_lines x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lines z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lines x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lines_dot_splitline_dot_261 :: OP_List C_Char -> ConstStore -> OP_Tuple2 (OP_List C_Char) (OP_List C_Char)
d_OP_lines_dot_splitline_dot_261 x1 x3500 = case x1 of
     OP_List -> OP_Tuple2 OP_List OP_List
     (OP_Cons x2 x3) -> d_OP__case_17 x2 x3 (d_OP_eq_eq x2 (C_Char '\n'#) x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261 x1002 x3500) (d_OP_lines_dot_splitline_dot_261 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1002 x3500) (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1002 x3500) (d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lines_dot___hash_selFP18_hash_l :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_lines_dot___hash_selFP18_hash_l x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot___hash_selFP18_hash_l x1002 x3500) (d_OP_lines_dot___hash_selFP18_hash_l x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot___hash_selFP18_hash_l z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot___hash_selFP18_hash_l x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lines_dot___hash_selFP19_hash_xs_l :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_lines_dot___hash_selFP19_hash_xs_l x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lines_dot___hash_selFP19_hash_xs_l x1002 x3500) (d_OP_lines_dot___hash_selFP19_hash_xs_l x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lines_dot___hash_selFP19_hash_xs_l z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lines_dot___hash_selFP19_hash_xs_l x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unlines :: OP_List (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_C_unlines x1 x3500 = d_C_apply (d_C_concatMap (d_C_flip (acceptCs id d_OP_plus_plus) (OP_Cons (C_Char '\n'#) OP_List)) x3500) x1 x3500

d_C_words :: OP_List C_Char -> ConstStore -> OP_List (OP_List C_Char)
d_C_words x1 x3500 = let
     x2 = d_C_dropWhile d_OP_words_dot_isSpace_dot_273 x1 x3500
      in (d_OP__case_16 x2 (d_OP_eq_eq x2 OP_List x3500) x3500)

d_OP_words_dot_isSpace_dot_273 :: C_Char -> ConstStore -> C_Bool
d_OP_words_dot_isSpace_dot_273 x1 x3500 = d_OP_bar_bar (d_OP_eq_eq x1 (C_Char ' '#) x3500) (d_OP_bar_bar (d_OP_eq_eq x1 (C_Char '\t'#) x3500) (d_OP_bar_bar (d_OP_eq_eq x1 (C_Char '\n'#) x3500) (d_OP_eq_eq x1 (C_Char '\r'#) x3500) x3500) x3500) x3500

d_OP_words_dot___hash_selFP21_hash_w :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_words_dot___hash_selFP21_hash_w x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x2
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_words_dot___hash_selFP21_hash_w x1002 x3500) (d_OP_words_dot___hash_selFP21_hash_w x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_words_dot___hash_selFP21_hash_w z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_words_dot___hash_selFP21_hash_w x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_words_dot___hash_selFP22_hash_s2 :: OP_Tuple2 (OP_List C_Char) (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_OP_words_dot___hash_selFP22_hash_s2 x1 x3500 = case x1 of
     (OP_Tuple2 x2 x3) -> x3
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_words_dot___hash_selFP22_hash_s2 x1002 x3500) (d_OP_words_dot___hash_selFP22_hash_s2 x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_words_dot___hash_selFP22_hash_s2 z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_words_dot___hash_selFP22_hash_s2 x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unwords :: OP_List (OP_List C_Char) -> ConstStore -> OP_List C_Char
d_C_unwords x1 x3500 = d_OP__case_15 x1 (d_OP_eq_eq x1 OP_List x3500) x3500

d_OP_unwords_dot___hash_lambda3 :: OP_List C_Char -> OP_List C_Char -> ConstStore -> OP_List C_Char
d_OP_unwords_dot___hash_lambda3 x1 x2 x3500 = d_OP_plus_plus x1 (OP_Cons (C_Char ' '#) x2) x3500

d_C_reverse :: Curry t0 => ConstStore -> OP_List t0 -> ConstStore -> OP_List t0
d_C_reverse x3500 = d_C_foldl (acceptCs id (d_C_flip (acceptCs (acceptCs id) OP_Cons))) OP_List

nd_C_reverse :: Curry t0 => IDSupply -> ConstStore -> Func (OP_List t0) (OP_List t0)
nd_C_reverse x3000 x3500 = wrapNX id (nd_C_foldl (wrapDX (wrapNX id) (acceptCs id (nd_C_flip (wrapDX (wrapDX id) (acceptCs (acceptCs id) OP_Cons))))) OP_List)

d_C_and :: ConstStore -> OP_List C_Bool -> ConstStore -> C_Bool
d_C_and x3500 = d_C_foldr (acceptCs id d_OP_ampersand_ampersand) C_True

nd_C_and :: IDSupply -> ConstStore -> Func (OP_List C_Bool) C_Bool
nd_C_and x3000 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_ampersand_ampersand)) C_True)

d_C_or :: ConstStore -> OP_List C_Bool -> ConstStore -> C_Bool
d_C_or x3500 = d_C_foldr (acceptCs id d_OP_bar_bar) C_False

nd_C_or :: IDSupply -> ConstStore -> Func (OP_List C_Bool) C_Bool
nd_C_or x3000 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_bar_bar)) C_False)

d_C_any :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> ConstStore -> OP_List t0 -> ConstStore -> C_Bool
d_C_any x1 x3500 = d_OP_dot (d_C_or x3500) (d_C_map x1) x3500

nd_C_any :: Curry t0 => Func t0 C_Bool -> IDSupply -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_any x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_or x2000 x3500) (wrapNX id (nd_C_map x1)) x2001 x3500)))))

d_C_all :: Curry t0 => (t0 -> ConstStore -> C_Bool) -> ConstStore -> OP_List t0 -> ConstStore -> C_Bool
d_C_all x1 x3500 = d_OP_dot (d_C_and x3500) (d_C_map x1) x3500

nd_C_all :: Curry t0 => Func t0 C_Bool -> IDSupply -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_all x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_and x2000 x3500) (wrapNX id (nd_C_map x1)) x2001 x3500)))))

d_C_elem :: Curry t0 => t0 -> ConstStore -> OP_List t0 -> ConstStore -> C_Bool
d_C_elem x1 x3500 = d_C_any (d_OP_eq_eq x1) x3500

nd_C_elem :: Curry t0 => t0 -> IDSupply -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_elem x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_any (wrapDX id (d_OP_eq_eq x1)) x2000 x3500))

d_C_notElem :: Curry t0 => t0 -> ConstStore -> OP_List t0 -> ConstStore -> C_Bool
d_C_notElem x1 x3500 = d_C_all (d_OP_slash_eq x1) x3500

nd_C_notElem :: Curry t0 => t0 -> IDSupply -> ConstStore -> Func (OP_List t0) C_Bool
nd_C_notElem x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_all (wrapDX id (d_OP_slash_eq x1)) x2000 x3500))

d_C_lookup :: (Curry t0,Curry t1) => t0 -> OP_List (OP_Tuple2 t0 t1) -> ConstStore -> C_Maybe t1
d_C_lookup x1 x2 x3500 = case x2 of
     OP_List -> C_Nothing
     (OP_Cons x3 x4) -> d_OP__case_14 x1 x4 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookup x1 x1002 x3500) (d_C_lookup x1 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookup x1 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookup x1 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_enumFrom :: C_Int -> ConstStore -> OP_List C_Int
d_C_enumFrom x1 x3500 = OP_Cons x1 (d_C_enumFrom (d_OP_plus x1 (C_Int 1#) x3500) x3500)

d_C_enumFromThen :: C_Int -> C_Int -> ConstStore -> OP_List C_Int
d_C_enumFromThen x1 x2 x3500 = d_C_iterate (d_OP_plus (d_OP_minus x2 x1 x3500)) x1 x3500

d_C_enumFromTo :: C_Int -> C_Int -> ConstStore -> OP_List C_Int
d_C_enumFromTo x1 x2 x3500 = d_OP__case_11 x1 x2 (d_OP_gt x1 x2 x3500) x3500

d_C_enumFromThenTo :: C_Int -> C_Int -> C_Int -> ConstStore -> OP_List C_Int
d_C_enumFromThenTo x1 x2 x3 x3500 = d_C_takeWhile (d_OP_enumFromThenTo_dot_p_dot_311 x3 x1 x2) (d_C_enumFromThen x1 x2 x3500) x3500

d_OP_enumFromThenTo_dot_p_dot_311 :: C_Int -> C_Int -> C_Int -> C_Int -> ConstStore -> C_Bool
d_OP_enumFromThenTo_dot_p_dot_311 x1 x2 x3 x4 x3500 = d_OP__case_10 x1 x2 x3 x4 (d_OP_gt_eq x3 x2 x3500) x3500

d_C_ord :: C_Char -> ConstStore -> C_Int
d_C_ord x1 x3500 = d_OP_dollar_hash d_C_prim_ord x1 x3500

d_C_chr :: C_Int -> ConstStore -> C_Char
d_C_chr x1 x3500 = d_OP__case_8 x1 (d_OP_gt_eq x1 (C_Int 0#) x3500) x3500

d_C_negate :: C_Int -> ConstStore -> C_Int
d_C_negate x1 x3500 = d_OP_minus (C_Int 0#) x1 x3500

d_OP_ampersand_gt :: Curry t0 => C_Success -> t0 -> ConstStore -> t0
d_OP_ampersand_gt x1 x2 x3500 = d_OP___cond_0__ampersand_gt x2 x1 x3500

d_OP___cond_0__ampersand_gt x1 x2 x3500 = case x2 of
     C_Success -> x1
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__ampersand_gt x1 x1002 x3500) (d_OP___cond_0__ampersand_gt x1 x1003 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__ampersand_gt x1 z x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__ampersand_gt x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__ampersand_gt x1 x2 x3000 x3500 = case x2 of
     C_Success -> x1
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__ampersand_gt x1 x1002 x3000 x3500) (nd_OP___cond_0__ampersand_gt x1 x1003 x3000 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__ampersand_gt x1 z x3000 x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__ampersand_gt x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_maybe :: (Curry t1,Curry t0) => t0 -> (t1 -> ConstStore -> t0) -> C_Maybe t1 -> ConstStore -> t0
d_C_maybe x1 x2 x3 x3500 = case x3 of
     C_Nothing -> x1
     (C_Just x4) -> d_C_apply x2 x4 x3500
     (Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maybe x1 x2 x1002 x3500) (d_C_maybe x1 x2 x1003 x3500)
     (Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maybe x1 x2 z x3500) x1002
     (Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maybe x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_maybe :: (Curry t1,Curry t0) => t0 -> Func t1 t0 -> C_Maybe t1 -> IDSupply -> ConstStore -> t0
nd_C_maybe x1 x2 x3 x3000 x3500 = case x3 of
     C_Nothing -> x1
     (C_Just x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x2 x4 x2000 x3500))
     (Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_maybe x1 x2 x1002 x3000 x3500) (nd_C_maybe x1 x2 x1003 x3000 x3500)
     (Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_maybe x1 x2 z x3000 x3500) x1002
     (Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_maybe x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_either :: (Curry t0,Curry t2,Curry t1) => (t0 -> ConstStore -> t1) -> (t2 -> ConstStore -> t1) -> C_Either t0 t2 -> ConstStore -> t1
d_C_either x1 x2 x3 x3500 = case x3 of
     (C_Left x4) -> d_C_apply x1 x4 x3500
     (C_Right x5) -> d_C_apply x2 x5 x3500
     (Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_either x1 x2 x1002 x3500) (d_C_either x1 x2 x1003 x3500)
     (Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_either x1 x2 z x3500) x1002
     (Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_either x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_either :: (Curry t0,Curry t2,Curry t1) => Func t0 t1 -> Func t2 t1 -> C_Either t0 t2 -> IDSupply -> ConstStore -> t1
nd_C_either x1 x2 x3 x3000 x3500 = case x3 of
     (C_Left x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x1 x4 x2000 x3500))
     (C_Right x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_apply x2 x5 x2000 x3500))
     (Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_either x1 x2 x1002 x3000 x3500) (nd_C_either x1 x2 x1003 x3000 x3500)
     (Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_either x1 x2 z x3000 x3500) x1002
     (Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_either x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_gt_gt :: (Curry t0,Curry t1) => C_IO t0 -> C_IO t1 -> ConstStore -> C_IO t1
d_OP_gt_gt x1 x2 x3500 = d_OP_gt_gt_eq x1 (d_OP_gt_gt_dot___hash_lambda4 x2) x3500

d_OP_gt_gt_dot___hash_lambda4 :: (Curry t731,Curry t730) => C_IO t730 -> t731 -> ConstStore -> C_IO t730
d_OP_gt_gt_dot___hash_lambda4 x1 x2 x3500 = x1

d_C_done :: ConstStore -> C_IO OP_Unit
d_C_done x3500 = d_C_return OP_Unit x3500

d_C_putChar :: C_Char -> ConstStore -> C_IO OP_Unit
d_C_putChar x1 x3500 = d_OP_dollar_hash_hash d_C_prim_putChar x1 x3500

d_C_readFile :: OP_List C_Char -> ConstStore -> C_IO (OP_List C_Char)
d_C_readFile x1 x3500 = d_OP_dollar_hash_hash d_C_prim_readFile x1 x3500

d_C_writeFile :: OP_List C_Char -> OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_writeFile x1 x2 x3500 = d_OP_dollar_hash_hash (d_OP_dollar_hash_hash (acceptCs id d_C_prim_writeFile) x1 x3500) x2 x3500

d_C_appendFile :: OP_List C_Char -> OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_appendFile x1 x2 x3500 = d_OP_dollar_hash_hash (d_OP_dollar_hash_hash (acceptCs id d_C_prim_appendFile) x1 x3500) x2 x3500

d_C_putStr :: OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_putStr x1 x3500 = case x1 of
     OP_List -> d_C_done x3500
     (OP_Cons x2 x3) -> d_OP_gt_gt (d_C_putChar x2 x3500) (d_C_putStr x3 x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_putStr x1002 x3500) (d_C_putStr x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_putStr z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_putStr x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_putStrLn :: OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_putStrLn x1 x3500 = d_OP_gt_gt (d_C_putStr x1 x3500) (d_C_putChar (C_Char '\n'#) x3500) x3500

d_C_getLine :: ConstStore -> C_IO (OP_List C_Char)
d_C_getLine x3500 = d_OP_gt_gt_eq (d_C_getChar x3500) d_OP_getLine_dot___hash_lambda5 x3500

d_OP_getLine_dot___hash_lambda5 :: C_Char -> ConstStore -> C_IO (OP_List C_Char)
d_OP_getLine_dot___hash_lambda5 x1 x3500 = d_OP__case_7 x1 (d_OP_eq_eq x1 (C_Char '\n'#) x3500) x3500

d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 :: C_Char -> OP_List C_Char -> ConstStore -> C_IO (OP_List C_Char)
d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3500 = d_C_return (OP_Cons x1 x2) x3500

d_C_userError :: OP_List C_Char -> ConstStore -> C_IOError
d_C_userError x1 x3500 = C_UserError x1

d_C_ioError :: Curry t0 => C_IOError -> ConstStore -> C_IO t0
d_C_ioError x1 x3500 = d_OP_dollar_hash_hash d_C_prim_ioError x1 x3500

d_C_showError :: C_IOError -> ConstStore -> OP_List C_Char
d_C_showError x1 x3500 = case x1 of
     (C_IOError x2) -> d_OP_plus_plus (OP_Cons (C_Char 'i'#) (OP_Cons (C_Char '/'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List))))))))))) x2 x3500
     (C_UserError x3) -> d_OP_plus_plus (OP_Cons (C_Char 'u'#) (OP_Cons (C_Char 's'#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))) x3 x3500
     (C_FailError x4) -> d_OP_plus_plus (OP_Cons (C_Char 'f'#) (OP_Cons (C_Char 'a'#) (OP_Cons (C_Char 'i'#) (OP_Cons (C_Char 'l'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))) x4 x3500
     (C_NondetError x5) -> d_OP_plus_plus (OP_Cons (C_Char 'n'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'n'#) (OP_Cons (C_Char 'd'#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 't'#) (OP_Cons (C_Char ' '#) (OP_Cons (C_Char 'e'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char 'o'#) (OP_Cons (C_Char 'r'#) (OP_Cons (C_Char ':'#) (OP_Cons (C_Char ' '#) OP_List)))))))))))))) x5 x3500
     (Choice_C_IOError x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showError x1002 x3500) (d_C_showError x1003 x3500)
     (Choices_C_IOError x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showError z x3500) x1002
     (Guard_C_IOError x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showError x1002) $! (addCs x1001 x3500))
     (Fail_C_IOError x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_show :: Curry t0 => t0 -> ConstStore -> OP_List C_Char
d_C_show x1 x3500 = d_OP_dollar_hash_hash d_C_prim_show x1 x3500

d_C_print :: Curry t0 => t0 -> ConstStore -> C_IO OP_Unit
d_C_print x1 x3500 = d_C_putStrLn (d_C_show x1 x3500) x3500

d_C_doSolve :: C_Success -> ConstStore -> C_IO OP_Unit
d_C_doSolve x1 x3500 = d_OP___cond_0_doSolve x1 x3500

d_OP___cond_0_doSolve x1 x3500 = case x1 of
     C_Success -> d_C_done x3500
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_doSolve x1002 x3500) (d_OP___cond_0_doSolve x1003 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_doSolve z x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_doSolve x1002) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_doSolve x1 x3000 x3500 = case x1 of
     C_Success -> d_C_done x3500
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_doSolve x1002 x3000 x3500) (nd_OP___cond_0_doSolve x1003 x3000 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_doSolve z x3000 x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_doSolve x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_sequenceIO :: Curry t0 => OP_List (C_IO t0) -> ConstStore -> C_IO (OP_List t0)
d_C_sequenceIO x1 x3500 = case x1 of
     OP_List -> d_C_return OP_List x3500
     (OP_Cons x2 x3) -> d_OP_gt_gt_eq x2 (d_OP_sequenceIO_dot___hash_lambda7 x3) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sequenceIO x1002 x3500) (d_C_sequenceIO x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sequenceIO z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sequenceIO x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_sequenceIO_dot___hash_lambda7 :: Curry t801 => OP_List (C_IO t801) -> t801 -> ConstStore -> C_IO (OP_List t801)
d_OP_sequenceIO_dot___hash_lambda7 x1 x2 x3500 = d_OP_gt_gt_eq (d_C_sequenceIO x1 x3500) (d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 x2) x3500

d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 :: Curry t801 => t801 -> OP_List t801 -> ConstStore -> C_IO (OP_List t801)
d_OP_sequenceIO_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3500 = d_C_return (OP_Cons x1 x2) x3500

d_C_sequenceIO_ :: Curry t0 => ConstStore -> OP_List (C_IO t0) -> ConstStore -> C_IO OP_Unit
d_C_sequenceIO_ x3500 = d_C_foldr (acceptCs id d_OP_gt_gt) (d_C_done x3500)

nd_C_sequenceIO_ :: Curry t0 => IDSupply -> ConstStore -> Func (OP_List (C_IO t0)) (C_IO OP_Unit)
nd_C_sequenceIO_ x3000 x3500 = wrapNX id (nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_gt_gt)) (d_C_done x3500))

d_C_mapIO :: (Curry t0,Curry t1) => (t0 -> ConstStore -> C_IO t1) -> ConstStore -> OP_List t0 -> ConstStore -> C_IO (OP_List t1)
d_C_mapIO x1 x3500 = d_OP_dot d_C_sequenceIO (d_C_map x1) x3500

nd_C_mapIO :: (Curry t0,Curry t1) => Func t0 (C_IO t1) -> IDSupply -> ConstStore -> Func (OP_List t0) (C_IO (OP_List t1))
nd_C_mapIO x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_dot (wrapDX id d_C_sequenceIO) (wrapNX id (nd_C_map x1)) x2000 x3500))

d_C_mapIO_ :: (Curry t1,Curry t0) => (t0 -> ConstStore -> C_IO t1) -> ConstStore -> OP_List t0 -> ConstStore -> C_IO OP_Unit
d_C_mapIO_ x1 x3500 = d_OP_dot (d_C_sequenceIO_ x3500) (d_C_map x1) x3500

nd_C_mapIO_ :: (Curry t1,Curry t0) => Func t0 (C_IO t1) -> IDSupply -> ConstStore -> Func (OP_List t0) (C_IO OP_Unit)
nd_C_mapIO_ x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_dot (nd_C_sequenceIO_ x2000 x3500) (wrapNX id (nd_C_map x1)) x2001 x3500)))))

nd_C_unknown :: Curry t0 => IDSupply -> ConstStore -> t0
nd_C_unknown x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x1 = generate x2000
           in x1))

nd_C_getSomeValue :: Curry t0 => t0 -> IDSupply -> ConstStore -> C_IO t0
nd_C_getSomeValue x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_return (nd_C_findfirst (wrapNX id (nd_C_flip (wrapDX (wrapDX id) (acceptCs id d_OP_eq_colon_eq)) x1)) x2000 x3500) x3500))

d_C_inject :: Curry t0 => (t0 -> ConstStore -> C_Success) -> (t0 -> ConstStore -> C_Success) -> ConstStore -> t0 -> ConstStore -> C_Success
d_C_inject x1 x2 x3500 = d_OP_inject_dot___hash_lambda9 x1 x2

nd_C_inject :: Curry t0 => Func t0 C_Success -> Func t0 C_Success -> IDSupply -> ConstStore -> Func t0 C_Success
nd_C_inject x1 x2 x3000 x3500 = wrapNX id (nd_OP_inject_dot___hash_lambda9 x1 x2)

d_OP_inject_dot___hash_lambda9 :: Curry t904 => (t904 -> ConstStore -> C_Success) -> (t904 -> ConstStore -> C_Success) -> t904 -> ConstStore -> C_Success
d_OP_inject_dot___hash_lambda9 x1 x2 x3 x3500 = d_OP_ampersand (d_C_apply x2 x3 x3500) (d_C_apply x1 x3 x3500) x3500

nd_OP_inject_dot___hash_lambda9 :: Curry t904 => Func t904 C_Success -> Func t904 C_Success -> t904 -> IDSupply -> ConstStore -> C_Success
nd_OP_inject_dot___hash_lambda9 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (d_OP_ampersand (nd_C_apply x2 x3 x2000 x3500) (nd_C_apply x1 x3 x2001 x3500) x3500)))))

d_C_solveAll :: Curry t0 => (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_C_solveAll x1 x3500 = d_OP_solveAll_dot_evalall_dot_399 (d_C_try x1 x3500) x3500

nd_C_solveAll :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_C_solveAll x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_solveAll_dot_evalall_dot_399 (nd_C_try x1 x2000 x3500) x2001 x3500)))))

d_OP_solveAll_dot_evalall3_dot_399 :: Curry t0 => OP_List (t0 -> ConstStore -> C_Success) -> OP_List (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_OP_solveAll_dot_evalall3_dot_399 x1 x2 x3500 = case x1 of
     OP_List -> d_OP_solveAll_dot_evalall2_dot_399 x2 x3500
     (OP_Cons x3 x4) -> d_OP__case_6 x2 x3 x4 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_solveAll_dot_evalall3_dot_399 x1002 x2 x3500) (d_OP_solveAll_dot_evalall3_dot_399 x1003 x2 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_solveAll_dot_evalall3_dot_399 z x2 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_solveAll_dot_evalall3_dot_399 x1002 x2) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_solveAll_dot_evalall3_dot_399 :: Curry t0 => OP_List (Func t0 C_Success) -> OP_List (Func t0 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_OP_solveAll_dot_evalall3_dot_399 x1 x2 x3000 x3500 = case x1 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_solveAll_dot_evalall2_dot_399 x2 x2000 x3500))
     (OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x2 x3 x4 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_solveAll_dot_evalall3_dot_399 x1002 x2 x3000 x3500) (nd_OP_solveAll_dot_evalall3_dot_399 x1003 x2 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_solveAll_dot_evalall3_dot_399 z x2 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_solveAll_dot_evalall3_dot_399 x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_solveAll_dot_evalall2_dot_399 :: Curry t0 => OP_List (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_OP_solveAll_dot_evalall2_dot_399 x1 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> d_OP_solveAll_dot_evalall3_dot_399 (d_C_try x2 x3500) x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_solveAll_dot_evalall2_dot_399 x1002 x3500) (d_OP_solveAll_dot_evalall2_dot_399 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_solveAll_dot_evalall2_dot_399 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_solveAll_dot_evalall2_dot_399 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_solveAll_dot_evalall2_dot_399 :: Curry t0 => OP_List (Func t0 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_OP_solveAll_dot_evalall2_dot_399 x1 x3000 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_solveAll_dot_evalall3_dot_399 (nd_C_try x2 x2000 x3500) x3 x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_solveAll_dot_evalall2_dot_399 x1002 x3000 x3500) (nd_OP_solveAll_dot_evalall2_dot_399 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_solveAll_dot_evalall2_dot_399 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_solveAll_dot_evalall2_dot_399 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_solveAll_dot_evalall_dot_399 :: Curry t0 => OP_List (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_OP_solveAll_dot_evalall_dot_399 x1 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> d_OP__case_5 x2 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_solveAll_dot_evalall_dot_399 x1002 x3500) (d_OP_solveAll_dot_evalall_dot_399 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_solveAll_dot_evalall_dot_399 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_solveAll_dot_evalall_dot_399 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_solveAll_dot_evalall_dot_399 :: Curry t0 => OP_List (Func t0 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_OP_solveAll_dot_evalall_dot_399 x1 x3000 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x2 x3 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_solveAll_dot_evalall_dot_399 x1002 x3000 x3500) (nd_OP_solveAll_dot_evalall_dot_399 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_solveAll_dot_evalall_dot_399 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_solveAll_dot_evalall_dot_399 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_solveAll2 :: Curry t0 => (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_C_solveAll2 x1 x3500 = d_OP_solveAll2_dot_evalResult_dot_417 (d_C_try x1 x3500) x3500

nd_C_solveAll2 :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_C_solveAll2 x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_solveAll2_dot_evalResult_dot_417 (nd_C_try x1 x2000 x3500) x2001 x3500)))))

d_OP_solveAll2_dot_evalResult_dot_417 :: Curry t0 => OP_List (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_OP_solveAll2_dot_evalResult_dot_417 x1 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> d_OP__case_4 x2 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_solveAll2_dot_evalResult_dot_417 x1002 x3500) (d_OP_solveAll2_dot_evalResult_dot_417 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_solveAll2_dot_evalResult_dot_417 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_solveAll2_dot_evalResult_dot_417 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_solveAll2_dot_evalResult_dot_417 :: Curry t0 => OP_List (Func t0 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_OP_solveAll2_dot_evalResult_dot_417 x1 x3000 x3500 = case x1 of
     OP_List -> OP_List
     (OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x2 x3 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_solveAll2_dot_evalResult_dot_417 x1002 x3000 x3500) (nd_OP_solveAll2_dot_evalResult_dot_417 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_solveAll2_dot_evalResult_dot_417 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_solveAll2_dot_evalResult_dot_417 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_once :: Curry t0 => (t0 -> ConstStore -> C_Success) -> ConstStore -> t0 -> ConstStore -> C_Success
d_C_once x1 x3500 = d_C_head (d_C_solveAll x1 x3500) x3500

nd_C_once :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> Func t0 C_Success
nd_C_once x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_head (nd_C_solveAll x1 x2000 x3500) x3500))

nd_C_best :: Curry t0 => Func t0 C_Success -> Func t0 (Func t0 C_Bool) -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_C_best x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x2 OP_List (nd_C_try x1 x2000 x3500) OP_List x2001 x3500)))))

nd_OP_best_dot_constrain_dot_427 :: Curry t1033 => Func t1033 (Func t1033 C_Bool) -> Func t1033 C_Success -> OP_List (Func t1033 C_Success) -> IDSupply -> ConstStore -> Func t1033 C_Success
nd_OP_best_dot_constrain_dot_427 x1 x2 x3 x3000 x3500 = case x3 of
     OP_List -> x2
     (OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x2 x4 x5 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_best_dot_constrain_dot_427 x1 x2 x1002 x3000 x3500) (nd_OP_best_dot_constrain_dot_427 x1 x2 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_best_dot_constrain_dot_427 x1 x2 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_best_dot_constrain_dot_427 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_best_dot_constrain_dot_427_dot___hash_lambda10 :: Curry t1033 => Func t1033 (Func t1033 C_Bool) -> Func t1033 C_Success -> t1033 -> IDSupply -> ConstStore -> C_Success
nd_OP_best_dot_constrain_dot_427_dot___hash_lambda10 x1 x2 x3 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2004 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2004 (seq x2005 (let
               x4 = generate x2005
                in (let
                    x2000 = leftSupply x2004
                    x2003 = rightSupply x2004
                     in (seq x2000 (seq x2003 (d_OP_ampersand (nd_C_apply x2 x4 x2000 x3500) (d_OP_eq_colon_eq (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (nd_C_apply (nd_C_apply x1 x3 x2001 x3500) x4 x2002 x3500)))) C_True x3500) x3500)))))))))

nd_OP_best_dot_bestHelp_dot_427 :: Curry t1033 => Func t1033 (Func t1033 C_Bool) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t1033 C_Success)
nd_OP_best_dot_bestHelp_dot_427 x1 x2 x3 x4 x3000 x3500 = case x2 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x4 x3 x2000 x3500))
     (OP_Cons x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_best_dot_evalX_dot_427 x1 (nd_C_try x7 x2000 x3500) x8 x3 x4 x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_best_dot_bestHelp_dot_427 x1 x1002 x3 x4 x3000 x3500) (nd_OP_best_dot_bestHelp_dot_427 x1 x1003 x3 x4 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_best_dot_bestHelp_dot_427 x1 z x3 x4 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_best_dot_bestHelp_dot_427 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_best_dot_evalY_dot_427 :: Curry t1033 => Func t1033 (Func t1033 C_Bool) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t1033 C_Success)
nd_OP_best_dot_evalY_dot_427 x1 x2 x3 x4 x3000 x3500 = case x2 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 OP_List x3 x4 x2000 x3500))
     (OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x3 x4 x5 x6 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_best_dot_evalY_dot_427 x1 x1002 x3 x4 x3000 x3500) (nd_OP_best_dot_evalY_dot_427 x1 x1003 x3 x4 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_best_dot_evalY_dot_427 x1 z x3 x4 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_best_dot_evalY_dot_427 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_best_dot_evalX_dot_427 :: Curry t1033 => Func t1033 (Func t1033 C_Bool) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> OP_List (Func t1033 C_Success) -> IDSupply -> ConstStore -> OP_List (Func t1033 C_Success)
nd_OP_best_dot_evalX_dot_427 x1 x2 x3 x4 x5 x3000 x3500 = case x2 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 x3 x4 x5 x2000 x3500))
     (OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x3 x4 x5 x6 x7 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_best_dot_evalX_dot_427 x1 x1002 x3 x4 x5 x3000 x3500) (nd_OP_best_dot_evalX_dot_427 x1 x1003 x3 x4 x5 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_best_dot_evalX_dot_427 x1 z x3 x4 x5 x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_best_dot_evalX_dot_427 x1 x1002 x3 x4 x5 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_findall :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> OP_List t0
nd_C_findall x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_map (wrapNX id nd_C_unpack) (nd_C_solveAll x1 x2000 x3500) x2001 x3500)))))

nd_C_findfirst :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> t0
nd_C_findfirst x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_head (nd_C_findall x1 x2000 x3500) x3500))

nd_C_browse :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> C_IO OP_Unit
nd_C_browse x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_putStr (d_C_show (nd_C_unpack x1 x2000 x3500) x3500) x3500))

nd_C_browseList :: Curry t0 => OP_List (Func t0 C_Success) -> IDSupply -> ConstStore -> C_IO OP_Unit
nd_C_browseList x1 x3000 x3500 = case x1 of
     OP_List -> d_C_done x3500
     (OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_OP_gt_gt (d_OP_gt_gt (nd_C_browse x2 x2000 x3500) (d_C_putChar (C_Char '\n'#) x3500) x3500) (nd_C_browseList x3 x2001 x3500) x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_browseList x1002 x3000 x3500) (nd_C_browseList x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_browseList z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_browseList x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_unpack :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> t0
nd_C_unpack x1 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x2 = generate x2003
                in (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP___cond_0_unpack x2 (nd_C_apply x1 x2 x2000 x3500) x2001 x3500)))))))))

d_OP___cond_0_unpack x1 x2 x3500 = case x2 of
     C_Success -> x1
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_unpack x1 x1002 x3500) (d_OP___cond_0_unpack x1 x1003 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_unpack x1 z x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_unpack x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_unpack x1 x2 x3000 x3500 = case x2 of
     C_Success -> x1
     (Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_unpack x1 x1002 x3000 x3500) (nd_OP___cond_0_unpack x1 x1003 x3000 x3500)
     (Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_unpack x1 z x3000 x3500) x1002
     (Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_unpack x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_normalForm :: Curry t0 => t0 -> ConstStore -> t0
d_C_normalForm x1 x3500 = d_OP_dollar_bang_bang d_C_id x1 x3500

d_C_groundNormalForm :: Curry t0 => t0 -> ConstStore -> t0
d_C_groundNormalForm x1 x3500 = d_OP_dollar_hash_hash d_C_id x1 x3500

nd_OP__case_0 x1 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 OP_List (d_OP_plus_plus x3 x4 x3500) (OP_Cons x6 OP_List) x2000 x3500))
     (OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 (d_OP_plus_plus (OP_Cons x6 (OP_Cons x8 x9)) x3 x3500) x4 x5 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_0 x1 x3 x4 x5 x6 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x3 x4 x5 x6 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 OP_List x3 (OP_Cons x5 OP_List) x2000 x3500))
     (OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_best_dot_bestHelp_dot_427 x1 (OP_Cons x5 (OP_Cons x7 x8)) x3 x4 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x5 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x5 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x4 x3 x3000 x3500 = case x3 of
     OP_List -> x4
     (OP_Cons x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_best_dot_evalY_dot_427 x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_try (nd_OP_best_dot_constrain_dot_427 x1 x5 x4 x2000 x3500) x2001 x3500)))) x6 x4 x2003 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x4 x1002 x3000 x3500) (nd_OP__case_2 x1 x4 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x4 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x4 x5 x3000 x3500 = case x5 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_inject x2 (wrapNX id (nd_OP_best_dot_constrain_dot_427_dot___hash_lambda10 x1 x4)) x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x4 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x4 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x3 x3500 = case x3 of
     OP_List -> OP_Cons x2 OP_List
     (OP_Cons x4 x5) -> d_C_apply (d_C_concatMap d_C_solveAll2 x3500) (OP_Cons x2 (OP_Cons x4 x5)) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3500) (d_OP__case_4 x2 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x3 x3000 x3500 = case x3 of
     OP_List -> OP_Cons x2 OP_List
     (OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_apply (nd_C_concatMap (wrapNX id nd_C_solveAll2) x2000 x3500) (OP_Cons x2 (OP_Cons x4 x5)) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x2 x1002 x3000 x3500) (nd_OP__case_4 x2 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x2 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3 x3500 = case x3 of
     OP_List -> OP_Cons x2 OP_List
     (OP_Cons x4 x5) -> d_OP_solveAll_dot_evalall3_dot_399 (d_C_try x2 x3500) (OP_Cons x4 x5) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x1002 x3500) (d_OP__case_5 x2 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3 x3000 x3500 = case x3 of
     OP_List -> OP_Cons x2 OP_List
     (OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_solveAll_dot_evalall3_dot_399 (nd_C_try x2 x2000 x3500) (OP_Cons x4 x5) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x1002 x3000 x3500) (nd_OP__case_5 x2 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x2 x3 x4 x3500 = case x4 of
     OP_List -> OP_Cons x3 (d_OP_solveAll_dot_evalall2_dot_399 x2 x3500)
     (OP_Cons x5 x6) -> d_OP_solveAll_dot_evalall3_dot_399 (d_C_try x3 x3500) (OP_Cons x5 (d_OP_plus_plus x6 x2 x3500)) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x2 x3 x1002 x3500) (d_OP__case_6 x2 x3 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x2 x3 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x2 x3 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x2 x3 x4 x3000 x3500 = case x4 of
     OP_List -> let
          x2000 = x3000
           in (seq x2000 (OP_Cons x3 (nd_OP_solveAll_dot_evalall2_dot_399 x2 x2000 x3500)))
     (OP_Cons x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_solveAll_dot_evalall3_dot_399 (nd_C_try x3 x2000 x3500) (OP_Cons x5 (d_OP_plus_plus x6 x2 x3500)) x2001 x3500)))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x2 x3 x1002 x3000 x3500) (nd_OP__case_6 x2 x3 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x2 x3 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3500 = case x2 of
     C_True -> d_C_return OP_List x3500
     C_False -> d_OP_gt_gt_eq (d_C_getLine x3500) (d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 x1) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x1002 x3500) (d_OP__case_7 x1 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3000 x3500 = case x2 of
     C_True -> d_C_return OP_List x3500
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_gt_gt_eq (d_C_getLine x3500) (wrapDX id (d_OP_getLine_dot___hash_lambda5_dot___hash_lambda6 x1)) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x1002 x3000 x3500) (nd_OP__case_7 x1 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x3500 = case x2 of
     C_True -> d_OP_dollar_hash d_C_prim_chr x1 x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3500) (d_OP__case_8 x1 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x3000 x3500 = case x2 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_dollar_hash (wrapDX id d_C_prim_chr) x1 x2000 x3500))
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x1002 x3000 x3500) (nd_OP__case_8 x1 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x3 x4 x5 x3500 = case x5 of
     C_True -> d_OP_lt_eq x4 x1 x3500
     C_False -> d_OP__case_9 x1 x4 (d_C_otherwise x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x3 x4 x1002 x3500) (d_OP__case_10 x1 x2 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> d_OP_lt_eq x4 x1 x3500
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x4 (d_C_otherwise x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x4 x5 x3500 = case x5 of
     C_True -> d_OP_gt_eq x4 x1 x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x4 x1002 x3500) (d_OP__case_9 x1 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x4 x5 x3000 x3500 = case x5 of
     C_True -> d_OP_gt_eq x4 x1 x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x4 x1002 x3000 x3500) (nd_OP__case_9 x1 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x2 x3 x3500 = case x3 of
     C_True -> OP_List
     C_False -> OP_Cons x1 (d_C_enumFromTo (d_OP_plus x1 (C_Int 1#) x3500) x2 x3500)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x2 x1002 x3500) (d_OP__case_11 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> OP_List
     C_False -> OP_Cons x1 (d_C_enumFromTo (d_OP_plus x1 (C_Int 1#) x3500) x2 x3500)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x2 x1002 x3000 x3500) (nd_OP__case_11 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x4 x3 x3500 = case x3 of
     (OP_Tuple2 x5 x6) -> d_OP__case_13 x1 x4 x5 x6 (d_OP_eq_eq x1 x5 x3500) x3500
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x4 x1002 x3500) (d_OP__case_14 x1 x4 x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x4 z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x4 x3 x3000 x3500 = case x3 of
     (OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x1 x4 x5 x6 (d_OP_eq_eq x1 x5 x3500) x2000 x3500))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x4 x1002 x3000 x3500) (nd_OP__case_14 x1 x4 x1003 x3000 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x4 z x3000 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x4 x5 x6 x7 x3500 = case x7 of
     C_True -> C_Just x6
     C_False -> d_OP__case_12 x1 x4 (d_C_otherwise x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x4 x5 x6 x1002 x3500) (d_OP__case_13 x1 x4 x5 x6 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x4 x5 x6 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x4 x5 x6 x7 x3000 x3500 = case x7 of
     C_True -> C_Just x6
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x4 (d_C_otherwise x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_13 x1 x4 x5 x6 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x4 x5 x6 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x4 x5 x3500 = case x5 of
     C_True -> d_C_lookup x1 x4 x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x4 x1002 x3500) (d_OP__case_12 x1 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x4 x5 x3000 x3500 = case x5 of
     C_True -> d_C_lookup x1 x4 x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x4 x1002 x3000 x3500) (nd_OP__case_12 x1 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x2 x3500 = case x2 of
     C_True -> OP_List
     C_False -> d_C_foldr1 (acceptCs id d_OP_unwords_dot___hash_lambda3) x1 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x1002 x3500) (d_OP__case_15 x1 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x2 x3000 x3500 = case x2 of
     C_True -> OP_List
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_foldr1 (wrapDX (wrapDX id) (acceptCs id d_OP_unwords_dot___hash_lambda3)) x1 x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x1002 x3000 x3500) (nd_OP__case_15 x1 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x2 x6 x3500 = case x6 of
     C_True -> OP_List
     C_False -> let
          x3 = d_C_apply (d_C_break d_OP_words_dot_isSpace_dot_273 x3500) x2 x3500
          x4 = d_OP_words_dot___hash_selFP21_hash_w x3 x3500
          x5 = d_OP_words_dot___hash_selFP22_hash_s2 x3 x3500
           in (OP_Cons x4 (d_C_words x5 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x2 x1002 x3500) (d_OP__case_16 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x2 x6 x3000 x3500 = case x6 of
     C_True -> OP_List
     C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x3 = let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_break (wrapDX id d_OP_words_dot_isSpace_dot_273) x2000 x3500) x2 x2001 x3500)))
               x4 = d_OP_words_dot___hash_selFP21_hash_w x3 x3500
               x5 = d_OP_words_dot___hash_selFP22_hash_s2 x3 x3500
                in (OP_Cons x4 (d_C_words x5 x3500))))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x2 x1002 x3000 x3500) (nd_OP__case_16 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x2 x3 x7 x3500 = case x7 of
     C_True -> OP_Tuple2 OP_List x3
     C_False -> let
          x4 = d_OP_lines_dot_splitline_dot_261 x3 x3500
          x5 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x4 x3500
          x6 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x4 x3500
           in (OP_Tuple2 (OP_Cons x2 x5) x6)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x2 x3 x1002 x3500) (d_OP__case_17 x2 x3 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x2 x3 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x2 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x2 x3 x7 x3000 x3500 = case x7 of
     C_True -> OP_Tuple2 OP_List x3
     C_False -> let
          x4 = d_OP_lines_dot_splitline_dot_261 x3 x3500
          x5 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP15_hash_ds x4 x3500
          x6 = d_OP_lines_dot_splitline_dot_261_dot___hash_selFP16_hash_es x4 x3500
           in (OP_Tuple2 (OP_Cons x2 x5) x6)
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x2 x3 x1002 x3000 x3500) (nd_OP__case_17 x2 x3 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x2 x3 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x3 x4 x8 x3500 = case x8 of
     C_True -> let
          x5 = d_C_span x1 x4 x3500
          x6 = d_OP_span_dot___hash_selFP12_hash_ys x5 x3500
          x7 = d_OP_span_dot___hash_selFP13_hash_zs x5 x3500
           in (OP_Tuple2 (OP_Cons x3 x6) x7)
     C_False -> d_OP__case_18 x3 x4 (d_C_otherwise x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x3 x4 x1002 x3500) (d_OP__case_19 x1 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x3 x4 x8 x3000 x3500 = case x8 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x5 = nd_C_span x1 x4 x2000 x3500
               x6 = d_OP_span_dot___hash_selFP12_hash_ys x5 x3500
               x7 = d_OP_span_dot___hash_selFP13_hash_zs x5 x3500
                in (OP_Tuple2 (OP_Cons x3 x6) x7)))
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x3 x4 (d_C_otherwise x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_19 x1 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x3 x4 x5 x3500 = case x5 of
     C_True -> OP_Tuple2 OP_List (OP_Cons x3 x4)
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x3 x4 x1002 x3500) (d_OP__case_18 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> OP_Tuple2 OP_List (OP_Cons x3 x4)
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x3 x4 x1002 x3000 x3500) (nd_OP__case_18 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x3 x4 x5 x3500 = case x5 of
     C_True -> d_C_dropWhile x1 x4 x3500
     C_False -> OP_Cons x3 x4
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x3 x4 x1002 x3500) (d_OP__case_20 x1 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_dropWhile x1 x4 x2000 x3500))
     C_False -> OP_Cons x3 x4
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_20 x1 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x3 x4 x5 x3500 = case x5 of
     C_True -> OP_Cons x3 (d_C_takeWhile x1 x4 x3500)
     C_False -> OP_List
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x3 x4 x1002 x3500) (d_OP__case_21 x1 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (OP_Cons x3 (nd_C_takeWhile x1 x4 x2000 x3500)))
     C_False -> OP_List
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_21 x1 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x3500 = case x3 of
     C_True -> OP_Tuple2 OP_List x2
     C_False -> d_OP_splitAt_dot_splitAtp_dot_229 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x1002 x3500) (d_OP__case_22 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> OP_Tuple2 OP_List x2
     C_False -> d_OP_splitAt_dot_splitAtp_dot_229 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x2 x3 x3500 = case x3 of
     C_True -> x2
     C_False -> d_OP_drop_dot_dropp_dot_219 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x2 x1002 x3500) (d_OP__case_23 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> x2
     C_False -> d_OP_drop_dot_dropp_dot_219 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x2 x1002 x3000 x3500) (nd_OP__case_23 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x2 x3 x3500 = case x3 of
     C_True -> OP_List
     C_False -> d_OP_take_dot_takep_dot_210 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x2 x1002 x3500) (d_OP__case_24 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> OP_List
     C_False -> d_OP_take_dot_takep_dot_210 x1 x2 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x2 x1002 x3000 x3500) (nd_OP__case_24 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x3 x2 x3500 = case x2 of
     (OP_Tuple3 x4 x5 x6) -> let
          x7 = d_C_unzip3 x3 x3500
          x8 = d_OP_unzip3_dot___hash_selFP5_hash_xs x7 x3500
          x9 = d_OP_unzip3_dot___hash_selFP6_hash_ys x7 x3500
          x10 = d_OP_unzip3_dot___hash_selFP7_hash_zs x7 x3500
           in (OP_Tuple3 (OP_Cons x4 x8) (OP_Cons x5 x9) (OP_Cons x6 x10))
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x3 x1002 x3500) (d_OP__case_25 x3 x1003 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x3 z x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x3 x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x3 x2 x3000 x3500 = case x2 of
     (OP_Tuple3 x4 x5 x6) -> let
          x7 = d_C_unzip3 x3 x3500
          x8 = d_OP_unzip3_dot___hash_selFP5_hash_xs x7 x3500
          x9 = d_OP_unzip3_dot___hash_selFP6_hash_ys x7 x3500
          x10 = d_OP_unzip3_dot___hash_selFP7_hash_zs x7 x3500
           in (OP_Tuple3 (OP_Cons x4 x8) (OP_Cons x5 x9) (OP_Cons x6 x10))
     (Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x3 x1002 x3000 x3500) (nd_OP__case_25 x3 x1003 x3000 x3500)
     (Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x3 z x3000 x3500) x1002
     (Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x3 x2 x3500 = case x2 of
     (OP_Tuple2 x4 x5) -> let
          x6 = d_C_unzip x3 x3500
          x7 = d_OP_unzip_dot___hash_selFP2_hash_xs x6 x3500
          x8 = d_OP_unzip_dot___hash_selFP3_hash_ys x6 x3500
           in (OP_Tuple2 (OP_Cons x4 x7) (OP_Cons x5 x8))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x3 x1002 x3500) (d_OP__case_26 x3 x1003 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x3 z x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x3 x1002) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x3 x2 x3000 x3500 = case x2 of
     (OP_Tuple2 x4 x5) -> let
          x6 = d_C_unzip x3 x3500
          x7 = d_OP_unzip_dot___hash_selFP2_hash_xs x6 x3500
          x8 = d_OP_unzip_dot___hash_selFP3_hash_ys x6 x3500
           in (OP_Tuple2 (OP_Cons x4 x7) (OP_Cons x5 x8))
     (Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x3 x1002 x3000 x3500) (nd_OP__case_26 x3 x1003 x3000 x3500)
     (Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x3 z x3000 x3500) x1002
     (Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x1 x4 x5 x6 x3 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x7 x8) -> d_OP__case_27 x1 x5 x6 x7 x8 x4 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x4 x5 x6 x1002 x3500) (d_OP__case_28 x1 x4 x5 x6 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x4 x5 x6 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x4 x5 x6 x3 x3000 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x5 x6 x7 x8 x4 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_28 x1 x4 x5 x6 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x4 x5 x6 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x5 x6 x7 x8 x4 x3500 = case x4 of
     OP_List -> OP_List
     (OP_Cons x9 x10) -> OP_Cons (d_C_apply (d_C_apply (d_C_apply x1 x5 x3500) x7 x3500) x9 x3500) (d_C_zipWith3 x1 x6 x8 x10 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x5 x6 x7 x8 x1002 x3500) (d_OP__case_27 x1 x5 x6 x7 x8 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x5 x6 x7 x8 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x5 x6 x7 x8 x4 x3000 x3500 = case x4 of
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
                          in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x5 x2000 x3500) x7 x2001 x3500)))) x9 x2003 x3500)))) (nd_C_zipWith3 x1 x6 x8 x10 x2005 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_27 x1 x5 x6 x7 x8 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x5 x6 x7 x8 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x1 x4 x5 x3 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> OP_Cons (d_C_apply (d_C_apply x1 x4 x3500) x6 x3500) (d_C_zipWith x1 x5 x7 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x4 x5 x1002 x3500) (d_OP__case_29 x1 x4 x5 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x4 x5 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x4 x5 x3 x3000 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (OP_Cons (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_apply (nd_C_apply x1 x4 x2000 x3500) x6 x2001 x3500)))) (nd_C_zipWith x1 x5 x7 x2003 x3500))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_29 x1 x4 x5 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x4 x5 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x3 x4 x5 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> d_OP__case_30 x4 x5 x6 x7 x3 x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x3 x4 x5 x1002 x3500) (d_OP__case_31 x3 x4 x5 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x3 x4 x5 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x3 x4 x5 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x4 x5 x6 x7 x3 x2000 x3500))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_31 x3 x4 x5 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x3 x4 x5 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x4 x5 x6 x7 x3 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x8 x9) -> OP_Cons (OP_Tuple3 x4 x6 x8) (d_C_zip3 x5 x7 x9 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x4 x5 x6 x7 x1002 x3500) (d_OP__case_30 x4 x5 x6 x7 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x4 x5 x6 x7 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x4 x5 x6 x7 x3 x3000 x3500 = case x3 of
     OP_List -> OP_List
     (OP_Cons x8 x9) -> OP_Cons (OP_Tuple3 x4 x6 x8) (d_C_zip3 x5 x7 x9 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_30 x4 x5 x6 x7 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x4 x5 x6 x7 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x3 x4 x2 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> OP_Cons (OP_Tuple2 x3 x5) (d_C_zip x4 x6 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x3 x4 x1002 x3500) (d_OP__case_32 x3 x4 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x3 x4 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x3 x4 x2 x3000 x3500 = case x2 of
     OP_List -> OP_List
     (OP_Cons x5 x6) -> OP_Cons (OP_Tuple2 x3 x5) (d_C_zip x4 x6 x3500)
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x3 x4 x1002 x3000 x3500) (nd_OP__case_32 x3 x4 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x3 x4 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x1 x3 x4 x5 x3500 = case x5 of
     C_True -> OP_Cons x3 (d_C_filter x1 x4 x3500)
     C_False -> d_C_filter x1 x4 x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1 x3 x4 x1002 x3500) (d_OP__case_33 x1 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x1 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> let
          x2000 = x3000
           in (seq x2000 (OP_Cons x3 (nd_C_filter x1 x4 x2000 x3500)))
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_filter x1 x4 x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_33 x1 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x1 x3 x4 x3500 = case x4 of
     OP_List -> x3
     (OP_Cons x5 x6) -> d_C_apply (d_C_apply x1 x3 x3500) (d_C_foldr1 x1 (OP_Cons x5 x6) x3500) x3500
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1 x3 x1002 x3500) (d_OP__case_34 x1 x3 x1003 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x1 x3 z x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1 x3 x1002) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x1 x3 x4 x3000 x3500 = case x4 of
     OP_List -> x3
     (OP_Cons x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_C_apply (nd_C_apply x1 x3 x2000 x3500) (nd_C_foldr1 x1 (OP_Cons x5 x6) x2001 x3500) x2002 x3500))))))))
     (Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1 x3 x1002 x3000 x3500) (nd_OP__case_34 x1 x3 x1003 x3000 x3500)
     (Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x1 x3 z x3000 x3500) x1002
     (Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x2 x3 x4 x5 x3500 = case x5 of
     C_True -> x3
     C_False -> d_OP__case_35 x2 x4 (d_OP_gt x2 (C_Int 0#) x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x2 x3 x4 x1002 x3500) (d_OP__case_36 x2 x3 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x2 x3 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x2 x3 x4 x5 x3000 x3500 = case x5 of
     C_True -> x3
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x2 x4 (d_OP_gt x2 (C_Int 0#) x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_36 x2 x3 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x2 x3 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x2 x4 x5 x3500 = case x5 of
     C_True -> d_OP_bang_bang x4 (d_OP_minus x2 (C_Int 1#) x3500) x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x2 x4 x1002 x3500) (d_OP__case_35 x2 x4 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x2 x4 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x2 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x2 x4 x5 x3000 x3500 = case x5 of
     C_True -> d_OP_bang_bang x4 (d_OP_minus x2 (C_Int 1#) x3500) x3500
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x2 x4 x1002 x3000 x3500) (nd_OP__case_35 x2 x4 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x2 x4 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x2 x3 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x2 x1002 x3500) (d_OP__case_37 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x2 x1002 x3000 x3500) (nd_OP__case_37 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x2 x3 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x2 x1002 x3500) (d_OP__case_38 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> x1
     C_False -> x2
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x2 x1002 x3000 x3500) (nd_OP__case_38 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x1 x2 x3 x3500 = case x3 of
     C_True -> C_EQ
     C_False -> d_OP__case_40 x1 x2 (d_OP_lt_eq x1 x2 x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x1 x2 x1002 x3500) (d_OP__case_41 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> C_EQ
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_40 x1 x2 (d_OP_lt_eq x1 x2 x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x2 x1002 x3000 x3500) (nd_OP__case_41 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x1 x2 x3 x3500 = case x3 of
     C_True -> C_LT
     C_False -> d_OP__case_39 (d_C_otherwise x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1 x2 x1002 x3500) (d_OP__case_40 x1 x2 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x1 x2 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x2 x3 x3000 x3500 = case x3 of
     C_True -> C_LT
     C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 (d_C_otherwise x3500) x2000 x3500))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x2 x1002 x3000 x3500) (nd_OP__case_40 x1 x2 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x2 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x1 x3500 = case x1 of
     C_True -> C_GT
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1002 x3500) (d_OP__case_39 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x3000 x3500 = case x1 of
     C_True -> C_GT
     C_False -> d_C_failed x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1002 x3000 x3500) (nd_OP__case_39 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x1 x2 x3 x4 x3500 = case x4 of
     C_True -> x3
     C_False -> d_C_until x1 x2 (d_C_apply x2 x3 x3500) x3500
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x2 x3 x1002 x3500) (d_OP__case_42 x1 x2 x3 x1003 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 x2 x3 z x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x2 x3 x4 x3000 x3500 = case x4 of
     C_True -> x3
     C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_until x1 x2 (nd_C_apply x2 x3 x2000 x3500) x2001 x3500)))))
     (Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_42 x1 x2 x3 x1003 x3000 x3500)
     (Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x2 x3 z x3000 x3500) x1002
     (Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_ensureNotFree :: Curry t0 => t0 -> ConstStore -> t0
d_C_ensureNotFree x1 x3500 = external_d_C_ensureNotFree x1 x3500

d_OP_dollar_bang :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_OP_dollar_bang x1 x2 x3500 = external_d_OP_dollar_bang x1 x2 x3500

nd_OP_dollar_bang :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_OP_dollar_bang x1 x2 x3000 x3500 = external_nd_OP_dollar_bang x1 x2 x3000 x3500

d_OP_dollar_bang_bang :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_OP_dollar_bang_bang x1 x2 x3500 = external_d_OP_dollar_bang_bang x1 x2 x3500

nd_OP_dollar_bang_bang :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_OP_dollar_bang_bang x1 x2 x3000 x3500 = external_nd_OP_dollar_bang_bang x1 x2 x3000 x3500

d_OP_dollar_hash_hash :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_OP_dollar_hash_hash x1 x2 x3500 = external_d_OP_dollar_hash_hash x1 x2 x3500

nd_OP_dollar_hash_hash :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_OP_dollar_hash_hash x1 x2 x3000 x3500 = external_nd_OP_dollar_hash_hash x1 x2 x3000 x3500

d_C_prim_error :: Curry t0 => OP_List C_Char -> ConstStore -> t0
d_C_prim_error x1 x3500 = external_d_C_prim_error x1 x3500

d_C_failed :: Curry t0 => ConstStore -> t0
d_C_failed x3500 = external_d_C_failed x3500

d_OP_eq_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_eq_eq x1 x2 x3500 = external_d_OP_eq_eq x1 x2 x3500

d_OP_lt_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Bool
d_OP_lt_eq x1 x2 x3500 = external_d_OP_lt_eq x1 x2 x3500

d_C_prim_ord :: C_Char -> ConstStore -> C_Int
d_C_prim_ord x1 x3500 = external_d_C_prim_ord x1 x3500

d_C_prim_chr :: C_Int -> ConstStore -> C_Char
d_C_prim_chr x1 x3500 = external_d_C_prim_chr x1 x3500

d_OP_plus :: C_Int -> C_Int -> ConstStore -> C_Int
d_OP_plus x1 x2 x3500 = external_d_OP_plus x1 x2 x3500

d_OP_minus :: C_Int -> C_Int -> ConstStore -> C_Int
d_OP_minus x1 x2 x3500 = external_d_OP_minus x1 x2 x3500

d_OP_star :: C_Int -> C_Int -> ConstStore -> C_Int
d_OP_star x1 x2 x3500 = external_d_OP_star x1 x2 x3500

d_C_div :: C_Int -> C_Int -> ConstStore -> C_Int
d_C_div x1 x2 x3500 = external_d_C_div x1 x2 x3500

d_C_mod :: C_Int -> C_Int -> ConstStore -> C_Int
d_C_mod x1 x2 x3500 = external_d_C_mod x1 x2 x3500

d_C_divMod :: C_Int -> C_Int -> ConstStore -> OP_Tuple2 C_Int C_Int
d_C_divMod x1 x2 x3500 = external_d_C_divMod x1 x2 x3500

d_C_quot :: C_Int -> C_Int -> ConstStore -> C_Int
d_C_quot x1 x2 x3500 = external_d_C_quot x1 x2 x3500

d_C_rem :: C_Int -> C_Int -> ConstStore -> C_Int
d_C_rem x1 x2 x3500 = external_d_C_rem x1 x2 x3500

d_C_quotRem :: C_Int -> C_Int -> ConstStore -> OP_Tuple2 C_Int C_Int
d_C_quotRem x1 x2 x3500 = external_d_C_quotRem x1 x2 x3500

d_C_negateFloat :: C_Float -> ConstStore -> C_Float
d_C_negateFloat x1 x3500 = external_d_C_negateFloat x1 x3500

d_OP_eq_colon_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Success
d_OP_eq_colon_eq x1 x2 x3500 = external_d_OP_eq_colon_eq x1 x2 x3500

d_C_success :: ConstStore -> C_Success
d_C_success x3500 = external_d_C_success x3500

d_OP_ampersand :: C_Success -> C_Success -> ConstStore -> C_Success
d_OP_ampersand x1 x2 x3500 = external_d_OP_ampersand x1 x2 x3500

d_OP_gt_gt_eq :: (Curry t0,Curry t1) => C_IO t0 -> (t0 -> ConstStore -> C_IO t1) -> ConstStore -> C_IO t1
d_OP_gt_gt_eq x1 x2 x3500 = external_d_OP_gt_gt_eq x1 x2 x3500

nd_OP_gt_gt_eq :: (Curry t0,Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> ConstStore -> C_IO t1
nd_OP_gt_gt_eq x1 x2 x3000 x3500 = external_nd_OP_gt_gt_eq x1 x2 x3000 x3500

d_C_return :: Curry t0 => t0 -> ConstStore -> C_IO t0
d_C_return x1 x3500 = external_d_C_return x1 x3500

d_C_prim_putChar :: C_Char -> ConstStore -> C_IO OP_Unit
d_C_prim_putChar x1 x3500 = external_d_C_prim_putChar x1 x3500

d_C_getChar :: ConstStore -> C_IO C_Char
d_C_getChar x3500 = external_d_C_getChar x3500

d_C_prim_readFile :: OP_List C_Char -> ConstStore -> C_IO (OP_List C_Char)
d_C_prim_readFile x1 x3500 = external_d_C_prim_readFile x1 x3500

d_C_prim_writeFile :: OP_List C_Char -> OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_prim_writeFile x1 x2 x3500 = external_d_C_prim_writeFile x1 x2 x3500

d_C_prim_appendFile :: OP_List C_Char -> OP_List C_Char -> ConstStore -> C_IO OP_Unit
d_C_prim_appendFile x1 x2 x3500 = external_d_C_prim_appendFile x1 x2 x3500

d_C_prim_ioError :: Curry t0 => C_IOError -> ConstStore -> C_IO t0
d_C_prim_ioError x1 x3500 = external_d_C_prim_ioError x1 x3500

d_C_catch :: Curry t0 => C_IO t0 -> (C_IOError -> ConstStore -> C_IO t0) -> ConstStore -> C_IO t0
d_C_catch x1 x2 x3500 = external_d_C_catch x1 x2 x3500

nd_C_catch :: Curry t0 => C_IO t0 -> Func C_IOError (C_IO t0) -> IDSupply -> ConstStore -> C_IO t0
nd_C_catch x1 x2 x3000 x3500 = external_nd_C_catch x1 x2 x3000 x3500

d_C_prim_show :: Curry t0 => t0 -> ConstStore -> OP_List C_Char
d_C_prim_show x1 x3500 = external_d_C_prim_show x1 x3500

nd_OP_qmark :: Curry t0 => t0 -> t0 -> IDSupply -> ConstStore -> t0
nd_OP_qmark x1 x2 x3000 x3500 = external_nd_OP_qmark x1 x2 x3000 x3500

d_C_try :: Curry t0 => (t0 -> ConstStore -> C_Success) -> ConstStore -> OP_List (t0 -> ConstStore -> C_Success)
d_C_try x1 x3500 = external_d_C_try x1 x3500

nd_C_try :: Curry t0 => Func t0 C_Success -> IDSupply -> ConstStore -> OP_List (Func t0 C_Success)
nd_C_try x1 x3000 x3500 = external_nd_C_try x1 x3000 x3500

d_C_apply :: (Curry t0,Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> t1
d_C_apply x1 x2 x3500 = external_d_C_apply x1 x2 x3500

nd_C_apply :: (Curry t0,Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> t1
nd_C_apply x1 x2 x3000 x3500 = external_nd_C_apply x1 x2 x3000 x3500

d_C_cond :: Curry t0 => C_Success -> t0 -> ConstStore -> t0
d_C_cond x1 x2 x3500 = external_d_C_cond x1 x2 x3500

d_OP_eq_colon_lt_eq :: Curry t0 => t0 -> t0 -> ConstStore -> C_Success
d_OP_eq_colon_lt_eq x1 x2 x3500 = external_d_OP_eq_colon_lt_eq x1 x2 x3500
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a, Coverable a)
      => Curry a where
  -- implementation of strict equalit (==) for a data type
  (=?=) :: a -> a -> ConstStore -> C_Bool
  (=?=) = error "(==) is undefined"
  -- implementation of less-or-equal (<=) for a data type
  (<?=) :: a -> a -> ConstStore -> C_Bool
  (<?=) = error "(<=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(==) is undefined for primitive data"
  (<?=) = error "(<=) is undefined for primitive data"


-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry_Prelude.Curry C_Success where
  (=?=) (Choice_C_Success cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Success cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Success cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Success cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Success cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Success cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Success cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Success cd info) _ = failCons cd info
  (=?=) C_Success C_Success cs = Curry_Prelude.C_True
  (<?=) (Choice_C_Success cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Success cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Success cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Success cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Success cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Success cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Success cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Success cd info) _ = failCons cd info
  (<?=) C_Success C_Success cs = Curry_Prelude.C_True
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
  showsPrec d (C_CurryInt x1) = case (const $## x1) emptyCs of
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
  generate s = Choices_C_Int defCover (freeID [1] s) [C_CurryInt (generate (leftSupply s))]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cs = cont x cs
  ($!!) cont (C_CurryInt x1) cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cs
  ($!!) cont (Choice_C_Int cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Int cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Int cd c x) cs = guardCons cd c ((cont $!! x) (addCs c cs))
  ($!!) _ (Fail_C_Int cd info) _ = failCons cd info
  ($##) cont x@(C_Int _) cs = cont x cs
  ($##) cont (C_CurryInt x1) cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cs
  ($##) cont (Choice_C_Int cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Int cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Int cd c x) cs = guardCons cd c ((cont $## x) (addCs c cs))
  ($##) _ (Fail_C_Int cd info) _ = failCons cd info
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) _ = if (x1 ==# y1) then C_Success else Fail_C_Success defCover defFailInfo
  (=.=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =:= y1) cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =:= (primint2curryint y1)) cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Int      x1) (C_Int      y1) _ = if (x1 ==# y1) then C_Success else Fail_C_Success defCover defFailInfo
  (=.<=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =:<= y1) cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =:<= (primint2curryint y1)) cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _= Fail_C_Success defCover defFailInfo
  bind i (C_Int      x2) = (i :=: ChooseN 0 1) : bind (leftID i) (primint2curryint x2)
  bind i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind (leftID i) x2
  bind i (Choice_C_Int cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Int cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Int cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ c@(Choices_C_Int cd i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ (Fail_C_Int cd info) = [Unsolvable info]
  bind i (Guard_C_Int cd cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primint2curryint x2))]
  lazyBind i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x2)]
  lazyBind i (Choice_C_Int cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Int cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Int cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Int cd i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ (Fail_C_Int cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Int cd cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry_Prelude.Curry C_Int where
  (=?=) (Choice_C_Int cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Int cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Int cd c x) y cs = guardCons cd c ((x =?= y) (addCs c cs))
  (=?=) (Fail_C_Int cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Int cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Int cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Int cd c x) cs = guardCons cd c ((y =?= x) (addCs c cs))
  (=?=) _ (Fail_C_Int cd info) _ = failCons cd info
  (=?=) (C_Int      x1) (C_Int      y1) _ = toCurry (x1 ==# y1)
  (=?=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =?= y1) cs
  (=?=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =?= (primint2curryint y1)) cs
  (=?=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =?= y1) cs
  (<?=) (Choice_C_Int cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Int cd i xs) y cs = narrows cs cd i (\x -> (x<?= y) cs) xs
  (<?=) (Guard_C_Int cd c x) y cs = guardCons cd c ((x <?= y) (addCs c cs))
  (<?=) (Fail_C_Int cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Int cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Int cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Int cd c x) cs = guardCons cd c ((y <?= x) (addCs c cs))
  (<?=) _ (Fail_C_Int cd info) _ = failCons cd info
  (<?=) (C_Int      x1) (C_Int      y1) _ = toCurry (x1 <=# y1)
  (<?=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) `d_C_lteqInteger` y1) cs
  (<?=) (C_CurryInt x1) (C_Int      y1) cs = (x1 `d_C_lteqInteger` (primint2curryint y1)) cs
  (<?=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 `d_C_lteqInteger` y1) cs
-- END GENERATED FROM PrimTypes.curry

instance Coverable C_Int where
  cover x@(C_Int _)             = x
  cover (C_CurryInt x)          = C_CurryInt (cover x)
  cover (Choice_C_Int cd i x y) = Choice_C_Int (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Int cd i xs) = Choices_C_Int (incCover cd) i (map cover xs)
  cover (Fail_C_Int cd info)    = Fail_C_Int (incCover cd) info
  cover (Guard_C_Int cd cs x)   = Guard_C_Int (incCover cd) cs (cover x)

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
  generate _ = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) cs = cont x cs
  ($!!) cont (Choice_C_Float cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Float cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Float cd c x) cs = guardCons cd c ((cont $!! x) (addCs c cs))
  ($!!) _ (Fail_C_Float cd info) _ = failCons cd info
  ($##) cont x@(C_Float _) cs = cont x cs
  ($##) cont (Choice_C_Float cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Float cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Float cd c x) cs = guardCons cd c ((cont $## x) (addCs c cs))
  ($##) _ (Fail_C_Float cd info) _ = failCons cd info
  searchNF search cont x@(C_Float _) = cont x
  searchNF _ _ x = error ("Prelude.Float.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Float where
  (=.=) _ _ _  = Fail_C_Success defCover defFailInfo
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (Choice_C_Float cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Float cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Float cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ c@(Choices_C_Float cd i _) = error ("Prelude.Float.bind: Choices with ChoiceID: " ++ (show c))
  bind _ (Fail_C_Float cd info) = [Unsolvable info]
  bind i (Guard_C_Float cd cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (Choice_C_Float cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Float cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Float cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Float cd i _) = error ("Prelude.Float.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ (Fail_C_Float cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Float cd cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Float cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Float cd c x) y cs = guardCons cd c ((x =?= y) (addCs c cs))
  (=?=) (Fail_C_Float cd info) _ _= failCons cd info
  (=?=) z (Choice_C_Float cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Float cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Float cd c x) cs = guardCons cd c ((y =?= x) (addCs c cs))
  (=?=) _ (Fail_C_Float cd info) _ = failCons cd info
  (=?=) (C_Float x1) (C_Float y1) _ = toCurry (x1 `eqFloat#` y1)
  (<?=) (Choice_C_Float cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Float cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Float cd c x) y cs = guardCons cd c ((x <?= y) (addCs c cs))
  (<?=) (Fail_C_Float cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Float cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Float cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Float cd c x) cs = guardCons cd c ((y <?= x) (addCs c cs))
  (<?=) _ (Fail_C_Float cd info) _ = failCons cd info
  (<?=) (C_Float x1) (C_Float y1) _ = toCurry (x1 `leFloat#` y1)

instance Coverable C_Float where
  cover f@(C_Float _)          = f
  cover (Choice_C_Float cd i x y) = Choice_C_Float (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Float cd i xs) = Choices_C_Float (incCover cd) i (map cover xs)
  cover (Fail_C_Float cd info) = Fail_C_Float (incCover cd) info
  cover (Guard_C_Float cd cs x)   = Guard_C_Float (incCover cd) cs (cover x)

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
  showsPrec d (CurryChar x1) = case (const $## x1) emptyCs of
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
  generate s = Choices_C_Char defCover (freeID [1] s) [CurryChar (generate (leftSupply s))]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) cs = cont x cs
  ($!!) cont (CurryChar x) cs = ((cont . CurryChar) $!! x) cs
  ($!!) cont (Choice_C_Char cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Char cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Char cd c x) cs = guardCons cd c ((cont $!! x) (addCs c cs))
  ($!!) _ (Fail_C_Char cd info) _ = failCons cd info
  ($##) cont x@(C_Char _) cs = cont x cs
  ($##) cont (CurryChar x) cs = ((cont . CurryChar) $## x) cs
  ($##) cont (Choice_C_Char cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Char cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Char cd c x) cs = guardCons cd c ((cont $## x) (addCs c cs))
  ($##) _ (Fail_C_Char cd info) _ = failCons cd info
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x
  searchNF _ _ x = error ("Prelude.Char.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) _ | x1 `eqChar#` x2 = C_Success
                                             | otherwise = Fail_C_Success defCover defFailInfo
  (=.=) (C_Char       x1) (CurryChar x2)   cs = (primChar2CurryChar x1 =:= x2) cs
  (=.=) (CurryChar  x1) (C_Char      x2)   cs = (x1 =:= primChar2CurryChar x2) cs
  (=.=) (CurryChar x1)    (CurryChar   x2) cs = (x1 =:= x2) cs
  (=.=) _                 _                _  = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Char       x1) (C_Char      x2) _ | x1 `eqChar#` x2 = C_Success
                                              | otherwise = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Char       x1) (CurryChar x2)   cs = (primChar2CurryChar x1 =:<= x2) cs
  (=.<=) (CurryChar  x1) (C_Char      x2)   cs = (x1 =:<= primChar2CurryChar x2) cs
  (=.<=) (CurryChar x1)    (CurryChar   x2) cs = (x1 =:<= x2) cs
  (=.<=) _                 _                _  = Fail_C_Success defCover defFailInfo
  bind i (C_Char    x) = (i :=: ChooseN 0 1) : bind (leftID i) (primChar2CurryChar x)
  bind i (CurryChar x) = (i :=: ChooseN 0 1) : bind (leftID i) x
  bind i (Choice_C_Char cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Char cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Char cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ c@(Choices_C_Char cd i _) = error ("Prelude.Char.bind: Choices with ChoiceID: " ++ (show c))
  bind _ (Fail_C_Char cd info) = [Unsolvable info]
  bind i (Guard_C_Char cd cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primChar2CurryChar x))]
  lazyBind i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x)]
  lazyBind i (Choice_C_Char cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Char cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Char cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Char cd i _) = error ("Prelude.Char.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ (Fail_C_Char cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Char cd cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char cd i x y) z cs = narrow cd i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Char cd i xs) y cs = narrows cs cd i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Char cd c x) y cs = guardCons cd c ((x =?= y) (addCs c cs))
  (=?=) (Fail_C_Char cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Char cd i x y) cs = narrow cd i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Char cd i xs) cs = narrows cs cd i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Char cd c x) cs = guardCons cd c ((y =?= x) (addCs c cs))
  (=?=) _ (Fail_C_Char cd info) _ = failCons cd info
  (=?=) (C_Char x1) (C_Char y1) _ = toCurry (x1 `eqChar#` y1)
  (=?=) (C_Char      x1) (CurryChar y1) cs = ((primChar2CurryChar x1) =?= y1) cs
  (=?=) (CurryChar x1) (C_Char      y1) cs = (x1 =?= (primChar2CurryChar y1)) cs
  (=?=) (CurryChar x1) (CurryChar y1) cs = (x1 =?= y1) cs
  (<?=) (Choice_C_Char cd i x y) z cs = narrow cd i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Char cd i xs) y cs = narrows cs cd i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Char cd c x) y cs = guardCons cd c ((x <?= y) (addCs c cs))
  (<?=) (Fail_C_Char cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Char cd i x y) cs = narrow cd i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Char cd i xs) cs = narrows cs cd i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Char cd c x) cs = guardCons cd c ((y <?= x) (addCs c cs))
  (<?=) _ (Fail_C_Char cd info) _ = failCons cd info
  (<?=) (C_Char x1) (C_Char y1) _ = toCurry (x1 `leChar#` y1)
  (<?=) (C_Char      x1) (CurryChar y1) cs = ((primChar2CurryChar x1) `d_C_lteqInteger` y1) cs
  (<?=) (CurryChar x1) (C_Char      y1) cs = (x1 `d_C_lteqInteger` (primChar2CurryChar y1)) cs
  (<?=) (CurryChar x1) (CurryChar y1) cs = (x1 `d_C_lteqInteger` y1) cs

instance Coverable C_Char where
  cover c@(C_Char _)          = c
  cover (CurryChar x)         = CurryChar (cover x)
  cover (Choice_C_Char cd i x y) = Choice_C_Char (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Char cd i xs) = Choices_C_Char (incCover cd) i (map cover xs)
  cover (Fail_C_Char cd info) = Fail_C_Char (incCover cd) info
  cover (Guard_C_Char cd cs x)   = Guard_C_Char (incCover cd) cs (cover x)


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

external_d_C_ensureNotFree :: Curry a => a -> ConstStore -> a
external_d_C_ensureNotFree x cs =
  case try x of
    Choice cd i a b  -> choiceCons cd i (external_d_C_ensureNotFree a cs)
                                        (external_d_C_ensureNotFree b cs)
    Narrowed cd i xs -> choicesCons cd i (map (flip external_d_C_ensureNotFree cs) xs)
    Free cd i xs     -> narrows cs cd i (flip external_d_C_ensureNotFree cs) xs
    Guard cd c e    -> guardCons cd c (external_d_C_ensureNotFree e (addCs c cs))
    _            -> x

external_d_C_failed :: NonDet a => ConstStore -> a
external_d_C_failed _ = failCons 0 defFailInfo

external_d_OP_eq_eq :: Curry a => a -> a -> ConstStore -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> ConstStore -> C_Bool
external_d_OP_lt_eq = (<?=)

-- characters

external_d_C_prim_ord :: C_Char -> ConstStore -> C_Int
external_d_C_prim_ord (C_Char c)    _ = C_Int (ord# c)
external_d_C_prim_ord (CurryChar c) _ = C_CurryInt c

external_d_C_prim_chr :: C_Int -> ConstStore -> C_Char
external_d_C_prim_chr (C_Int i)      _ = C_Char (chr# i)
external_d_C_prim_chr (C_CurryInt i) _ = CurryChar i

-- int arithmetics

external_d_OP_plus :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) _  = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_plus_hash` y) cs)
external_d_OP_plus (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_OP_plus_hash` (primint2curryint y)) cs)
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_plus_hash` y) cs)
external_d_OP_plus x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_plus` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_OP_minus :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) _  = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_minus_hash` y) cs)
external_d_OP_minus (C_CurryInt x) (C_Int y)      cs = C_CurryInt ((x `d_OP_minus_hash` (primint2curryint y)) cs)
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_minus_hash` y) cs)
external_d_OP_minus x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_minus` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_OP_star :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) _  = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_star_hash` y) cs)
external_d_OP_star (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_OP_star_hash` (primint2curryint y)) cs)
external_d_OP_star (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_star_hash` y) cs)
external_d_OP_star x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_star` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_quot :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int defCover defFailInfo
  | otherwise = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_quotInteger` y) cs)
external_d_C_quot (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_quotInteger` (primint2curryint y)) cs)
external_d_C_quot (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_quotInteger` y) cs)
external_d_C_quot x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_quot` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_rem :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int defCover defFailInfo
  | otherwise = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_remInteger` y) cs)
external_d_C_rem (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_remInteger` (primint2curryint y)) cs)
external_d_C_rem (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_remInteger` y) cs)
external_d_C_rem x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_rem` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_quotRem :: C_Int -> C_Int -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_OP_Tuple2 defCover defFailInfo
  | otherwise = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) cs = (mkIntTuple `d_dollar_bang` (((primint2curryint x) `d_C_quotRemInteger` y) cs)) cs
external_d_C_quotRem (C_CurryInt x) (C_Int      y) cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` (primint2curryint y)) cs)) cs
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` y) cs)) cs
external_d_C_quotRem x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_quotRem` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_div :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_div (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int defCover defFailInfo
  | otherwise = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_divInteger` y) cs)
external_d_C_div (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_divInteger` (primint2curryint y)) cs)
external_d_C_div (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_divInteger` y) cs)
external_d_C_div x y cs = ((\a cs1-> ((\b cs2-> ((a `external_d_C_div` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

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

external_d_C_mod :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int defCover defFailInfo
  | otherwise = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_modInteger` y) cs)
external_d_C_mod (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_modInteger` (primint2curryint y)) cs)
external_d_C_mod (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_modInteger` y) cs)
external_d_C_mod x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_mod` b)) cs2) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

-- PrimOp taken from GHC.Base
modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | (x# ># 0#) && (y# <# 0#) ||
      (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
    | otherwise                   = r#
    where
    !r# = x# `remInt#` y#

-- TODO: $! instead of $#?
external_d_C_divMod :: C_Int -> C_Int ->  ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_OP_Tuple2 defCover defFailInfo
  | otherwise = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) cs = (mkIntTuple `d_OP_dollar_hash` (((primint2curryint x) `d_C_divModInteger` y) cs)) cs
external_d_C_divMod (C_CurryInt x) (C_Int      y) cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` (primint2curryint y)) cs)) cs
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` y) cs)) cs
external_d_C_divMod x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_divMod` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

mkIntTuple :: OP_Tuple2 BinInt BinInt -> ConstStore -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) _ = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> ConstStore -> C_Float
external_d_C_negateFloat (C_Float x) _ = C_Float (negateFloat# x)
external_d_C_negateFloat x cs          = (external_d_C_negateFloat `d_OP_dollar_hash` x) cs

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> ConstStore -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: ConstStore -> C_Success
external_d_C_success _ = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> ConstStore -> C_Success
external_d_OP_ampersand = (&)

-- IO stuff

external_d_C_return :: a -> ConstStore -> C_IO a
external_d_C_return a _ = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> ConstStore -> C_IO OP_Unit
external_d_C_prim_putChar c _ = toCurry putChar c

external_d_C_getChar :: ConstStore -> C_IO C_Char
external_d_C_getChar _ = toCurry getChar

external_d_C_prim_readFile :: C_String -> ConstStore -> C_IO C_String
external_d_C_prim_readFile s cs = toCurry readFile s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String -> ConstStore -> C_IO OP_Unit
external_d_C_prim_writeFile s1 s2 _ = toCurry writeFile s1 s2

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String -> ConstStore -> C_IO OP_Unit
external_d_C_prim_appendFile s1 s2 _ = toCurry appendFile s1 s2

external_d_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> ConstStore -> C_IO t1) -> ConstStore -> C_IO t1
external_d_OP_gt_gt_eq m f cs = fromIO $ do
  x <- toIO m cs
  cs1 <- lookupGlobalCs
  let cs2 = combineCs cs cs1
  toIO  (f x cs2) cs2

external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> ConstStore -> C_IO t1
external_nd_OP_gt_gt_eq m f s cs = fromIO $ do
 x <- toIO m cs
 cs1 <- lookupGlobalCs
 let cs2 = combineCs cs cs1
 toIO (nd_apply f x s cs2) cs2

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

external_d_C_prim_error :: C_String -> ConstStore -> a
external_d_C_prim_error s _ = C.throw $ UserException (fromCurry s)

external_d_C_prim_ioError :: C_IOError -> ConstStore -> C_IO a
external_d_C_prim_ioError e _ = C.throw $ (fromCurry e :: CurryException)

external_d_C_catch :: C_IO a -> (C_IOError -> ConstStore -> C_IO a) -> ConstStore -> C_IO a
external_d_C_catch act hndl cs = fromIO $ C.catches (toIO act cs) handlers
  where handlers = exceptionHandlers cs (\e -> hndl e cs)

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> ConstStore -> C_IO a
external_nd_C_catch act hndl s cs = fromIO $ C.catches (toIO act cs) handlers
  where handlers = exceptionHandlers cs (\e -> nd_apply hndl e s cs)

exceptionHandlers :: ConstStore -> (C_IOError -> C_IO a) -> [C.Handler a]
exceptionHandlers cs hndl =
  [ C.Handler (\ (e :: CurryException) -> toIO (hndl $ toCurry         e) cs)
  , C.Handler (\ (e ::  C.IOException) -> toIO (hndl $ fromIOException e) cs)
  ] where fromIOException = toCurry . IOException . show

-- other stuff

external_d_C_prim_show :: Show a => a -> ConstStore -> C_String
external_d_C_prim_show a _ = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> ConstStore -> a
external_d_C_cond succ a cs = ((\_ _ -> a) `d_OP_dollar_hash` succ) cs

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> ConstStore -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> ConstStore -> a
external_nd_OP_qmark x y ids _ = let i = thisID ids in i `seq` choiceCons defCover i x y

-- External HO
-- -----------

external_d_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_bang_bang f x s cs = ((\y cs1-> nd_apply f y s cs1) $!! x) cs

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_hash_hash f x s cs = ((\y cs1 -> nd_apply f y s cs1) $## x) cs

external_d_C_apply :: (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_C_apply = nd_apply



-- Encapsulated search
-- -------------------

-- external_d_C_try :: (a -> Success) -> [a -> Success]
external_d_C_try = error "external_dho_C_try"

-- external_nd_C_try :: Func a Success -> [Func a Success]
external_nd_C_try = error "external_ndho_C_try"

-- Functions on Integer and Nat added from PrimTypes
-- -------------------------------------------------

instance Curry_Prelude.Curry Nat where
  (=?=) (Choice_Nat cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_Nat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_Nat cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_Nat cd info) _ _ = failCons cd info
  (=?=) z (Choice_Nat cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_Nat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_Nat cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_Nat cd info) _ = failCons cd info
  (=?=) IHi IHi cs = Curry_Prelude.C_True
  (=?=) (O x1) (O y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (I x1) (I y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_Nat cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_Nat cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_Nat cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_Nat cd info) _ _ = failCons cd info
  (<?=) z (Choice_Nat cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_Nat cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_Nat cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_Nat cd info) _ = failCons cd info
  (<?=) IHi IHi cs = Curry_Prelude.C_True
  (<?=) IHi (O _) _ = Curry_Prelude.C_True
  (<?=) IHi (I _) _ = Curry_Prelude.C_True
  (<?=) (O x1) (O y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (O _) (I _) _ = Curry_Prelude.C_True
  (<?=) (I x1) (I y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Curry_Prelude.Curry BinInt where
  (=?=) (Choice_BinInt cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_BinInt cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_BinInt cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_BinInt cd info) _ _ = failCons cd info
  (=?=) z (Choice_BinInt cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_BinInt cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_BinInt cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_BinInt cd info) _ = failCons cd info
  (=?=) (Neg x1) (Neg y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) Zero Zero cs = Curry_Prelude.C_True
  (=?=) (Pos x1) (Pos y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_BinInt cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_BinInt cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_BinInt cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_BinInt cd info) _ _ = failCons cd info
  (<?=) z (Choice_BinInt cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_BinInt cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_BinInt cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_BinInt cd info) _ = failCons cd info
  (<?=) (Neg x1) (Neg y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (Neg _) Zero _ = Curry_Prelude.C_True
  (<?=) (Neg _) (Pos _) _ = Curry_Prelude.C_True
  (<?=) Zero Zero cs = Curry_Prelude.C_True
  (<?=) Zero (Pos _) _ = Curry_Prelude.C_True
  (<?=) (Pos x1) (Pos y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False




d_C_cmpNat :: Nat -> Nat -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpNat x1 x2 x3500 = case x1 of
     IHi-> d_OP__casePT_33 x2 x3500
     (O x5) -> d_OP__casePT_32 x5 x2 x3500
     (I x8) -> d_OP__casePT_30 x8 x2 x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpNat x1002 x2 x3500) (d_C_cmpNat x1003 x2 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpNat z x2 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpNat x1002 x2) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_succ :: Nat -> ConstStore -> Nat
d_C_succ x1 x3500 = case x1 of
     IHi-> O IHi
     (O x2) -> I x2
     (I x3) -> O (d_C_succ x3 x3500)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_succ x1002 x3500) (d_C_succ x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_succ z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_succ x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_pred :: Nat -> ConstStore -> Nat
d_C_pred x1 x3500 = case x1 of
     IHi-> Curry_Prelude.d_C_failed x3500
     (O x2) -> d_OP__casePT_28 x2 x3500
     (I x5) -> O x5
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_pred x1002 x3500) (d_C_pred x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_pred z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_pred x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_plus_caret :: Nat -> Nat -> ConstStore -> Nat
d_OP_plus_caret x1 x2 x3500 = case x1 of
     IHi-> d_C_succ x2 x3500
     (O x3) -> d_OP__casePT_27 x3 x2 x3500
     (I x6) -> d_OP__casePT_26 x6 x2 x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_caret x1002 x2 x3500) (d_OP_plus_caret x1003 x2 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_caret z x2 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_caret x1002 x2) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_minus_caret :: Nat -> Nat -> ConstStore -> BinInt
d_OP_minus_caret x1 x2 x3500 = case x1 of
     IHi-> d_C_inc (Neg x2) x3500
     (O x3) -> d_OP__casePT_25 x1 x3 x2 x3500
     (I x6) -> d_OP__casePT_24 x6 x2 x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_caret x1002 x2 x3500) (d_OP_minus_caret x1003 x2 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_caret z x2 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_caret x1002 x2) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mult2 :: BinInt -> ConstStore -> BinInt
d_C_mult2 x1 x3500 = case x1 of
     (Pos x2) -> Pos (O x2)
     Zero -> Zero
     (Neg x3) -> Neg (O x3)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mult2 x1002 x3500) (d_C_mult2 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mult2 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mult2 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_star_caret :: Nat -> Nat -> ConstStore -> Nat
d_OP_star_caret x1 x2 x3500 = case x1 of
     IHi-> x2
     (O x3) -> O (d_OP_star_caret x3 x2 x3500)
     (I x4) -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2 x3500)) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_caret x1002 x2 x3500) (d_OP_star_caret x1003 x2 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_caret z x2 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_caret x1002 x2) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_div2 :: Nat -> ConstStore -> Nat
d_C_div2 x1 x3500 = case x1 of
     IHi-> Curry_Prelude.d_C_failed x3500
     (O x2) -> x2
     (I x3) -> x3
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_div2 x1002 x3500) (d_C_div2 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_div2 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_div2 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mod2 :: Nat -> ConstStore -> BinInt
d_C_mod2 x1 x3500 = case x1 of
     IHi-> Pos IHi
     (O x2) -> Zero
     (I x3) -> Pos IHi
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mod2 x1002 x3500) (d_C_mod2 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mod2 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mod2 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_quotRemNat :: Nat -> Nat -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 x3500 = d_OP__casePT_23 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 IHi x3500) x3500

d_OP_quotRemNat_dot_shift_dot_104 :: Nat -> Nat -> ConstStore -> Nat
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 x3500 = case x1 of
     (O x3) -> O x2
     (I x4) -> I x2
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3500) (d_OP_quotRemNat_dot_shift_dot_104 x1003 x2 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemNat_dot_shift_dot_104 z x2 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemNat_dot_shift_dot_104 x1002 x2) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lteqInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.C_Bool
d_C_lteqInteger x1 x2 x3500 = Curry_Prelude.d_OP_slash_eq (d_C_cmpInteger x1 x2 x3500) Curry_Prelude.C_GT x3500

d_C_cmpInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpInteger x1 x2 x3500 = case x1 of
     Zero -> d_OP__casePT_14 x2 x3500
     (Pos x5) -> d_OP__casePT_13 x5 x2 x3500
     (Neg x8) -> d_OP__casePT_12 x8 x2 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpInteger x1002 x2 x3500) (d_C_cmpInteger x1003 x2 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpInteger z x2 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpInteger x1002 x2) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_neg :: BinInt -> ConstStore -> BinInt
d_C_neg x1 x3500 = case x1 of
     Zero -> Zero
     (Pos x2) -> Neg x2
     (Neg x3) -> Pos x3
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_neg x1002 x3500) (d_C_neg x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_neg z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_neg x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_inc :: BinInt -> ConstStore -> BinInt
d_C_inc x1 x3500 = case x1 of
     Zero -> Pos IHi
     (Pos x2) -> Pos (d_C_succ x2 x3500)
     (Neg x3) -> d_OP__casePT_11 x3 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_inc x1002 x3500) (d_C_inc x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_inc z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_inc x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dec :: BinInt -> ConstStore -> BinInt
d_C_dec x1 x3500 = case x1 of
     Zero -> Neg IHi
     (Pos x2) -> d_OP__casePT_10 x2 x3500
     (Neg x5) -> Neg (d_C_succ x5 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dec x1002 x3500) (d_C_dec x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dec z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dec x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_plus_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_plus_hash x1 x2 x3500 = case x1 of
     Zero -> x2
     (Pos x3) -> d_OP__casePT_9 x1 x3 x2 x3500
     (Neg x6) -> d_OP__casePT_8 x1 x6 x2 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_plus_hash x1002 x2 x3500) (d_OP_plus_hash x1003 x2 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_plus_hash z x2 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_plus_hash x1002 x2) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_minus_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_minus_hash x1 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x3) -> d_OP_plus_hash x1 (Neg x3) x3500
     (Neg x4) -> d_OP_plus_hash x1 (Pos x4) x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minus_hash x1 x1002 x3500) (d_OP_minus_hash x1 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minus_hash x1 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minus_hash x1 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_star_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_star_hash x1 x2 x3500 = case x1 of
     Zero -> Zero
     (Pos x3) -> d_OP__casePT_7 x3 x2 x3500
     (Neg x6) -> d_OP__casePT_6 x6 x2 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_hash x1002 x2 x3500) (d_OP_star_hash x1003 x2 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_hash z x2 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_hash x1002 x2) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_quotRemInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3500
     (Pos x3) -> d_OP__casePT_5 x3 x1 x3500
     (Neg x9) -> d_OP__casePT_4 x9 x1 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_quotRemInteger x1 x1002 x3500) (d_C_quotRemInteger x1 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_quotRemInteger x1 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_quotRemInteger x1 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP2_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP2_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP3_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP3_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP5_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP5_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP6_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP6_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP8_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP8_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_quotRemInteger_dot___hash_selFP9_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3500) (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quotRemInteger_dot___hash_selFP9_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_divModInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3500
     (Pos x3) -> d_OP__casePT_3 x3 x1 x3500
     (Neg x11) -> d_OP__casePT_1 x11 x1 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_divModInteger x1 x1002 x3500) (d_C_divModInteger x1 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_divModInteger x1 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_divModInteger x1 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP11_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP11_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3500) (d_OP_divModInteger_dot___hash_selFP11_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP11_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP11_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP12_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP12_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3500) (d_OP_divModInteger_dot___hash_selFP12_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP12_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP12_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP14_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP14_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3500) (d_OP_divModInteger_dot___hash_selFP14_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP14_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP14_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP15_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP15_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3500) (d_OP_divModInteger_dot___hash_selFP15_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP15_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP15_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP17_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP17_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3500) (d_OP_divModInteger_dot___hash_selFP17_hash_d x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP17_hash_d z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP17_hash_d x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_divModInteger_dot___hash_selFP18_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP18_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3500) (d_OP_divModInteger_dot___hash_selFP18_hash_m x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_divModInteger_dot___hash_selFP18_hash_m z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_divModInteger_dot___hash_selFP18_hash_m x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_divInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_divInteger x1 x2 x3500 = Curry_Prelude.d_C_fst (d_C_divModInteger x1 x2 x3500) x3500

d_C_modInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_modInteger x1 x2 x3500 = Curry_Prelude.d_C_snd (d_C_divModInteger x1 x2 x3500) x3500

d_C_quotInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_quotInteger x1 x2 x3500 = Curry_Prelude.d_C_fst (d_C_quotRemInteger x1 x2 x3500) x3500

d_C_remInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_remInteger x1 x2 x3500 = Curry_Prelude.d_C_snd (d_C_quotRemInteger x1 x2 x3500) x3500

d_OP__casePT_1 x11 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x13 = d_C_quotRemNat x12 x11 x3500
          x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3500
          x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3500
           in (d_OP__casePT_0 x11 x14 x15 x3500)
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_1 x11 x1002 x3500) (d_OP__casePT_1 x11 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_1 x11 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_1 x11 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_1 x11 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = d_C_quotRemNat x12 x11 x3500
               x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3500
               x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3500
                in (nd_OP__casePT_0 x11 x14 x15 x2000 x3500)))
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_1 x11 x1002 x3000 x3500) (nd_OP__casePT_1 x11 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_1 x11 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_1 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_0 x11 x14 x15 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_0 x11 x14 x1002 x3500) (d_OP__casePT_0 x11 x14 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_0 x11 x14 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_0 x11 x14 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_0 x11 x14 x15 x3000 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_0 x11 x14 x1002 x3000 x3500) (nd_OP__casePT_0 x11 x14 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_0 x11 x14 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_0 x11 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_3 x3 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3500
          x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3500
           in (d_OP__casePT_2 x3 x7 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_3 x3 x1002 x3500) (d_OP__casePT_3 x3 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_3 x3 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_3 x3 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_3 x3 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = d_C_quotRemNat x5 x3 x3500
               x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3500
               x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3500
                in (nd_OP__casePT_2 x3 x7 x8 x2000 x3500)))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_3 x3 x1002 x3000 x3500) (nd_OP__casePT_3 x3 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_3 x3 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_3 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_2 x3 x7 x8 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_2 x3 x7 x1002 x3500) (d_OP__casePT_2 x3 x7 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_2 x3 x7 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_2 x3 x7 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_2 x3 x7 x8 x3000 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_2 x3 x7 x1002 x3000 x3500) (nd_OP__casePT_2 x3 x7 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_2 x3 x7 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_2 x3 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_4 x9 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_4 x9 x1002 x3500) (d_OP__casePT_4 x9 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_4 x9 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_4 x9 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_4 x9 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_4 x9 x1002 x3000 x3500) (nd_OP__casePT_4 x9 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_4 x9 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_4 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_5 x3 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) (d_C_neg x8 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_5 x3 x1002 x3500) (d_OP__casePT_5 x3 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_5 x3 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_5 x3 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_5 x3 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) (d_C_neg x8 x3500))
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_5 x3 x1002 x3000 x3500) (nd_OP__casePT_5 x3 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_5 x3 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_5 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_6 x6 x2 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_6 x6 x1002 x3500) (d_OP__casePT_6 x6 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_6 x6 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_6 x6 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_6 x6 x2 x3000 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_6 x6 x1002 x3000 x3500) (nd_OP__casePT_6 x6 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_6 x6 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_6 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_7 x3 x2 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_7 x3 x1002 x3500) (d_OP__casePT_7 x3 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_7 x3 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_7 x3 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_7 x3 x2 x3000 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_7 x3 x1002 x3000 x3500) (nd_OP__casePT_7 x3 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_7 x3 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_7 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_8 x1 x6 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_8 x1 x6 x1002 x3500) (d_OP__casePT_8 x1 x6 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_8 x1 x6 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_8 x1 x6 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_8 x1 x6 x2 x3000 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_8 x1 x6 x1002 x3000 x3500) (nd_OP__casePT_8 x1 x6 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_8 x1 x6 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_8 x1 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_9 x1 x3 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_9 x1 x3 x1002 x3500) (d_OP__casePT_9 x1 x3 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_9 x1 x3 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_9 x1 x3 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_9 x1 x3 x2 x3000 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_9 x1 x3 x1002 x3000 x3500) (nd_OP__casePT_9 x1 x3 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_9 x1 x3 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_9 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_10 x2 x3500 = case x2 of
     IHi-> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_10 x1002 x3500) (d_OP__casePT_10 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_10 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_10 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_10 x2 x3000 x3500 = case x2 of
     IHi-> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_10 x1002 x3000 x3500) (nd_OP__casePT_10 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_10 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_10 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_11 x3 x3500 = case x3 of
     IHi-> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_11 x1002 x3500) (d_OP__casePT_11 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_11 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_11 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_11 x3 x3000 x3500 = case x3 of
     IHi-> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_11 x1002 x3000 x3500) (nd_OP__casePT_11 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_11 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_11 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_12 x8 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_12 x8 x1002 x3500) (d_OP__casePT_12 x8 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_12 x8 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_12 x8 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_12 x8 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_12 x8 x1002 x3000 x3500) (nd_OP__casePT_12 x8 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_12 x8 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_12 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_13 x5 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_13 x5 x1002 x3500) (d_OP__casePT_13 x5 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_13 x5 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_13 x5 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_13 x5 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_13 x5 x1002 x3000 x3500) (nd_OP__casePT_13 x5 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_13 x5 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_13 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_14 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_14 x1002 x3500) (d_OP__casePT_14 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_14 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_14 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_14 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_14 x1002 x3000 x3500) (nd_OP__casePT_14 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_14 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_14 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_23 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> d_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_23 x1 x2 x1002 x3500) (d_OP__casePT_23 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_23 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_23 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_23 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_23 x1 x2 x1002 x3000 x3500) (nd_OP__casePT_23 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_23 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_23 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_22 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> d_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_22 x1 x2 x1002 x3500) (d_OP__casePT_22 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_22 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_22 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_22 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_22 x1 x2 x1002 x3000 x3500) (nd_OP__casePT_22 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_22 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_22 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_21 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_21 x1 x2 x1002 x3500) (d_OP__casePT_21 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_21 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_21 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_21 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_21 x1 x2 x1002 x3000 x3500) (nd_OP__casePT_21 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_21 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_21 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_20 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> d_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_20 x1 x2 x1002 x3500) (d_OP__casePT_20 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_20 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_20 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_20 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3500) x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_20 x1 x2 x1002 x3000 x3500) (nd_OP__casePT_20 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_20 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_20 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_19 x1 x2 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__casePT_18 x1 x2 x4 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_19 x1 x2 x1002 x3500) (d_OP__casePT_19 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_19 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_19 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_19 x1 x2 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_18 x1 x2 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_19 x1 x2 x1002 x3000 x3500) (nd_OP__casePT_19 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_19 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_19 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_18 x1 x2 x4 x3 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3500)
     (Pos x5) -> d_OP__casePT_17 x1 x2 x5 x4 x3500
     (Neg x12) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_18 x1 x2 x4 x1002 x3500) (d_OP__casePT_18 x1 x2 x4 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_18 x1 x2 x4 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_18 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_18 x1 x2 x4 x3 x3000 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3500)
     (Pos x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_17 x1 x2 x5 x4 x2000 x3500))
     (Neg x12) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3500) (nd_OP__casePT_18 x1 x2 x4 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_18 x1 x2 x4 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_18 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_17 x1 x2 x5 x4 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3500)
     (Pos x6) -> d_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3500) x2 x3500) x3500
     (Neg x11) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_17 x1 x2 x5 x1002 x3500) (d_OP__casePT_17 x1 x2 x5 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_17 x1 x2 x5 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_17 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_17 x1 x2 x5 x4 x3000 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3500)
     (Pos x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3500) x2 x3500) x2000 x3500))
     (Neg x11) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3500) (nd_OP__casePT_17 x1 x2 x5 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_17 x1 x2 x5 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_17 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_16 x1 x2 x5 x6 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__casePT_15 x5 x8 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_16 x1 x2 x5 x6 x1002 x3500) (d_OP__casePT_16 x1 x2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_16 x1 x2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_16 x1 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_16 x1 x2 x5 x6 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_15 x5 x8 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3500) (nd_OP__casePT_16 x1 x2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_16 x1 x2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_15 x5 x8 x7 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_15 x5 x8 x1002 x3500) (d_OP__casePT_15 x5 x8 x1003 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_15 x5 x8 z x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_15 x5 x8 x1002) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_15 x5 x8 x7 x3000 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_15 x5 x8 x1002 x3000 x3500) (nd_OP__casePT_15 x5 x8 x1003 x3000 x3500)
     (Choices_BinInt x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_15 x5 x8 z x3000 x3500) x1002
     (Guard_BinInt x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_15 x5 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_BinInt x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_24 x6 x2 x3500 = case x2 of
     IHi-> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3500) x3500) x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3500) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_24 x6 x1002 x3500) (d_OP__casePT_24 x6 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_24 x6 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_24 x6 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_24 x6 x2 x3000 x3500 = case x2 of
     IHi-> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3500) x3500) x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3500) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_24 x6 x1002 x3000 x3500) (nd_OP__casePT_24 x6 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_24 x6 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_24 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_25 x1 x3 x2 x3500 = case x2 of
     IHi-> Pos (d_C_pred x1 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3500) x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3500) x3500) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_25 x1 x3 x1002 x3500) (d_OP__casePT_25 x1 x3 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_25 x1 x3 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_25 x1 x3 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_25 x1 x3 x2 x3000 x3500 = case x2 of
     IHi-> Pos (d_C_pred x1 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3500) x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3500) x3500) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_25 x1 x3 x1002 x3000 x3500) (nd_OP__casePT_25 x1 x3 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_25 x1 x3 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_25 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_26 x6 x2 x3500 = case x2 of
     IHi-> O (d_C_succ x6 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3500) x8 x3500)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_26 x6 x1002 x3500) (d_OP__casePT_26 x6 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_26 x6 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_26 x6 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_26 x6 x2 x3000 x3500 = case x2 of
     IHi-> O (d_C_succ x6 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3500) x8 x3500)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_26 x6 x1002 x3000 x3500) (nd_OP__casePT_26 x6 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_26 x6 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_26 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_27 x3 x2 x3500 = case x2 of
     IHi-> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3500)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_27 x3 x1002 x3500) (d_OP__casePT_27 x3 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_27 x3 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_27 x3 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_27 x3 x2 x3000 x3500 = case x2 of
     IHi-> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3500)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_27 x3 x1002 x3000 x3500) (nd_OP__casePT_27 x3 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_27 x3 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_27 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_28 x2 x3500 = case x2 of
     IHi-> IHi
     (O x3) -> I (d_C_pred x2 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_28 x1002 x3500) (d_OP__casePT_28 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_28 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_28 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_28 x2 x3000 x3500 = case x2 of
     IHi-> IHi
     (O x3) -> I (d_C_pred x2 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_28 x1002 x3000 x3500) (nd_OP__casePT_28 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_28 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_28 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_30 x8 x2 x3500 = case x2 of
     IHi-> Curry_Prelude.C_GT
     (O x9) -> d_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3500) x3500
     (I x10) -> d_C_cmpNat x8 x10 x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_30 x8 x1002 x3500) (d_OP__casePT_30 x8 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_30 x8 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_30 x8 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_30 x8 x2 x3000 x3500 = case x2 of
     IHi-> Curry_Prelude.C_GT
     (O x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3500) x2000 x3500))
     (I x10) -> d_C_cmpNat x8 x10 x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_30 x8 x1002 x3000 x3500) (nd_OP__casePT_30 x8 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_30 x8 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_30 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_29 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_29 x8 x9 x1002 x3500) (d_OP__casePT_29 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_29 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_29 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_29 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_29 x8 x9 x1002 x3000 x3500) (nd_OP__casePT_29 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_29 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_29 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_32 x5 x2 x3500 = case x2 of
     IHi-> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3500
     (I x7) -> d_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3500) x3500
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_32 x5 x1002 x3500) (d_OP__casePT_32 x5 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_32 x5 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_32 x5 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_32 x5 x2 x3000 x3500 = case x2 of
     IHi-> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3500
     (I x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3500) x2000 x3500))
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_32 x5 x1002 x3000 x3500) (nd_OP__casePT_32 x5 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_32 x5 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_32 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_31 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_31 x5 x7 x1002 x3500) (d_OP__casePT_31 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_31 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_31 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_31 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_31 x5 x7 x1002 x3000 x3500) (nd_OP__casePT_31 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_31 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Ordering x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_31 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Ordering x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__casePT_33 x2 x3500 = case x2 of
     IHi-> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__casePT_33 x1002 x3500) (d_OP__casePT_33 x1003 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__casePT_33 z x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__casePT_33 x1002) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__casePT_33 x2 x3000 x3500 = case x2 of
     IHi-> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__casePT_33 x1002 x3000 x3500) (nd_OP__casePT_33 x1003 x3000 x3500)
     (Choices_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__casePT_33 z x3000 x3500) x1002
     (Guard_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__casePT_33 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

