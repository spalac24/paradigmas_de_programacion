{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}


module Curry_SearchTree (C_SearchTree (..), C_ValueSequence (..), C_Strategy, C_AbortList, d_C_getSearchTree, d_C_isDefined, d_C_showSearchTree, d_C_searchTreeSize, d_C_allValuesDFS, nd_C_allValuesDFS, d_C_dfsStrategy, d_C_allValuesBFS, d_C_bfsStrategy, d_C_allValuesIDS, d_C_idsStrategy, d_C_allValuesIDSwith, nd_C_allValuesIDSwith, d_C_idsStrategyWith, nd_C_idsStrategyWith, d_C_someValue, nd_C_someValue, d_C_someValueBy, nd_C_someValueBy, d_C_vsToList, d_C_someSearchTree) where

import Basics
import qualified Curry_Prelude
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch
import MonadSearch
import GHC.Exts (Int (I#), (<#))


type C_Strategy t0 = C_SearchTree t0 -> ConstStore -> C_ValueSequence t0

data C_SearchTree t0
     = C_Value t0
     | C_Fail Curry_Prelude.C_Int
     | C_Or (C_SearchTree t0) (C_SearchTree t0)
     | Choice_C_SearchTree Cover ID (C_SearchTree t0) (C_SearchTree t0)
     | Choices_C_SearchTree Cover ID ([C_SearchTree t0])
     | Fail_C_SearchTree Cover FailInfo
     | Guard_C_SearchTree Cover Constraints (C_SearchTree t0)

instance Show t0 => Show (C_SearchTree t0) where
  showsPrec d (Choice_C_SearchTree cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_SearchTree cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_SearchTree cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_SearchTree cd info) = showChar '!'
  showsPrec _ (C_Value x1) = (showString "(Value") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Fail x1) = (showString "(Fail") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Or x1 x2) = (showString "(Or") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_SearchTree t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Value x1,r1) | (_,r0) <- readQualified "SearchTree" "Value" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Fail x1,r1) | (_,r0) <- readQualified "SearchTree" "Fail" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Or x1 x2,r2) | (_,r0) <- readQualified "SearchTree" "Or" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


instance NonDet (C_SearchTree t0) where
  choiceCons = Choice_C_SearchTree
  choicesCons = Choices_C_SearchTree
  failCons = Fail_C_SearchTree
  guardCons = Guard_C_SearchTree
  try (Choice_C_SearchTree cd i x y) = tryChoice cd i x y
  try (Choices_C_SearchTree cd i xs) = tryChoices cd i xs
  try (Fail_C_SearchTree cd info) = Fail cd info
  try (Guard_C_SearchTree cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_SearchTree cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_SearchTree cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_SearchTree cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_SearchTree cd i _) = error ("SearchTree.SearchTree.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_SearchTree cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_SearchTree cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_SearchTree t0) where
  generate s = Choices_C_SearchTree defCover (freeID [1,1,2] s) [(C_Value (generate (leftSupply s))),(C_Fail (generate (leftSupply s))),(C_Or (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm t0 => NormalForm (C_SearchTree t0) where
  ($!!) cont (C_Value x1) cs = ((\y1 cs -> cont (C_Value y1) cs) $!! x1) cs
  ($!!) cont (C_Fail x1) cs = ((\y1 cs -> cont (C_Fail y1) cs) $!! x1) cs
  ($!!) cont (C_Or x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Or y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_SearchTree cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_SearchTree cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_SearchTree cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_SearchTree cd info) _ = failCons cd info
  ($##) cont (C_Value x1) cs = ((\y1 cs -> cont (C_Value y1) cs) $## x1) cs
  ($##) cont (C_Fail x1) cs = ((\y1 cs -> cont (C_Fail y1) cs) $## x1) cs
  ($##) cont (C_Or x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Or y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_SearchTree cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_SearchTree cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_SearchTree cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_SearchTree cd info) _ = failCons cd info
  searchNF search cont (C_Value x1) = search (\y1 -> cont (C_Value y1)) x1
  searchNF search cont (C_Fail x1) = search (\y1 -> cont (C_Fail y1)) x1
  searchNF search cont (C_Or x1 x2) = search (\y1 -> search (\y2 -> cont (C_Or y1 y2)) x2) x1
  searchNF _ _ x = error ("SearchTree.SearchTree.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_SearchTree t0) where
  (=.=) (C_Value x1) (C_Value y1) cs = (x1 =:= y1) cs
  (=.=) (C_Fail x1) (C_Fail y1) cs = (x1 =:= y1) cs
  (=.=) (C_Or x1 x2) (C_Or y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Value x1) (C_Value y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Fail x1) (C_Fail y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Or x1 x2) (C_Or y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Value x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Fail x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Or x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_SearchTree cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_SearchTree cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_SearchTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_SearchTree cd i _) = error ("SearchTree.SearchTree.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_SearchTree cd info) = [(Unsolvable info)]
  bind i (Guard_C_SearchTree cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Value x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Fail x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Or x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_SearchTree cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_SearchTree cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_SearchTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_SearchTree cd i _) = error ("SearchTree.SearchTree.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_SearchTree cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_SearchTree cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_SearchTree t0) where
  (=?=) (Choice_C_SearchTree cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_SearchTree cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_SearchTree cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_SearchTree cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_SearchTree cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_SearchTree cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_SearchTree cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_SearchTree cd info) _ = failCons cd info
  (=?=) (C_Value x1) (C_Value y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Fail x1) (C_Fail y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Or x1 x2) (C_Or y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SearchTree cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_SearchTree cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_SearchTree cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_SearchTree cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_SearchTree cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_SearchTree cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_SearchTree cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_SearchTree cd info) _ = failCons cd info
  (<?=) (C_Value x1) (C_Value y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Value _) (C_Fail _) _ = Curry_Prelude.C_True
  (<?=) (C_Value _) (C_Or _ _) _ = Curry_Prelude.C_True
  (<?=) (C_Fail x1) (C_Fail y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Fail _) (C_Or _ _) _ = Curry_Prelude.C_True
  (<?=) (C_Or x1 x2) (C_Or y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_SearchTree t0) where
  cover (C_Value x1) = C_Value (cover x1)
  cover (C_Fail x1) = C_Fail (cover x1)
  cover (C_Or x1 x2) = C_Or (cover x1) (cover x2)
  cover (Choice_C_SearchTree cd i x y) = Choice_C_SearchTree (incCover cd) i (cover x) (cover y)
  cover (Choices_C_SearchTree cd i xs) = Choices_C_SearchTree (incCover cd) i (map cover xs)
  cover (Fail_C_SearchTree cd info) = Fail_C_SearchTree (incCover cd) info
  cover (Guard_C_SearchTree cd c e) = Guard_C_SearchTree (incCover cd) c (cover e)




data C_AbortList t0
     = C_Nil
     | C_Cons t0 (C_AbortList t0)
     | C_FCons Curry_Prelude.C_Int (C_AbortList t0)
     | C_Abort
     | Choice_C_AbortList Cover ID (C_AbortList t0) (C_AbortList t0)
     | Choices_C_AbortList Cover ID ([C_AbortList t0])
     | Fail_C_AbortList Cover FailInfo
     | Guard_C_AbortList Cover Constraints (C_AbortList t0)

instance Show t0 => Show (C_AbortList t0) where
  showsPrec d (Choice_C_AbortList cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AbortList cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AbortList cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AbortList cd info) = showChar '!'
  showsPrec _ C_Nil = showString "Nil"
  showsPrec _ (C_Cons x1 x2) = (showString "(Cons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_FCons x1 x2) = (showString "(FCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ C_Abort = showString "Abort"


instance Read t0 => Read (C_AbortList t0) where
  readsPrec d s = (readParen False (\r -> [ (C_Nil,r0) | (_,r0) <- readQualified "SearchTree" "Nil" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_Cons x1 x2,r2) | (_,r0) <- readQualified "SearchTree" "Cons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_FCons x1 x2,r2) | (_,r0) <- readQualified "SearchTree" "FCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen False (\r -> [ (C_Abort,r0) | (_,r0) <- readQualified "SearchTree" "Abort" r]) s)))


instance NonDet (C_AbortList t0) where
  choiceCons = Choice_C_AbortList
  choicesCons = Choices_C_AbortList
  failCons = Fail_C_AbortList
  guardCons = Guard_C_AbortList
  try (Choice_C_AbortList cd i x y) = tryChoice cd i x y
  try (Choices_C_AbortList cd i xs) = tryChoices cd i xs
  try (Fail_C_AbortList cd info) = Fail cd info
  try (Guard_C_AbortList cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AbortList cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AbortList cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AbortList cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AbortList cd i _) = error ("SearchTree.AbortList.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AbortList cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AbortList cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_AbortList t0) where
  generate s = Choices_C_AbortList defCover (freeID [0,2,2,0] s) [C_Nil,(C_Cons (generate (leftSupply s)) (generate (rightSupply s))),(C_FCons (generate (leftSupply s)) (generate (rightSupply s))),C_Abort]


instance NormalForm t0 => NormalForm (C_AbortList t0) where
  ($!!) cont C_Nil cs = cont C_Nil cs
  ($!!) cont (C_Cons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Cons y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_FCons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_FCons y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont C_Abort cs = cont C_Abort cs
  ($!!) cont (Choice_C_AbortList cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_AbortList cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_AbortList cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_AbortList cd info) _ = failCons cd info
  ($##) cont C_Nil cs = cont C_Nil cs
  ($##) cont (C_Cons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Cons y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_FCons x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_FCons y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont C_Abort cs = cont C_Abort cs
  ($##) cont (Choice_C_AbortList cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_AbortList cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_AbortList cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_AbortList cd info) _ = failCons cd info
  searchNF _ cont C_Nil = cont C_Nil
  searchNF search cont (C_Cons x1 x2) = search (\y1 -> search (\y2 -> cont (C_Cons y1 y2)) x2) x1
  searchNF search cont (C_FCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_FCons y1 y2)) x2) x1
  searchNF _ cont C_Abort = cont C_Abort
  searchNF _ _ x = error ("SearchTree.AbortList.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_AbortList t0) where
  (=.=) C_Nil C_Nil cs = C_Success
  (=.=) (C_Cons x1 x2) (C_Cons y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_FCons x1 x2) (C_FCons y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) C_Abort C_Abort cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Nil C_Nil cs = C_Success
  (=.<=) (C_Cons x1 x2) (C_Cons y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_FCons x1 x2) (C_FCons y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) C_Abort C_Abort cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Nil = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_Cons x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_FCons x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i C_Abort = ((i :=: (ChooseN 3 0)):(concat []))
  bind i (Choice_C_AbortList cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_AbortList cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_AbortList cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_AbortList cd i _) = error ("SearchTree.AbortList.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_AbortList cd info) = [(Unsolvable info)]
  bind i (Guard_C_AbortList cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Nil = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_Cons x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_FCons x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i C_Abort = [(i :=: (ChooseN 3 0))]
  lazyBind i (Choice_C_AbortList cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_AbortList cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_AbortList cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_AbortList cd i _) = error ("SearchTree.AbortList.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_AbortList cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_AbortList cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_AbortList t0) where
  (=?=) (Choice_C_AbortList cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_AbortList cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_AbortList cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_AbortList cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_AbortList cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_AbortList cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_AbortList cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_AbortList cd info) _ = failCons cd info
  (=?=) C_Nil C_Nil cs = Curry_Prelude.C_True
  (=?=) (C_Cons x1 x2) (C_Cons y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_FCons x1 x2) (C_FCons y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) C_Abort C_Abort cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_AbortList cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_AbortList cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_AbortList cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_AbortList cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_AbortList cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_AbortList cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_AbortList cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_AbortList cd info) _ = failCons cd info
  (<?=) C_Nil C_Nil cs = Curry_Prelude.C_True
  (<?=) C_Nil (C_Cons _ _) _ = Curry_Prelude.C_True
  (<?=) C_Nil (C_FCons _ _) _ = Curry_Prelude.C_True
  (<?=) C_Nil C_Abort _ = Curry_Prelude.C_True
  (<?=) (C_Cons x1 x2) (C_Cons y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_Cons _ _) (C_FCons _ _) _ = Curry_Prelude.C_True
  (<?=) (C_Cons _ _) C_Abort _ = Curry_Prelude.C_True
  (<?=) (C_FCons x1 x2) (C_FCons y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_FCons _ _) C_Abort _ = Curry_Prelude.C_True
  (<?=) C_Abort C_Abort cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_AbortList t0) where
  cover C_Nil = C_Nil
  cover (C_Cons x1 x2) = C_Cons (cover x1) (cover x2)
  cover (C_FCons x1 x2) = C_FCons (cover x1) (cover x2)
  cover C_Abort = C_Abort
  cover (Choice_C_AbortList cd i x y) = Choice_C_AbortList (incCover cd) i (cover x) (cover y)
  cover (Choices_C_AbortList cd i xs) = Choices_C_AbortList (incCover cd) i (map cover xs)
  cover (Fail_C_AbortList cd info) = Fail_C_AbortList (incCover cd) info
  cover (Guard_C_AbortList cd c e) = Guard_C_AbortList (incCover cd) c (cover e)


d_C_getSearchTree :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.C_IO (C_SearchTree t0)
d_C_getSearchTree x1 x3500 = Curry_Prelude.d_C_return (d_C_someSearchTree x1 x3500) x3500

d_C_isDefined :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isDefined x1 x3500 = d_OP_isDefined_dot_hasValue_dot_4 (d_C_someSearchTree x1 x3500) x3500

d_OP_isDefined_dot_hasValue_dot_4 :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isDefined_dot_hasValue_dot_4 x1 x3500 = case x1 of
     (C_Value x2) -> Curry_Prelude.C_True
     (C_Fail x3) -> Curry_Prelude.C_False
     (C_Or x4 x5) -> Curry_Prelude.d_OP_bar_bar (d_OP_isDefined_dot_hasValue_dot_4 x4 x3500) (d_OP_isDefined_dot_hasValue_dot_4 x5 x3500) x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isDefined_dot_hasValue_dot_4 x1002 x3500) (d_OP_isDefined_dot_hasValue_dot_4 x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isDefined_dot_hasValue_dot_4 z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isDefined_dot_hasValue_dot_4 x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSearchTree :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSearchTree x1 x3500 = let
     x2 = d_OP_showSearchTree_dot_showChar_dot_16 (Curry_Prelude.C_Char '\n'#) x3500
      in (Curry_Prelude.d_C_apply (d_OP_showSearchTree_dot_showsST_dot_16 x2 Curry_Prelude.OP_List x1 x3500) Curry_Prelude.OP_List x3500)

d_OP_showSearchTree_dot_showChar_dot_16 :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_showSearchTree_dot_showChar_dot_16 x1 x3500 = acceptCs id (Curry_Prelude.OP_Cons x1)

nd_OP_showSearchTree_dot_showChar_dot_16 :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_showSearchTree_dot_showChar_dot_16 x1 x3000 x3500 = wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x1))

d_OP_showSearchTree_dot_showString_dot_16 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_showSearchTree_dot_showString_dot_16 x1 x3500 = Curry_Prelude.d_OP_plus_plus x1

nd_OP_showSearchTree_dot_showString_dot_16 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_showSearchTree_dot_showString_dot_16 x1 x3000 x3500 = wrapDX id (Curry_Prelude.d_OP_plus_plus x1)

d_OP_showSearchTree_dot_indent_dot_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_indent_dot_16 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_id
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showString_dot_16 (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_concatMap d_OP_showSearchTree_dot_indent_dot_16_dot_showIndent_dot_27 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x3 x3500) x3500) x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_16 (d_OP__case_7 x2 x3500) x3500) (d_OP_showSearchTree_dot_showString_dot_16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 9472#)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showSearchTree_dot_indent_dot_16 x1002 x3500) (d_OP_showSearchTree_dot_indent_dot_16 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showSearchTree_dot_indent_dot_16 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showSearchTree_dot_indent_dot_16 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_showSearchTree_dot_indent_dot_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_indent_dot_16 x1 x3000 x3500 = case x1 of
     Curry_Prelude.OP_List -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2017 = x3000
           in (seq x2017 (let
               x2016 = leftSupply x2017
               x2018 = rightSupply x2017
                in (seq x2016 (seq x2018 (let
                    x2008 = leftSupply x2018
                    x2014 = rightSupply x2018
                     in (seq x2008 (seq x2014 (Curry_Prelude.nd_OP_dot (let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (nd_OP_showSearchTree_dot_showString_dot_16 (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dollar (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showSearchTree_dot_indent_dot_16_dot_showIndent_dot_27) x2000 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2001 x3500) x3 x2002 x3500)))) x2004 x3500))))))) x2007 x3500)))) (let
                         x2013 = leftSupply x2014
                         x2015 = rightSupply x2014
                          in (seq x2013 (seq x2015 (let
                              x2011 = leftSupply x2015
                              x2012 = rightSupply x2015
                               in (seq x2011 (seq x2012 (Curry_Prelude.nd_OP_dot (let
                                   x2010 = leftSupply x2011
                                   x2009 = rightSupply x2011
                                    in (seq x2010 (seq x2009 (nd_OP_showSearchTree_dot_showChar_dot_16 (nd_OP__case_7 x2 x2009 x3500) x2010 x3500)))) (nd_OP_showSearchTree_dot_showString_dot_16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 9472#)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x2012 x3500) x2013 x3500))))))) x2016 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showSearchTree_dot_indent_dot_16 x1002 x3000 x3500) (nd_OP_showSearchTree_dot_indent_dot_16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showSearchTree_dot_indent_dot_16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showSearchTree_dot_indent_dot_16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showSearchTree_dot_indent_dot_16_dot_showIndent_dot_27 :: Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_indent_dot_16_dot_showIndent_dot_27 x1 x3500 = Curry_Prelude.OP_Cons (d_OP__case_6 x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))

d_OP_showSearchTree_dot_shows_dot_16 :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_shows_dot_16 x1 x3500 = d_OP_showSearchTree_dot_showString_dot_16 (Curry_Prelude.d_C_show x1 x3500) x3500

nd_OP_showSearchTree_dot_shows_dot_16 :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_shows_dot_16 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_showSearchTree_dot_showString_dot_16 (Curry_Prelude.d_C_show x1 x3500) x2000 x3500))

d_OP_showSearchTree_dot_showsST_dot_16 :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x3 x3500 = case x3 of
     (C_Value x4) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_16 x2 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_shows_dot_16 x4 x3500) x1 x3500) x3500
     (C_Fail x5) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_16 x2 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_16 (Curry_Prelude.C_Char '!'#) x3500) x1 x3500) x3500
     (C_Or x6 x7) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_16 x2 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_16 (Curry_Prelude.C_Char '?'#) x3500) (Curry_Prelude.d_OP_dot x1 (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showsST_dot_16 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_False x2) x6 x3500) (d_OP_showSearchTree_dot_showsST_dot_16 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_True x2) x7 x3500) x3500) x3500) x3500) x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1002 x3500) (d_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showSearchTree_dot_showsST_dot_16 x1 x2 z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_showSearchTree_dot_showsST_dot_16 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> C_SearchTree t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x3 x3000 x3500 = case x3 of
     (C_Value x4) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_16 x2 x2000 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_shows_dot_16 x4 x2001 x3500) x1 x2002 x3500)))) x2004 x3500))))))))
     (C_Fail x5) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_16 x2 x2000 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showChar_dot_16 (Curry_Prelude.C_Char '!'#) x2001 x3500) x1 x2002 x3500)))) x2004 x3500))))))))
     (C_Or x6 x7) -> let
          x2013 = x3000
           in (seq x2013 (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2000 = leftSupply x2014
                    x2010 = rightSupply x2014
                     in (seq x2000 (seq x2010 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_16 x2 x2000 x3500) (let
                         x2009 = leftSupply x2010
                         x2011 = rightSupply x2010
                          in (seq x2009 (seq x2011 (let
                              x2001 = leftSupply x2011
                              x2008 = rightSupply x2011
                               in (seq x2001 (seq x2008 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showChar_dot_16 (Curry_Prelude.C_Char '?'#) x2001 x3500) (let
                                   x2007 = leftSupply x2008
                                   x2005 = rightSupply x2008
                                    in (seq x2007 (seq x2005 (Curry_Prelude.nd_OP_dot x1 (let
                                        x2004 = leftSupply x2005
                                        x2006 = rightSupply x2005
                                         in (seq x2004 (seq x2006 (let
                                             x2002 = leftSupply x2006
                                             x2003 = rightSupply x2006
                                              in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showsST_dot_16 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_False x2) x6 x2002 x3500) (nd_OP_showSearchTree_dot_showsST_dot_16 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_True x2) x7 x2003 x3500) x2004 x3500))))))) x2007 x3500)))) x2009 x3500))))))) x2012 x3500))))))))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1002 x3000 x3500) (nd_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1003 x3000 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showSearchTree_dot_showsST_dot_16 x1 x2 z x3000 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showSearchTree_dot_showsST_dot_16 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_searchTreeSize :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int
d_C_searchTreeSize x1 x3500 = case x1 of
     (C_Value x2) -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.C_Int 1#) (Curry_Prelude.C_Int 0#) (Curry_Prelude.C_Int 0#)
     (C_Fail x3) -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.C_Int 0#) (Curry_Prelude.C_Int 1#) (Curry_Prelude.C_Int 0#)
     (C_Or x4 x5) -> let
          x6 = d_C_searchTreeSize x4 x3500
          x7 = d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x6 x3500
          x8 = d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x6 x3500
          x9 = d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x6 x3500
          x10 = d_C_searchTreeSize x5 x3500
          x11 = d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x10 x3500
          x12 = d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x10 x3500
          x13 = d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x10 x3500
           in (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_OP_plus x7 x11 x3500) (Curry_Prelude.d_OP_plus x8 x12 x3500) (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x9 x13 x3500) (Curry_Prelude.C_Int 1#) x3500))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_searchTreeSize x1002 x3500) (d_C_searchTreeSize x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_searchTreeSize z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_searchTreeSize x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1002 x3500) (d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allValuesDFS :: Curry_Prelude.Curry t0 => ConstStore -> C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesDFS x3500 = Curry_Prelude.d_OP_dot d_C_vsToList d_C_dfsStrategy x3500

nd_C_allValuesDFS :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func (C_SearchTree t0) (Curry_Prelude.OP_List t0)
nd_C_allValuesDFS x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_vsToList) (wrapDX id d_C_dfsStrategy) x2000 x3500))

d_C_dfsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> C_ValueSequence t0
d_C_dfsStrategy x1 x3500 = case x1 of
     (C_Fail x2) -> d_C_failVS x2 x3500
     (C_Value x3) -> d_C_addVS x3 (d_C_emptyVS x3500) x3500
     (C_Or x4 x5) -> d_OP_bar_plus_plus_bar (d_C_dfsStrategy x4 x3500) (d_C_dfsStrategy x5 x3500) x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dfsStrategy x1002 x3500) (d_C_dfsStrategy x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dfsStrategy z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dfsStrategy x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allValuesBFS :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesBFS x1 x3500 = d_C_vsToList (d_C_bfsStrategy x1 x3500) x3500

d_C_children :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> ConstStore -> Curry_Prelude.OP_List (C_SearchTree t0)
d_C_children x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_5 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_children x1002 x3500) (d_C_children x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_children z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_children x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_values :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> ConstStore -> C_ValueSequence t0
d_C_values x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_C_emptyVS x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_values x1002 x3500) (d_C_values x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_values z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_values x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allBFS :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> ConstStore -> C_ValueSequence t0
d_C_allBFS x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_C_emptyVS x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP_bar_plus_plus_bar (d_C_values (Curry_Prelude.OP_Cons x2 x3) x3500) (d_C_allBFS (d_C_children (Curry_Prelude.OP_Cons x2 x3) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allBFS x1002 x3500) (d_C_allBFS x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allBFS z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allBFS x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_bfsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> C_ValueSequence t0
d_C_bfsStrategy x1 x3500 = d_C_allBFS (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3500

d_C_defIDSDepth :: ConstStore -> Curry_Prelude.C_Int
d_C_defIDSDepth x3500 = Curry_Prelude.C_Int 100#

d_C_defIDSInc :: ConstStore -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_defIDSInc x3500 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#)

nd_C_defIDSInc :: IDSupply -> ConstStore -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int
nd_C_defIDSInc x3000 x3500 = wrapDX id (Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#))

d_C_allValuesIDS :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesIDS x1 x3500 = Curry_Prelude.d_C_apply (d_C_allValuesIDSwith (d_C_defIDSDepth x3500) (d_C_defIDSInc x3500) x3500) x1 x3500

d_C_idsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> ConstStore -> C_ValueSequence t0
d_C_idsStrategy x1 x3500 = d_C_idsStrategyWith (d_C_defIDSDepth x3500) (d_C_defIDSInc x3500) x1 x3500

d_C_allValuesIDSwith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int) -> ConstStore -> C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesIDSwith x1 x2 x3500 = Curry_Prelude.d_OP_dot d_C_vsToList (d_C_idsStrategyWith x1 x2) x3500

nd_C_allValuesIDSwith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Func (C_SearchTree t0) (Curry_Prelude.OP_List t0)
nd_C_allValuesIDSwith x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_vsToList) (wrapNX id (nd_C_idsStrategyWith x1 x2)) x2000 x3500))

d_C_idsStrategyWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int) -> C_SearchTree t0 -> ConstStore -> C_ValueSequence t0
d_C_idsStrategyWith x1 x2 x3 x3500 = d_OP_idsStrategyWith_dot_iterIDS_dot_108 x2 x3 x1 (d_C_collectInBounds (Curry_Prelude.C_Int 0#) x1 x3 x3500) x3500

nd_C_idsStrategyWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> C_SearchTree t0 -> IDSupply -> ConstStore -> C_ValueSequence t0
nd_C_idsStrategyWith x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x2 x3 x1 (d_C_collectInBounds (Curry_Prelude.C_Int 0#) x1 x3 x3500) x2000 x3500))

d_OP_idsStrategyWith_dot_iterIDS_dot_108 :: Curry_Prelude.Curry t310 => (Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int) -> C_SearchTree t310 -> Curry_Prelude.C_Int -> C_AbortList t310 -> ConstStore -> C_ValueSequence t310
d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x4 x3500 = case x4 of
     C_Nil -> d_C_emptyVS x3500
     (C_Cons x5 x6) -> d_C_addVS x5 (d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x6 x3500) x3500
     (C_FCons x7 x8) -> d_OP_bar_plus_plus_bar (d_C_failVS x7 x3500) (d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x8 x3500) x3500
     C_Abort -> let
          x9 = Curry_Prelude.d_C_apply x1 x3 x3500
           in (d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x9 (d_C_collectInBounds x3 x9 x2 x3500) x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1002 x3500) (d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1003 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 z x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_idsStrategyWith_dot_iterIDS_dot_108 :: Curry_Prelude.Curry t310 => Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> C_SearchTree t310 -> Curry_Prelude.C_Int -> C_AbortList t310 -> IDSupply -> ConstStore -> C_ValueSequence t310
nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x4 x3000 x3500 = case x4 of
     C_Nil -> d_C_emptyVS x3500
     (C_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (d_C_addVS x5 (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x6 x2000 x3500) x3500))
     (C_FCons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (d_OP_bar_plus_plus_bar (d_C_failVS x7 x3500) (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x8 x2000 x3500) x3500))
     C_Abort -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x9 = Curry_Prelude.nd_C_apply x1 x3 x2000 x3500
                     in (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x9 (d_C_collectInBounds x3 x9 x2 x3500) x2001 x3500))))))
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1002 x3000 x3500) (nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1003 x3000 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 z x3000 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_idsStrategyWith_dot_iterIDS_dot_108 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_collectInBounds :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_SearchTree t0 -> ConstStore -> C_AbortList t0
d_C_collectInBounds x1 x2 x3 x3500 = d_OP_collectInBounds_dot_collectLevel_dot_122 x2 x1 x2 x3 x3500

d_OP_collectInBounds_dot_collectLevel_dot_122 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_SearchTree t0 -> ConstStore -> C_AbortList t0
d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 x3 x4 x3500 = case x4 of
     (C_Fail x5) -> d_OP__case_3 x1 x2 x3 x5 (Curry_Prelude.d_OP_lt x3 (Curry_Prelude.d_OP_minus x1 x2 x3500) x3500) x3500
     (C_Value x6) -> d_OP__case_2 x1 x2 x3 x6 (Curry_Prelude.d_OP_lt x3 (Curry_Prelude.d_OP_minus x1 x2 x3500) x3500) x3500
     (C_Or x7 x8) -> d_OP__case_1 x1 x2 x3 x7 x8 (Curry_Prelude.d_OP_gt x3 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 x3 x1002 x3500) (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 x3 x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 x3 z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_concA :: Curry_Prelude.Curry t0 => C_AbortList t0 -> C_AbortList t0 -> ConstStore -> C_AbortList t0
d_C_concA x1 x2 x3500 = case x1 of
     C_Abort -> d_OP__case_0 x2 x3500
     C_Nil -> x2
     (C_Cons x7 x8) -> C_Cons x7 (d_C_concA x8 x2 x3500)
     (C_FCons x9 x10) -> C_FCons x9 (d_C_concA x10 x2 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_concA x1002 x2 x3500) (d_C_concA x1003 x2 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_concA z x2 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_concA x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_someValue :: Curry_Prelude.Curry t0 => ConstStore -> t0 -> ConstStore -> t0
d_C_someValue x3500 = d_C_someValueBy d_C_allValuesBFS

nd_C_someValue :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func t0 t0
nd_C_someValue x3000 x3500 = wrapNX id (nd_C_someValueBy (wrapDX id d_C_allValuesBFS))

d_C_someValueBy :: Curry_Prelude.Curry t0 => (C_SearchTree t0 -> ConstStore -> Curry_Prelude.OP_List t0) -> t0 -> ConstStore -> t0
d_C_someValueBy x1 x2 x3500 = Curry_Prelude.d_C_head (Curry_Prelude.d_C_apply x1 (d_C_someSearchTree x2 x3500) x3500) x3500

nd_C_someValueBy :: Curry_Prelude.Curry t0 => Func (C_SearchTree t0) (Curry_Prelude.OP_List t0) -> t0 -> IDSupply -> ConstStore -> t0
nd_C_someValueBy x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_head (Curry_Prelude.nd_C_apply x1 (d_C_someSearchTree x2 x3500) x2000 x3500) x3500))

d_OP__case_0 x2 x3500 = case x2 of
     C_Abort -> C_Abort
     C_Nil -> C_Abort
     (C_Cons x3 x4) -> C_Cons x3 (d_C_concA C_Abort x4 x3500)
     (C_FCons x5 x6) -> C_FCons x5 (d_C_concA C_Abort x6 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3500) (d_OP__case_0 x1003 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x3000 x3500 = case x2 of
     C_Abort -> C_Abort
     C_Nil -> C_Abort
     (C_Cons x3 x4) -> C_Cons x3 (d_C_concA C_Abort x4 x3500)
     (C_FCons x5 x6) -> C_FCons x5 (d_C_concA C_Abort x6 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1002 x3000 x3500) (nd_OP__case_0 x1003 x3000 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 z x3000 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_concA (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x7 x3500) (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x8 x3500) x3500
     Curry_Prelude.C_False -> C_Abort
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x3 x7 x8 x1002 x3500) (d_OP__case_1 x1 x2 x3 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 x3 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x3 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_concA (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x7 x3500) (d_OP_collectInBounds_dot_collectLevel_dot_122 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3500) x8 x3500) x3500
     Curry_Prelude.C_False -> C_Abort
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x3 x7 x8 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x3 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 x3 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x3 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> C_Cons x6 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x3 x6 x1002 x3500) (d_OP__case_2 x1 x2 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> C_Cons x6 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x3 x6 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> C_FCons x5 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x5 x1002 x3500) (d_OP__case_3 x1 x2 x3 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> C_FCons x5 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x5 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x3 x2 x3500 = case x2 of
     (C_Fail x4) -> d_OP_bar_plus_plus_bar (d_C_failVS x4 x3500) (d_C_values x3 x3500) x3500
     (C_Value x5) -> d_C_addVS x5 (d_C_values x3 x3500) x3500
     (C_Or x6 x7) -> d_C_values x3 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x1002 x3500) (d_OP__case_4 x3 x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x3 x2 x3000 x3500 = case x2 of
     (C_Fail x4) -> d_OP_bar_plus_plus_bar (d_C_failVS x4 x3500) (d_C_values x3 x3500) x3500
     (C_Value x5) -> d_C_addVS x5 (d_C_values x3 x3500) x3500
     (C_Or x6 x7) -> d_C_values x3 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x3 x1002 x3000 x3500) (nd_OP__case_4 x3 x1003 x3000 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x3 z x3000 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x3 x2 x3500 = case x2 of
     (C_Fail x4) -> d_C_children x3 x3500
     (C_Value x5) -> d_C_children x3 x3500
     (C_Or x6 x7) -> Curry_Prelude.OP_Cons x6 (Curry_Prelude.OP_Cons x7 (d_C_children x3 x3500))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x1002 x3500) (d_OP__case_5 x3 x1003 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 z x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x3 x2 x3000 x3500 = case x2 of
     (C_Fail x4) -> d_C_children x3 x3500
     (C_Value x5) -> d_C_children x3 x3500
     (C_Or x6 x7) -> Curry_Prelude.OP_Cons x6 (Curry_Prelude.OP_Cons x7 (d_C_children x3 x3500))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x3 x1002 x3000 x3500) (nd_OP__case_5 x3 x1003 x3000 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x3 z x3000 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char ' '#
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9474#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3500) (d_OP__case_6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char ' '#
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9474#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1002 x3000 x3500) (nd_OP__case_6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char (nonAsciiChr 9492#)
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9500#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3500) (d_OP__case_7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char (nonAsciiChr 9492#)
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9500#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1002 x3000 x3500) (nd_OP__case_7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_emptyVS :: Curry_Prelude.Curry t0 => ConstStore -> C_ValueSequence t0
d_C_emptyVS x3500 = external_d_C_emptyVS x3500

d_C_addVS :: Curry_Prelude.Curry t0 => t0 -> C_ValueSequence t0 -> ConstStore -> C_ValueSequence t0
d_C_addVS x1 x2 x3500 = external_d_C_addVS x1 x2 x3500

d_C_failVS :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> ConstStore -> C_ValueSequence t0
d_C_failVS x1 x3500 = external_d_C_failVS x1 x3500

d_C_vsToList :: Curry_Prelude.Curry t0 => C_ValueSequence t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_vsToList x1 x3500 = external_d_C_vsToList x1 x3500

d_OP_bar_plus_plus_bar :: Curry_Prelude.Curry t0 => C_ValueSequence t0 -> C_ValueSequence t0 -> ConstStore -> C_ValueSequence t0
d_OP_bar_plus_plus_bar x1 x2 x3500 = external_d_OP_bar_plus_plus_bar x1 x2 x3500

d_C_someSearchTree :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> C_SearchTree t0
d_C_someSearchTree x1 x3500 = external_d_C_someSearchTree x1 x3500
instance Monad C_SearchTree where
  return = C_Value

  C_Fail d  >>= _ = C_Fail d
  C_Value x >>= f = f x
  C_Or x y  >>= f = C_Or (x >>= f) (y >>= f)

  Choice_C_SearchTree cd i x y >>= f = Choice_C_SearchTree cd i (x >>= f) (y >>= f)
  Choices_C_SearchTree cd i xs >>= f = Choices_C_SearchTree cd i (map (>>= f) xs)
  Guard_C_SearchTree cd cs x   >>= f = Guard_C_SearchTree cd cs (x >>= f)
  Fail_C_SearchTree cd info >>= _ = Fail_C_SearchTree cd info   
  

instance MonadPlus C_SearchTree where
  mzero = C_Fail (Curry_Prelude.C_Int -1#)
  mplus = C_Or

instance MonadSearch C_SearchTree where
  splus            = Choice_C_SearchTree
  ssum             = Choices_C_SearchTree
  szero (I# d) _   = C_Fail (Curry_Prelude.C_Int d)
  constrainMSearch = Guard_C_SearchTree

external_d_C_someSearchTree :: NormalForm a => a -> ConstStore -> C_SearchTree a
external_d_C_someSearchTree = encapsulatedSearch

external_d_OP_bar_plus_plus_bar ::  Curry_Prelude.Curry a =>
 C_ValueSequence a -> C_ValueSequence a -> ConstStore -> C_ValueSequence a
external_d_OP_bar_plus_plus_bar l1 l2 _ = l1 |++| l2

data C_ValueSequence a = EmptyVS | Values (Curry_Prelude.OP_List a) 
                       | FailVS (Curry_Prelude.C_Int)
                       | Choice_VS Cover ID (C_ValueSequence a) (C_ValueSequence a)
                       | Choices_VS Cover ID [C_ValueSequence a]
                       | Guard_VS Cover Constraints (C_ValueSequence a)

instance Curry_Prelude.Curry (C_ValueSequence a) where

instance Show (C_ValueSequence a) where
  showsPrec = error "SearchTree: ValueSequence: showsPrec"

instance Read (C_ValueSequence a) where
  readsPrec = error "SearchTree: ValueSequence: readsPrec"

instance Unifiable (C_ValueSequence a) where
  (=.=)      = error "SearchTree: ValueSequence: (=.=)"
  (=.<=)     = error "SearchTree: ValueSequence: (=.<=)"
  bind       = error "SearchTree: ValueSequence: bind"
  lazyBind   = error "SearchTree: ValueSequence: lazyBind"

instance NonDet (C_ValueSequence a) where
  choiceCons  = Choice_VS
  choicesCons = Choices_VS
  guardCons   = Guard_VS
  failCons    = error "SearchTree: ValueSequence: failCons"
  try        = error "SearchTree: ValueSequence: try" 
  match      = error "SearchTree: ValueSequence: match" 

instance Generable (C_ValueSequence a) where
  generate = error "SearchTree: ValueSequence: generate"

instance Coverable (C_ValueSequence a) where
  cover   = error "SearchTree: ValueSequence: cover"

instance NormalForm (C_ValueSequence a) where
 ($!!)    = error "SearchTree: ValueSequence: ($!!)"
 ($##)    = error "SearchTree: ValueSequence: ($##)"
 searchNF = error "SearchTree: ValueSequence: searchNF"

external_d_C_emptyVS :: ConstStore -> C_ValueSequence a
external_d_C_emptyVS _ = EmptyVS

external_d_C_addVS :: a -> C_ValueSequence a -> ConstStore -> C_ValueSequence a
external_d_C_addVS x vs _ = Values (Curry_Prelude.OP_Cons x (getValues vs))

external_d_C_failVS :: Curry_Prelude.C_Int -> ConstStore -> C_ValueSequence a 
external_d_C_failVS d@(Curry_Prelude.C_Int d') _
  | d' <# 0#   = Values (Curry_Prelude.OP_List)
  | otherwise  = FailVS d 

external_d_C_vsToList :: C_ValueSequence a -> ConstStore -> Curry_Prelude.OP_List a
external_d_C_vsToList (Values xs)   _ = xs
external_d_C_vsToList (FailVS (Curry_Prelude.C_Int d))    _ = failCons (I# d) defFailInfo
external_d_C_vsToList (Choice_VS cd i x y) cs = choiceCons cd i (external_d_C_vsToList x cs)
                                                                (external_d_C_vsToList y cs)
external_d_C_vsToList (Choices_VS cd i xs) cs = 
  choicesCons cd i (map (flip external_d_C_vsToList cs) xs )
external_d_C_vsToList (Guard_VS cd c x) cs =
  guardCons cd c (external_d_C_vsToList x cs)

(|++|) :: Curry_Prelude.Curry a => C_ValueSequence a -> C_ValueSequence a -> C_ValueSequence a 
EmptyVS            |++| vs = vs
Values xs          |++| vs = Values (Curry_Prelude.d_OP_plus_plus xs (getValues vs) emptyCs)
FailVS d           |++| vs = failSmallest d vs
Choice_VS cd i x y |++| vs = choiceCons cd i (x |++| vs) (y |++| vs) 
Choices_VS cd i xs |++| vs = choicesCons cd i (map (|++| vs) xs)
Guard_VS cd cs xs  |++| vs = guardCons cd cs (xs |++| vs)

getValues EmptyVS              = Curry_Prelude.OP_List
getValues (FailVS _)           = Curry_Prelude.OP_List
getValues (Values xs)          = xs
getValues (Choice_VS cd i x y) = choiceCons cd i (getValues x) (getValues y)
getValues (Choices_VS cd i xs) = choicesCons cd i (map getValues xs)
getValues (Guard_VS cd cs x)   = guardCons cd cs (getValues x)

failSmallest d EmptyVS              = FailVS d
failSmallest d (FailVS d2)          = FailVS (Curry_Prelude.d_C_min d d2 emptyCs)
failSmallest _ vs@(Values xs)       = vs
failSmallest d (Choice_VS cd i x y) = choiceCons cd i (failSmallest d x) (failSmallest d y)
failSmallest d (Choices_VS cd i xs) = choicesCons cd i (map (failSmallest d) xs)
failSmallest d (Guard_VS cd cs x)   = guardCons cd cs (failSmallest d x)


