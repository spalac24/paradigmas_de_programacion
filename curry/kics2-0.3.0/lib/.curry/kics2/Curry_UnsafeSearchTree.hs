{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}


module Curry_UnsafeSearchTree (C_SearchTree (..), C_Strategy, C_AbortList, d_C_isVar, d_C_identicalVars, d_C_varId, d_C_getSearchTree, d_C_isDefined, d_C_showSearchTree, d_C_searchTreeSize, d_C_allValuesDFS, nd_C_allValuesDFS, d_C_dfsStrategy, d_C_allValuesBFS, d_C_bfsStrategy, d_C_allValuesIDS, d_C_idsStrategy, d_C_allValuesIDSwith, nd_C_allValuesIDSwith, d_C_idsStrategyWith, nd_C_idsStrategyWith, d_C_getAllValuesWith, nd_C_getAllValuesWith, d_C_someValue, nd_C_someValue, d_C_someValueWith, nd_C_someValueWith, d_C_someSearchTree) where

import Basics
import qualified Curry_Prelude
import qualified Curry_ValueSequence
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch
import MonadSearch
import GHC.Exts (Int (I#), (<#))


type C_Strategy t0 = C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0

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
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Value x1,r1) | (_,r0) <- readQualified "UnsafeSearchTree" "Value" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Fail x1,r1) | (_,r0) <- readQualified "UnsafeSearchTree" "Fail" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Or x1 x2,r2) | (_,r0) <- readQualified "UnsafeSearchTree" "Or" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


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
  match _ _ _ _ _ _ (Choices_C_SearchTree cd i _) = error ("UnsafeSearchTree.SearchTree.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_SearchTree cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_SearchTree cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_SearchTree t0) where
  generate s c = Choices_C_SearchTree c (freeID [1,1,2] s) [(C_Value (generate (leftSupply s) c)),(C_Fail (generate (leftSupply s) c)),(C_Or (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_SearchTree t0) where
  ($!!) cont (C_Value x1) d cs = (((\y1 d cs -> cont (C_Value y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Fail x1) d cs = (((\y1 d cs -> cont (C_Fail y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Or x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Or y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_SearchTree cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_SearchTree cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_SearchTree cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_SearchTree cd info) _ _ = failCons cd info
  ($##) cont (C_Value x1) d cs = (((\y1 d cs -> cont (C_Value y1) d cs) $## x1) d) cs
  ($##) cont (C_Fail x1) d cs = (((\y1 d cs -> cont (C_Fail y1) d cs) $## x1) d) cs
  ($##) cont (C_Or x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Or y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_SearchTree cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_SearchTree cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_SearchTree cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_SearchTree cd info) _ _ = failCons cd info
  searchNF search cont (C_Value x1) = search (\y1 -> cont (C_Value y1)) x1
  searchNF search cont (C_Fail x1) = search (\y1 -> cont (C_Fail y1)) x1
  searchNF search cont (C_Or x1 x2) = search (\y1 -> search (\y2 -> cont (C_Or y1 y2)) x2) x1
  searchNF _ _ x = error ("UnsafeSearchTree.SearchTree.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_SearchTree t0) where
  (=.=) (C_Value x1) (C_Value y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Fail x1) (C_Fail y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Or x1 x2) (C_Or y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Value x1) (C_Value y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Fail x1) (C_Fail y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Or x1 x2) (C_Or y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Value x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Fail x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Or x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_SearchTree cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_SearchTree cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_SearchTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_SearchTree cd i _) = error ("UnsafeSearchTree.SearchTree.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_SearchTree cd info) = [(Unsolvable info)]
  bind d i (Guard_C_SearchTree cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Value x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Fail x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Or x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_SearchTree cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_SearchTree cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_SearchTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_SearchTree cd i _) = error ("UnsafeSearchTree.SearchTree.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_SearchTree cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_SearchTree cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_SearchTree t0) where
  (=?=) (Choice_C_SearchTree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_SearchTree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_SearchTree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_SearchTree cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_SearchTree cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_SearchTree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_SearchTree cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_SearchTree cd info) _ _ = failCons cd info
  (=?=) (C_Value x1) (C_Value y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Fail x1) (C_Fail y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Or x1 x2) (C_Or y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SearchTree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_SearchTree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_SearchTree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_SearchTree cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_SearchTree cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_SearchTree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_SearchTree cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_SearchTree cd info) _ _ = failCons cd info
  (<?=) (C_Value x1) (C_Value y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Value _) (C_Fail _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Value _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Fail x1) (C_Fail y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Fail _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Or x1 x2) (C_Or y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  readsPrec d s = (readParen False (\r -> [ (C_Nil,r0) | (_,r0) <- readQualified "UnsafeSearchTree" "Nil" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_Cons x1 x2,r2) | (_,r0) <- readQualified "UnsafeSearchTree" "Cons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_FCons x1 x2,r2) | (_,r0) <- readQualified "UnsafeSearchTree" "FCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen False (\r -> [ (C_Abort,r0) | (_,r0) <- readQualified "UnsafeSearchTree" "Abort" r]) s)))


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
  match _ _ _ _ _ _ (Choices_C_AbortList cd i _) = error ("UnsafeSearchTree.AbortList.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AbortList cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AbortList cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_AbortList t0) where
  generate s c = Choices_C_AbortList c (freeID [0,2,2,0] s) [C_Nil,(C_Cons (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_FCons (generate (leftSupply s) c) (generate (rightSupply s) c)),C_Abort]


instance NormalForm t0 => NormalForm (C_AbortList t0) where
  ($!!) cont C_Nil d cs = cont C_Nil d cs
  ($!!) cont (C_Cons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Cons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_FCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_FCons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont C_Abort d cs = cont C_Abort d cs
  ($!!) cont (Choice_C_AbortList cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AbortList cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AbortList cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AbortList cd info) _ _ = failCons cd info
  ($##) cont C_Nil d cs = cont C_Nil d cs
  ($##) cont (C_Cons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Cons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_FCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_FCons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont C_Abort d cs = cont C_Abort d cs
  ($##) cont (Choice_C_AbortList cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AbortList cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AbortList cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AbortList cd info) _ _ = failCons cd info
  searchNF _ cont C_Nil = cont C_Nil
  searchNF search cont (C_Cons x1 x2) = search (\y1 -> search (\y2 -> cont (C_Cons y1 y2)) x2) x1
  searchNF search cont (C_FCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_FCons y1 y2)) x2) x1
  searchNF _ cont C_Abort = cont C_Abort
  searchNF _ _ x = error ("UnsafeSearchTree.AbortList.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_AbortList t0) where
  (=.=) C_Nil C_Nil d cs = C_Success
  (=.=) (C_Cons x1 x2) (C_Cons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_FCons x1 x2) (C_FCons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) C_Abort C_Abort d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Nil C_Nil d cs = C_Success
  (=.<=) (C_Cons x1 x2) (C_Cons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_FCons x1 x2) (C_FCons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) C_Abort C_Abort d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Nil = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_Cons x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_FCons x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i C_Abort = ((i :=: (ChooseN 3 0)):(concat []))
  bind d i (Choice_C_AbortList cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AbortList cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AbortList cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AbortList cd i _) = error ("UnsafeSearchTree.AbortList.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AbortList cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AbortList cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Nil = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_Cons x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_FCons x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i C_Abort = [(i :=: (ChooseN 3 0))]
  lazyBind d i (Choice_C_AbortList cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AbortList cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AbortList cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AbortList cd i _) = error ("UnsafeSearchTree.AbortList.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AbortList cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AbortList cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_AbortList t0) where
  (=?=) (Choice_C_AbortList cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AbortList cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AbortList cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AbortList cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AbortList cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AbortList cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AbortList cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AbortList cd info) _ _ = failCons cd info
  (=?=) C_Nil C_Nil d cs = Curry_Prelude.C_True
  (=?=) (C_Cons x1 x2) (C_Cons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_FCons x1 x2) (C_FCons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) C_Abort C_Abort d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_AbortList cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AbortList cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AbortList cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AbortList cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AbortList cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AbortList cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AbortList cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AbortList cd info) _ _ = failCons cd info
  (<?=) C_Nil C_Nil d cs = Curry_Prelude.C_True
  (<?=) C_Nil (C_Cons _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_Nil (C_FCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) C_Nil C_Abort _ _ = Curry_Prelude.C_True
  (<?=) (C_Cons x1 x2) (C_Cons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Cons _ _) (C_FCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Cons _ _) C_Abort _ _ = Curry_Prelude.C_True
  (<?=) (C_FCons x1 x2) (C_FCons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_FCons _ _) C_Abort _ _ = Curry_Prelude.C_True
  (<?=) C_Abort C_Abort d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_isVar :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isVar x1 x3250 x3500 = Curry_Prelude.d_C_maybe Curry_Prelude.C_False (Curry_Prelude.d_C_const Curry_Prelude.C_True) (d_C_lookupVarId x1 x3250 x3500) x3250 x3500

d_C_identicalVars :: Curry_Prelude.Curry t0 => t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_identicalVars x1 x2 x3250 x3500 = Curry_Prelude.d_C_maybe Curry_Prelude.C_False (d_OP_identicalVars_dot___hash_lambda1 x2) (d_C_lookupVarId x1 x3250 x3500) x3250 x3500

d_OP_identicalVars_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_identicalVars_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_C_maybe Curry_Prelude.C_False (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) x2) (d_C_lookupVarId x1 x3250 x3500) x3250 x3500

d_C_varId :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_varId x1 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) Curry_Prelude.d_C_id (d_C_lookupVarId x1 x3250 x3500) x3250 x3500

d_C_getSearchTree :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (C_SearchTree t0)
d_C_getSearchTree x1 x3250 x3500 = Curry_Prelude.d_C_return (d_C_someSearchTree x1 x3250 x3500) x3250 x3500

d_C_isDefined :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isDefined x1 x3250 x3500 = d_OP_isDefined_dot_hasValue_dot_11 (d_C_someSearchTree x1 x3250 x3500) x3250 x3500

d_OP_isDefined_dot_hasValue_dot_11 :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isDefined_dot_hasValue_dot_11 x1 x3250 x3500 = case x1 of
     (C_Value x2) -> Curry_Prelude.C_True
     (C_Fail x3) -> Curry_Prelude.C_False
     (C_Or x4 x5) -> Curry_Prelude.d_OP_bar_bar (d_OP_isDefined_dot_hasValue_dot_11 x4 x3250 x3500) (d_OP_isDefined_dot_hasValue_dot_11 x5 x3250 x3500) x3250 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isDefined_dot_hasValue_dot_11 x1002 x3250 x3500) (d_OP_isDefined_dot_hasValue_dot_11 x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isDefined_dot_hasValue_dot_11 z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isDefined_dot_hasValue_dot_11 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSearchTree :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSearchTree x1 x3250 x3500 = let
     x2 = d_OP_showSearchTree_dot_showChar_dot_23 (Curry_Prelude.C_Char '\n'#) x3250 x3500
      in (Curry_Prelude.d_C_apply (d_OP_showSearchTree_dot_showsST_dot_23 x2 Curry_Prelude.OP_List x1 x3250 x3500) Curry_Prelude.OP_List x3250 x3500)

d_OP_showSearchTree_dot_showChar_dot_23 :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_showSearchTree_dot_showChar_dot_23 x1 x3250 x3500 = acceptCs id (Curry_Prelude.OP_Cons x1)

nd_OP_showSearchTree_dot_showChar_dot_23 :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_showSearchTree_dot_showChar_dot_23 x1 x3000 x3250 x3500 = wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x1))

d_OP_showSearchTree_dot_showString_dot_23 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_showSearchTree_dot_showString_dot_23 x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x1

nd_OP_showSearchTree_dot_showString_dot_23 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_showSearchTree_dot_showString_dot_23 x1 x3000 x3250 x3500 = wrapDX id (Curry_Prelude.d_OP_plus_plus x1)

d_OP_showSearchTree_dot_indent_dot_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_indent_dot_23 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_id
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showString_dot_23 (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_concatMap d_OP_showSearchTree_dot_indent_dot_23_dot_showIndent_dot_34 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_23 (d_OP__case_7 x2 x3250 x3500) x3250 x3500) (d_OP_showSearchTree_dot_showString_dot_23 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 9472#)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showSearchTree_dot_indent_dot_23 x1002 x3250 x3500) (d_OP_showSearchTree_dot_indent_dot_23 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showSearchTree_dot_indent_dot_23 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showSearchTree_dot_indent_dot_23 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_showSearchTree_dot_indent_dot_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_indent_dot_23 x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2008 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2008 (seq x2012 (Curry_Prelude.nd_OP_dot (let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (nd_OP_showSearchTree_dot_showString_dot_23 (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dollar (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showSearchTree_dot_indent_dot_23_dot_showIndent_dot_34) x2000 x3250 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2001 x3250 x3500) x3 x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))) (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2009 = leftSupply x2013
                              x2010 = rightSupply x2013
                               in (seq x2009 (seq x2010 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showChar_dot_23 (d_OP__case_7 x2 x3250 x3500) x2009 x3250 x3500) (nd_OP_showSearchTree_dot_showString_dot_23 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 9472#)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x2010 x3250 x3500) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showSearchTree_dot_indent_dot_23 x1002 x3000 x3250 x3500) (nd_OP_showSearchTree_dot_indent_dot_23 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showSearchTree_dot_indent_dot_23 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showSearchTree_dot_indent_dot_23 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showSearchTree_dot_indent_dot_23_dot_showIndent_dot_34 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_indent_dot_23_dot_showIndent_dot_34 x1 x3250 x3500 = Curry_Prelude.OP_Cons (d_OP__case_6 x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))

d_OP_showSearchTree_dot_shows_dot_23 :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_shows_dot_23 x1 x3250 x3500 = d_OP_showSearchTree_dot_showString_dot_23 (Curry_Prelude.d_C_show x1 x3250 x3500) x3250 x3500

nd_OP_showSearchTree_dot_shows_dot_23 :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_shows_dot_23 x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_showSearchTree_dot_showString_dot_23 (Curry_Prelude.d_C_show x1 x3250 x3500) x2000 x3250 x3500))

d_OP_showSearchTree_dot_showsST_dot_23 :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x3 x3250 x3500 = case x3 of
     (C_Value x4) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_23 x2 x3250 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_shows_dot_23 x4 x3250 x3500) x1 x3250 x3500) x3250 x3500
     (C_Fail x5) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_23 x2 x3250 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_23 (Curry_Prelude.C_Char '!'#) x3250 x3500) x1 x3250 x3500) x3250 x3500
     (C_Or x6 x7) -> Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_indent_dot_23 x2 x3250 x3500) (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showChar_dot_23 (Curry_Prelude.C_Char '?'#) x3250 x3500) (Curry_Prelude.d_OP_dot x1 (Curry_Prelude.d_OP_dot (d_OP_showSearchTree_dot_showsST_dot_23 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_False x2) x6 x3250 x3500) (d_OP_showSearchTree_dot_showsST_dot_23 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_True x2) x7 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1002 x3250 x3500) (d_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showSearchTree_dot_showsST_dot_23 x1 x2 z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_showSearchTree_dot_showsST_dot_23 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Bool -> C_SearchTree t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (C_Value x4) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_23 x2 x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_shows_dot_23 x4 x2001 x3250 x3500) x1 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     (C_Fail x5) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_23 x2 x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showChar_dot_23 (Curry_Prelude.C_Char '!'#) x2001 x3250 x3500) x1 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     (C_Or x6 x7) -> let
          x2013 = x3000
           in (seq x2013 (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2000 = leftSupply x2014
                    x2010 = rightSupply x2014
                     in (seq x2000 (seq x2010 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_indent_dot_23 x2 x2000 x3250 x3500) (let
                         x2009 = leftSupply x2010
                         x2011 = rightSupply x2010
                          in (seq x2009 (seq x2011 (let
                              x2001 = leftSupply x2011
                              x2008 = rightSupply x2011
                               in (seq x2001 (seq x2008 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showChar_dot_23 (Curry_Prelude.C_Char '?'#) x2001 x3250 x3500) (let
                                   x2007 = leftSupply x2008
                                   x2005 = rightSupply x2008
                                    in (seq x2007 (seq x2005 (Curry_Prelude.nd_OP_dot x1 (let
                                        x2004 = leftSupply x2005
                                        x2006 = rightSupply x2005
                                         in (seq x2004 (seq x2006 (let
                                             x2002 = leftSupply x2006
                                             x2003 = rightSupply x2006
                                              in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_dot (nd_OP_showSearchTree_dot_showsST_dot_23 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_False x2) x6 x2002 x3250 x3500) (nd_OP_showSearchTree_dot_showsST_dot_23 x1 (Curry_Prelude.OP_Cons Curry_Prelude.C_True x2) x7 x2003 x3250 x3500) x2004 x3250 x3500))))))) x2007 x3250 x3500)))) x2009 x3250 x3500))))))) x2012 x3250 x3500))))))))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showSearchTree_dot_showsST_dot_23 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showSearchTree_dot_showsST_dot_23 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_searchTreeSize :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int
d_C_searchTreeSize x1 x3250 x3500 = case x1 of
     (C_Value x2) -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.C_Int 1#) (Curry_Prelude.C_Int 0#) (Curry_Prelude.C_Int 0#)
     (C_Fail x3) -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.C_Int 0#) (Curry_Prelude.C_Int 1#) (Curry_Prelude.C_Int 0#)
     (C_Or x4 x5) -> let
          x6 = d_C_searchTreeSize x4 x3250 x3500
          x7 = d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x6 x3250 x3500
          x8 = d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x6 x3250 x3500
          x9 = d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x6 x3250 x3500
          x10 = d_C_searchTreeSize x5 x3250 x3500
          x11 = d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x10 x3250 x3500
          x12 = d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x10 x3250 x3500
          x13 = d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x10 x3250 x3500
           in (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_OP_plus x7 x11 x3250 x3500) (Curry_Prelude.d_OP_plus x8 x12 x3250 x3500) (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x9 x13 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_searchTreeSize x1002 x3250 x3500) (d_C_searchTreeSize x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_searchTreeSize z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_searchTreeSize x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP6_hash_v1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP7_hash_f1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP8_hash_o1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP3_hash_v2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP4_hash_f2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1002 x3250 x3500) (d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_searchTreeSize_dot___hash_selFP5_hash_o2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_allValuesDFS :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesDFS x3250 x3500 = Curry_Prelude.d_OP_dot Curry_ValueSequence.d_C_vsToList d_C_dfsStrategy x3250 x3500

nd_C_allValuesDFS :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (C_SearchTree t0) (Curry_Prelude.OP_List t0)
nd_C_allValuesDFS x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_ValueSequence.d_C_vsToList) (wrapDX id d_C_dfsStrategy) x2000 x3250 x3500))

d_C_dfsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_dfsStrategy x1 x3250 x3500 = case x1 of
     (C_Fail x2) -> Curry_ValueSequence.d_C_failVS x2 x3250 x3500
     (C_Value x3) -> Curry_ValueSequence.d_C_addVS x3 (Curry_ValueSequence.d_C_emptyVS x3250 x3500) x3250 x3500
     (C_Or x4 x5) -> Curry_ValueSequence.d_OP_bar_plus_plus_bar (d_C_dfsStrategy x4 x3250 x3500) (d_C_dfsStrategy x5 x3250 x3500) x3250 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dfsStrategy x1002 x3250 x3500) (d_C_dfsStrategy x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dfsStrategy z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dfsStrategy x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_allValuesBFS :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesBFS x1 x3250 x3500 = Curry_ValueSequence.d_C_vsToList (d_C_bfsStrategy x1 x3250 x3500) x3250 x3500

d_C_children :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_SearchTree t0)
d_C_children x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_5 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_children x1002 x3250 x3500) (d_C_children x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_children z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_children x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_values :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_values x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_ValueSequence.d_C_emptyVS x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_values x1002 x3250 x3500) (d_C_values x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_values z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_values x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_allBFS :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_allBFS x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_ValueSequence.d_C_emptyVS x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_ValueSequence.d_OP_bar_plus_plus_bar (d_C_values (Curry_Prelude.OP_Cons x2 x3) x3250 x3500) (d_C_allBFS (d_C_children (Curry_Prelude.OP_Cons x2 x3) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allBFS x1002 x3250 x3500) (d_C_allBFS x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allBFS z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allBFS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_bfsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_bfsStrategy x1 x3250 x3500 = d_C_allBFS (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500

d_C_defIDSDepth :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_defIDSDepth x3250 x3500 = Curry_Prelude.C_Int 100#

d_C_defIDSInc :: Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_defIDSInc x3250 x3500 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#)

nd_C_defIDSInc :: IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int
nd_C_defIDSInc x3000 x3250 x3500 = wrapDX id (Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#))

d_C_allValuesIDS :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesIDS x1 x3250 x3500 = Curry_Prelude.d_C_apply (d_C_allValuesIDSwith (d_C_defIDSDepth x3250 x3500) (d_C_defIDSInc x3250 x3500) x3250 x3500) x1 x3250 x3500

d_C_idsStrategy :: Curry_Prelude.Curry t0 => C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_idsStrategy x1 x3250 x3500 = d_C_idsStrategyWith (d_C_defIDSDepth x3250 x3500) (d_C_defIDSInc x3250 x3500) x1 x3250 x3500

d_C_allValuesIDSwith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> Cover -> ConstStore -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_allValuesIDSwith x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_ValueSequence.d_C_vsToList (d_C_idsStrategyWith x1 x2) x3250 x3500

nd_C_allValuesIDSwith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func (C_SearchTree t0) (Curry_Prelude.OP_List t0)
nd_C_allValuesIDSwith x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_ValueSequence.d_C_vsToList) (wrapNX id (nd_C_idsStrategyWith x1 x2)) x2000 x3250 x3500))

d_C_idsStrategyWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_C_idsStrategyWith x1 x2 x3 x3250 x3500 = d_OP_idsStrategyWith_dot_iterIDS_dot_115 x2 x3 x1 (d_C_collectInBounds (Curry_Prelude.C_Int 0#) x1 x3 x3250 x3500) x3250 x3500

nd_C_idsStrategyWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> C_SearchTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
nd_C_idsStrategyWith x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x2 x3 x1 (d_C_collectInBounds (Curry_Prelude.C_Int 0#) x1 x3 x3250 x3500) x2000 x3250 x3500))

d_OP_idsStrategyWith_dot_iterIDS_dot_115 :: Curry_Prelude.Curry t0 => (Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> C_SearchTree t0 -> Curry_Prelude.C_Int -> C_AbortList t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x4 x3250 x3500 = case x4 of
     C_Nil -> Curry_ValueSequence.d_C_emptyVS x3250 x3500
     (C_Cons x5 x6) -> Curry_ValueSequence.d_C_addVS x5 (d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x6 x3250 x3500) x3250 x3500
     (C_FCons x7 x8) -> Curry_ValueSequence.d_OP_bar_plus_plus_bar (Curry_ValueSequence.d_C_failVS x7 x3250 x3500) (d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x8 x3250 x3500) x3250 x3500
     C_Abort -> let
          x9 = Curry_Prelude.d_C_apply x1 x3 x3250 x3500
           in (d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x9 (d_C_collectInBounds x3 x9 x2 x3250 x3500) x3250 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1002 x3250 x3500) (d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1003 x3250 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 z x3250 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_idsStrategyWith_dot_iterIDS_dot_115 :: Curry_Prelude.Curry t0 => Func Curry_Prelude.C_Int Curry_Prelude.C_Int -> C_SearchTree t0 -> Curry_Prelude.C_Int -> C_AbortList t0 -> IDSupply -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     C_Nil -> Curry_ValueSequence.d_C_emptyVS x3250 x3500
     (C_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_ValueSequence.d_C_addVS x5 (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x6 x2000 x3250 x3500) x3250 x3500))
     (C_FCons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_ValueSequence.d_OP_bar_plus_plus_bar (Curry_ValueSequence.d_C_failVS x7 x3250 x3500) (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x8 x2000 x3250 x3500) x3250 x3500))
     C_Abort -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x9 = Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500
                     in (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x9 (d_C_collectInBounds x3 x9 x2 x3250 x3500) x2001 x3250 x3500))))))
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_idsStrategyWith_dot_iterIDS_dot_115 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_collectInBounds :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_SearchTree t0 -> Cover -> ConstStore -> C_AbortList t0
d_C_collectInBounds x1 x2 x3 x3250 x3500 = d_OP_collectInBounds_dot_collectLevel_dot_129 x2 x1 x2 x3 x3250 x3500

d_OP_collectInBounds_dot_collectLevel_dot_129 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_SearchTree t0 -> Cover -> ConstStore -> C_AbortList t0
d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (C_Fail x5) -> d_OP__case_3 x2 x1 x3 x5 (Curry_Prelude.d_OP_lt x3 (Curry_Prelude.d_OP_minus x1 x2 x3250 x3500) x3250 x3500) x3250 x3500
     (C_Value x6) -> d_OP__case_2 x2 x1 x3 x6 (Curry_Prelude.d_OP_lt x3 (Curry_Prelude.d_OP_minus x1 x2 x3250 x3500) x3250 x3500) x3250 x3500
     (C_Or x7 x8) -> d_OP__case_1 x3 x8 x2 x1 x7 (Curry_Prelude.d_OP_gt x3 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 x3 x1002 x3250 x3500) (d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 x3 x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 x3 z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_concA :: Curry_Prelude.Curry t0 => C_AbortList t0 -> C_AbortList t0 -> Cover -> ConstStore -> C_AbortList t0
d_C_concA x1 x2 x3250 x3500 = case x1 of
     C_Abort -> d_OP__case_0 x2 x3250 x3500
     C_Nil -> x2
     (C_Cons x7 x8) -> C_Cons x7 (d_C_concA x8 x2 x3250 x3500)
     (C_FCons x9 x10) -> C_FCons x9 (d_C_concA x10 x2 x3250 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_concA x1002 x2 x3250 x3500) (d_C_concA x1003 x2 x3250 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_concA z x2 x3250 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_concA x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getAllValuesWith :: Curry_Prelude.Curry t0 => (C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_C_getAllValuesWith x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getSearchTree x2 x3250 x3500) (d_OP_getAllValuesWith_dot___hash_lambda3 x1) x3250 x3500

nd_C_getAllValuesWith :: Curry_Prelude.Curry t0 => Func (C_SearchTree t0) (Curry_ValueSequence.C_ValueSequence t0) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_C_getAllValuesWith x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getSearchTree x2 x3250 x3500) (wrapNX id (nd_OP_getAllValuesWith_dot___hash_lambda3 x1)) x2000 x3250 x3500))

d_OP_getAllValuesWith_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => (C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0) -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_OP_getAllValuesWith_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_ValueSequence.d_C_vsToList (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) x3250 x3500) x3250 x3500

nd_OP_getAllValuesWith_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Func (C_SearchTree t0) (Curry_ValueSequence.C_ValueSequence t0) -> C_SearchTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_OP_getAllValuesWith_dot___hash_lambda3 x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_ValueSequence.d_C_vsToList (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) x3250 x3500) x3250 x3500))

d_C_someValue :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0
d_C_someValue x3250 x3500 = d_C_someValueWith d_C_bfsStrategy

nd_C_someValue :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func t0 t0
nd_C_someValue x3000 x3250 x3500 = wrapNX id (nd_C_someValueWith (wrapDX id d_C_bfsStrategy))

d_C_someValueWith :: Curry_Prelude.Curry t0 => (C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0) -> t0 -> Cover -> ConstStore -> t0
d_C_someValueWith x1 x2 x3250 x3500 = Curry_Prelude.d_C_head (Curry_ValueSequence.d_C_vsToList (Curry_Prelude.d_C_apply x1 (d_C_someSearchTree x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_someValueWith :: Curry_Prelude.Curry t0 => Func (C_SearchTree t0) (Curry_ValueSequence.C_ValueSequence t0) -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_someValueWith x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_head (Curry_ValueSequence.d_C_vsToList (Curry_Prelude.nd_C_apply x1 (d_C_someSearchTree x2 x3250 x3500) x2000 x3250 x3500) x3250 x3500) x3250 x3500))

d_OP__case_0 :: Curry_Prelude.Curry t0 => C_AbortList t0 -> Cover -> ConstStore -> C_AbortList t0
d_OP__case_0 x2 x3250 x3500 = case x2 of
     C_Abort -> C_Abort
     C_Nil -> C_Abort
     (C_Cons x3 x4) -> C_Cons x3 (d_C_concA C_Abort x4 x3250 x3500)
     (C_FCons x5 x6) -> C_FCons x5 (d_C_concA C_Abort x6 x3250 x3500)
     (Choice_C_AbortList x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Choices_C_AbortList x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Guard_C_AbortList x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AbortList x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> C_SearchTree t0 -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_SearchTree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_AbortList t0
d_OP__case_1 x3 x8 x2 x1 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_concA (d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) x7 x3250 x3500) (d_OP_collectInBounds_dot_collectLevel_dot_129 x1 x2 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) x8 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> C_Abort
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x8 x2 x1 x7 x1002 x3250 x3500) (d_OP__case_1 x3 x8 x2 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x8 x2 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x8 x2 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_AbortList t0
d_OP__case_2 x2 x1 x3 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> C_Cons x6 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x1 x3 x6 x1002 x3250 x3500) (d_OP__case_2 x2 x1 x3 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x1 x3 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x1 x3 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_AbortList t0
d_OP__case_3 x2 x1 x3 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> C_FCons x5 C_Nil
     Curry_Prelude.C_False -> C_Nil
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1 x3 x5 x1002 x3250 x3500) (d_OP__case_3 x2 x1 x3 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 x1 x3 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_ValueSequence.C_ValueSequence t0
d_OP__case_4 x3 x2 x3250 x3500 = case x2 of
     (C_Fail x4) -> Curry_ValueSequence.d_OP_bar_plus_plus_bar (Curry_ValueSequence.d_C_failVS x4 x3250 x3500) (d_C_values x3 x3250 x3500) x3250 x3500
     (C_Value x5) -> Curry_ValueSequence.d_C_addVS x5 (d_C_values x3 x3250 x3500) x3250 x3500
     (C_Or x6 x7) -> d_C_values x3 x3250 x3500
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x1002 x3250 x3500) (d_OP__case_4 x3 x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_SearchTree t0) -> C_SearchTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_SearchTree t0)
d_OP__case_5 x3 x2 x3250 x3500 = case x2 of
     (C_Fail x4) -> d_C_children x3 x3250 x3500
     (C_Value x5) -> d_C_children x3 x3250 x3500
     (C_Or x6 x7) -> Curry_Prelude.OP_Cons x6 (Curry_Prelude.OP_Cons x7 (d_C_children x3 x3250 x3500))
     (Choice_C_SearchTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x1002 x3250 x3500) (d_OP__case_5 x3 x1003 x3250 x3500)
     (Choices_C_SearchTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 z x3250 x3500) x1002
     (Guard_C_SearchTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SearchTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Char
d_OP__case_6 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char ' '#
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9474#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Char
d_OP__case_7 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Char (nonAsciiChr 9492#)
     Curry_Prelude.C_False -> Curry_Prelude.C_Char (nonAsciiChr 9500#)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lookupVarId :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int
d_C_lookupVarId x1 x3250 x3500 = external_d_C_lookupVarId x1 x3250 x3500

d_C_someSearchTree :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> C_SearchTree t0
d_C_someSearchTree x1 x3250 x3500 = external_d_C_someSearchTree x1 x3250 x3500
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
  var x _          = x

external_d_C_someSearchTree :: NormalForm a => a -> Cover -> ConstStore -> C_SearchTree a
external_d_C_someSearchTree = encapsulatedSearch


external_d_C_lookupVarId :: Basics.NonDet a => a -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.C_Int)
external_d_C_lookupVarId x _ _ = case try x of
  Free _ i _   -> Curry_Prelude.C_Just (Basics.toCurry (Basics.getKey i))
  _            -> Curry_Prelude.C_Nothing


