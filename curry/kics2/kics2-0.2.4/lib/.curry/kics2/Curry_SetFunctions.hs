{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}



module Curry_SetFunctions (C_Values, d_C_set0, d_C_set0With, nd_C_set0With, d_C_set1, nd_C_set1, d_C_set1With, nd_C_set1With, d_C_set2, nd_C_set2, d_C_set2With, nd_C_set2With, d_C_set3, nd_C_set3, d_C_set3With, nd_C_set3With, d_C_set4, nd_C_set4, d_C_set4With, nd_C_set4With, d_C_set5, nd_C_set5, d_C_set5With, nd_C_set5With, d_C_set6, nd_C_set6, d_C_set6With, nd_C_set6With, d_C_set7, nd_C_set7, d_C_set7With, nd_C_set7With, d_C_isEmpty, d_C_valueOf, nd_C_choose, nd_C_chooseValue, d_C_select, d_C_selectValue, d_C_mapValues, nd_C_mapValues, d_C_foldValues, nd_C_foldValues, d_C_minValue, nd_C_minValue, d_C_maxValue, nd_C_maxValue, d_C_values2list, d_C_printValues, d_C_sortValues, nd_C_sortValues, d_C_sortValuesBy, nd_C_sortValuesBy) where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_SearchTree
import qualified Curry_Sort
data C_Values t0
     = C_Values (Curry_Prelude.OP_List t0)
     | Choice_C_Values Cover ID (C_Values t0) (C_Values t0)
     | Choices_C_Values Cover ID ([C_Values t0])
     | Fail_C_Values Cover FailInfo
     | Guard_C_Values Cover Constraints (C_Values t0)

instance Show t0 => Show (C_Values t0) where
  showsPrec d (Choice_C_Values cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Values cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Values cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Values cd info) = showChar '!'
  showsPrec _ (C_Values x1) = (showString "(Values") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_Values t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Values x1,r1) | (_,r0) <- readQualified "SetFunctions" "Values" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet (C_Values t0) where
  choiceCons = Choice_C_Values
  choicesCons = Choices_C_Values
  failCons = Fail_C_Values
  guardCons = Guard_C_Values
  try (Choice_C_Values cd i x y) = tryChoice cd i x y
  try (Choices_C_Values cd i xs) = tryChoices cd i xs
  try (Fail_C_Values cd info) = Fail cd info
  try (Guard_C_Values cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Values cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Values cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Values cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Values cd i _) = error ("SetFunctions.Values.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Values cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Values cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Values t0) where
  generate s = Choices_C_Values defCover (freeID [1] s) [(C_Values (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_Values t0) where
  ($!!) cont (C_Values x1) cs = ((\y1 cs -> cont (C_Values y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Values cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Values cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Values cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Values cd info) _ = failCons cd info
  ($##) cont (C_Values x1) cs = ((\y1 cs -> cont (C_Values y1) cs) $## x1) cs
  ($##) cont (Choice_C_Values cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Values cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Values cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Values cd info) _ = failCons cd info
  searchNF search cont (C_Values x1) = search (\y1 -> cont (C_Values y1)) x1
  searchNF _ _ x = error ("SetFunctions.Values.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Values t0) where
  (=.=) (C_Values x1) (C_Values y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Values x1) (C_Values y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Values x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Values cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Values cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Values cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Values cd i _) = error ("SetFunctions.Values.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Values cd info) = [(Unsolvable info)]
  bind i (Guard_C_Values cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Values x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Values cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Values cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Values cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Values cd i _) = error ("SetFunctions.Values.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Values cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Values cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Values t0) where
  (=?=) (Choice_C_Values cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Values cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Values cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Values cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Values cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Values cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Values cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Values cd info) _ = failCons cd info
  (=?=) (C_Values x1) (C_Values y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (<?=) (Choice_C_Values cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Values cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Values cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Values cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Values cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Values cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Values cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Values cd info) _ = failCons cd info
  (<?=) (C_Values x1) (C_Values y1) cs = (x1 Curry_Prelude.<?= y1) cs


instance Coverable t0 => Coverable (C_Values t0) where
  cover (C_Values x1) = C_Values (cover x1)
  cover (Choice_C_Values cd i x y) = Choice_C_Values (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Values cd i xs) = Choices_C_Values (incCover cd) i (map cover xs)
  cover (Fail_C_Values cd info) = Fail_C_Values (incCover cd) info
  cover (Guard_C_Values cd c e) = Guard_C_Values (incCover cd) c (cover e)


d_C_set0 :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> C_Values t0
d_C_set0 x1 x3500 = d_C_set0With Curry_SearchTree.d_C_dfsStrategy x1 x3500

d_C_set0With :: Curry_Prelude.Curry t0 => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> t0 -> ConstStore -> C_Values t0
d_C_set0With x1 x2 x3500 = d_C_allVs x1 x2 x3500

nd_C_set0With :: Curry_Prelude.Curry t0 => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> t0 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set0With x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_allVs x1 x2 x2000 x3500))

d_C_set1 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t1) -> t0 -> ConstStore -> C_Values t1
d_C_set1 x1 x2 x3500 = d_C_set1With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3500

nd_C_set1 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> t0 -> IDSupply -> ConstStore -> C_Values t1
nd_C_set1 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set1With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x2000 x3500))

d_C_set1With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t0) -> t1 -> ConstStore -> C_Values t0
d_C_set1With x1 x2 x3 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) x3500

nd_C_set1With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 t0 -> t1 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set1With x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_allVs x1 (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) x2001 x3500)))))

d_C_set2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> t2) -> t0 -> t1 -> ConstStore -> C_Values t2
d_C_set2 x1 x2 x3 x3500 = d_C_set2With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x3500

nd_C_set2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 (Func t1 t2) -> t0 -> t1 -> IDSupply -> ConstStore -> C_Values t2
nd_C_set2 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set2With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x2000 x3500))

d_C_set2With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t0) -> t1 -> t2 -> ConstStore -> C_Values t0
d_C_set2With x1 x2 x3 x4 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) x3500

nd_C_set2With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 t0) -> t1 -> t2 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set2With x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (nd_C_allVs x1 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) x2003 x3500)))))

d_C_set3 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3) -> t0 -> t1 -> t2 -> ConstStore -> C_Values t3
d_C_set3 x1 x2 x3 x4 x3500 = d_C_set3With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x4 x3500

nd_C_set3 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Func t0 (Func t1 (Func t2 t3)) -> t0 -> t1 -> t2 -> IDSupply -> ConstStore -> C_Values t3
nd_C_set3 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set3With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x4 x2000 x3500))

d_C_set3With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t0) -> t1 -> t2 -> t3 -> ConstStore -> C_Values t0
d_C_set3With x1 x2 x3 x4 x5 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) (d_C_cover x5 x3500) x3500) x3500

nd_C_set3With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 (Func t3 t0)) -> t1 -> t2 -> t3 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set3With x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (nd_C_allVs x1 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) (d_C_cover x5 x3500) x2003 x3500)))) x2005 x3500)))))

d_C_set4 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4) -> t0 -> t1 -> t2 -> t3 -> ConstStore -> C_Values t4
d_C_set4 x1 x2 x3 x4 x5 x3500 = d_C_set4With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x4 x5 x3500

nd_C_set4 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => Func t0 (Func t1 (Func t2 (Func t3 t4))) -> t0 -> t1 -> t2 -> t3 -> IDSupply -> ConstStore -> C_Values t4
nd_C_set4 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set4With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x4 x5 x2000 x3500))

d_C_set4With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t0) -> t1 -> t2 -> t3 -> t4 -> ConstStore -> C_Values t0
d_C_set4With x1 x2 x3 x4 x5 x6 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) (d_C_cover x5 x3500) x3500) (d_C_cover x6 x3500) x3500) x3500

nd_C_set4With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 (Func t3 (Func t4 t0))) -> t1 -> t2 -> t3 -> t4 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set4With x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (nd_C_allVs x1 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) (d_C_cover x5 x3500) x2003 x3500)))) (d_C_cover x6 x3500) x2005 x3500)))) x2007 x3500)))))

d_C_set5 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5) -> t0 -> t1 -> t2 -> t3 -> t4 -> ConstStore -> C_Values t5
d_C_set5 x1 x2 x3 x4 x5 x6 x3500 = d_C_set5With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x4 x5 x6 x3500

nd_C_set5 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5)))) -> t0 -> t1 -> t2 -> t3 -> t4 -> IDSupply -> ConstStore -> C_Values t5
nd_C_set5 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set5With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x4 x5 x6 x2000 x3500))

d_C_set5With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5 -> ConstStore -> t0) -> t1 -> t2 -> t3 -> t4 -> t5 -> ConstStore -> C_Values t0
d_C_set5With x1 x2 x3 x4 x5 x6 x7 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) (d_C_cover x5 x3500) x3500) (d_C_cover x6 x3500) x3500) (d_C_cover x7 x3500) x3500) x3500

nd_C_set5With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t0)))) -> t1 -> t2 -> t3 -> t4 -> t5 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set5With x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (nd_C_allVs x1 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) (d_C_cover x5 x3500) x2003 x3500)))) (d_C_cover x6 x3500) x2005 x3500)))) (d_C_cover x7 x3500) x2007 x3500)))) x2009 x3500)))))

d_C_set6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5 -> ConstStore -> t6) -> t0 -> t1 -> t2 -> t3 -> t4 -> t5 -> ConstStore -> C_Values t6
d_C_set6 x1 x2 x3 x4 x5 x6 x7 x3500 = d_C_set6With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x4 x5 x6 x7 x3500

nd_C_set6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6))))) -> t0 -> t1 -> t2 -> t3 -> t4 -> t5 -> IDSupply -> ConstStore -> C_Values t6
nd_C_set6 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set6With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x4 x5 x6 x7 x2000 x3500))

d_C_set6With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5 -> ConstStore -> t6 -> ConstStore -> t0) -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> ConstStore -> C_Values t0
d_C_set6With x1 x2 x3 x4 x5 x6 x7 x8 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) (d_C_cover x5 x3500) x3500) (d_C_cover x6 x3500) x3500) (d_C_cover x7 x3500) x3500) (d_C_cover x8 x3500) x3500) x3500

nd_C_set6With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 (Func t3 (Func t4 (Func t5 (Func t6 t0))))) -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set6With x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2012 = x3000
      in (seq x2012 (let
          x2011 = leftSupply x2012
          x2010 = rightSupply x2012
           in (seq x2011 (seq x2010 (nd_C_allVs x1 (let
               x2009 = leftSupply x2010
               x2008 = rightSupply x2010
                in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
                    x2007 = leftSupply x2008
                    x2006 = rightSupply x2008
                     in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                         x2005 = leftSupply x2006
                         x2004 = rightSupply x2006
                          in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) (d_C_cover x5 x3500) x2003 x3500)))) (d_C_cover x6 x3500) x2005 x3500)))) (d_C_cover x7 x3500) x2007 x3500)))) (d_C_cover x8 x3500) x2009 x3500)))) x2011 x3500)))))

d_C_set7 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7) => (t0 -> ConstStore -> t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5 -> ConstStore -> t6 -> ConstStore -> t7) -> t0 -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> ConstStore -> C_Values t7
d_C_set7 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = d_C_set7With Curry_SearchTree.d_C_dfsStrategy x1 x2 x3 x4 x5 x6 x7 x8 x3500

nd_C_set7 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 (Func t6 t7)))))) -> t0 -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IDSupply -> ConstStore -> C_Values t7
nd_C_set7 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_set7With (wrapDX id Curry_SearchTree.d_C_dfsStrategy) x1 x2 x3 x4 x5 x6 x7 x8 x2000 x3500))

d_C_set7With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t0) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t0) -> (t1 -> ConstStore -> t2 -> ConstStore -> t3 -> ConstStore -> t4 -> ConstStore -> t5 -> ConstStore -> t6 -> ConstStore -> t7 -> ConstStore -> t0) -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> ConstStore -> C_Values t0
d_C_set7With x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = d_C_allVs x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 (d_C_cover x3 x3500) x3500) (d_C_cover x4 x3500) x3500) (d_C_cover x5 x3500) x3500) (d_C_cover x6 x3500) x3500) (d_C_cover x7 x3500) x3500) (d_C_cover x8 x3500) x3500) (d_C_cover x9 x3500) x3500) x3500

nd_C_set7With :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t0) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t0) -> Func t1 (Func t2 (Func t3 (Func t4 (Func t5 (Func t6 (Func t7 t0)))))) -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> IDSupply -> ConstStore -> C_Values t0
nd_C_set7With x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = let
     x2014 = x3000
      in (seq x2014 (let
          x2013 = leftSupply x2014
          x2012 = rightSupply x2014
           in (seq x2013 (seq x2012 (nd_C_allVs x1 (let
               x2011 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2011 (seq x2010 (Curry_Prelude.nd_C_apply (let
                    x2009 = leftSupply x2010
                    x2008 = rightSupply x2010
                     in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
                         x2007 = leftSupply x2008
                         x2006 = rightSupply x2008
                          in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                              x2005 = leftSupply x2006
                              x2004 = rightSupply x2006
                               in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                                   x2003 = leftSupply x2004
                                   x2002 = rightSupply x2004
                                    in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 (d_C_cover x3 x3500) x2000 x3500) (d_C_cover x4 x3500) x2001 x3500)))) (d_C_cover x5 x3500) x2003 x3500)))) (d_C_cover x6 x3500) x2005 x3500)))) (d_C_cover x7 x3500) x2007 x3500)))) (d_C_cover x8 x3500) x2009 x3500)))) (d_C_cover x9 x3500) x2011 x3500)))) x2013 x3500)))))

d_C_allVs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (Curry_SearchTree.C_SearchTree t0 -> ConstStore -> Curry_SearchTree.C_ValueSequence t1) -> t0 -> ConstStore -> C_Values t1
d_C_allVs x1 x2 x3500 = C_Values (Curry_SearchTree.d_C_vsToList (Curry_Prelude.d_C_apply x1 (Curry_SearchTree.d_C_someSearchTree x2 x3500) x3500) x3500)

nd_C_allVs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func (Curry_SearchTree.C_SearchTree t0) (Curry_SearchTree.C_ValueSequence t1) -> t0 -> IDSupply -> ConstStore -> C_Values t1
nd_C_allVs x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (C_Values (Curry_SearchTree.d_C_vsToList (Curry_Prelude.nd_C_apply x1 (Curry_SearchTree.d_C_someSearchTree x2 x3500) x2000 x3500) x3500)))

d_C_isEmpty :: Curry_Prelude.Curry t0 => C_Values t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmpty x1 x3500 = case x1 of
     (C_Values x2) -> Curry_Prelude.d_C_null x2 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isEmpty x1002 x3500) (d_C_isEmpty x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isEmpty z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isEmpty x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_valueOf :: Curry_Prelude.Curry t0 => t0 -> C_Values t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_valueOf x1 x2 x3500 = case x2 of
     (C_Values x3) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) x3 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_valueOf x1 x1002 x3500) (d_C_valueOf x1 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_valueOf x1 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_valueOf x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_choose :: Curry_Prelude.Curry t0 => C_Values t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (C_Values t0)
nd_C_choose x1 x3000 x3500 = case x1 of
     (C_Values x2) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2000 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2000 (seq x2005 (let
                    x3 = Curry_Prelude.nd_C_foldr1 (wrapDX (wrapNX id) (acceptCs id Curry_Prelude.nd_OP_qmark)) x2 x2000 x3500
                    x4 = let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_List.nd_C_delete x2001 x3500) x3 x2002 x3500)))) x2 x2004 x3500)))
                     in (Curry_Prelude.OP_Tuple2 x3 (C_Values x4)))))))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_choose x1002 x3000 x3500) (nd_C_choose x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_choose z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_choose x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_chooseValue :: Curry_Prelude.Curry t0 => C_Values t0 -> IDSupply -> ConstStore -> t0
nd_C_chooseValue x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_fst (nd_C_choose x1 x2000 x3500) x3500))

d_C_select :: Curry_Prelude.Curry t0 => C_Values t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (C_Values t0)
d_C_select x1 x3500 = case x1 of
     (C_Values x2) -> d_OP__case_4 x2 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_select x1002 x3500) (d_C_select x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_select z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_select x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_selectValue :: Curry_Prelude.Curry t0 => C_Values t0 -> ConstStore -> t0
d_C_selectValue x1 x3500 = Curry_Prelude.d_C_fst (d_C_select x1 x3500) x3500

d_C_mapValues :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t1) -> C_Values t0 -> ConstStore -> C_Values t1
d_C_mapValues x1 x2 x3500 = case x2 of
     (C_Values x3) -> C_Values (Curry_Prelude.d_C_map x1 x3 x3500)
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mapValues x1 x1002 x3500) (d_C_mapValues x1 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mapValues x1 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mapValues x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_mapValues :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> C_Values t0 -> IDSupply -> ConstStore -> C_Values t1
nd_C_mapValues x1 x2 x3000 x3500 = case x2 of
     (C_Values x3) -> let
          x2000 = x3000
           in (seq x2000 (C_Values (Curry_Prelude.nd_C_map x1 x3 x2000 x3500)))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mapValues x1 x1002 x3000 x3500) (nd_C_mapValues x1 x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mapValues x1 z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mapValues x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_foldValues :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> t0) -> t0 -> C_Values t0 -> ConstStore -> t0
d_C_foldValues x1 x2 x3 x3500 = case x3 of
     (C_Values x4) -> Curry_Prelude.d_C_foldr x1 x2 x4 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldValues x1 x2 x1002 x3500) (d_C_foldValues x1 x2 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldValues x1 x2 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldValues x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldValues :: Curry_Prelude.Curry t0 => Func t0 (Func t0 t0) -> t0 -> C_Values t0 -> IDSupply -> ConstStore -> t0
nd_C_foldValues x1 x2 x3 x3000 x3500 = case x3 of
     (C_Values x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_foldr x1 x2 x4 x2000 x3500))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldValues x1 x2 x1002 x3000 x3500) (nd_C_foldValues x1 x2 x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldValues x1 x2 z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldValues x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_minValue :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> C_Values t0 -> ConstStore -> t0
d_C_minValue x1 x2 x3500 = case x2 of
     (C_Values x3) -> d_OP_minValue_dot_minOf_dot_54 x1 x3 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_minValue x1 x1002 x3500) (d_C_minValue x1 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_minValue x1 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_minValue x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_minValue :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Values t0 -> IDSupply -> ConstStore -> t0
nd_C_minValue x1 x2 x3000 x3500 = case x2 of
     (C_Values x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_minValue_dot_minOf_dot_54 x1 x3 x2000 x3500))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_minValue x1 x1002 x3000 x3500) (nd_C_minValue x1 x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_minValue x1 z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_minValue x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_minValue_dot_minOf_dot_54 :: Curry_Prelude.Curry t356 => (t356 -> ConstStore -> t356 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t356 -> ConstStore -> t356
d_OP_minValue_dot_minOf_dot_54 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_3 x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_minValue_dot_minOf_dot_54 x1 x1002 x3500) (d_OP_minValue_dot_minOf_dot_54 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_minValue_dot_minOf_dot_54 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_minValue_dot_minOf_dot_54 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_minValue_dot_minOf_dot_54 :: Curry_Prelude.Curry t356 => Func t356 (Func t356 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t356 -> IDSupply -> ConstStore -> t356
nd_OP_minValue_dot_minOf_dot_54 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x3 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_minValue_dot_minOf_dot_54 x1 x1002 x3000 x3500) (nd_OP_minValue_dot_minOf_dot_54 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_minValue_dot_minOf_dot_54 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_minValue_dot_minOf_dot_54 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_maxValue :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> C_Values t0 -> ConstStore -> t0
d_C_maxValue x1 x2 x3500 = case x2 of
     (C_Values x3) -> d_OP_maxValue_dot_maxOf_dot_62 x1 x3 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maxValue x1 x1002 x3500) (d_C_maxValue x1 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maxValue x1 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maxValue x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_maxValue :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Values t0 -> IDSupply -> ConstStore -> t0
nd_C_maxValue x1 x2 x3000 x3500 = case x2 of
     (C_Values x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_maxValue_dot_maxOf_dot_62 x1 x3 x2000 x3500))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_maxValue x1 x1002 x3000 x3500) (nd_C_maxValue x1 x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_maxValue x1 z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_maxValue x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_maxValue_dot_maxOf_dot_62 :: Curry_Prelude.Curry t376 => (t376 -> ConstStore -> t376 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t376 -> ConstStore -> t376
d_OP_maxValue_dot_maxOf_dot_62 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_1 x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_maxValue_dot_maxOf_dot_62 x1 x1002 x3500) (d_OP_maxValue_dot_maxOf_dot_62 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_maxValue_dot_maxOf_dot_62 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_maxValue_dot_maxOf_dot_62 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_maxValue_dot_maxOf_dot_62 :: Curry_Prelude.Curry t376 => Func t376 (Func t376 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t376 -> IDSupply -> ConstStore -> t376
nd_OP_maxValue_dot_maxOf_dot_62 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x3 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_maxValue_dot_maxOf_dot_62 x1 x1002 x3000 x3500) (nd_OP_maxValue_dot_maxOf_dot_62 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_maxValue_dot_maxOf_dot_62 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_maxValue_dot_maxOf_dot_62 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_values2list :: Curry_Prelude.Curry t0 => C_Values t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_C_values2list x1 x3500 = case x1 of
     (C_Values x2) -> Curry_Prelude.d_C_return x2 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_values2list x1002 x3500) (d_C_values2list x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_values2list z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_values2list x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_printValues :: Curry_Prelude.Curry t0 => C_Values t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_printValues x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_values2list x1 x3500) (Curry_Prelude.d_C_mapIO_ Curry_Prelude.d_C_print x3500) x3500

d_C_sortValues :: Curry_Prelude.Curry t0 => ConstStore -> C_Values t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sortValues x3500 = d_C_sortValuesBy (acceptCs id Curry_Prelude.d_OP_lt_eq)

nd_C_sortValues :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func (C_Values t0) (Curry_Prelude.OP_List t0)
nd_C_sortValues x3000 x3500 = wrapNX id (nd_C_sortValuesBy (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt_eq)))

d_C_sortValuesBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> C_Values t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sortValuesBy x1 x2 x3500 = case x2 of
     (C_Values x3) -> Curry_Sort.d_C_mergeSort x1 x3 x3500
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sortValuesBy x1 x1002 x3500) (d_C_sortValuesBy x1 x1003 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sortValuesBy x1 z x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sortValuesBy x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_sortValuesBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Values t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_sortValuesBy x1 x2 x3000 x3500 = case x2 of
     (C_Values x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Sort.nd_C_mergeSort x1 x3 x2000 x3500))
     (Choice_C_Values x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_sortValuesBy x1 x1002 x3000 x3500) (nd_C_sortValuesBy x1 x1003 x3000 x3500)
     (Choices_C_Values x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_sortValuesBy x1 z x3000 x3500) x1002
     (Guard_C_Values x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_sortValuesBy x1 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Values x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_OP_maxValue_dot_maxOf_dot_62 x1 (Curry_Prelude.OP_Cons x5 x6) x3500
           in (d_OP__case_0 x1 x3 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3500) x7 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x1002 x3500) (d_OP__case_1 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2000 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2000 (seq x2005 (let
                    x7 = nd_OP_maxValue_dot_maxOf_dot_62 x1 (Curry_Prelude.OP_Cons x5 x6) x2000 x3500
                     in (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (nd_OP__case_0 x1 x3 x7 (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2001 x3500) x7 x2002 x3500)))) x2004 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x3 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> x7
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x3 x7 x1002 x3500) (d_OP__case_0 x1 x3 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x3 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x3 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> x7
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x3 x7 x1002 x3000 x3500) (nd_OP__case_0 x1 x3 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x3 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x3 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_OP_minValue_dot_minOf_dot_54 x1 (Curry_Prelude.OP_Cons x5 x6) x3500
           in (d_OP__case_2 x1 x3 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3500) x7 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x3 x1002 x3500) (d_OP__case_3 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2000 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2000 (seq x2005 (let
                    x7 = nd_OP_minValue_dot_minOf_dot_54 x1 (Curry_Prelude.OP_Cons x5 x6) x2000 x3500
                     in (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (nd_OP__case_2 x1 x3 x7 (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2001 x3500) x7 x2002 x3500)))) x2004 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x3 x1002 x3000 x3500) (nd_OP__case_3 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x3 x7 x1002 x3500) (d_OP__case_2 x1 x3 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x3 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x3 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x3 x7 x1002 x3000 x3500) (nd_OP__case_2 x1 x3 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x3 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x3 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (C_Values x4)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3500) (d_OP__case_4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (C_Values x4)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1002 x3000 x3500) (nd_OP__case_4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_cover :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> t0
d_C_cover x1 x3500 = external_d_C_cover x1 x3500
external_d_C_cover :: Coverable a => a -> ConstStore -> a
external_d_C_cover x _ = cover x

