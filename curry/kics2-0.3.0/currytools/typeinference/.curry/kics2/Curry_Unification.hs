{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Unification (C_RTerm, d_C_unify, nd_C_unify) where

import Basics
import qualified Curry_FiniteMap
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_UnificationSpec
type C_RefTable = Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm

type C_REq = Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm

type C_REqs = Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)

data C_RTerm
     = C_RTermCons (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_RTerm)
     | C_RTermVar Curry_Prelude.C_Int
     | C_Ref Curry_Prelude.C_Int
     | Choice_C_RTerm Cover ID C_RTerm C_RTerm
     | Choices_C_RTerm Cover ID ([C_RTerm])
     | Fail_C_RTerm Cover FailInfo
     | Guard_C_RTerm Cover Constraints C_RTerm

instance Show C_RTerm where
  showsPrec d (Choice_C_RTerm cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_RTerm cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_RTerm cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_RTerm cd info) = showChar '!'
  showsPrec _ (C_RTermCons x1 x2) = (showString "(RTermCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_RTermVar x1) = (showString "(RTermVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Ref x1) = (showString "(Ref") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_RTerm where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_RTermCons x1 x2,r2) | (_,r0) <- readQualified "Unification" "RTermCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_RTermVar x1,r1) | (_,r0) <- readQualified "Unification" "RTermVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Ref x1,r1) | (_,r0) <- readQualified "Unification" "Ref" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_RTerm where
  choiceCons = Choice_C_RTerm
  choicesCons = Choices_C_RTerm
  failCons = Fail_C_RTerm
  guardCons = Guard_C_RTerm
  try (Choice_C_RTerm cd i x y) = tryChoice cd i x y
  try (Choices_C_RTerm cd i xs) = tryChoices cd i xs
  try (Fail_C_RTerm cd info) = Fail cd info
  try (Guard_C_RTerm cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_RTerm cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_RTerm cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_RTerm cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_RTerm cd i _) = error ("Unification.RTerm.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_RTerm cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_RTerm cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_RTerm where
  generate s c = Choices_C_RTerm c (freeID [2,1,1] s) [(C_RTermCons (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_RTermVar (generate (leftSupply s) c)),(C_Ref (generate (leftSupply s) c))]


instance NormalForm C_RTerm where
  ($!!) cont (C_RTermCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_RTermCons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_RTermVar x1) d cs = (((\y1 d cs -> cont (C_RTermVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Ref x1) d cs = (((\y1 d cs -> cont (C_Ref y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_RTerm cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_RTerm cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_RTerm cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_RTerm cd info) _ _ = failCons cd info
  ($##) cont (C_RTermCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_RTermCons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_RTermVar x1) d cs = (((\y1 d cs -> cont (C_RTermVar y1) d cs) $## x1) d) cs
  ($##) cont (C_Ref x1) d cs = (((\y1 d cs -> cont (C_Ref y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_RTerm cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_RTerm cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_RTerm cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_RTerm cd info) _ _ = failCons cd info
  searchNF search cont (C_RTermCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_RTermCons y1 y2)) x2) x1
  searchNF search cont (C_RTermVar x1) = search (\y1 -> cont (C_RTermVar y1)) x1
  searchNF search cont (C_Ref x1) = search (\y1 -> cont (C_Ref y1)) x1
  searchNF _ _ x = error ("Unification.RTerm.searchNF: no constructor: " ++ (show x))


instance Unifiable C_RTerm where
  (=.=) (C_RTermCons x1 x2) (C_RTermCons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_RTermVar x1) (C_RTermVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Ref x1) (C_Ref y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_RTermCons x1 x2) (C_RTermCons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_RTermVar x1) (C_RTermVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Ref x1) (C_Ref y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_RTermCons x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_RTermVar x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Ref x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_RTerm cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_RTerm cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_RTerm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_RTerm cd i _) = error ("Unification.RTerm.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_RTerm cd info) = [(Unsolvable info)]
  bind d i (Guard_C_RTerm cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_RTermCons x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_RTermVar x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Ref x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_RTerm cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_RTerm cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_RTerm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_RTerm cd i _) = error ("Unification.RTerm.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_RTerm cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_RTerm cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_RTerm where
  (=?=) (Choice_C_RTerm cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_RTerm cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_RTerm cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_RTerm cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_RTerm cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_RTerm cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_RTerm cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_RTerm cd info) _ _ = failCons cd info
  (=?=) (C_RTermCons x1 x2) (C_RTermCons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_RTermVar x1) (C_RTermVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Ref x1) (C_Ref y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_RTerm cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_RTerm cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_RTerm cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_RTerm cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_RTerm cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_RTerm cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_RTerm cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_RTerm cd info) _ _ = failCons cd info
  (<?=) (C_RTermCons x1 x2) (C_RTermCons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_RTermCons _ _) (C_RTermVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_RTermCons _ _) (C_Ref _) _ _ = Curry_Prelude.C_True
  (<?=) (C_RTermVar x1) (C_RTermVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_RTermVar _) (C_Ref _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Ref x1) (C_Ref y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_unify :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term) -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
d_C_unify x1 x3250 x3500 = let
     x2 = d_C_termEqsToREqs x1 x3250 x3500
     x3 = d_OP_unify_dot___hash_selFP2_hash_r x2 x3250 x3500
     x4 = d_OP_unify_dot___hash_selFP3_hash_rts x2 x3250 x3500
      in (d_OP__case_18 x4 x3 (d_C_unify' x3 x4 Curry_Prelude.OP_List x3250 x3500) x3250 x3500)

nd_C_unify :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
nd_C_unify x1 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2007 = leftSupply x2006
          x2008 = rightSupply x2006
           in (seq x2007 (seq x2008 (let
               x2000 = leftSupply x2007
               x2001 = rightSupply x2007
                in (seq x2000 (seq x2001 (let
                    x2002 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2002 (seq x2005 (let
                         x2 = nd_C_termEqsToREqs x1 x2000 x3250 x3500
                         x3 = nd_OP_unify_dot___hash_selFP2_hash_r x2 x2001 x3250 x3500
                         x4 = nd_OP_unify_dot___hash_selFP3_hash_rts x2 x2002 x3250 x3500
                          in (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (nd_OP__case_18 x4 x3 (nd_C_unify' x3 x4 Curry_Prelude.OP_List x2003 x3250 x3500) x2004 x3250 x3500)))))))))))))))

d_OP_unify_dot___hash_selFP2_hash_r :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
d_OP_unify_dot___hash_selFP2_hash_r x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unify_dot___hash_selFP2_hash_r x1002 x3250 x3500) (d_OP_unify_dot___hash_selFP2_hash_r x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unify_dot___hash_selFP2_hash_r z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unify_dot___hash_selFP2_hash_r x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_unify_dot___hash_selFP2_hash_r :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
nd_OP_unify_dot___hash_selFP2_hash_r x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_unify_dot___hash_selFP2_hash_r x1002 x3000 x3250 x3500) (nd_OP_unify_dot___hash_selFP2_hash_r x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_unify_dot___hash_selFP2_hash_r z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_unify_dot___hash_selFP2_hash_r x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_unify_dot___hash_selFP3_hash_rts :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)
d_OP_unify_dot___hash_selFP3_hash_rts x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unify_dot___hash_selFP3_hash_rts x1002 x3250 x3500) (d_OP_unify_dot___hash_selFP3_hash_rts x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unify_dot___hash_selFP3_hash_rts z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unify_dot___hash_selFP3_hash_rts x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_unify_dot___hash_selFP3_hash_rts :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)
nd_OP_unify_dot___hash_selFP3_hash_rts x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_unify_dot___hash_selFP3_hash_rts x1002 x3000 x3250 x3500) (nd_OP_unify_dot___hash_selFP3_hash_rts x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_unify_dot___hash_selFP3_hash_rts z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_unify_dot___hash_selFP3_hash_rts x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_termEqsToREqs :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm))
d_C_termEqsToREqs x1 x3250 x3500 = Curry_List.d_C_mapAccumL (acceptCs id d_C_termEqToREq) (Curry_FiniteMap.d_C_emptyFM (acceptCs id Curry_Prelude.d_OP_lt) x3250 x3500) x1 x3250 x3500

nd_C_termEqsToREqs :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm))
nd_C_termEqsToREqs x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_List.nd_C_mapAccumL (wrapDX (wrapNX id) (acceptCs id nd_C_termEqToREq)) (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3250 x3500) x1 x2001 x3250 x3500)))))

d_C_termEqToREq :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)
d_C_termEqToREq x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x5 = d_C_termToRTerm x1 x3 x3250 x3500
          x6 = d_OP_termEqToREq_dot___hash_selFP8_hash_r1 x5 x3250 x3500
          x7 = d_OP_termEqToREq_dot___hash_selFP9_hash_a' x5 x3250 x3500
          x8 = d_C_termToRTerm x6 x4 x3250 x3500
          x9 = d_OP_termEqToREq_dot___hash_selFP6_hash_r2 x8 x3250 x3500
          x10 = d_OP_termEqToREq_dot___hash_selFP7_hash_b' x8 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Tuple2 x7 x10))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_termEqToREq x1 x1002 x3250 x3500) (d_C_termEqToREq x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_termEqToREq x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_termEqToREq x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_termEqToREq :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 Curry_UnificationSpec.C_Term Curry_UnificationSpec.C_Term -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)
nd_C_termEqToREq x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2007 = leftSupply x2006
               x2009 = rightSupply x2006
                in (seq x2007 (seq x2009 (let
                    x2000 = leftSupply x2007
                    x2008 = rightSupply x2007
                     in (seq x2000 (seq x2008 (let
                         x2001 = leftSupply x2008
                         x2002 = rightSupply x2008
                          in (seq x2001 (seq x2002 (let
                              x2003 = leftSupply x2009
                              x2010 = rightSupply x2009
                               in (seq x2003 (seq x2010 (let
                                   x2004 = leftSupply x2010
                                   x2005 = rightSupply x2010
                                    in (seq x2004 (seq x2005 (let
                                        x5 = nd_C_termToRTerm x1 x3 x2000 x3250 x3500
                                        x6 = nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 x5 x2001 x3250 x3500
                                        x7 = nd_OP_termEqToREq_dot___hash_selFP9_hash_a' x5 x2002 x3250 x3500
                                        x8 = nd_C_termToRTerm x6 x4 x2003 x3250 x3500
                                        x9 = nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 x8 x2004 x3250 x3500
                                        x10 = nd_OP_termEqToREq_dot___hash_selFP7_hash_b' x8 x2005 x3250 x3500
                                         in (Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Tuple2 x7 x10)))))))))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_termEqToREq x1 x1002 x3000 x3250 x3500) (nd_C_termEqToREq x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_termEqToREq x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_termEqToREq x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termEqToREq_dot___hash_selFP8_hash_r1 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
d_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1002 x3250 x3500) (d_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termEqToREq_dot___hash_selFP8_hash_r1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1002 x3000 x3250 x3500) (nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termEqToREq_dot___hash_selFP8_hash_r1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termEqToREq_dot___hash_selFP9_hash_a' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> Cover -> ConstStore -> C_RTerm
d_OP_termEqToREq_dot___hash_selFP9_hash_a' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termEqToREq_dot___hash_selFP9_hash_a' x1002 x3250 x3500) (d_OP_termEqToREq_dot___hash_selFP9_hash_a' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termEqToREq_dot___hash_selFP9_hash_a' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termEqToREq_dot___hash_selFP9_hash_a' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termEqToREq_dot___hash_selFP9_hash_a' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> IDSupply -> Cover -> ConstStore -> C_RTerm
nd_OP_termEqToREq_dot___hash_selFP9_hash_a' x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termEqToREq_dot___hash_selFP9_hash_a' x1002 x3000 x3250 x3500) (nd_OP_termEqToREq_dot___hash_selFP9_hash_a' x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termEqToREq_dot___hash_selFP9_hash_a' z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termEqToREq_dot___hash_selFP9_hash_a' x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termEqToREq_dot___hash_selFP6_hash_r2 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
d_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1002 x3250 x3500) (d_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termEqToREq_dot___hash_selFP6_hash_r2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1002 x3000 x3250 x3500) (nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termEqToREq_dot___hash_selFP6_hash_r2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termEqToREq_dot___hash_selFP7_hash_b' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> Cover -> ConstStore -> C_RTerm
d_OP_termEqToREq_dot___hash_selFP7_hash_b' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termEqToREq_dot___hash_selFP7_hash_b' x1002 x3250 x3500) (d_OP_termEqToREq_dot___hash_selFP7_hash_b' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termEqToREq_dot___hash_selFP7_hash_b' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termEqToREq_dot___hash_selFP7_hash_b' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termEqToREq_dot___hash_selFP7_hash_b' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm -> IDSupply -> Cover -> ConstStore -> C_RTerm
nd_OP_termEqToREq_dot___hash_selFP7_hash_b' x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termEqToREq_dot___hash_selFP7_hash_b' x1002 x3000 x3250 x3500) (nd_OP_termEqToREq_dot___hash_selFP7_hash_b' x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termEqToREq_dot___hash_selFP7_hash_b' z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termEqToREq_dot___hash_selFP7_hash_b' x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_termToRTerm :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_UnificationSpec.C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm
d_C_termToRTerm x1 x2 x3250 x3500 = case x2 of
     (Curry_UnificationSpec.C_TermVar x3) -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.d_C_addToFM x1 x3 (C_RTermVar x3) x3250 x3500) (C_Ref x3)
     (Curry_UnificationSpec.C_TermCons x4 x5) -> let
          x6 = Curry_List.d_C_mapAccumL (acceptCs id d_C_termToRTerm) x1 x5 x3250 x3500
          x7 = d_OP_termToRTerm_dot___hash_selFP11_hash_r' x6 x3250 x3500
          x8 = d_OP_termToRTerm_dot___hash_selFP12_hash_l' x6 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 x7 (C_RTermCons x4 x8))
     (Curry_UnificationSpec.Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_termToRTerm x1 x1002 x3250 x3500) (d_C_termToRTerm x1 x1003 x3250 x3500)
     (Curry_UnificationSpec.Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_termToRTerm x1 z x3250 x3500) x1002
     (Curry_UnificationSpec.Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_termToRTerm x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_UnificationSpec.Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_termToRTerm :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_UnificationSpec.C_Term -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) C_RTerm
nd_C_termToRTerm x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_UnificationSpec.C_TermVar x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.nd_C_addToFM x1 x3 (C_RTermVar x3) x2000 x3250 x3500) (C_Ref x3)))
     (Curry_UnificationSpec.C_TermCons x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (let
                         x6 = Curry_List.nd_C_mapAccumL (wrapDX (wrapNX id) (acceptCs id nd_C_termToRTerm)) x1 x5 x2000 x3250 x3500
                         x7 = nd_OP_termToRTerm_dot___hash_selFP11_hash_r' x6 x2001 x3250 x3500
                         x8 = nd_OP_termToRTerm_dot___hash_selFP12_hash_l' x6 x2002 x3250 x3500
                          in (Curry_Prelude.OP_Tuple2 x7 (C_RTermCons x4 x8))))))))))
     (Curry_UnificationSpec.Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_termToRTerm x1 x1002 x3000 x3250 x3500) (nd_C_termToRTerm x1 x1003 x3000 x3250 x3500)
     (Curry_UnificationSpec.Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_termToRTerm x1 z x3000 x3250 x3500) x1002
     (Curry_UnificationSpec.Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_termToRTerm x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_UnificationSpec.Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termToRTerm_dot___hash_selFP11_hash_r' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List C_RTerm) -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
d_OP_termToRTerm_dot___hash_selFP11_hash_r' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termToRTerm_dot___hash_selFP11_hash_r' x1002 x3250 x3500) (d_OP_termToRTerm_dot___hash_selFP11_hash_r' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termToRTerm_dot___hash_selFP11_hash_r' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termToRTerm_dot___hash_selFP11_hash_r' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termToRTerm_dot___hash_selFP11_hash_r' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List C_RTerm) -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm
nd_OP_termToRTerm_dot___hash_selFP11_hash_r' x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termToRTerm_dot___hash_selFP11_hash_r' x1002 x3000 x3250 x3500) (nd_OP_termToRTerm_dot___hash_selFP11_hash_r' x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termToRTerm_dot___hash_selFP11_hash_r' z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termToRTerm_dot___hash_selFP11_hash_r' x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_termToRTerm_dot___hash_selFP12_hash_l' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List C_RTerm) -> Cover -> ConstStore -> Curry_Prelude.OP_List C_RTerm
d_OP_termToRTerm_dot___hash_selFP12_hash_l' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_termToRTerm_dot___hash_selFP12_hash_l' x1002 x3250 x3500) (d_OP_termToRTerm_dot___hash_selFP12_hash_l' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_termToRTerm_dot___hash_selFP12_hash_l' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_termToRTerm_dot___hash_selFP12_hash_l' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_termToRTerm_dot___hash_selFP12_hash_l' :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List C_RTerm) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_RTerm
nd_OP_termToRTerm_dot___hash_selFP12_hash_l' x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_termToRTerm_dot___hash_selFP12_hash_l' x1002 x3000 x3250 x3500) (nd_OP_termToRTerm_dot___hash_selFP12_hash_l' x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_termToRTerm_dot___hash_selFP12_hash_l' z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_termToRTerm_dot___hash_selFP12_hash_l' x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_eqsToSubst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
d_C_eqsToSubst x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_UnificationSpec.d_C_emptySubst x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_16 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_eqsToSubst x1 x1002 x3250 x3500) (d_C_eqsToSubst x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_eqsToSubst x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_eqsToSubst x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_eqsToSubst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
nd_C_eqsToSubst x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_UnificationSpec.nd_C_emptySubst x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x4 x1 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_eqsToSubst x1 x1002 x3000 x3250 x3500) (nd_C_eqsToSubst x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_eqsToSubst x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_eqsToSubst x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_rTermToTerm :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_UnificationSpec.C_Term
d_C_rTermToTerm x1 x2 x3250 x3500 = case x2 of
     (C_Ref x3) -> d_C_rTermToTerm x1 (d_C_deref x1 x2 x3250 x3500) x3250 x3500
     (C_RTermVar x4) -> Curry_UnificationSpec.C_TermVar x4
     (C_RTermCons x5 x6) -> Curry_UnificationSpec.C_TermCons x5 (Curry_Prelude.d_C_map (d_C_rTermToTerm x1) x6 x3250 x3500)
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_rTermToTerm x1 x1002 x3250 x3500) (d_C_rTermToTerm x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_rTermToTerm x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_rTermToTerm x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_rTermToTerm :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_UnificationSpec.C_Term
nd_C_rTermToTerm x1 x2 x3000 x3250 x3500 = case x2 of
     (C_Ref x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_rTermToTerm x1 (nd_C_deref x1 x2 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (C_RTermVar x4) -> Curry_UnificationSpec.C_TermVar x4
     (C_RTermCons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_UnificationSpec.C_TermCons x5 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_rTermToTerm x1)) x6 x2000 x3250 x3500)))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_rTermToTerm x1 x1002 x3000 x3250 x3500) (nd_C_rTermToTerm x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_rTermToTerm x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_rTermToTerm x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deref :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> C_RTerm
d_C_deref x1 x2 x3250 x3500 = case x2 of
     (C_Ref x3) -> d_OP__case_13 x3 x1 (Curry_FiniteMap.d_C_lookupFM x1 x3 x3250 x3500) x3250 x3500
     (C_RTermVar x9) -> x2
     (C_RTermCons x10 x11) -> x2
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deref x1 x1002 x3250 x3500) (d_C_deref x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deref x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deref x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_deref :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> C_RTerm
nd_C_deref x1 x2 x3000 x3250 x3500 = case x2 of
     (C_Ref x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_13 x3 x1 (Curry_FiniteMap.nd_C_lookupFM x1 x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (C_RTermVar x9) -> x2
     (C_RTermCons x10 x11) -> x2
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_deref x1 x1002 x3000 x3250 x3500) (nd_C_deref x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_deref x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_deref x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unify' :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_C_unify' x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Right (Curry_Prelude.OP_Tuple2 x1 x3)
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_11 x3 x5 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unify' x1 x1002 x3 x3250 x3500) (d_C_unify' x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unify' x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unify' x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_unify' :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_C_unify' x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Right (Curry_Prelude.OP_Tuple2 x1 x3)
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x3 x5 x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_unify' x1 x1002 x3 x3000 x3250 x3500) (nd_C_unify' x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_unify' x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_unify' x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_elim :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Int -> C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_C_elim x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_2 x3 x2 x1 x5 x4 (d_C_dependsOn x1 (C_RTermVar x2) x3 x3250 x3500) x3250 x3500

nd_C_elim :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Int -> C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_C_elim x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_2 x3 x2 x1 x5 x4 (nd_C_dependsOn x1 (C_RTermVar x2) x3 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_dependsOn :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_dependsOn x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x2 x3 x3250 x3500) (d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x2 x1 x3 x3250 x3500) x3250 x3500

nd_C_dependsOn :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_dependsOn x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x2 x3 x3250 x3500) (nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x2 x1 x3 x2000 x3250 x3500) x3250 x3500))

d_OP_dependsOn_dot_dependsOnRecurse_dot_122 :: C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x3 x3250 x3500 = case x3 of
     (C_RTermVar x4) -> Curry_Prelude.d_OP_eq_eq x1 x3 x3250 x3500
     (C_RTermCons x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3250 x3500) (Curry_Prelude.d_C_map (d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2) x6 x3250 x3500) x3250 x3500
     (C_Ref x7) -> Curry_Prelude.d_OP_eq_eq (d_C_deref x2 x3 x3250 x3500) x1 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1002 x3250 x3500) (d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 :: C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (C_RTermVar x4) -> Curry_Prelude.d_OP_eq_eq x1 x3 x3250 x3500
     (C_RTermCons x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_or x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2)) x6 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (C_Ref x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_eq_eq (nd_C_deref x2 x3 x2000 x3250 x3500) x1 x3250 x3500))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_dependsOn_dot_dependsOnRecurse_dot_122 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_RTerm -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_2 x3 x2 x1 x5 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Left (Curry_UnificationSpec.C_OccurCheck x2 (d_C_rTermToTerm x1 x3 x3250 x3500))
     Curry_Prelude.C_False -> d_OP__case_1 x3 x5 x2 x4 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x2 x1 x5 x4 x1002 x3250 x3500) (d_OP__case_2 x3 x2 x1 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 x2 x1 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x2 x1 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: C_RTerm -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_2 x3 x2 x1 x5 x4 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.C_Left (Curry_UnificationSpec.C_OccurCheck x2 (nd_C_rTermToTerm x1 x3 x2000 x3250 x3500))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x3 x5 x2 x4 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x2 x1 x5 x4 x1002 x3000 x3250 x3500) (nd_OP__case_2 x3 x2 x1 x5 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 x2 x1 x5 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x2 x1 x5 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_1 x3 x5 x2 x4 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_0 x5 x2 x4 x1 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x5 x2 x4 x1 x1002 x3250 x3500) (d_OP__case_1 x3 x5 x2 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x5 x2 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x5 x2 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_1 x3 x5 x2 x4 x1 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x5 x2 x4 x1 x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x3 x5 x2 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_1 x3 x5 x2 x4 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x3 x5 x2 x4 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x3 x5 x2 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_0 x5 x2 x4 x1 x3 x3250 x3500 = case x3 of
     (C_RTermVar x6) -> d_C_unify' (Curry_FiniteMap.d_C_addToFM x1 x2 (C_Ref x6) x3250 x3500) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_RTermVar x2) (C_Ref x6)) x5) x3250 x3500
     (C_RTermCons x7 x8) -> d_C_unify' (Curry_FiniteMap.d_C_addToFM x1 x2 x3 x3250 x3500) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_RTermVar x2) x3) x5) x3250 x3500
     (C_Ref x9) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x2 x4 x1 x1002 x3250 x3500) (d_OP__case_0 x5 x2 x4 x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 x2 x4 x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x2 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_0 x5 x2 x4 x1 x3 x3000 x3250 x3500 = case x3 of
     (C_RTermVar x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' (Curry_FiniteMap.nd_C_addToFM x1 x2 (C_Ref x6) x2000 x3250 x3500) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_RTermVar x2) (C_Ref x6)) x5) x2001 x3250 x3500)))))
     (C_RTermCons x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' (Curry_FiniteMap.nd_C_addToFM x1 x2 x3 x2000 x3250 x3500) x4 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_RTermVar x2) x3) x5) x2001 x3250 x3500)))))
     (C_Ref x9) -> Curry_Prelude.d_C_failed x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x5 x2 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_0 x5 x2 x4 x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x5 x2 x4 x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x5 x2 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_11 x3 x5 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_10 x7 x3 x5 x1 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x3 x5 x1 x1002 x3250 x3500) (d_OP__case_11 x3 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x3 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x3 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_11 x3 x5 x1 x4 x3000 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x7 x3 x5 x1 x6 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x3 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_11 x3 x5 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x3 x5 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x3 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_10 x7 x3 x5 x1 x6 x3250 x3500 = case x6 of
     (C_RTermVar x8) -> d_OP__case_9 x3 x5 x1 x6 x8 x7 x3250 x3500
     (C_RTermCons x13 x14) -> d_OP__case_6 x3 x5 x1 x6 x13 x14 x7 x3250 x3500
     (C_Ref x19) -> d_OP__case_3 x3 x5 x1 x6 x7 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x7 x3 x5 x1 x1002 x3250 x3500) (d_OP__case_10 x7 x3 x5 x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x7 x3 x5 x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x7 x3 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_10 x7 x3 x5 x1 x6 x3000 x3250 x3500 = case x6 of
     (C_RTermVar x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x3 x5 x1 x6 x8 x7 x2000 x3250 x3500))
     (C_RTermCons x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x3 x5 x1 x6 x13 x14 x7 x2000 x3250 x3500))
     (C_Ref x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x3 x5 x1 x6 x7 x2000 x3250 x3500))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x7 x3 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_10 x7 x3 x5 x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x7 x3 x5 x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x7 x3 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_3 x3 x5 x1 x6 x7 x3250 x3500 = case x7 of
     (C_RTermVar x20) -> d_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_deref x1 x6 x3250 x3500) x7) x5) x3 x3250 x3500
     (C_RTermCons x21 x22) -> d_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_deref x1 x6 x3250 x3500) x7) x5) x3 x3250 x3500
     (C_Ref x23) -> d_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_deref x1 x6 x3250 x3500) (d_C_deref x1 x7 x3250 x3500)) x5) x3 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x5 x1 x6 x1002 x3250 x3500) (d_OP__case_3 x3 x5 x1 x6 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x5 x1 x6 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x5 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_3 x3 x5 x1 x6 x7 x3000 x3250 x3500 = case x7 of
     (C_RTermVar x20) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (nd_C_deref x1 x6 x2000 x3250 x3500) x7) x5) x3 x2001 x3250 x3500)))))
     (C_RTermCons x21 x22) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (nd_C_deref x1 x6 x2000 x3250 x3500) x7) x5) x3 x2001 x3250 x3500)))))
     (C_Ref x23) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_unify' x1 (Curry_Prelude.OP_Cons (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (nd_C_deref x1 x6 x2000 x3250 x3500) (nd_C_deref x1 x7 x2001 x3250 x3500))))) x5) x3 x2003 x3250 x3500)))))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x3 x5 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_3 x3 x5 x1 x6 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x3 x5 x1 x6 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x3 x5 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_6 x3 x5 x1 x6 x13 x14 x7 x3250 x3500 = case x7 of
     (C_RTermVar x15) -> d_C_elim x1 x15 x6 x5 x3 x3250 x3500
     (C_RTermCons x16 x17) -> d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 (Curry_Prelude.d_OP_eq_eq x13 x16 x3250 x3500) x3250 x3500
     (C_Ref x18) -> d_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (d_C_deref x1 x7 x3250 x3500)) x5) x3 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x3 x5 x1 x6 x13 x14 x1002 x3250 x3500) (d_OP__case_6 x3 x5 x1 x6 x13 x14 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x3 x5 x1 x6 x13 x14 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x3 x5 x1 x6 x13 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_6 x3 x5 x1 x6 x13 x14 x7 x3000 x3250 x3500 = case x7 of
     (C_RTermVar x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_elim x1 x15 x6 x5 x3 x2000 x3250 x3500))
     (C_RTermCons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 (Curry_Prelude.d_OP_eq_eq x13 x16 x3250 x3500) x2000 x3250 x3500))
     (C_Ref x18) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (nd_C_deref x1 x7 x2000 x3250 x3500)) x5) x3 x2001 x3250 x3500)))))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x3 x5 x1 x6 x13 x14 x1002 x3000 x3250 x3500) (nd_OP__case_6 x3 x5 x1 x6 x13 x14 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x3 x5 x1 x6 x13 x14 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x3 x5 x1 x6 x13 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List C_RTerm -> Curry_Prelude.OP_List C_RTerm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> d_C_unify' x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_zip x14 x17 x3250 x3500) x5 x3250 x3500) x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x7 x1 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1002 x3250 x3500) (d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List C_RTerm -> Curry_Prelude.OP_List C_RTerm -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x18 x3000 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_unify' x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_zip x14 x17 x3250 x3500) x5 x3250 x3500) x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x7 x1 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1002 x3000 x3250 x3500) (nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x16 x13 x7 x1 x6 x3 x5 x17 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_4 x7 x1 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Left (Curry_UnificationSpec.C_Clash (d_C_rTermToTerm x1 x6 x3250 x3500) (d_C_rTermToTerm x1 x7 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x7 x1 x6 x1002 x3250 x3500) (d_OP__case_4 x7 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x7 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x7 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_4 x7 x1 x6 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.C_Left (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_UnificationSpec.C_Clash (nd_C_rTermToTerm x1 x6 x2000 x3250 x3500) (nd_C_rTermToTerm x1 x7 x2001 x3250 x3500)))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x7 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_4 x7 x1 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x7 x1 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x7 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.C_Int -> C_RTerm -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_9 x3 x5 x1 x6 x8 x7 x3250 x3500 = case x7 of
     (C_RTermCons x9 x10) -> d_C_elim x1 x8 x7 x5 x3 x3250 x3500
     (C_RTermVar x11) -> d_OP__case_8 x11 x8 x3 x5 x7 x1 (Curry_Prelude.d_OP_eq_eq x8 x11 x3250 x3500) x3250 x3500
     (C_Ref x12) -> d_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (d_C_deref x1 x7 x3250 x3500)) x5) x3 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x3 x5 x1 x6 x8 x1002 x3250 x3500) (d_OP__case_9 x3 x5 x1 x6 x8 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x3 x5 x1 x6 x8 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x3 x5 x1 x6 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Curry_Prelude.C_Int -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_9 x3 x5 x1 x6 x8 x7 x3000 x3250 x3500 = case x7 of
     (C_RTermCons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_elim x1 x8 x7 x5 x3 x2000 x3250 x3500))
     (C_RTermVar x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x11 x8 x3 x5 x7 x1 (Curry_Prelude.d_OP_eq_eq x8 x11 x3250 x3500) x2000 x3250 x3500))
     (C_Ref x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_unify' x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 (nd_C_deref x1 x7 x2000 x3250 x3500)) x5) x3 x2001 x3250 x3500)))))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x3 x5 x1 x6 x8 x1002 x3000 x3250 x3500) (nd_OP__case_9 x3 x5 x1 x6 x8 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x3 x5 x1 x6 x8 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x3 x5 x1 x6 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_8 x11 x8 x3 x5 x7 x1 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_unify' x1 x5 x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_7 x3 x5 x7 x8 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x11 x8 x3 x5 x7 x1 x1002 x3250 x3500) (d_OP__case_8 x11 x8 x3 x5 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x11 x8 x3 x5 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x11 x8 x3 x5 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> C_RTerm -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_8 x11 x8 x3 x5 x7 x1 x12 x3000 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_unify' x1 x5 x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x3 x5 x7 x8 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x11 x8 x3 x5 x7 x1 x1002 x3000 x3250 x3500) (nd_OP__case_8 x11 x8 x3 x5 x7 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x11 x8 x3 x5 x7 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x11 x8 x3 x5 x7 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> C_RTerm -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
d_OP__case_7 x3 x5 x7 x8 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_elim x1 x8 x7 x5 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x3 x5 x7 x8 x1 x1002 x3250 x3500) (d_OP__case_7 x3 x5 x7 x8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x3 x5 x7 x8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x3 x5 x7 x8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> C_RTerm -> Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)))
nd_OP__case_7 x3 x5 x7 x8 x1 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_elim x1 x8 x7 x5 x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x3 x5 x7 x8 x1 x1002 x3000 x3250 x3500) (nd_OP__case_7 x3 x5 x7 x8 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x3 x5 x7 x8 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x3 x5 x7 x8 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Maybe C_RTerm -> Cover -> ConstStore -> C_RTerm
d_OP__case_13 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.C_Just x4) -> d_OP__case_12 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x3 x1 x1002 x3250 x3500) (d_OP__case_13 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry_Prelude.C_Int -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Maybe C_RTerm -> IDSupply -> Cover -> ConstStore -> C_RTerm
nd_OP__case_13 x3 x1 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_Nothing -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.C_Just x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_13 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> C_RTerm
d_OP__case_12 x1 x4 x3250 x3500 = case x4 of
     (C_RTermVar x5) -> x4
     (C_RTermCons x6 x7) -> x4
     (C_Ref x8) -> d_C_deref x1 x4 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x1002 x3250 x3500) (d_OP__case_12 x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> C_RTerm
nd_OP__case_12 x1 x4 x3000 x3250 x3500 = case x4 of
     (C_RTermVar x5) -> x4
     (C_RTermCons x6 x7) -> x4
     (C_Ref x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_deref x1 x4 x2000 x3250 x3500))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x1002 x3000 x3250 x3500) (nd_OP__case_12 x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
d_OP__case_16 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_15 x6 x4 x1 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x4 x1 x1002 x3250 x3500) (d_OP__case_16 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
nd_OP__case_16 x4 x1 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x6 x4 x1 x5 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_16 x4 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x4 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
d_OP__case_15 x6 x4 x1 x5 x3250 x3500 = case x5 of
     (C_Ref x7) -> d_C_eqsToSubst x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_deref x1 x5 x3250 x3500) x6) x4) x3250 x3500
     (C_RTermVar x8) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_UnificationSpec.d_C_extendSubst x3250 x3500) (d_C_eqsToSubst x1 x4 x3250 x3500) x3250 x3500) x8 x3250 x3500) (d_C_rTermToTerm x1 x6 x3250 x3500) x3250 x3500
     (C_RTermCons x9 x10) -> d_OP__case_14 x4 x1 x5 x6 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x6 x4 x1 x1002 x3250 x3500) (d_OP__case_15 x6 x4 x1 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x6 x4 x1 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x6 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_15 :: C_RTerm -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
nd_OP__case_15 x6 x4 x1 x5 x3000 x3250 x3500 = case x5 of
     (C_Ref x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_eqsToSubst x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (nd_C_deref x1 x5 x2000 x3250 x3500) x6) x4) x2001 x3250 x3500)))))
     (C_RTermVar x8) -> let
          x2009 = x3000
           in (seq x2009 (let
               x2008 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2008 (seq x2010 (let
                    x2006 = leftSupply x2010
                    x2007 = rightSupply x2010
                     in (seq x2006 (seq x2007 (Curry_Prelude.nd_C_apply (let
                         x2005 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2002 = leftSupply x2003
                              x2004 = rightSupply x2003
                               in (seq x2002 (seq x2004 (let
                                   x2000 = leftSupply x2004
                                   x2001 = rightSupply x2004
                                    in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_UnificationSpec.nd_C_extendSubst x2000 x3250 x3500) (nd_C_eqsToSubst x1 x4 x2001 x3250 x3500) x2002 x3250 x3500))))))) x8 x2005 x3250 x3500)))) (nd_C_rTermToTerm x1 x6 x2007 x3250 x3500) x2008 x3250 x3500))))))))
     (C_RTermCons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x4 x1 x5 x6 x2000 x3250 x3500))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x6 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_15 x6 x4 x1 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x6 x4 x1 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x6 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
d_OP__case_14 x4 x1 x5 x6 x3250 x3500 = case x6 of
     (C_RTermVar x11) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_UnificationSpec.d_C_extendSubst x3250 x3500) (d_C_eqsToSubst x1 x4 x3250 x3500) x3250 x3500) x11 x3250 x3500) (d_C_rTermToTerm x1 x5 x3250 x3500) x3250 x3500
     (C_Ref x12) -> d_C_eqsToSubst x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 (d_C_deref x1 x6 x3250 x3500)) x4) x3250 x3500
     (C_RTermCons x13 x14) -> d_C_eqsToSubst x1 x4 x3250 x3500
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x4 x1 x5 x1002 x3250 x3500) (d_OP__case_14 x4 x1 x5 x1003 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x4 x1 x5 z x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x4 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> C_RTerm -> C_RTerm -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term
nd_OP__case_14 x4 x1 x5 x6 x3000 x3250 x3500 = case x6 of
     (C_RTermVar x11) -> let
          x2009 = x3000
           in (seq x2009 (let
               x2008 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2008 (seq x2010 (let
                    x2006 = leftSupply x2010
                    x2007 = rightSupply x2010
                     in (seq x2006 (seq x2007 (Curry_Prelude.nd_C_apply (let
                         x2005 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2002 = leftSupply x2003
                              x2004 = rightSupply x2003
                               in (seq x2002 (seq x2004 (let
                                   x2000 = leftSupply x2004
                                   x2001 = rightSupply x2004
                                    in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_UnificationSpec.nd_C_extendSubst x2000 x3250 x3500) (nd_C_eqsToSubst x1 x4 x2001 x3250 x3500) x2002 x3250 x3500))))))) x11 x2005 x3250 x3500)))) (nd_C_rTermToTerm x1 x5 x2007 x3250 x3500) x2008 x3250 x3500))))))))
     (C_Ref x12) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_eqsToSubst x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 (nd_C_deref x1 x6 x2000 x3250 x3500)) x4) x2001 x3250 x3500)))))
     (C_RTermCons x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_eqsToSubst x1 x4 x2000 x3250 x3500))
     (Choice_C_RTerm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x4 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_14 x4 x1 x5 x1003 x3000 x3250 x3500)
     (Choices_C_RTerm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x4 x1 x5 z x3000 x3250 x3500) x1002
     (Guard_C_RTerm x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x4 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RTerm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm))) -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
d_OP__case_18 x4 x3 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.C_Right x5) -> d_OP__case_17 x5 x3250 x3500
     (Curry_Prelude.C_Left x8) -> Curry_Prelude.C_Left x8
     (Curry_Prelude.Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x4 x3 x1002 x3250 x3500) (d_OP__case_18 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm) -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
nd_OP__case_18 x4 x3 x9 x3000 x3250 x3500 = case x9 of
     (Curry_Prelude.C_Right x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x5 x2000 x3250 x3500))
     (Curry_Prelude.C_Left x8) -> Curry_Prelude.C_Left x8
     (Curry_Prelude.Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x4 x3 x1002 x3000 x3250 x3500) (nd_OP__case_18 x4 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x4 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x4 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
d_OP__case_17 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_Prelude.C_Right (d_C_eqsToSubst x6 x7 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1002 x3250 x3500) (d_OP__case_17 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Curry_Prelude.OP_Tuple2 (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_RTerm) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_RTerm C_RTerm)) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either Curry_UnificationSpec.C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_UnificationSpec.C_Term)
nd_OP__case_17 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.C_Right (nd_C_eqsToSubst x6 x7 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1002 x3000 x3250 x3500) (nd_OP__case_17 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
