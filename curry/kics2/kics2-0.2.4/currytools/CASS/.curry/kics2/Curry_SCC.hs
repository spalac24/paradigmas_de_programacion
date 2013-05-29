{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SCC (C_Node, d_C_scc, nd_C_scc) where

import Basics
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
import qualified Curry_SetRBT
data C_Node t0 t1
     = C_Node Curry_Prelude.C_Int (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1) t0
     | Choice_C_Node Cover ID (C_Node t0 t1) (C_Node t0 t1)
     | Choices_C_Node Cover ID ([C_Node t0 t1])
     | Fail_C_Node Cover FailInfo
     | Guard_C_Node Cover Constraints (C_Node t0 t1)

instance (Show t0,Show t1) => Show (C_Node t0 t1) where
  showsPrec d (Choice_C_Node cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Node cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Node cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Node cd info) = showChar '!'
  showsPrec _ (C_Node x1 x2 x3 x4) = (showString "(Node") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance (Read t0,Read t1) => Read (C_Node t0 t1) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Node x1 x2 x3 x4,r4) | (_,r0) <- readQualified "SCC" "Node" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet (C_Node t0 t1) where
  choiceCons = Choice_C_Node
  choicesCons = Choices_C_Node
  failCons = Fail_C_Node
  guardCons = Guard_C_Node
  try (Choice_C_Node cd i x y) = tryChoice cd i x y
  try (Choices_C_Node cd i xs) = tryChoices cd i xs
  try (Fail_C_Node cd info) = Fail cd info
  try (Guard_C_Node cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Node cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Node cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Node cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Node cd i _) = error ("SCC.Node.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Node cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Node cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1) => Generable (C_Node t0 t1) where
  generate s = Choices_C_Node defCover (freeID [4] s) [(C_Node (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance (NormalForm t0,NormalForm t1) => NormalForm (C_Node t0 t1) where
  ($!!) cont (C_Node x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_Node y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Node cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Node cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Node cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Node cd info) _ = failCons cd info
  ($##) cont (C_Node x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_Node y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Node cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Node cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Node cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Node cd info) _ = failCons cd info
  searchNF search cont (C_Node x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Node y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("SCC.Node.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1) => Unifiable (C_Node t0 t1) where
  (=.=) (C_Node x1 x2 x3 x4) (C_Node y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Node x1 x2 x3 x4) (C_Node y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Node x2 x3 x4 x5) = ((i :=: (ChooseN 0 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (Choice_C_Node cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Node cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Node cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Node cd i _) = error ("SCC.Node.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Node cd info) = [(Unsolvable info)]
  bind i (Guard_C_Node cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Node x2 x3 x4 x5) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (Choice_C_Node cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Node cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Node cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Node cd i _) = error ("SCC.Node.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Node cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Node cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.Curry (C_Node t0 t1) where
  (=?=) (Choice_C_Node cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Node cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Node cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Node cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Node cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Node cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Node cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Node cd info) _ = failCons cd info
  (=?=) (C_Node x1 x2 x3 x4) (C_Node y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (<?=) (Choice_C_Node cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Node cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Node cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Node cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Node cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Node cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Node cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Node cd info) _ = failCons cd info
  (<?=) (C_Node x1 x2 x3 x4) (C_Node y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs


instance (Coverable t0,Coverable t1) => Coverable (C_Node t0 t1) where
  cover (C_Node x1 x2 x3 x4) = C_Node (cover x1) (cover x2) (cover x3) (cover x4)
  cover (Choice_C_Node cd i x y) = Choice_C_Node (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Node cd i xs) = Choices_C_Node (incCover cd) i (map cover xs)
  cover (Fail_C_Node cd info) = Fail_C_Node (incCover cd) info
  cover (Guard_C_Node cd c e) = Guard_C_Node (incCover cd) c (cover e)


d_C_cmpNode :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Node t0 t1 -> C_Node t0 t1 -> ConstStore -> Curry_Prelude.C_Bool
d_C_cmpNode x1 x2 x3500 = Curry_Prelude.d_OP_lt (d_C_key x1 x3500) (d_C_key x2 x3500) x3500

d_C_key :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Node t0 t1 -> ConstStore -> Curry_Prelude.C_Int
d_C_key x1 x3500 = case x1 of
     (C_Node x2 x3 x4 x5) -> x2
     (Choice_C_Node x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_key x1002 x3500) (d_C_key x1003 x3500)
     (Choices_C_Node x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_key z x3500) x1002
     (Guard_C_Node x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_key x1002) $! (addCs x1001 x3500))
     (Fail_C_Node x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_bvs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Node t0 t1 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_bvs x1 x3500 = case x1 of
     (C_Node x2 x3 x4 x5) -> x3
     (Choice_C_Node x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_bvs x1002 x3500) (d_C_bvs x1003 x3500)
     (Choices_C_Node x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_bvs z x3500) x1002
     (Guard_C_Node x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_bvs x1002) $! (addCs x1001 x3500))
     (Fail_C_Node x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fvs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_Node t0 t1 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_fvs x1 x3500 = case x1 of
     (C_Node x2 x3 x4 x5) -> x4
     (Choice_C_Node x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fvs x1002 x3500) (d_C_fvs x1003 x3500)
     (Choices_C_Node x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fvs z x3500) x1002
     (Guard_C_Node x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fvs x1002) $! (addCs x1001 x3500))
     (Fail_C_Node x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_node :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => C_Node t0 t1 -> ConstStore -> t0
d_C_node x1 x3500 = case x1 of
     (C_Node x2 x3 x4 x5) -> x5
     (Choice_C_Node x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_node x1002 x3500) (d_C_node x1003 x3500)
     (Choices_C_Node x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_node z x3500) x1002
     (Guard_C_Node x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_node x1002) $! (addCs x1001 x3500))
     (Fail_C_Node x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scc :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> Curry_Prelude.OP_List t1) -> (t0 -> ConstStore -> Curry_Prelude.OP_List t1) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_scc x1 x2 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map (Curry_Prelude.d_C_map d_C_node)) (Curry_Prelude.d_OP_dot d_C_tsort' (Curry_Prelude.d_OP_dot d_C_tsort (Curry_Prelude.d_C_zipWith (acceptCs id (d_OP_scc_dot_wrap_dot_24 x1 x2)) (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 0#) x3500)) x3500) x3500) x3500

nd_C_scc :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_List t1) -> Func t0 (Curry_Prelude.OP_List t1) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List t0))
nd_C_scc x1 x2 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_map (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_C_node))))) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_tsort') (Curry_Prelude.nd_OP_dot (wrapDX id d_C_tsort) (wrapNX id (Curry_Prelude.nd_C_zipWith (wrapDX (wrapNX id) (acceptCs id (nd_OP_scc_dot_wrap_dot_24 x1 x2))) (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 0#) x3500))) x2000 x3500) x2001 x3500)))) x2003 x3500)))))

d_OP_scc_dot_wrap_dot_24 :: (Curry_Prelude.Curry t137,Curry_Prelude.Curry t139) => (t137 -> ConstStore -> Curry_Prelude.OP_List t139) -> (t137 -> ConstStore -> Curry_Prelude.OP_List t139) -> Curry_Prelude.C_Int -> t137 -> ConstStore -> C_Node t137 t139
d_OP_scc_dot_wrap_dot_24 x1 x2 x3 x4 x3500 = C_Node x3 (Curry_Prelude.d_C_apply x1 x4 x3500) (Curry_Prelude.d_C_apply x2 x4 x3500) x4

nd_OP_scc_dot_wrap_dot_24 :: (Curry_Prelude.Curry t137,Curry_Prelude.Curry t139) => Func t137 (Curry_Prelude.OP_List t139) -> Func t137 (Curry_Prelude.OP_List t139) -> Curry_Prelude.C_Int -> t137 -> IDSupply -> ConstStore -> C_Node t137 t139
nd_OP_scc_dot_wrap_dot_24 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (C_Node x3 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) (Curry_Prelude.nd_C_apply x2 x4 x2001 x3500) x4)))))

d_C_tsort :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (C_Node t0 t1) -> ConstStore -> Curry_Prelude.OP_List (C_Node t0 t1)
d_C_tsort x1 x3500 = Curry_Prelude.d_C_snd (d_OP_tsort_dot_dfs_dot_29 x1 x1 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_cmpNode) x3500) Curry_Prelude.OP_List x3500) x3500

d_OP_tsort_dot_dfs_dot_29 :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_List (C_Node t82 t83) -> Curry_Prelude.OP_List (C_Node t82 t83) -> Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83) -> Curry_Prelude.OP_List (C_Node t82 t83) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83))
d_OP_tsort_dot_dfs_dot_29 x1 x2 x3 x4 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_OP_tsort_dot_dfs_dot_29 x1 (d_OP_tsort_dot_dfs_dot_29_dot_defs_dot_33 x1 x5 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x5 x3500) x3 x3500) x4 x3500
          x8 = d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x7 x3500
          x9 = d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x7 x3500
           in (d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x5 x3500) x3 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort_dot_dfs_dot_29 x1 x1002 x3 x4 x3500) (d_OP_tsort_dot_dfs_dot_29 x1 x1003 x3 x4 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort_dot_dfs_dot_29 x1 z x3 x4 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort_dot_dfs_dot_29 x1 x1002 x3 x4) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort_dot_dfs_dot_29 :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_List (C_Node t82 t83) -> Curry_Prelude.OP_List (C_Node t82 t83) -> Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83) -> Curry_Prelude.OP_List (C_Node t82 t83) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83))
nd_OP_tsort_dot_dfs_dot_29 x1 x2 x3 x4 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2014 = x3000
           in (seq x2014 (let
               x2015 = leftSupply x2014
               x2016 = rightSupply x2014
                in (seq x2015 (seq x2016 (let
                    x2006 = leftSupply x2015
                    x2007 = rightSupply x2015
                     in (seq x2006 (seq x2007 (let
                         x2008 = leftSupply x2016
                         x2013 = rightSupply x2016
                          in (seq x2008 (seq x2013 (let
                              x7 = let
                                   x2005 = leftSupply x2006
                                   x2004 = rightSupply x2006
                                    in (seq x2005 (seq x2004 (nd_OP_tsort_dot_dfs_dot_29 x1 (d_OP_tsort_dot_dfs_dot_29_dot_defs_dot_33 x1 x5 x3500) (let
                                        x2003 = leftSupply x2004
                                        x2002 = rightSupply x2004
                                         in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                             x2001 = leftSupply x2002
                                             x2000 = rightSupply x2002
                                              in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x5 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))
                              x8 = nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x7 x2007 x3500
                              x9 = nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x7 x2008 x3500
                               in (let
                                   x2012 = leftSupply x2013
                                   x2011 = rightSupply x2013
                                    in (seq x2012 (seq x2011 (nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 (let
                                        x2010 = leftSupply x2011
                                        x2009 = rightSupply x2011
                                         in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x5 x2009 x3500) x3 x2010 x3500)))) x2012 x3500)))))))))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort_dot_dfs_dot_29 x1 x1002 x3 x4 x3000 x3500) (nd_OP_tsort_dot_dfs_dot_29 x1 x1003 x3 x4 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort_dot_dfs_dot_29 x1 z x3 x4 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort_dot_dfs_dot_29 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_tsort_dot_dfs_dot_29_dot_defs_dot_33 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_List (C_Node t82 t83) -> C_Node t0 t83 -> ConstStore -> Curry_Prelude.OP_List (C_Node t82 t83)
d_OP_tsort_dot_dfs_dot_29_dot_defs_dot_33 x1 x2 x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_fvs x2 x3500)) x3500) d_C_bvs x3500) x1 x3500

d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83)) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)
d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1002 x3500) (d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83)) -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)
nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1002 x3000 x3500) (nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP2_hash_marks' x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83)) -> ConstStore -> Curry_Prelude.OP_List (C_Node t82 t83)
d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1002 x3500) (d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' :: (Curry_Prelude.Curry t82,Curry_Prelude.Curry t83) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t82 t83)) (Curry_Prelude.OP_List (C_Node t82 t83)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_Node t82 t83)
nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1002 x3000 x3500) (nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort_dot_dfs_dot_29_dot___hash_selFP3_hash_stack' x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tsort' :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (C_Node t0 t1) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t0 t1))
d_C_tsort' x1 x3500 = Curry_Prelude.d_C_snd (d_OP_tsort'_dot_dfs_dot_38 x1 x1 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_cmpNode) x3500) Curry_Prelude.OP_List x3500) x3500

d_OP_tsort'_dot_dfs_dot_38 :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_List (C_Node t128 t129) -> Curry_Prelude.OP_List (C_Node t128 t129) -> Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129)) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129)))
d_OP_tsort'_dot_dfs_dot_38 x1 x2 x3 x4 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_OP_tsort'_dot_dfs_dot_38 x1 (d_OP_tsort'_dot_dfs_dot_38_dot_uses_dot_42 x1 x5 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x5 x3500) x3 x3500) Curry_Prelude.OP_List x3500
          x8 = d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x7 x3500
          x9 = d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x7 x3500
           in (d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x5 x3500) x3 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort'_dot_dfs_dot_38 x1 x1002 x3 x4 x3500) (d_OP_tsort'_dot_dfs_dot_38 x1 x1003 x3 x4 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort'_dot_dfs_dot_38 x1 z x3 x4 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort'_dot_dfs_dot_38 x1 x1002 x3 x4) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort'_dot_dfs_dot_38 :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_List (C_Node t128 t129) -> Curry_Prelude.OP_List (C_Node t128 t129) -> Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129)))
nd_OP_tsort'_dot_dfs_dot_38 x1 x2 x3 x4 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2014 = x3000
           in (seq x2014 (let
               x2015 = leftSupply x2014
               x2016 = rightSupply x2014
                in (seq x2015 (seq x2016 (let
                    x2006 = leftSupply x2015
                    x2007 = rightSupply x2015
                     in (seq x2006 (seq x2007 (let
                         x2008 = leftSupply x2016
                         x2013 = rightSupply x2016
                          in (seq x2008 (seq x2013 (let
                              x7 = let
                                   x2005 = leftSupply x2006
                                   x2004 = rightSupply x2006
                                    in (seq x2005 (seq x2004 (nd_OP_tsort'_dot_dfs_dot_38 x1 (d_OP_tsort'_dot_dfs_dot_38_dot_uses_dot_42 x1 x5 x3500) (let
                                        x2003 = leftSupply x2004
                                        x2002 = rightSupply x2004
                                         in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                             x2001 = leftSupply x2002
                                             x2000 = rightSupply x2002
                                              in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x5 x2001 x3500)))) x3 x2003 x3500)))) Curry_Prelude.OP_List x2005 x3500)))
                              x8 = nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x7 x2007 x3500
                              x9 = nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x7 x2008 x3500
                               in (let
                                   x2012 = leftSupply x2013
                                   x2011 = rightSupply x2013
                                    in (seq x2012 (seq x2011 (nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 (let
                                        x2010 = leftSupply x2011
                                        x2009 = rightSupply x2011
                                         in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x5 x2009 x3500) x3 x2010 x3500)))) x2012 x3500)))))))))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort'_dot_dfs_dot_38 x1 x1002 x3 x4 x3000 x3500) (nd_OP_tsort'_dot_dfs_dot_38 x1 x1003 x3 x4 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort'_dot_dfs_dot_38 x1 z x3 x4 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort'_dot_dfs_dot_38 x1 x1002 x3 x4 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_tsort'_dot_dfs_dot_38_dot_uses_dot_42 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_List (C_Node t128 t129) -> C_Node t0 t129 -> ConstStore -> Curry_Prelude.OP_List (C_Node t128 t129)
d_OP_tsort'_dot_dfs_dot_38_dot_uses_dot_42 x1 x2 x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_bvs x2 x3500)) x3500) d_C_fvs x3500) x1 x3500

d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)
d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1002 x3500) (d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))) -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)
nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1002 x3000 x3500) (nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP5_hash_marks' x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))
d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1002 x3500) (d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' :: (Curry_Prelude.Curry t128,Curry_Prelude.Curry t129) => Curry_Prelude.OP_Tuple2 (Curry_RedBlackTree.C_RedBlackTree (C_Node t128 t129)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (C_Node t128 t129))
nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1002 x3000 x3500) (nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_tsort'_dot_dfs_dot_38_dot___hash_selFP6_hash_stack' x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_tsort'_dot_dfs_dot_38 x1 x6 x3 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x1 x4 x5 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1002 x3500) (d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_tsort'_dot_dfs_dot_38 x1 x6 x3 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x4 x5 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x4 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_tsort'_dot_dfs_dot_38 x1 x6 x8 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_C_concat x9 x3500)) x4) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x4 x5 x6 x8 x9 x1002 x3500) (d_OP__case_0 x1 x4 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x4 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x4 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x4 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_tsort'_dot_dfs_dot_38 x1 x6 x8 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_C_concat x9 x3500)) x4) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x4 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_0 x1 x4 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x4 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x4 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_tsort_dot_dfs_dot_29 x1 x6 x3 x4 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x1 x5 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1002 x3500) (d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_tsort_dot_dfs_dot_29 x1 x6 x3 x4 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x5 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x3 x4 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_tsort_dot_dfs_dot_29 x1 x6 x8 (Curry_Prelude.OP_Cons x5 x9) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x5 x6 x8 x9 x1002 x3500) (d_OP__case_2 x1 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_tsort_dot_dfs_dot_29 x1 x6 x8 (Curry_Prelude.OP_Cons x5 x9) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_2 x1 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
