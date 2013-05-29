{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ERD2Graph (C_DotGraph, C_Node, C_Edge, d_C_viewERD, nd_C_viewERD) where

import Basics
import qualified Curry_Char
import qualified Curry_ERD
import qualified Curry_IO
import qualified Curry_IOExts
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_RCFile
data C_DotGraph
     = C_Graph (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_Node) (Curry_Prelude.OP_List C_Edge)
     | Choice_C_DotGraph Cover ID C_DotGraph C_DotGraph
     | Choices_C_DotGraph Cover ID ([C_DotGraph])
     | Fail_C_DotGraph Cover FailInfo
     | Guard_C_DotGraph Cover Constraints C_DotGraph

instance Show C_DotGraph where
  showsPrec d (Choice_C_DotGraph cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_DotGraph cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_DotGraph cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_DotGraph cd info) = showChar '!'
  showsPrec _ (C_Graph x1 x2 x3) = (showString "(Graph") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_DotGraph where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Graph x1 x2 x3,r3) | (_,r0) <- readQualified "ERD2Graph" "Graph" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_DotGraph where
  choiceCons = Choice_C_DotGraph
  choicesCons = Choices_C_DotGraph
  failCons = Fail_C_DotGraph
  guardCons = Guard_C_DotGraph
  try (Choice_C_DotGraph cd i x y) = tryChoice cd i x y
  try (Choices_C_DotGraph cd i xs) = tryChoices cd i xs
  try (Fail_C_DotGraph cd info) = Fail cd info
  try (Guard_C_DotGraph cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_DotGraph cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_DotGraph cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_DotGraph cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_DotGraph cd i _) = error ("ERD2Graph.DotGraph.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_DotGraph cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_DotGraph cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_DotGraph where
  generate s = Choices_C_DotGraph defCover (freeID [3] s) [(C_Graph (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_DotGraph where
  ($!!) cont (C_Graph x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Graph y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_DotGraph cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_DotGraph cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_DotGraph cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_DotGraph cd info) _ = failCons cd info
  ($##) cont (C_Graph x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Graph y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_DotGraph cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_DotGraph cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_DotGraph cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_DotGraph cd info) _ = failCons cd info
  searchNF search cont (C_Graph x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Graph y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("ERD2Graph.DotGraph.searchNF: no constructor: " ++ (show x))


instance Unifiable C_DotGraph where
  (=.=) (C_Graph x1 x2 x3) (C_Graph y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Graph x1 x2 x3) (C_Graph y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Graph x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_DotGraph cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_DotGraph cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_DotGraph cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_DotGraph cd i _) = error ("ERD2Graph.DotGraph.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_DotGraph cd info) = [(Unsolvable info)]
  bind i (Guard_C_DotGraph cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Graph x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_DotGraph cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_DotGraph cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_DotGraph cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_DotGraph cd i _) = error ("ERD2Graph.DotGraph.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_DotGraph cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_DotGraph cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_DotGraph where
  (=?=) (Choice_C_DotGraph cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_DotGraph cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_DotGraph cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_DotGraph cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_DotGraph cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_DotGraph cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_DotGraph cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_DotGraph cd info) _ = failCons cd info
  (=?=) (C_Graph x1 x2 x3) (C_Graph y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_DotGraph cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_DotGraph cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_DotGraph cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_DotGraph cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_DotGraph cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_DotGraph cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_DotGraph cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_DotGraph cd info) _ = failCons cd info
  (<?=) (C_Graph x1 x2 x3) (C_Graph y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_DotGraph where
  cover (C_Graph x1 x2 x3) = C_Graph (cover x1) (cover x2) (cover x3)
  cover (Choice_C_DotGraph cd i x y) = Choice_C_DotGraph (incCover cd) i (cover x) (cover y)
  cover (Choices_C_DotGraph cd i xs) = Choices_C_DotGraph (incCover cd) i (map cover xs)
  cover (Fail_C_DotGraph cd info) = Fail_C_DotGraph (incCover cd) info
  cover (Guard_C_DotGraph cd c e) = Guard_C_DotGraph (incCover cd) c (cover e)


data C_Node
     = C_Node (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | Choice_C_Node Cover ID C_Node C_Node
     | Choices_C_Node Cover ID ([C_Node])
     | Fail_C_Node Cover FailInfo
     | Guard_C_Node Cover Constraints C_Node

instance Show C_Node where
  showsPrec d (Choice_C_Node cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Node cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Node cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Node cd info) = showChar '!'
  showsPrec _ (C_Node x1 x2) = (showString "(Node") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Node where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Node x1 x2,r2) | (_,r0) <- readQualified "ERD2Graph" "Node" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_Node where
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
  match _ _ _ _ _ _ (Choices_C_Node cd i _) = error ("ERD2Graph.Node.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Node cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Node cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Node where
  generate s = Choices_C_Node defCover (freeID [2] s) [(C_Node (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm C_Node where
  ($!!) cont (C_Node x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Node y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Node cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Node cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Node cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Node cd info) _ = failCons cd info
  ($##) cont (C_Node x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Node y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Node cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Node cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Node cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Node cd info) _ = failCons cd info
  searchNF search cont (C_Node x1 x2) = search (\y1 -> search (\y2 -> cont (C_Node y1 y2)) x2) x1
  searchNF _ _ x = error ("ERD2Graph.Node.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Node where
  (=.=) (C_Node x1 x2) (C_Node y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Node x1 x2) (C_Node y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Node x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_Node cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Node cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Node cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Node cd i _) = error ("ERD2Graph.Node.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Node cd info) = [(Unsolvable info)]
  bind i (Guard_C_Node cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Node x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_Node cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Node cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Node cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Node cd i _) = error ("ERD2Graph.Node.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Node cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Node cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Node where
  (=?=) (Choice_C_Node cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Node cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Node cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Node cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Node cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Node cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Node cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Node cd info) _ = failCons cd info
  (=?=) (C_Node x1 x2) (C_Node y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (<?=) (Choice_C_Node cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Node cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Node cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Node cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Node cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Node cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Node cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Node cd info) _ = failCons cd info
  (<?=) (C_Node x1 x2) (C_Node y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs


instance Coverable C_Node where
  cover (C_Node x1 x2) = C_Node (cover x1) (cover x2)
  cover (Choice_C_Node cd i x y) = Choice_C_Node (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Node cd i xs) = Choices_C_Node (incCover cd) i (map cover xs)
  cover (Fail_C_Node cd info) = Fail_C_Node (incCover cd) info
  cover (Guard_C_Node cd c e) = Guard_C_Node (incCover cd) c (cover e)


data C_Edge
     = C_Edge (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | Choice_C_Edge Cover ID C_Edge C_Edge
     | Choices_C_Edge Cover ID ([C_Edge])
     | Fail_C_Edge Cover FailInfo
     | Guard_C_Edge Cover Constraints C_Edge

instance Show C_Edge where
  showsPrec d (Choice_C_Edge cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Edge cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Edge cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Edge cd info) = showChar '!'
  showsPrec _ (C_Edge x1 x2 x3) = (showString "(Edge") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_Edge where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Edge x1 x2 x3,r3) | (_,r0) <- readQualified "ERD2Graph" "Edge" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_Edge where
  choiceCons = Choice_C_Edge
  choicesCons = Choices_C_Edge
  failCons = Fail_C_Edge
  guardCons = Guard_C_Edge
  try (Choice_C_Edge cd i x y) = tryChoice cd i x y
  try (Choices_C_Edge cd i xs) = tryChoices cd i xs
  try (Fail_C_Edge cd info) = Fail cd info
  try (Guard_C_Edge cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Edge cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Edge cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Edge cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Edge cd i _) = error ("ERD2Graph.Edge.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Edge cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Edge cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Edge where
  generate s = Choices_C_Edge defCover (freeID [3] s) [(C_Edge (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_Edge where
  ($!!) cont (C_Edge x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Edge y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Edge cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Edge cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Edge cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Edge cd info) _ = failCons cd info
  ($##) cont (C_Edge x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_Edge y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Edge cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Edge cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Edge cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Edge cd info) _ = failCons cd info
  searchNF search cont (C_Edge x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Edge y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("ERD2Graph.Edge.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Edge where
  (=.=) (C_Edge x1 x2 x3) (C_Edge y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Edge x1 x2 x3) (C_Edge y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Edge x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_Edge cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Edge cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Edge cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Edge cd i _) = error ("ERD2Graph.Edge.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Edge cd info) = [(Unsolvable info)]
  bind i (Guard_C_Edge cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Edge x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_Edge cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Edge cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Edge cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Edge cd i _) = error ("ERD2Graph.Edge.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Edge cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Edge cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Edge where
  (=?=) (Choice_C_Edge cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Edge cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Edge cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Edge cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Edge cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Edge cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Edge cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Edge cd info) _ = failCons cd info
  (=?=) (C_Edge x1 x2 x3) (C_Edge y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_Edge cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Edge cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Edge cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Edge cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Edge cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Edge cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Edge cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Edge cd info) _ = failCons cd info
  (<?=) (C_Edge x1 x2 x3) (C_Edge y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_Edge where
  cover (C_Edge x1 x2 x3) = C_Edge (cover x1) (cover x2) (cover x3)
  cover (Choice_C_Edge cd i x y) = Choice_C_Edge (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Edge cd i xs) = Choices_C_Edge (incCover cd) i (map cover xs)
  cover (Fail_C_Edge cd info) = Fail_C_Edge (incCover cd) info
  cover (Guard_C_Edge cd c e) = Guard_C_Edge (incCover cd) c (cover e)


d_C_relationAsNode :: ConstStore -> Curry_Prelude.C_Bool
d_C_relationAsNode x3500 = Curry_Prelude.C_True

d_C_viewERD :: ConstStore -> Curry_ERD.C_ERD -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_viewERD x3500 = Curry_Prelude.d_OP_dot d_C_viewDot (Curry_Prelude.d_OP_dot d_C_showDotGraph d_C_erd2dot x3500) x3500

nd_C_viewERD :: IDSupply -> ConstStore -> Func Curry_ERD.C_ERD (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_viewERD x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_viewDot) (Curry_Prelude.nd_OP_dot (wrapDX id d_C_showDotGraph) (wrapDX id d_C_erd2dot) x2000 x3500) x2001 x3500)))))

d_C_erd2dot :: Curry_ERD.C_ERD -> ConstStore -> C_DotGraph
d_C_erd2dot x1 x3500 = case x1 of
     (Curry_ERD.C_ERD x2 x3 x4) -> let
          x5 = Curry_Prelude.d_C_map d_OP_erd2dot_dot_entity2dot_dot_6 x3 x3500
          x6 = Curry_Prelude.d_C_unzip (Curry_Prelude.d_C_map d_OP_erd2dot_dot_relationship2dot_dot_6 x4 x3500) x3500
          x7 = d_OP_erd2dot_dot___hash_selFP2_hash_rnodes x6 x3500
          x8 = d_OP_erd2dot_dot___hash_selFP3_hash_redges x6 x3500
           in (C_Graph x2 (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_C_concat x7 x3500) x3500) (Curry_Prelude.d_C_concat x8 x3500))
     (Curry_ERD.Choice_C_ERD x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_erd2dot x1002 x3500) (d_C_erd2dot x1003 x3500)
     (Curry_ERD.Choices_C_ERD x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_erd2dot z x3500) x1002
     (Curry_ERD.Guard_C_ERD x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_erd2dot x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_ERD x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_showDomain_dot_6 :: Curry_ERD.C_Domain -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_erd2dot_dot_showDomain_dot_6 x1 x3500 = case x1 of
     (Curry_ERD.C_IntDom x2) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))
     (Curry_ERD.C_FloatDom x3) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))
     (Curry_ERD.C_CharDom x4) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))
     (Curry_ERD.C_StringDom x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List)))))
     (Curry_ERD.C_BoolDom x6) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'B'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))
     (Curry_ERD.C_DateDom x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))
     (Curry_ERD.C_UserDefined x8 x9) -> x8
     (Curry_ERD.C_KeyDom x10) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))
     (Curry_ERD.Choice_C_Domain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_showDomain_dot_6 x1002 x3500) (d_OP_erd2dot_dot_showDomain_dot_6 x1003 x3500)
     (Curry_ERD.Choices_C_Domain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_showDomain_dot_6 z x3500) x1002
     (Curry_ERD.Guard_C_Domain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_showDomain_dot_6 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Domain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_showAttr_dot_6 :: Curry_ERD.C_Attribute -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_erd2dot_dot_showAttr_dot_6 x1 x3500 = case x1 of
     (Curry_ERD.C_Attribute x2 x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_OP_erd2dot_dot_showDomain_dot_6 x3 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_13 x4 (Curry_Prelude.d_OP_eq_eq x4 Curry_ERD.C_NoKey x3500) x3500) (d_OP__case_12 x5 x3500) x3500) x3500) x3500) x3500
     (Curry_ERD.Choice_C_Attribute x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_showAttr_dot_6 x1002 x3500) (d_OP_erd2dot_dot_showAttr_dot_6 x1003 x3500)
     (Curry_ERD.Choices_C_Attribute x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_showAttr_dot_6 z x3500) x1002
     (Curry_ERD.Guard_C_Attribute x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_showAttr_dot_6 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Attribute x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_entity2dot_dot_6 :: Curry_ERD.C_Entity -> ConstStore -> C_Node
d_OP_erd2dot_dot_entity2dot_dot_6 x1 x3500 = case x1 of
     (Curry_ERD.C_Entity x2 x3) -> C_Node x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map d_OP_erd2dot_dot_showAttr_dot_6 x3 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500)) Curry_Prelude.OP_List)))
     (Curry_ERD.Choice_C_Entity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_entity2dot_dot_6 x1002 x3500) (d_OP_erd2dot_dot_entity2dot_dot_6 x1003 x3500)
     (Curry_ERD.Choices_C_Entity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_entity2dot_dot_6 z x3500) x1002
     (Curry_ERD.Guard_C_Entity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_entity2dot_dot_6 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Entity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_showCard_dot_6 :: Curry_ERD.C_Cardinality -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_erd2dot_dot_showCard_dot_6 x1 x3500 = case x1 of
     (Curry_ERD.C_Exactly x2) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500)
     (Curry_ERD.C_Between x3 x4) -> d_OP__case_11 x3 x4 x3500
     (Curry_ERD.Choice_C_Cardinality x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_showCard_dot_6 x1002 x3500) (d_OP_erd2dot_dot_showCard_dot_6 x1003 x3500)
     (Curry_ERD.Choices_C_Cardinality x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_showCard_dot_6 z x3500) x1002
     (Curry_ERD.Guard_C_Cardinality x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_showCard_dot_6 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Cardinality x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_relationship2dot_dot_6 :: Curry_ERD.C_Relationship -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Node) (Curry_Prelude.OP_List C_Edge)
d_OP_erd2dot_dot_relationship2dot_dot_6 x1 x3500 = case x1 of
     (Curry_ERD.C_Relationship x2 x3) -> d_OP__case_10 x2 x3 x3500
     (Curry_ERD.Choice_C_Relationship x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_relationship2dot_dot_6 x1002 x3500) (d_OP_erd2dot_dot_relationship2dot_dot_6 x1003 x3500)
     (Curry_ERD.Choices_C_Relationship x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_relationship2dot_dot_6 z x3500) x1002
     (Curry_ERD.Guard_C_Relationship x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_relationship2dot_dot_6 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_Relationship x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_Edge
d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> C_Edge x2 x3 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 x1002 x3500) (d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot___hash_selFP2_hash_rnodes :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Node)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Edge)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Node)
d_OP_erd2dot_dot___hash_selFP2_hash_rnodes x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot___hash_selFP2_hash_rnodes x1002 x3500) (d_OP_erd2dot_dot___hash_selFP2_hash_rnodes x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot___hash_selFP2_hash_rnodes z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot___hash_selFP2_hash_rnodes x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_erd2dot_dot___hash_selFP3_hash_redges :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Node)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Edge)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List C_Edge)
d_OP_erd2dot_dot___hash_selFP3_hash_redges x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_erd2dot_dot___hash_selFP3_hash_redges x1002 x3500) (d_OP_erd2dot_dot___hash_selFP3_hash_redges x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_erd2dot_dot___hash_selFP3_hash_redges z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_erd2dot_dot___hash_selFP3_hash_redges x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showDotGraph :: C_DotGraph -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showDotGraph x1 x3500 = case x1 of
     (C_Graph x2 x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showDotGraph_dot_node2dot_dot_47 x3500) x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showDotGraph_dot_edge2dot_dot_47 x3500) x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500) x3500
     (Choice_C_DotGraph x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showDotGraph x1002 x3500) (d_C_showDotGraph x1003 x3500)
     (Choices_C_DotGraph x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showDotGraph z x3500) x1002
     (Guard_C_DotGraph x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showDotGraph x1002) $! (addCs x1001 x3500))
     (Fail_C_DotGraph x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showDotGraph_dot_showDotID_dot_47 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_showDotID_dot_47 x1 x3500 = d_OP__case_4 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isAlphaNum x3500) x1 x3500) x3500

d_OP_showDotGraph_dot_showDotID_dot_47_dot_escapeDQ_dot_55 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_showDotID_dot_47_dot_escapeDQ_dot_55 x1 x3500 = d_OP__case_2 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '"'#) x3500) x3500

d_OP_showDotGraph_dot_node2dot_dot_47 :: C_Node -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_node2dot_dot_47 x1 x3500 = case x1 of
     (C_Node x2 x3) -> d_OP__case_1 x2 x3 (Curry_Prelude.d_C_null x3 x3500) x3500
     (Choice_C_Node x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showDotGraph_dot_node2dot_dot_47 x1002 x3500) (d_OP_showDotGraph_dot_node2dot_dot_47 x1003 x3500)
     (Choices_C_Node x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showDotGraph_dot_node2dot_dot_47 z x3500) x1002
     (Guard_C_Node x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showDotGraph_dot_node2dot_dot_47 x1002) $! (addCs x1001 x3500))
     (Fail_C_Node x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 x1002 x3500) (d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showDotGraph_dot_edge2dot_dot_47 :: C_Edge -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_edge2dot_dot_47 x1 x3500 = case x1 of
     (C_Edge x2 x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x3 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_0 x4 (Curry_Prelude.d_C_null x4 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500
     (Choice_C_Edge x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showDotGraph_dot_edge2dot_dot_47 x1002 x3500) (d_OP_showDotGraph_dot_edge2dot_dot_47 x1003 x3500)
     (Choices_C_Edge x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showDotGraph_dot_edge2dot_dot_47 z x3500) x1002
     (Guard_C_Edge x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showDotGraph_dot_edge2dot_dot_47 x1002) $! (addCs x1001 x3500))
     (Fail_C_Edge x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 x1002 x3500) (d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_viewDot :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_viewDot x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getDotViewCmd x3500) (d_OP_viewDot_dot___hash_lambda4 x1) x3500

d_OP_viewDot_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_viewDot_dot___hash_lambda4 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_connectToCommand x2 x3500) (d_OP_viewDot_dot___hash_lambda4_dot___hash_lambda5 x1) x3500

d_OP_viewDot_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_viewDot_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStr x2 x1 x3500) (Curry_IO.d_C_hClose x2 x3500) x3500

d_C_getDotViewCmd :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getDotViewCmd x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_RCFile.d_C_readRC x3500) d_OP_getDotViewCmd_dot___hash_lambda6 x3500

d_OP_getDotViewCmd_dot___hash_lambda6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getDotViewCmd_dot___hash_lambda6 x1 x3500 = Curry_Prelude.d_C_return (Curry_RCFile.d_C_rcValue x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))) x3500) x3500

d_OP__case_0 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3 x4 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x4 x1002 x3500) (d_OP__case_0 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.nd_C_map (wrapDX id d_OP_showDotGraph_dot_edge2dot_dot_47_dot___hash_lambda3) x4 x2000 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x4 x1002 x3000 x3500) (nd_OP__case_0 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2 x3 x3500) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500)) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x3 x1002 x3500) (d_OP__case_1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_OP_showDotGraph_dot_showDotID_dot_47 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.nd_C_map (wrapDX id d_OP_showDotGraph_dot_node2dot_dot_47_dot___hash_lambda2) x3 x2000 x3500) x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500)) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x3 x1002 x3000 x3500) (nd_OP__case_1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP__case_3 x1 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3500) (d_OP__case_4 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x1002 x3000 x3500) (nd_OP__case_4 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showDotGraph_dot_showDotID_dot_47_dot_escapeDQ_dot_55 x3500) x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x1002 x3500) (d_OP__case_3 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showDotGraph_dot_showDotID_dot_47_dot_escapeDQ_dot_55) x2000 x3500) x1 x2001 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x1002 x3000 x3500) (nd_OP__case_3 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_9 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x1002 x3500) (d_OP__case_10 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x2 x1002 x3000 x3500) (nd_OP__case_10 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x2 x5 x4 x3500 = case x4 of
     (Curry_ERD.C_REnd x6 x7 x8) -> d_OP__case_8 x2 x6 x7 x8 x5 x3500
     (Curry_ERD.Choice_C_REnd x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x2 x5 x1002 x3500) (d_OP__case_9 x2 x5 x1003 x3500)
     (Curry_ERD.Choices_C_REnd x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x2 x5 z x3500) x1002
     (Curry_ERD.Guard_C_REnd x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_REnd x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_ERD.C_REnd x6 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x2 x6 x7 x8 x5 x2000 x3500))
     (Curry_ERD.Choice_C_REnd x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x2 x5 x1002 x3000 x3500) (nd_OP__case_9 x2 x5 x1003 x3000 x3500)
     (Curry_ERD.Choices_C_REnd x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x2 x5 z x3000 x3500) x1002
     (Curry_ERD.Guard_C_REnd x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_REnd x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x6 x7 x8 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x9 x10) -> d_OP__case_7 x2 x6 x7 x8 x10 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x6 x7 x8 x1002 x3500) (d_OP__case_8 x2 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x6 x7 x8 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x2 x6 x7 x8 x10 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_8 x2 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x2 x6 x7 x8 x10 x9 x3500 = case x9 of
     (Curry_ERD.C_REnd x11 x12 x13) -> d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x10 x3500
     (Curry_ERD.Choice_C_REnd x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x6 x7 x8 x10 x1002 x3500) (d_OP__case_7 x2 x6 x7 x8 x10 x1003 x3500)
     (Curry_ERD.Choices_C_REnd x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 x6 x7 x8 x10 z x3500) x1002
     (Curry_ERD.Guard_C_REnd x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x6 x7 x8 x10 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_REnd x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x2 x6 x7 x8 x10 x9 x3000 x3500 = case x9 of
     (Curry_ERD.C_REnd x11 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x10 x2000 x3500))
     (Curry_ERD.Choice_C_REnd x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x2 x6 x7 x8 x10 x1002 x3000 x3500) (nd_OP__case_7 x2 x6 x7 x8 x10 x1003 x3000 x3500)
     (Curry_ERD.Choices_C_REnd x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x2 x6 x7 x8 x10 z x3000 x3500) x1002
     (Curry_ERD.Guard_C_REnd x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x2 x6 x7 x8 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_REnd x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x10 x3500 = case x10 of
     Curry_Prelude.OP_List -> d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 (d_C_relationAsNode x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1002 x3500) (d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x10 x3000 x3500 = case x10 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 (d_C_relationAsNode x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x2 x6 x7 x8 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_Node x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Node (Curry_Prelude.d_OP_plus_plus x2 x7 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x8 x3500) x3500) x3500)) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Node (Curry_Prelude.d_OP_plus_plus x2 x12 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x13 x3500) x3500) x3500)) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_map d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_OP_plus_plus x2 x7 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 x7 x3500) x6) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_OP_plus_plus x2 x12 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 x12 x3500) x11) Curry_Prelude.OP_List)))) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_Node x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (C_Edge x2 x6 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x8 x3500) x3500) x3500)) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Edge x2 x11 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x13 x3500) x3500) x3500)) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1002 x3500) (d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_Node x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Node (Curry_Prelude.d_OP_plus_plus x2 x7 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x8 x3500) x3500) x3500)) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Node (Curry_Prelude.d_OP_plus_plus x2 x12 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x13 x3500) x3500) x3500)) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))) (Curry_Prelude.nd_C_map (wrapDX id d_OP_erd2dot_dot_relationship2dot_dot_6_dot___hash_lambda1) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_OP_plus_plus x2 x7 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 x7 x3500) x6) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_OP_plus_plus x2 x12 x3500)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 x12 x3500) x11) Curry_Prelude.OP_List)))) x2000 x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_Node x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (C_Edge x2 x6 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x8 x3500) x3500) x3500)) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (C_Edge x2 x11 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x12 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) (d_OP_erd2dot_dot_showCard_dot_6 x13 x3500) x3500) x3500)) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x6 x7 x8 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x3 x4 x3500 = case x4 of
     Curry_ERD.C_Infinite -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))) x3500)
     (Curry_ERD.C_Max x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x5 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500)
     (Curry_ERD.Choice_C_MaxValue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x3 x1002 x3500) (d_OP__case_11 x3 x1003 x3500)
     (Curry_ERD.Choices_C_MaxValue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x3 z x3500) x1002
     (Curry_ERD.Guard_C_MaxValue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x3 x1002) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_MaxValue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x3 x4 x3000 x3500 = case x4 of
     Curry_ERD.C_Infinite -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))) x3500)
     (Curry_ERD.C_Max x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x5 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500) x3500) x3500)
     (Curry_ERD.Choice_C_MaxValue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x3 x1002 x3000 x3500) (nd_OP__case_11 x3 x1003 x3000 x3500)
     (Curry_ERD.Choices_C_MaxValue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x3 z x3000 x3500) x1002
     (Curry_ERD.Guard_C_MaxValue x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_ERD.Fail_C_MaxValue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1002 x3500) (d_OP__case_12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1002 x3000 x3500) (nd_OP__case_12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_show x4 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x4 x1002 x3500) (d_OP__case_13 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_show x4 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x4 x1002 x3000 x3500) (nd_OP__case_13 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
