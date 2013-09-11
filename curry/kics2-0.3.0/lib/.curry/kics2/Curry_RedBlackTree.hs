{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_RedBlackTree (C_RedBlackTree, C_Color, C_Tree, d_C_empty, nd_C_empty, d_C_isEmpty, nd_C_isEmpty, d_C_newTreeLike, nd_C_newTreeLike, d_C_lookup, nd_C_lookup, d_C_update, nd_C_update, d_C_delete, nd_C_delete, d_C_tree2list, nd_C_tree2list, d_C_sort, nd_C_sort, d_C_setInsertEquivalence, nd_C_setInsertEquivalence) where

import Basics
import qualified Curry_Prelude
data C_RedBlackTree t0
     = C_RedBlackTree (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) (C_Tree t0)
     | HO_C_RedBlackTree (Func t0 (Func t0 Curry_Prelude.C_Bool)) (Func t0 (Func t0 Curry_Prelude.C_Bool)) (Func t0 (Func t0 Curry_Prelude.C_Bool)) (C_Tree t0)
     | Choice_C_RedBlackTree Cover ID (C_RedBlackTree t0) (C_RedBlackTree t0)
     | Choices_C_RedBlackTree Cover ID ([C_RedBlackTree t0])
     | Fail_C_RedBlackTree Cover FailInfo
     | Guard_C_RedBlackTree Cover Constraints (C_RedBlackTree t0)

instance Show t0 => Show (C_RedBlackTree t0) where
  showsPrec d (Choice_C_RedBlackTree cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_RedBlackTree cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_RedBlackTree cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_RedBlackTree cd info) = showChar '!'
  showsPrec _ (C_RedBlackTree x1 x2 x3 x4) = (showString "(RedBlackTree") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (HO_C_RedBlackTree x1 x2 x3 x4) = (showString "(RedBlackTree") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read t0 => Read (C_RedBlackTree t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_RedBlackTree x1 x2 x3 x4,r4) | (_,r0) <- readQualified "RedBlackTree" "RedBlackTree" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet (C_RedBlackTree t0) where
  choiceCons = Choice_C_RedBlackTree
  choicesCons = Choices_C_RedBlackTree
  failCons = Fail_C_RedBlackTree
  guardCons = Guard_C_RedBlackTree
  try (Choice_C_RedBlackTree cd i x y) = tryChoice cd i x y
  try (Choices_C_RedBlackTree cd i xs) = tryChoices cd i xs
  try (Fail_C_RedBlackTree cd info) = Fail cd info
  try (Guard_C_RedBlackTree cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_RedBlackTree cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_RedBlackTree cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_RedBlackTree cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_RedBlackTree cd i _) = error ("RedBlackTree.RedBlackTree.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_RedBlackTree cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_RedBlackTree cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_RedBlackTree t0) where
  generate s c = Choices_C_RedBlackTree c (freeID [4] s) [(HO_C_RedBlackTree (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_RedBlackTree t0) where
  ($!!) cont (C_RedBlackTree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_RedBlackTree y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_RedBlackTree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_RedBlackTree y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_RedBlackTree cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_RedBlackTree cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_RedBlackTree cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_RedBlackTree cd info) _ _ = failCons cd info
  ($##) cont (C_RedBlackTree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_RedBlackTree y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_RedBlackTree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_RedBlackTree y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_RedBlackTree cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_RedBlackTree cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_RedBlackTree cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_RedBlackTree cd info) _ _ = failCons cd info
  searchNF search cont (C_RedBlackTree x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_RedBlackTree y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (HO_C_RedBlackTree x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (HO_C_RedBlackTree y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("RedBlackTree.RedBlackTree.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_RedBlackTree t0) where
  (=.=) (C_RedBlackTree x1 x2 x3 x4) (C_RedBlackTree y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_RedBlackTree x1 x2 x3 x4) (HO_C_RedBlackTree y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_RedBlackTree x1 x2 x3 x4) (C_RedBlackTree y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_RedBlackTree x1 x2 x3 x4) (HO_C_RedBlackTree y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_RedBlackTree x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (HO_C_RedBlackTree x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_RedBlackTree cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_RedBlackTree cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_RedBlackTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_RedBlackTree cd i _) = error ("RedBlackTree.RedBlackTree.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_RedBlackTree cd info) = [(Unsolvable info)]
  bind d i (Guard_C_RedBlackTree cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_RedBlackTree x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (HO_C_RedBlackTree x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_RedBlackTree cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_RedBlackTree cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_RedBlackTree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_RedBlackTree cd i _) = error ("RedBlackTree.RedBlackTree.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_RedBlackTree cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_RedBlackTree cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_RedBlackTree t0) where
  (=?=) (Choice_C_RedBlackTree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_RedBlackTree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_RedBlackTree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_RedBlackTree cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_RedBlackTree cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_RedBlackTree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_RedBlackTree cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_RedBlackTree cd info) _ _ = failCons cd info
  (=?=) (C_RedBlackTree x1 x2 x3 x4) (C_RedBlackTree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (HO_C_RedBlackTree x1 x2 x3 x4) (HO_C_RedBlackTree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_RedBlackTree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_RedBlackTree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_RedBlackTree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_RedBlackTree cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_RedBlackTree cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_RedBlackTree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_RedBlackTree cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_RedBlackTree cd info) _ _ = failCons cd info
  (<?=) (C_RedBlackTree x1 x2 x3 x4) (C_RedBlackTree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_RedBlackTree x1 x2 x3 x4) (HO_C_RedBlackTree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_Color
     = C_Red
     | C_Black
     | C_DoublyBlack
     | Choice_C_Color Cover ID C_Color C_Color
     | Choices_C_Color Cover ID ([C_Color])
     | Fail_C_Color Cover FailInfo
     | Guard_C_Color Cover Constraints C_Color

instance Show C_Color where
  showsPrec d (Choice_C_Color cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Color cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Color cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Color cd info) = showChar '!'
  showsPrec _ C_Red = showString "Red"
  showsPrec _ C_Black = showString "Black"
  showsPrec _ C_DoublyBlack = showString "DoublyBlack"


instance Read C_Color where
  readsPrec _ s = (readParen False (\r -> [ (C_Red,r0) | (_,r0) <- readQualified "RedBlackTree" "Red" r]) s) ++ ((readParen False (\r -> [ (C_Black,r0) | (_,r0) <- readQualified "RedBlackTree" "Black" r]) s) ++ (readParen False (\r -> [ (C_DoublyBlack,r0) | (_,r0) <- readQualified "RedBlackTree" "DoublyBlack" r]) s))


instance NonDet C_Color where
  choiceCons = Choice_C_Color
  choicesCons = Choices_C_Color
  failCons = Fail_C_Color
  guardCons = Guard_C_Color
  try (Choice_C_Color cd i x y) = tryChoice cd i x y
  try (Choices_C_Color cd i xs) = tryChoices cd i xs
  try (Fail_C_Color cd info) = Fail cd info
  try (Guard_C_Color cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Color cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Color cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Color cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Color cd i _) = error ("RedBlackTree.Color.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Color cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Color cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Color where
  generate s c = Choices_C_Color c (freeID [0,0,0] s) [C_Red,C_Black,C_DoublyBlack]


instance NormalForm C_Color where
  ($!!) cont C_Red d cs = cont C_Red d cs
  ($!!) cont C_Black d cs = cont C_Black d cs
  ($!!) cont C_DoublyBlack d cs = cont C_DoublyBlack d cs
  ($!!) cont (Choice_C_Color cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Color cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Color cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Color cd info) _ _ = failCons cd info
  ($##) cont C_Red d cs = cont C_Red d cs
  ($##) cont C_Black d cs = cont C_Black d cs
  ($##) cont C_DoublyBlack d cs = cont C_DoublyBlack d cs
  ($##) cont (Choice_C_Color cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Color cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Color cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Color cd info) _ _ = failCons cd info
  searchNF _ cont C_Red = cont C_Red
  searchNF _ cont C_Black = cont C_Black
  searchNF _ cont C_DoublyBlack = cont C_DoublyBlack
  searchNF _ _ x = error ("RedBlackTree.Color.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Color where
  (=.=) C_Red C_Red d cs = C_Success
  (=.=) C_Black C_Black d cs = C_Success
  (=.=) C_DoublyBlack C_DoublyBlack d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Red C_Red d cs = C_Success
  (=.<=) C_Black C_Black d cs = C_Success
  (=.<=) C_DoublyBlack C_DoublyBlack d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Red = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Black = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_DoublyBlack = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_Color cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Color cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Color cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Color cd i _) = error ("RedBlackTree.Color.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Color cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Color cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Red = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Black = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_DoublyBlack = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Color cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Color cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Color cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Color cd i _) = error ("RedBlackTree.Color.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Color cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Color cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Color where
  (=?=) (Choice_C_Color cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Color cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Color cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Color cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Color cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Color cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Color cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Color cd info) _ _ = failCons cd info
  (=?=) C_Red C_Red d cs = Curry_Prelude.C_True
  (=?=) C_Black C_Black d cs = Curry_Prelude.C_True
  (=?=) C_DoublyBlack C_DoublyBlack d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Color cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Color cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Color cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Color cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Color cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Color cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Color cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Color cd info) _ _ = failCons cd info
  (<?=) C_Red C_Red d cs = Curry_Prelude.C_True
  (<?=) C_Red C_Black _ _ = Curry_Prelude.C_True
  (<?=) C_Red C_DoublyBlack _ _ = Curry_Prelude.C_True
  (<?=) C_Black C_Black d cs = Curry_Prelude.C_True
  (<?=) C_Black C_DoublyBlack _ _ = Curry_Prelude.C_True
  (<?=) C_DoublyBlack C_DoublyBlack d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Tree t0
     = C_Tree C_Color t0 (C_Tree t0) (C_Tree t0)
     | C_Empty
     | Choice_C_Tree Cover ID (C_Tree t0) (C_Tree t0)
     | Choices_C_Tree Cover ID ([C_Tree t0])
     | Fail_C_Tree Cover FailInfo
     | Guard_C_Tree Cover Constraints (C_Tree t0)

instance Show t0 => Show (C_Tree t0) where
  showsPrec d (Choice_C_Tree cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Tree cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Tree cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Tree cd info) = showChar '!'
  showsPrec _ (C_Tree x1 x2 x3 x4) = (showString "(Tree") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ C_Empty = showString "Empty"


instance Read t0 => Read (C_Tree t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Tree x1 x2 x3 x4,r4) | (_,r0) <- readQualified "RedBlackTree" "Tree" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ (readParen False (\r -> [ (C_Empty,r0) | (_,r0) <- readQualified "RedBlackTree" "Empty" r]) s)


instance NonDet (C_Tree t0) where
  choiceCons = Choice_C_Tree
  choicesCons = Choices_C_Tree
  failCons = Fail_C_Tree
  guardCons = Guard_C_Tree
  try (Choice_C_Tree cd i x y) = tryChoice cd i x y
  try (Choices_C_Tree cd i xs) = tryChoices cd i xs
  try (Fail_C_Tree cd info) = Fail cd info
  try (Guard_C_Tree cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Tree cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Tree cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Tree cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Tree cd i _) = error ("RedBlackTree.Tree.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Tree cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Tree cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Tree t0) where
  generate s c = Choices_C_Tree c (freeID [4,0] s) [(C_Tree (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),C_Empty]


instance NormalForm t0 => NormalForm (C_Tree t0) where
  ($!!) cont (C_Tree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Tree y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont C_Empty d cs = cont C_Empty d cs
  ($!!) cont (Choice_C_Tree cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Tree cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Tree cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Tree cd info) _ _ = failCons cd info
  ($##) cont (C_Tree x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Tree y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont C_Empty d cs = cont C_Empty d cs
  ($##) cont (Choice_C_Tree cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Tree cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Tree cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Tree cd info) _ _ = failCons cd info
  searchNF search cont (C_Tree x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Tree y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ cont C_Empty = cont C_Empty
  searchNF _ _ x = error ("RedBlackTree.Tree.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Tree t0) where
  (=.=) (C_Tree x1 x2 x3 x4) (C_Tree y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) C_Empty C_Empty d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Tree x1 x2 x3 x4) (C_Tree y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) C_Empty C_Empty d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Tree x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i C_Empty = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_Tree cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Tree cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Tree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Tree cd i _) = error ("RedBlackTree.Tree.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Tree cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Tree cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Tree x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i C_Empty = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_Tree cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Tree cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Tree cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Tree cd i _) = error ("RedBlackTree.Tree.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Tree cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Tree cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Tree t0) where
  (=?=) (Choice_C_Tree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Tree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Tree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Tree cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Tree cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Tree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Tree cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Tree cd info) _ _ = failCons cd info
  (=?=) (C_Tree x1 x2 x3 x4) (C_Tree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Tree cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Tree cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Tree cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Tree cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Tree cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Tree cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Tree cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Tree cd info) _ _ = failCons cd info
  (<?=) (C_Tree x1 x2 x3 x4) (C_Tree y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_Tree _ _ _ _) C_Empty _ _ = Curry_Prelude.C_True
  (<?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_empty :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> C_RedBlackTree t0
d_C_empty x1 x2 x3 x3250 x3500 = C_RedBlackTree x1 x2 x3 C_Empty

nd_C_empty :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> IDSupply -> Cover -> ConstStore -> C_RedBlackTree t0
nd_C_empty x1 x2 x3 x3000 x3250 x3500 = HO_C_RedBlackTree x1 x2 x3 C_Empty

d_C_isEmpty :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmpty x1 x3250 x3500 = case x1 of
     (C_RedBlackTree x2 x3 x4 x5) -> d_OP__case_177 x5 x3250 x3500
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isEmpty x1002 x3250 x3500) (d_C_isEmpty x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isEmpty z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isEmpty x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isEmpty :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isEmpty x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RedBlackTree x2 x3 x4 x5) -> d_OP__case_177 x5 x3250 x3500
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isEmpty x1002 x3000 x3250 x3500) (nd_C_isEmpty x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isEmpty z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isEmpty x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_newTreeLike :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> Cover -> ConstStore -> C_RedBlackTree t0
d_C_newTreeLike x1 x3250 x3500 = case x1 of
     (C_RedBlackTree x2 x3 x4 x5) -> C_RedBlackTree x2 x3 x4 C_Empty
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_newTreeLike x1002 x3250 x3500) (d_C_newTreeLike x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_newTreeLike z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_newTreeLike x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_newTreeLike :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> C_RedBlackTree t0
nd_C_newTreeLike x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RedBlackTree x2 x3 x4 x5) -> HO_C_RedBlackTree x2 x3 x4 C_Empty
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_newTreeLike x1002 x3000 x3250 x3500) (nd_C_newTreeLike x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_newTreeLike z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_newTreeLike x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lookup :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_C_lookup x1 x2 x3250 x3500 = case x2 of
     (C_RedBlackTree x3 x4 x5 x6) -> d_C_lookupTree x4 x5 x1 x6 x3250 x3500
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookup x1 x1002 x3250 x3500) (d_C_lookup x1 x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookup x1 z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookup x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_lookup :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
nd_C_lookup x1 x2 x3000 x3250 x3500 = case x2 of
     (HO_C_RedBlackTree x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_lookupTree x4 x5 x1 x6 x2000 x3250 x3500))
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_lookup x1 x1002 x3000 x3250 x3500) (nd_C_lookup x1 x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_lookup x1 z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_lookup x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lookupTree :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_C_lookupTree x1 x2 x3 x4 x3250 x3500 = case x4 of
     C_Empty -> Curry_Prelude.C_Nothing
     (C_Tree x5 x6 x7 x8) -> d_OP__case_176 x6 x3 x1 x2 x8 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupTree x1 x2 x3 x1002 x3250 x3500) (d_C_lookupTree x1 x2 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupTree x1 x2 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupTree x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_lookupTree :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
nd_C_lookupTree x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     C_Empty -> Curry_Prelude.C_Nothing
     (C_Tree x5 x6 x7 x8) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_176 x6 x3 x1 x2 x8 x7 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_lookupTree x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_lookupTree x1 x2 x3 x1003 x3000 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_lookupTree x1 x2 x3 z x3000 x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_lookupTree x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_update :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> Cover -> ConstStore -> C_RedBlackTree t0
d_C_update x1 x2 x3250 x3500 = case x2 of
     (C_RedBlackTree x3 x4 x5 x6) -> C_RedBlackTree x3 x4 x5 (d_C_updateTree x3 x5 x1 x6 x3250 x3500)
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_update x1 x1002 x3250 x3500) (d_C_update x1 x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_update x1 z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_update x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_update :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> C_RedBlackTree t0
nd_C_update x1 x2 x3000 x3250 x3500 = case x2 of
     (HO_C_RedBlackTree x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (HO_C_RedBlackTree x3 x4 x5 (nd_C_updateTree x3 x5 x1 x6 x2000 x3250 x3500)))
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_update x1 x1002 x3000 x3250 x3500) (nd_C_update x1 x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_update x1 z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_update x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_updateTree :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_updateTree x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_OP_updateTree_dot_upd_dot_34 x3 x1 x2 x4 x3250 x3500
     x6 = d_OP_updateTree_dot___hash_selFP2_hash_e2 x5 x3250 x3500
     x7 = d_OP_updateTree_dot___hash_selFP3_hash_l x5 x3250 x3500
     x8 = d_OP_updateTree_dot___hash_selFP4_hash_r x5 x3250 x3500
      in (C_Tree C_Black x6 x7 x8)

nd_C_updateTree :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_C_updateTree x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = nd_OP_updateTree_dot_upd_dot_34 x3 x1 x2 x4 x2000 x3250 x3500
          x6 = d_OP_updateTree_dot___hash_selFP2_hash_e2 x5 x3250 x3500
          x7 = d_OP_updateTree_dot___hash_selFP3_hash_l x5 x3250 x3500
          x8 = d_OP_updateTree_dot___hash_selFP4_hash_r x5 x3250 x3500
           in (C_Tree C_Black x6 x7 x8)))

d_OP_updateTree_dot_upd_dot_34 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x4 x3250 x3500 = case x4 of
     C_Empty -> C_Tree C_Red x1 C_Empty C_Empty
     (C_Tree x5 x6 x7 x8) -> d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x1 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1002 x3250 x3500) (d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_updateTree_dot_upd_dot_34 :: Curry_Prelude.Curry t0 => t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     C_Empty -> C_Tree C_Red x1 C_Empty C_Empty
     (C_Tree x5 x6 x7 x8) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x1 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateTree_dot___hash_selFP2_hash_e2 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_updateTree_dot___hash_selFP2_hash_e2 x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateTree_dot___hash_selFP2_hash_e2 x1002 x3250 x3500) (d_OP_updateTree_dot___hash_selFP2_hash_e2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateTree_dot___hash_selFP2_hash_e2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateTree_dot___hash_selFP2_hash_e2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateTree_dot___hash_selFP3_hash_l :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_updateTree_dot___hash_selFP3_hash_l x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateTree_dot___hash_selFP3_hash_l x1002 x3250 x3500) (d_OP_updateTree_dot___hash_selFP3_hash_l x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateTree_dot___hash_selFP3_hash_l z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateTree_dot___hash_selFP3_hash_l x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateTree_dot___hash_selFP4_hash_r :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_updateTree_dot___hash_selFP4_hash_r x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateTree_dot___hash_selFP4_hash_r x1002 x3250 x3500) (d_OP_updateTree_dot___hash_selFP4_hash_r x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateTree_dot___hash_selFP4_hash_r z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateTree_dot___hash_selFP4_hash_r x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_delete :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> Cover -> ConstStore -> C_RedBlackTree t0
d_C_delete x1 x2 x3250 x3500 = case x2 of
     (C_RedBlackTree x3 x4 x5 x6) -> C_RedBlackTree x3 x4 x5 (d_OP_delete_dot_blackenRoot_dot_43 (d_C_deleteTree x4 x5 x1 x6 x3250 x3500) x3250 x3500)
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_delete x1 x1002 x3250 x3500) (d_C_delete x1 x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_delete x1 z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_delete x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_delete :: Curry_Prelude.Curry t0 => t0 -> C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> C_RedBlackTree t0
nd_C_delete x1 x2 x3000 x3250 x3500 = case x2 of
     (HO_C_RedBlackTree x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (HO_C_RedBlackTree x3 x4 x5 (d_OP_delete_dot_blackenRoot_dot_43 (nd_C_deleteTree x4 x5 x1 x6 x2000 x3250 x3500) x3250 x3500)))
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_delete x1 x1002 x3000 x3250 x3500) (nd_C_delete x1 x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_delete x1 z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_delete x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_delete_dot_blackenRoot_dot_43 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_delete_dot_blackenRoot_dot_43 x1 x3250 x3500 = case x1 of
     C_Empty -> C_Empty
     (C_Tree x2 x3 x4 x5) -> C_Tree C_Black x3 x4 x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_delete_dot_blackenRoot_dot_43 x1002 x3250 x3500) (d_OP_delete_dot_blackenRoot_dot_43 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_delete_dot_blackenRoot_dot_43 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_delete_dot_blackenRoot_dot_43 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deleteTree :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_deleteTree x1 x2 x3 x4 x3250 x3500 = case x4 of
     C_Empty -> C_Empty
     (C_Tree x5 x6 x7 x8) -> d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteTree x1 x2 x3 x1002 x3250 x3500) (d_C_deleteTree x1 x2 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteTree x1 x2 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteTree x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_deleteTree :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_C_deleteTree x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     C_Empty -> C_Empty
     (C_Tree x5 x6 x7 x8) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_deleteTree x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_deleteTree x1 x2 x3 x1003 x3000 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_deleteTree x1 x2 x3 z x3000 x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_deleteTree x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_deleteTree_dot_addColor_dot_55 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_deleteTree_dot_addColor_dot_55 x1 x2 x3250 x3500 = case x1 of
     C_Red -> x2
     C_Black -> d_OP__case_165 x2 x3250 x3500
     (Choice_C_Color x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deleteTree_dot_addColor_dot_55 x1002 x2 x3250 x3500) (d_OP_deleteTree_dot_addColor_dot_55 x1003 x2 x3250 x3500)
     (Choices_C_Color x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deleteTree_dot_addColor_dot_55 z x2 x3250 x3500) x1002
     (Guard_C_Color x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deleteTree_dot_addColor_dot_55 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Color x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_deleteTree_dot_rightMost_dot_55 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_deleteTree_dot_rightMost_dot_55 x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_163 x5 x3 (Curry_Prelude.d_OP_eq_eq x5 C_Empty x3250 x3500) x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deleteTree_dot_rightMost_dot_55 x1002 x3250 x3500) (d_OP_deleteTree_dot_rightMost_dot_55 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deleteTree_dot_rightMost_dot_55 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deleteTree_dot_rightMost_dot_55 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_tree2list :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_tree2list x1 x3250 x3500 = case x1 of
     (C_RedBlackTree x2 x3 x4 x5) -> d_C_tree2listTree x5 x3250 x3500
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tree2list x1002 x3250 x3500) (d_C_tree2list x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tree2list z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tree2list x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_tree2list :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_tree2list x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RedBlackTree x2 x3 x4 x5) -> d_C_tree2listTree x5 x3250 x3500
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_tree2list x1002 x3000 x3250 x3500) (nd_C_tree2list x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_tree2list z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_tree2list x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_tree2listTree :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_tree2listTree x1 x3250 x3500 = d_OP_tree2listTree_dot_t2l_dot_76 x1 Curry_Prelude.OP_List x3250 x3500

d_OP_tree2listTree_dot_t2l_dot_76 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_tree2listTree_dot_t2l_dot_76 x1 x2 x3250 x3500 = case x1 of
     C_Empty -> x2
     (C_Tree x3 x4 x5 x6) -> d_OP_tree2listTree_dot_t2l_dot_76 x5 (Curry_Prelude.OP_Cons x4 (d_OP_tree2listTree_dot_t2l_dot_76 x6 x2 x3250 x3500)) x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tree2listTree_dot_t2l_dot_76 x1002 x2 x3250 x3500) (d_OP_tree2listTree_dot_t2l_dot_76 x1003 x2 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tree2listTree_dot_t2l_dot_76 z x2 x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tree2listTree_dot_t2l_dot_76 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_sort :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sort x1 x2 x3250 x3500 = d_C_tree2list (Curry_Prelude.d_C_foldr (acceptCs id d_C_update) (d_C_empty (acceptCs id d_OP_sort_dot___hash_lambda1) (acceptCs id Curry_Prelude.d_OP_eq_eq) x1 x3250 x3500) x2 x3250 x3500) x3250 x3500

nd_C_sort :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_sort x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (nd_C_tree2list (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_C_update)) (nd_C_empty (wrapDX (wrapDX id) (acceptCs id d_OP_sort_dot___hash_lambda1)) (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) x1 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_sort_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_sort_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.C_False

d_C_setInsertEquivalence :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_RedBlackTree t0 -> Cover -> ConstStore -> C_RedBlackTree t0
d_C_setInsertEquivalence x1 x2 x3250 x3500 = case x2 of
     (C_RedBlackTree x3 x4 x5 x6) -> C_RedBlackTree x1 x4 x5 x6
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setInsertEquivalence x1 x1002 x3250 x3500) (d_C_setInsertEquivalence x1 x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setInsertEquivalence x1 z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setInsertEquivalence x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_setInsertEquivalence :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> C_RedBlackTree t0
nd_C_setInsertEquivalence x1 x2 x3000 x3250 x3500 = case x2 of
     (HO_C_RedBlackTree x3 x4 x5 x6) -> HO_C_RedBlackTree x1 x4 x5 x6
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_setInsertEquivalence x1 x1002 x3000 x3250 x3500) (nd_C_setInsertEquivalence x1 x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_setInsertEquivalence x1 z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_setInsertEquivalence x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_rbt :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_rbt x1 x3250 x3500 = case x1 of
     (C_RedBlackTree x2 x3 x4 x5) -> x5
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_rbt x1002 x3250 x3500) (d_C_rbt x1003 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_rbt z x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_rbt x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_rbt :: Curry_Prelude.Curry t0 => C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_C_rbt x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RedBlackTree x2 x3 x4 x5) -> x5
     (Choice_C_RedBlackTree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_rbt x1002 x3000 x3250 x3500) (nd_C_rbt x1003 x3000 x3250 x3500)
     (Choices_C_RedBlackTree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_rbt z x3000 x3250 x3500) x1002
     (Guard_C_RedBlackTree x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_rbt x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RedBlackTree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isBlack :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isBlack x1 x3250 x3500 = case x1 of
     C_Empty -> Curry_Prelude.C_True
     (C_Tree x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x2 C_Black x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isBlack x1002 x3250 x3500) (d_C_isBlack x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isBlack z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isBlack x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isRed :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isRed x1 x3250 x3500 = case x1 of
     C_Empty -> Curry_Prelude.C_False
     (C_Tree x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x2 C_Red x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isRed x1002 x3250 x3500) (d_C_isRed x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isRed z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isRed x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isDoublyBlack :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isDoublyBlack x1 x3250 x3500 = case x1 of
     C_Empty -> Curry_Prelude.C_True
     (C_Tree x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x2 C_DoublyBlack x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isDoublyBlack x1002 x3250 x3500) (d_C_isDoublyBlack x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isDoublyBlack z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isDoublyBlack x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_element :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_C_element x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_element x1002 x3250 x3500) (d_C_element x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_element z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_element x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_left :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_left x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_left x1002 x3250 x3500) (d_C_left x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_left z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_left x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_right :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_right x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_right x1002 x3250 x3500) (d_C_right x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_right z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_right x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_singleBlack :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_singleBlack x1 x3250 x3500 = case x1 of
     C_Empty -> C_Empty
     (C_Tree x2 x3 x4 x5) -> d_OP__case_162 x5 x4 x3 x2 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_singleBlack x1002 x3250 x3500) (d_C_singleBlack x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_singleBlack z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_singleBlack x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_balanceL :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_balanceL x1 x3250 x3500 = let
     x2 = d_C_left x1 x3250 x3500
      in (d_OP__case_161 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isRed x2 x3250 x3500) (d_C_isRed (d_C_left x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_balanceL_dot___hash_selFP6_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP6_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_158 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP6_hash_z x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP6_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP6_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP6_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP7_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP7_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_156 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP7_hash_y x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP7_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP7_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP7_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP8_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP8_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_154 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP8_hash_x x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP8_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP8_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP8_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP9_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP9_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_152 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP9_hash_a x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP9_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP9_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP9_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP10_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP10_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_150 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP10_hash_b x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP10_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP10_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP10_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP11_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP11_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_148 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP11_hash_c x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP11_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP11_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP11_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP12_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP12_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_146 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP12_hash_d x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP12_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP12_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP12_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP14_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP14_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_144 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP14_hash_z x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP14_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP14_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP14_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP15_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP15_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_142 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP15_hash_x x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP15_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP15_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP15_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP16_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP16_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_140 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP16_hash_a x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP16_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP16_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP16_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP17_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceL_dot___hash_selFP17_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_138 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP17_hash_y x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP17_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP17_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP17_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP18_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP18_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_136 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP18_hash_b x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP18_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP18_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP18_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP19_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP19_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_134 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP19_hash_c x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP19_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP19_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP19_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceL_dot___hash_selFP20_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceL_dot___hash_selFP20_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_132 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceL_dot___hash_selFP20_hash_d x1002 x3250 x3500) (d_OP_balanceL_dot___hash_selFP20_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceL_dot___hash_selFP20_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceL_dot___hash_selFP20_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_balanceR :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_balanceR x1 x3250 x3500 = let
     x2 = d_C_right x1 x3250 x3500
      in (d_OP__case_130 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isRed x2 x3250 x3500) (d_C_isRed (d_C_right x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_balanceR_dot___hash_selFP22_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP22_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_127 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP22_hash_x x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP22_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP22_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP22_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP23_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP23_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_125 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP23_hash_a x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP23_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP23_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP23_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP24_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP24_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_123 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP24_hash_y x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP24_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP24_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP24_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP25_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP25_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_121 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP25_hash_b x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP25_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP25_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP25_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP26_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP26_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_119 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP26_hash_z x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP26_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP26_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP26_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP27_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP27_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_117 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP27_hash_c x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP27_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP27_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP27_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP28_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP28_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_115 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP28_hash_d x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP28_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP28_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP28_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP30_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP30_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_113 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP30_hash_x x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP30_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP30_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP30_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP31_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP31_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_111 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP31_hash_a x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP31_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP31_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP31_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP32_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP32_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_109 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP32_hash_z x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP32_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP32_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP32_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP33_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_balanceR_dot___hash_selFP33_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_107 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP33_hash_y x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP33_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP33_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP33_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP34_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP34_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_105 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP34_hash_b x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP34_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP34_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP34_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP35_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP35_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_103 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP35_hash_c x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP35_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP35_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP35_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_balanceR_dot___hash_selFP36_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_balanceR_dot___hash_selFP36_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_101 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_balanceR_dot___hash_selFP36_hash_d x1002 x3250 x3500) (d_OP_balanceR_dot___hash_selFP36_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_balanceR_dot___hash_selFP36_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_balanceR_dot___hash_selFP36_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_delBalanceL :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_delBalanceL x1 x3250 x3500 = d_OP__case_99 x1 (d_C_isDoublyBlack (d_C_left x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_reviseLeft :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_reviseLeft x1 x3250 x3500 = let
     x2 = d_C_right x1 x3250 x3500
     x3 = d_C_isBlack x2 x3250 x3500
      in (d_OP__case_98 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 C_Empty x3250 x3500) x3250 x3500)

d_OP_reviseLeft_dot___hash_selFP38_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseLeft_dot___hash_selFP38_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_92 x2 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP38_hash_col x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP38_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP38_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP38_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP39_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP39_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_90 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP39_hash_x x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP39_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP39_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP39_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP40_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP40_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_88 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP40_hash_a x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP40_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP40_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP40_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP41_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP41_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_86 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP41_hash_z x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP41_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP41_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP41_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP42_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP42_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_84 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP42_hash_y x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP42_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP42_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP42_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP43_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP43_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_82 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP43_hash_b x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP43_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP43_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP43_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP44_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP44_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_80 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP44_hash_c x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP44_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP44_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP44_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP45_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP45_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_78 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP45_hash_d x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP45_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP45_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP45_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP47_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseLeft_dot___hash_selFP47_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_76 x2 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP47_hash_col x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP47_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP47_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP47_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP48_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP48_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_74 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP48_hash_x x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP48_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP48_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP48_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP49_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP49_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_72 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP49_hash_a x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP49_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP49_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP49_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP50_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP50_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_70 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP50_hash_y x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP50_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP50_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP50_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP51_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP51_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_68 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP51_hash_b x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP51_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP51_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP51_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP52_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP52_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_66 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP52_hash_z x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP52_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP52_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP52_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP53_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP53_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_64 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP53_hash_c x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP53_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP53_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP53_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP54_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP54_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_62 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP54_hash_d x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP54_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP54_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP54_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP56_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseLeft_dot___hash_selFP56_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_60 x2 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP56_hash_col x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP56_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP56_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP56_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP57_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP57_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_59 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP57_hash_x x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP57_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP57_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP57_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP58_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP58_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_58 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP58_hash_a x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP58_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP58_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP58_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP59_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP59_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_57 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP59_hash_y x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP59_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP59_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP59_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP60_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP60_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_56 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP60_hash_b x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP60_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP60_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP60_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP61_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP61_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_55 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP61_hash_c x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP61_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP61_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP61_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP63_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP63_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_54 x3 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP63_hash_x x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP63_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP63_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP63_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP64_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP64_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_53 x4 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP64_hash_a x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP64_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP64_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP64_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP65_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseLeft_dot___hash_selFP65_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_52 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP65_hash_y x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP65_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP65_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP65_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP66_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP66_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_51 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP66_hash_b x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP66_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP66_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP66_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseLeft_dot___hash_selFP67_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseLeft_dot___hash_selFP67_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_50 x5 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseLeft_dot___hash_selFP67_hash_c x1002 x3250 x3500) (d_OP_reviseLeft_dot___hash_selFP67_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseLeft_dot___hash_selFP67_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseLeft_dot___hash_selFP67_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_delBalanceR :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_delBalanceR x1 x3250 x3500 = d_OP__case_49 x1 (d_C_isDoublyBlack (d_C_right x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_reviseRight :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_C_reviseRight x1 x3250 x3500 = let
     x2 = d_C_left x1 x3250 x3500
     x3 = d_C_isBlack x2 x3250 x3500
      in (d_OP__case_48 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 C_Empty x3250 x3500) x3250 x3500)

d_OP_reviseRight_dot___hash_selFP69_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseRight_dot___hash_selFP69_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_42 x2 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP69_hash_col x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP69_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP69_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP69_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP70_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP70_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_40 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP70_hash_x x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP70_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP70_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP70_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP71_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP71_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_38 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP71_hash_y x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP71_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP71_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP71_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP72_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP72_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_36 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP72_hash_z x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP72_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP72_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP72_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP73_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP73_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_34 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP73_hash_d x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP73_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP73_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP73_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP74_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP74_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_32 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP74_hash_c x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP74_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP74_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP74_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP75_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP75_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_30 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP75_hash_b x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP75_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP75_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP75_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP76_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP76_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_28 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP76_hash_a x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP76_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP76_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP76_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP78_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseRight_dot___hash_selFP78_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_26 x2 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP78_hash_col x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP78_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP78_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP78_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP79_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP79_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_24 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP79_hash_x x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP79_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP79_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP79_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP80_hash_z :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP80_hash_z x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_22 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP80_hash_z x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP80_hash_z x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP80_hash_z z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP80_hash_z x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP81_hash_d :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP81_hash_d x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_20 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP81_hash_d x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP81_hash_d x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP81_hash_d z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP81_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP82_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP82_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_18 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP82_hash_y x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP82_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP82_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP82_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP83_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP83_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_16 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP83_hash_c x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP83_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP83_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP83_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP84_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP84_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_14 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP84_hash_b x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP84_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP84_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP84_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP85_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP85_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_12 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP85_hash_a x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP85_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP85_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP85_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP87_hash_col :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP_reviseRight_dot___hash_selFP87_hash_col x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_10 x2 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP87_hash_col x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP87_hash_col x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP87_hash_col z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP87_hash_col x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP88_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP88_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_9 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP88_hash_x x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP88_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP88_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP88_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP89_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP89_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_8 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP89_hash_y x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP89_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP89_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP89_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP90_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP90_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_7 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP90_hash_c x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP90_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP90_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP90_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP91_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP91_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_6 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP91_hash_b x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP91_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP91_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP91_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP92_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP92_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_5 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP92_hash_a x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP92_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP92_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP92_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP94_hash_x :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP94_hash_x x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_4 x3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP94_hash_x x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP94_hash_x x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP94_hash_x z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP94_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP95_hash_y :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP_reviseRight_dot___hash_selFP95_hash_y x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_3 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP95_hash_y x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP95_hash_y x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP95_hash_y z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP95_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP96_hash_c :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP96_hash_c x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_2 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP96_hash_c x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP96_hash_c x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP96_hash_c z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP96_hash_c x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP97_hash_b :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP97_hash_b x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_1 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP97_hash_b x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP97_hash_b x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP97_hash_b z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP97_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reviseRight_dot___hash_selFP98_hash_a :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP_reviseRight_dot___hash_selFP98_hash_a x1 x3250 x3500 = case x1 of
     (C_Tree x2 x3 x4 x5) -> d_OP__case_0 x5 x4 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reviseRight_dot___hash_selFP98_hash_a x1002 x3250 x3500) (d_OP_reviseRight_dot___hash_selFP98_hash_a x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reviseRight_dot___hash_selFP98_hash_a z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reviseRight_dot___hash_selFP98_hash_a x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_0 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x1002 x3250 x3500) (d_OP__case_0 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_1 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_2 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_4 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x1002 x3250 x3500) (d_OP__case_4 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_5 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x1002 x3250 x3500) (d_OP__case_5 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_6 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_7 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_8 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1002 x3250 x3500) (d_OP__case_8 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_9 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x3 x1002 x3250 x3500) (d_OP__case_9 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_10 x2 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x1002 x3250 x3500) (d_OP__case_10 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_12 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_11 x5 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x5 x1002 x3250 x3500) (d_OP__case_12 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_11 x5 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x5 x1002 x3250 x3500) (d_OP__case_11 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_14 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_13 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1002 x3250 x3500) (d_OP__case_14 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_13 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1002 x3250 x3500) (d_OP__case_13 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_16 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_15 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1002 x3250 x3500) (d_OP__case_16 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_15 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1002 x3250 x3500) (d_OP__case_15 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_18 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_17 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1002 x3250 x3500) (d_OP__case_18 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_17 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1002 x3250 x3500) (d_OP__case_17 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_20 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_19 x8 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1002 x3250 x3500) (d_OP__case_20 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_19 x8 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x8 x1002 x3250 x3500) (d_OP__case_19 x8 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x8 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_22 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_21 x7 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1002 x3250 x3500) (d_OP__case_22 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_21 x7 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x7 x1002 x3250 x3500) (d_OP__case_21 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_24 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_23 x3 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x3 x1002 x3250 x3500) (d_OP__case_24 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_23 x3 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x3 x1002 x3250 x3500) (d_OP__case_23 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_26 x2 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_25 x2 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x2 x1002 x3250 x3500) (d_OP__case_26 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_25 x2 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x2 x1002 x3250 x3500) (d_OP__case_25 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_28 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_27 x5 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x5 x1002 x3250 x3500) (d_OP__case_28 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_27 x5 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x5 x1002 x3250 x3500) (d_OP__case_27 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_30 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_29 x9 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1002 x3250 x3500) (d_OP__case_30 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_29 x9 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x9 x1002 x3250 x3500) (d_OP__case_29 x9 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x9 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_32 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_31 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1002 x3250 x3500) (d_OP__case_32 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_31 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1002 x3250 x3500) (d_OP__case_31 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_34 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_33 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1002 x3250 x3500) (d_OP__case_34 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_33 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1002 x3250 x3500) (d_OP__case_33 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_36 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_35 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1002 x3250 x3500) (d_OP__case_36 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_35 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1002 x3250 x3500) (d_OP__case_35 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_38 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_37 x7 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1002 x3250 x3500) (d_OP__case_38 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_37 x7 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x7 x1002 x3250 x3500) (d_OP__case_37 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_40 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_39 x3 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x3 x1002 x3250 x3500) (d_OP__case_40 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_39 x3 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x3 x1002 x3250 x3500) (d_OP__case_39 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_42 x2 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_41 x2 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x2 x1002 x3250 x3500) (d_OP__case_42 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_41 x2 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x2 x1002 x3250 x3500) (d_OP__case_41 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_48 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP__case_47 x2 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand x3 (d_C_isRed (d_C_left x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_48 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_47 x2 x3 x1 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x4 = d_OP_reviseRight_dot___hash_selFP69_hash_col x1 x3250 x3500
          x5 = d_OP_reviseRight_dot___hash_selFP70_hash_x x1 x3250 x3500
          x6 = d_OP_reviseRight_dot___hash_selFP71_hash_y x1 x3250 x3500
          x7 = d_OP_reviseRight_dot___hash_selFP72_hash_z x1 x3250 x3500
          x8 = d_OP_reviseRight_dot___hash_selFP73_hash_d x1 x3250 x3500
          x9 = d_OP_reviseRight_dot___hash_selFP74_hash_c x1 x3250 x3500
          x10 = d_OP_reviseRight_dot___hash_selFP75_hash_b x1 x3250 x3500
          x11 = d_OP_reviseRight_dot___hash_selFP76_hash_a x1 x3250 x3500
           in (C_Tree x4 x6 (C_Tree C_Black x7 x8 x9) (C_Tree C_Black x5 x10 (d_C_singleBlack x11 x3250 x3500)))
     Curry_Prelude.C_False -> d_OP__case_46 x2 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand x3 (d_C_isRed (d_C_right x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_47 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_46 x2 x3 x1 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x12 = d_OP_reviseRight_dot___hash_selFP78_hash_col x1 x3250 x3500
          x13 = d_OP_reviseRight_dot___hash_selFP79_hash_x x1 x3250 x3500
          x14 = d_OP_reviseRight_dot___hash_selFP80_hash_z x1 x3250 x3500
          x15 = d_OP_reviseRight_dot___hash_selFP81_hash_d x1 x3250 x3500
          x16 = d_OP_reviseRight_dot___hash_selFP82_hash_y x1 x3250 x3500
          x17 = d_OP_reviseRight_dot___hash_selFP83_hash_c x1 x3250 x3500
          x18 = d_OP_reviseRight_dot___hash_selFP84_hash_b x1 x3250 x3500
          x19 = d_OP_reviseRight_dot___hash_selFP85_hash_a x1 x3250 x3500
           in (C_Tree x12 x16 (C_Tree C_Black x14 x15 x17) (C_Tree C_Black x13 x18 (d_C_singleBlack x19 x3250 x3500)))
     Curry_Prelude.C_False -> d_OP__case_45 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_46 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_45 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x20 = d_OP_reviseRight_dot___hash_selFP87_hash_col x1 x3250 x3500
          x21 = d_OP_reviseRight_dot___hash_selFP88_hash_x x1 x3250 x3500
          x22 = d_OP_reviseRight_dot___hash_selFP89_hash_y x1 x3250 x3500
          x23 = d_OP_reviseRight_dot___hash_selFP90_hash_c x1 x3250 x3500
          x24 = d_OP_reviseRight_dot___hash_selFP91_hash_b x1 x3250 x3500
          x25 = d_OP_reviseRight_dot___hash_selFP92_hash_a x1 x3250 x3500
           in (C_Tree (d_OP__case_44 x20 (Curry_Prelude.d_OP_eq_eq x20 C_Red x3250 x3500) x3250 x3500) x21 (C_Tree C_Red x22 x23 x24) (d_C_singleBlack x25 x3250 x3500))
     Curry_Prelude.C_False -> d_OP__case_43 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x1 x1002 x3250 x3500) (d_OP__case_45 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_43 x1 x31 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> let
          x26 = d_OP_reviseRight_dot___hash_selFP94_hash_x x1 x3250 x3500
          x27 = d_OP_reviseRight_dot___hash_selFP95_hash_y x1 x3250 x3500
          x28 = d_OP_reviseRight_dot___hash_selFP96_hash_c x1 x3250 x3500
          x29 = d_OP_reviseRight_dot___hash_selFP97_hash_b x1 x3250 x3500
          x30 = d_OP_reviseRight_dot___hash_selFP98_hash_a x1 x3250 x3500
           in (C_Tree C_Black x27 x28 (d_C_reviseRight (C_Tree C_Red x26 x29 x30) x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x1 x1002 x3250 x3500) (d_OP__case_43 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Color
d_OP__case_44 x20 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> C_Black
     Curry_Prelude.C_False -> C_DoublyBlack
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x20 x1002 x3250 x3500) (d_OP__case_44 x20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_49 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_reviseRight x1 x3250 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x1 x1002 x3250 x3500) (d_OP__case_49 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_50 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x1002 x3250 x3500) (d_OP__case_50 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_51 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x1002 x3250 x3500) (d_OP__case_51 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_52 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x1002 x3250 x3500) (d_OP__case_52 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_53 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x4 x1002 x3250 x3500) (d_OP__case_53 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_54 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x3 x1002 x3250 x3500) (d_OP__case_54 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_55 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x1002 x3250 x3500) (d_OP__case_55 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_56 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x1002 x3250 x3500) (d_OP__case_56 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_57 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x1002 x3250 x3500) (d_OP__case_57 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_58 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x4 x1002 x3250 x3500) (d_OP__case_58 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_59 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x3 x1002 x3250 x3500) (d_OP__case_59 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_60 x2 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x2 x1002 x3250 x3500) (d_OP__case_60 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_62 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_61 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x1002 x3250 x3500) (d_OP__case_62 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_61 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x1002 x3250 x3500) (d_OP__case_61 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_64 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_63 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x1002 x3250 x3500) (d_OP__case_64 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_63 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x1002 x3250 x3500) (d_OP__case_63 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_66 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_65 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x1002 x3250 x3500) (d_OP__case_66 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_65 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x1002 x3250 x3500) (d_OP__case_65 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_68 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_67 x8 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x1002 x3250 x3500) (d_OP__case_68 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_67 x8 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x8 x1002 x3250 x3500) (d_OP__case_67 x8 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x8 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_70 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_69 x7 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x1002 x3250 x3500) (d_OP__case_70 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_69 x7 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x7 x1002 x3250 x3500) (d_OP__case_69 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_72 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_72 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_71 x4 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x4 x1002 x3250 x3500) (d_OP__case_72 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_71 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_71 x4 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x4 x1002 x3250 x3500) (d_OP__case_71 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_74 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_74 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_73 x3 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x3 x1002 x3250 x3500) (d_OP__case_74 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_73 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_73 x3 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x3 x1002 x3250 x3500) (d_OP__case_73 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_76 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_76 x2 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_75 x2 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x2 x1002 x3250 x3500) (d_OP__case_76 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_75 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_75 x2 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x2 x1002 x3250 x3500) (d_OP__case_75 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_78 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_77 x9 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x1002 x3250 x3500) (d_OP__case_78 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_77 x9 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x9 x1002 x3250 x3500) (d_OP__case_77 x9 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x9 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_80 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_79 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x1002 x3250 x3500) (d_OP__case_80 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_79 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x1002 x3250 x3500) (d_OP__case_79 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_82 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_81 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x1002 x3250 x3500) (d_OP__case_82 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_81 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1002 x3250 x3500) (d_OP__case_81 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_84 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_84 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_83 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x1002 x3250 x3500) (d_OP__case_84 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_83 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x1002 x3250 x3500) (d_OP__case_83 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_86 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_86 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_85 x7 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x1002 x3250 x3500) (d_OP__case_86 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_85 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_85 x7 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x7 x1002 x3250 x3500) (d_OP__case_85 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_88 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_88 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_87 x4 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x4 x1002 x3250 x3500) (d_OP__case_88 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_87 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_87 x4 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x4 x1002 x3250 x3500) (d_OP__case_87 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_90 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_90 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_89 x3 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x3 x1002 x3250 x3500) (d_OP__case_90 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_89 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_89 x3 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x3 x1002 x3250 x3500) (d_OP__case_89 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_92 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_92 x2 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_91 x2 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x2 x1002 x3250 x3500) (d_OP__case_92 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_91 :: Curry_Prelude.Curry t0 => C_Color -> C_Tree t0 -> Cover -> ConstStore -> C_Color
d_OP__case_91 x2 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x2
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x2 x1002 x3250 x3500) (d_OP__case_91 x2 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x2 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_98 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_98 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP__case_97 x2 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand x3 (d_C_isRed (d_C_left x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_98 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_97 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_97 x2 x3 x1 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x4 = d_OP_reviseLeft_dot___hash_selFP38_hash_col x1 x3250 x3500
          x5 = d_OP_reviseLeft_dot___hash_selFP39_hash_x x1 x3250 x3500
          x6 = d_OP_reviseLeft_dot___hash_selFP40_hash_a x1 x3250 x3500
          x7 = d_OP_reviseLeft_dot___hash_selFP41_hash_z x1 x3250 x3500
          x8 = d_OP_reviseLeft_dot___hash_selFP42_hash_y x1 x3250 x3500
          x9 = d_OP_reviseLeft_dot___hash_selFP43_hash_b x1 x3250 x3500
          x10 = d_OP_reviseLeft_dot___hash_selFP44_hash_c x1 x3250 x3500
          x11 = d_OP_reviseLeft_dot___hash_selFP45_hash_d x1 x3250 x3500
           in (C_Tree x4 x8 (C_Tree C_Black x5 (d_C_singleBlack x6 x3250 x3500) x9) (C_Tree C_Black x7 x10 x11))
     Curry_Prelude.C_False -> d_OP__case_96 x2 x3 x1 (Curry_Prelude.d_OP_ampersand_ampersand x3 (d_C_isRed (d_C_right x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_97 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_96 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_96 x2 x3 x1 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x12 = d_OP_reviseLeft_dot___hash_selFP47_hash_col x1 x3250 x3500
          x13 = d_OP_reviseLeft_dot___hash_selFP48_hash_x x1 x3250 x3500
          x14 = d_OP_reviseLeft_dot___hash_selFP49_hash_a x1 x3250 x3500
          x15 = d_OP_reviseLeft_dot___hash_selFP50_hash_y x1 x3250 x3500
          x16 = d_OP_reviseLeft_dot___hash_selFP51_hash_b x1 x3250 x3500
          x17 = d_OP_reviseLeft_dot___hash_selFP52_hash_z x1 x3250 x3500
          x18 = d_OP_reviseLeft_dot___hash_selFP53_hash_c x1 x3250 x3500
          x19 = d_OP_reviseLeft_dot___hash_selFP54_hash_d x1 x3250 x3500
           in (C_Tree x12 x15 (C_Tree C_Black x13 (d_C_singleBlack x14 x3250 x3500) x16) (C_Tree C_Black x17 x18 x19))
     Curry_Prelude.C_False -> d_OP__case_95 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_96 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_95 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_95 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x20 = d_OP_reviseLeft_dot___hash_selFP56_hash_col x1 x3250 x3500
          x21 = d_OP_reviseLeft_dot___hash_selFP57_hash_x x1 x3250 x3500
          x22 = d_OP_reviseLeft_dot___hash_selFP58_hash_a x1 x3250 x3500
          x23 = d_OP_reviseLeft_dot___hash_selFP59_hash_y x1 x3250 x3500
          x24 = d_OP_reviseLeft_dot___hash_selFP60_hash_b x1 x3250 x3500
          x25 = d_OP_reviseLeft_dot___hash_selFP61_hash_c x1 x3250 x3500
           in (C_Tree (d_OP__case_94 x20 (Curry_Prelude.d_OP_eq_eq x20 C_Red x3250 x3500) x3250 x3500) x21 (d_C_singleBlack x22 x3250 x3500) (C_Tree C_Red x23 x24 x25))
     Curry_Prelude.C_False -> d_OP__case_93 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x1 x1002 x3250 x3500) (d_OP__case_95 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_93 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_93 x1 x31 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> let
          x26 = d_OP_reviseLeft_dot___hash_selFP63_hash_x x1 x3250 x3500
          x27 = d_OP_reviseLeft_dot___hash_selFP64_hash_a x1 x3250 x3500
          x28 = d_OP_reviseLeft_dot___hash_selFP65_hash_y x1 x3250 x3500
          x29 = d_OP_reviseLeft_dot___hash_selFP66_hash_b x1 x3250 x3500
          x30 = d_OP_reviseLeft_dot___hash_selFP67_hash_c x1 x3250 x3500
           in (C_Tree C_Black x28 (d_C_reviseLeft (C_Tree C_Red x26 x27 x29) x3250 x3500) x30)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x1 x1002 x3250 x3500) (d_OP__case_93 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_94 :: C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Color
d_OP__case_94 x20 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> C_Black
     Curry_Prelude.C_False -> C_DoublyBlack
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x20 x1002 x3250 x3500) (d_OP__case_94 x20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_99 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_99 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_reviseLeft x1 x3250 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x1 x1002 x3250 x3500) (d_OP__case_99 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_101 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_101 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_100 x9 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x1002 x3250 x3500) (d_OP__case_101 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_100 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_100 x9 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x9 x1002 x3250 x3500) (d_OP__case_100 x9 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x9 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_103 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_103 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_102 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x1002 x3250 x3500) (d_OP__case_103 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_102 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_102 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x1002 x3250 x3500) (d_OP__case_102 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_105 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_105 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_104 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x1002 x3250 x3500) (d_OP__case_105 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_104 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_104 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x1002 x3250 x3500) (d_OP__case_104 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_107 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_107 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_106 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x1002 x3250 x3500) (d_OP__case_107 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_106 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_106 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x1002 x3250 x3500) (d_OP__case_106 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_109 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_109 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_108 x7 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x1002 x3250 x3500) (d_OP__case_109 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_108 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_108 x7 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x7 x1002 x3250 x3500) (d_OP__case_108 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_111 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_111 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_110 x4 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x4 x1002 x3250 x3500) (d_OP__case_111 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_110 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_110 x4 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x4 x1002 x3250 x3500) (d_OP__case_110 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_113 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_113 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_112 x3 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x3 x1002 x3250 x3500) (d_OP__case_113 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_112 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_112 x3 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x3 x1002 x3250 x3500) (d_OP__case_112 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_115 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_115 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_114 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x1002 x3250 x3500) (d_OP__case_115 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_114 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_114 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x1002 x3250 x3500) (d_OP__case_114 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_117 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_117 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_116 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x1002 x3250 x3500) (d_OP__case_117 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_116 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_116 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x1002 x3250 x3500) (d_OP__case_116 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_119 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_119 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_118 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x1002 x3250 x3500) (d_OP__case_119 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_118 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_118 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x1002 x3250 x3500) (d_OP__case_118 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_121 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_121 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_120 x8 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x1002 x3250 x3500) (d_OP__case_121 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_120 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_120 x8 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x8 x1002 x3250 x3500) (d_OP__case_120 x8 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 x8 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_123 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_123 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_122 x7 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x1002 x3250 x3500) (d_OP__case_123 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_122 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_122 x7 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x7 x1002 x3250 x3500) (d_OP__case_122 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_125 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_125 x4 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_124 x4 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x4 x1002 x3250 x3500) (d_OP__case_125 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_124 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_124 x4 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x4
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x4 x1002 x3250 x3500) (d_OP__case_124 x4 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x4 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_127 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_127 x3 x5 x3250 x3500 = case x5 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_126 x3 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x3 x1002 x3250 x3500) (d_OP__case_127 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_126 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_126 x3 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x3 x1002 x3250 x3500) (d_OP__case_126 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_130 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_130 x2 x1 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x3 = d_OP_balanceR_dot___hash_selFP22_hash_x x1 x3250 x3500
          x4 = d_OP_balanceR_dot___hash_selFP23_hash_a x1 x3250 x3500
          x5 = d_OP_balanceR_dot___hash_selFP24_hash_y x1 x3250 x3500
          x6 = d_OP_balanceR_dot___hash_selFP25_hash_b x1 x3250 x3500
          x7 = d_OP_balanceR_dot___hash_selFP26_hash_z x1 x3250 x3500
          x8 = d_OP_balanceR_dot___hash_selFP27_hash_c x1 x3250 x3500
          x9 = d_OP_balanceR_dot___hash_selFP28_hash_d x1 x3250 x3500
           in (C_Tree C_Red x5 (C_Tree C_Black x3 x4 x6) (C_Tree C_Black x7 x8 x9))
     Curry_Prelude.C_False -> d_OP__case_129 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isRed x2 x3250 x3500) (d_C_isRed (d_C_left x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x2 x1 x1002 x3250 x3500) (d_OP__case_130 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_129 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_129 x2 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x10 = d_OP_balanceR_dot___hash_selFP30_hash_x x1 x3250 x3500
          x11 = d_OP_balanceR_dot___hash_selFP31_hash_a x1 x3250 x3500
          x12 = d_OP_balanceR_dot___hash_selFP32_hash_z x1 x3250 x3500
          x13 = d_OP_balanceR_dot___hash_selFP33_hash_y x1 x3250 x3500
          x14 = d_OP_balanceR_dot___hash_selFP34_hash_b x1 x3250 x3500
          x15 = d_OP_balanceR_dot___hash_selFP35_hash_c x1 x3250 x3500
          x16 = d_OP_balanceR_dot___hash_selFP36_hash_d x1 x3250 x3500
           in (C_Tree C_Red x13 (C_Tree C_Black x10 x11 x14) (C_Tree C_Black x12 x15 x16))
     Curry_Prelude.C_False -> d_OP__case_128 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x2 x1 x1002 x3250 x3500) (d_OP__case_129 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_128 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_128 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x1 x1002 x3250 x3500) (d_OP__case_128 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_132 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_132 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_131 x5 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_132 x5 x1002 x3250 x3500) (d_OP__case_132 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_132 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_132 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_131 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_131 x5 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_131 x5 x1002 x3250 x3500) (d_OP__case_131 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_131 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_131 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_134 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_134 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_133 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_134 x1002 x3250 x3500) (d_OP__case_134 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_134 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_134 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_133 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_133 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_133 x1002 x3250 x3500) (d_OP__case_133 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_133 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_133 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_136 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_136 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_135 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_136 x1002 x3250 x3500) (d_OP__case_136 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_136 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_136 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_135 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_135 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_135 x1002 x3250 x3500) (d_OP__case_135 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_135 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_135 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_138 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_138 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_137 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_138 x1002 x3250 x3500) (d_OP__case_138 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_138 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_138 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_137 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_137 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x1002 x3250 x3500) (d_OP__case_137 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_140 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_140 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_139 x8 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_140 x1002 x3250 x3500) (d_OP__case_140 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_140 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_140 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_139 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_139 x8 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x8
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x8 x1002 x3250 x3500) (d_OP__case_139 x8 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x8 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_142 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_142 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_141 x7 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_142 x1002 x3250 x3500) (d_OP__case_142 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_142 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_142 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_141 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_141 x7 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_141 x7 x1002 x3250 x3500) (d_OP__case_141 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_141 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_141 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_144 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_144 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_143 x3 x9 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_144 x3 x1002 x3250 x3500) (d_OP__case_144 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_144 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_144 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_143 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_143 x3 x9 x3250 x3500 = case x9 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_143 x3 x1002 x3250 x3500) (d_OP__case_143 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_143 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_143 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_146 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_146 x5 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_145 x5 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_146 x5 x1002 x3250 x3500) (d_OP__case_146 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_146 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_146 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_145 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_145 x5 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x5
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_145 x5 x1002 x3250 x3500) (d_OP__case_145 x5 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_145 x5 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_145 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_148 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_148 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_147 x9 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_148 x1002 x3250 x3500) (d_OP__case_148 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_148 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_148 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_147 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_147 x9 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x9
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_147 x9 x1002 x3250 x3500) (d_OP__case_147 x9 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_147 x9 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_147 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_150 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_150 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_149 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_150 x1002 x3250 x3500) (d_OP__case_150 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_150 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_150 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_149 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_149 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x13
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_149 x1002 x3250 x3500) (d_OP__case_149 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_149 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_149 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_152 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_152 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_151 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_152 x1002 x3250 x3500) (d_OP__case_152 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_152 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_152 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_151 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_151 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x12
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_151 x1002 x3250 x3500) (d_OP__case_151 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_151 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_151 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_154 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_154 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_153 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_154 x1002 x3250 x3500) (d_OP__case_154 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_154 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_154 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_153 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_153 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x11
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_153 x1002 x3250 x3500) (d_OP__case_153 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_153 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_153 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_156 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_156 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_155 x7 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_156 x1002 x3250 x3500) (d_OP__case_156 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_156 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_156 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_155 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_155 x7 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x7
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_155 x7 x1002 x3250 x3500) (d_OP__case_155 x7 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_155 x7 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_155 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_158 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_158 x3 x4 x3250 x3500 = case x4 of
     (C_Tree x6 x7 x8 x9) -> d_OP__case_157 x3 x8 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_158 x3 x1002 x3250 x3500) (d_OP__case_158 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_158 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_158 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_157 :: Curry_Prelude.Curry t0 => t0 -> C_Tree t0 -> Cover -> ConstStore -> t0
d_OP__case_157 x3 x8 x3250 x3500 = case x8 of
     (C_Tree x10 x11 x12 x13) -> x3
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_157 x3 x1002 x3250 x3500) (d_OP__case_157 x3 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_157 x3 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_157 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_161 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_161 x2 x1 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x3 = d_OP_balanceL_dot___hash_selFP6_hash_z x1 x3250 x3500
          x4 = d_OP_balanceL_dot___hash_selFP7_hash_y x1 x3250 x3500
          x5 = d_OP_balanceL_dot___hash_selFP8_hash_x x1 x3250 x3500
          x6 = d_OP_balanceL_dot___hash_selFP9_hash_a x1 x3250 x3500
          x7 = d_OP_balanceL_dot___hash_selFP10_hash_b x1 x3250 x3500
          x8 = d_OP_balanceL_dot___hash_selFP11_hash_c x1 x3250 x3500
          x9 = d_OP_balanceL_dot___hash_selFP12_hash_d x1 x3250 x3500
           in (C_Tree C_Red x4 (C_Tree C_Black x5 x6 x7) (C_Tree C_Black x3 x8 x9))
     Curry_Prelude.C_False -> d_OP__case_160 x2 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isRed x2 x3250 x3500) (d_C_isRed (d_C_right x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_161 x2 x1 x1002 x3250 x3500) (d_OP__case_161 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_161 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_161 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_160 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_160 x2 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x10 = d_OP_balanceL_dot___hash_selFP14_hash_z x1 x3250 x3500
          x11 = d_OP_balanceL_dot___hash_selFP15_hash_x x1 x3250 x3500
          x12 = d_OP_balanceL_dot___hash_selFP16_hash_a x1 x3250 x3500
          x13 = d_OP_balanceL_dot___hash_selFP17_hash_y x1 x3250 x3500
          x14 = d_OP_balanceL_dot___hash_selFP18_hash_b x1 x3250 x3500
          x15 = d_OP_balanceL_dot___hash_selFP19_hash_c x1 x3250 x3500
          x16 = d_OP_balanceL_dot___hash_selFP20_hash_d x1 x3250 x3500
           in (C_Tree C_Red x13 (C_Tree C_Black x11 x12 x14) (C_Tree C_Black x10 x15 x16))
     Curry_Prelude.C_False -> d_OP__case_159 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_160 x2 x1 x1002 x3250 x3500) (d_OP__case_160 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_160 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_160 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_159 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_159 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_159 x1 x1002 x3250 x3500) (d_OP__case_159 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_159 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_159 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_162 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> t0 -> C_Color -> Cover -> ConstStore -> C_Tree t0
d_OP__case_162 x5 x4 x3 x2 x3250 x3500 = case x2 of
     C_DoublyBlack -> C_Tree C_Black x3 x4 x5
     (Choice_C_Color x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_162 x5 x4 x3 x1002 x3250 x3500) (d_OP__case_162 x5 x4 x3 x1003 x3250 x3500)
     (Choices_C_Color x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_162 x5 x4 x3 z x3250 x3500) x1002
     (Guard_C_Color x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_162 x5 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Color x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_163 :: Curry_Prelude.Curry t0 => C_Tree t0 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> t0
d_OP__case_163 x5 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_OP_deleteTree_dot_rightMost_dot_55 x5 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_163 x5 x3 x1002 x3250 x3500) (d_OP__case_163 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_163 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_163 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_165 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> C_Tree t0
d_OP__case_165 x2 x3250 x3500 = case x2 of
     C_Empty -> C_Empty
     (C_Tree x3 x4 x5 x6) -> d_OP__case_164 x6 x5 x4 x3 x3250 x3500
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_165 x1002 x3250 x3500) (d_OP__case_165 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_165 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_165 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_164 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> t0 -> C_Color -> Cover -> ConstStore -> C_Tree t0
d_OP__case_164 x6 x5 x4 x3 x3250 x3500 = case x3 of
     C_Red -> C_Tree C_Black x4 x5 x6
     C_Black -> C_Tree C_DoublyBlack x4 x5 x6
     (Choice_C_Color x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_164 x6 x5 x4 x1002 x3250 x3500) (d_OP__case_164 x6 x5 x4 x1003 x3250 x3500)
     (Choices_C_Color x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_164 x6 x5 x4 z x3250 x3500) x1002
     (Guard_C_Color x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_164 x6 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Color x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_170 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_169 x7 x8 x2 x1 x5 (Curry_Prelude.d_OP_eq_eq x7 C_Empty x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x3 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1002 x3250 x3500) (d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_170 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_169 x7 x8 x2 x1 x5 (Curry_Prelude.d_OP_eq_eq x7 C_Empty x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x3 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_170 x6 x3 x1 x2 x8 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_167 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_delBalanceL (C_Tree x5 x6 (d_C_deleteTree x1 x2 x3 x7 x3250 x3500) x8) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1002 x3250 x3500) (d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_167 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_delBalanceL (C_Tree x5 x6 (nd_C_deleteTree x1 x2 x3 x7 x2000 x3250 x3500) x8) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_167 x6 x3 x2 x8 x1 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_166 :: Curry_Prelude.Curry t0 => C_Tree t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_delBalanceR (C_Tree x5 x6 x7 (d_C_deleteTree x1 x2 x3 x8 x3250 x3500)) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1002 x3250 x3500) (d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_166 :: Curry_Prelude.Curry t0 => C_Tree t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_delBalanceR (C_Tree x5 x6 x7 (nd_C_deleteTree x1 x2 x3 x8 x2000 x3250 x3500)) x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_166 x8 x3 x2 x1 x7 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_169 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_169 x7 x8 x2 x1 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP_deleteTree_dot_addColor_dot_55 x5 x8 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_168 x8 x7 x2 x1 x5 (Curry_Prelude.d_OP_eq_eq x8 C_Empty x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_169 x7 x8 x2 x1 x5 x1002 x3250 x3500) (d_OP__case_169 x7 x8 x2 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_169 x7 x8 x2 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_169 x7 x8 x2 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_169 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_169 x7 x8 x2 x1 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP_deleteTree_dot_addColor_dot_55 x5 x8 x3250 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_168 x8 x7 x2 x1 x5 (Curry_Prelude.d_OP_eq_eq x8 C_Empty x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_169 x7 x8 x2 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_169 x7 x8 x2 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_169 x7 x8 x2 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_169 x7 x8 x2 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_168 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_168 x8 x7 x2 x1 x5 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_deleteTree_dot_addColor_dot_55 x5 x7 x3250 x3500
     Curry_Prelude.C_False -> let
          x9 = d_OP_deleteTree_dot_rightMost_dot_55 x7 x3250 x3500
           in (d_C_delBalanceL (C_Tree x5 x9 (d_C_deleteTree x1 x2 x9 x7 x3250 x3500) x8) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_168 x8 x7 x2 x1 x5 x1002 x3250 x3500) (d_OP__case_168 x8 x7 x2 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_168 x8 x7 x2 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_168 x8 x7 x2 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_168 :: Curry_Prelude.Curry t0 => C_Tree t0 -> C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_168 x8 x7 x2 x1 x5 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP_deleteTree_dot_addColor_dot_55 x5 x7 x3250 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x9 = d_OP_deleteTree_dot_rightMost_dot_55 x7 x3250 x3500
                in (d_C_delBalanceL (C_Tree x5 x9 (nd_C_deleteTree x1 x2 x9 x7 x2000 x3250 x3500) x8) x3250 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_168 x8 x7 x2 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_168 x8 x7 x2 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_168 x8 x7 x2 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_168 x8 x7 x2 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_173 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> C_Tree x5 x1 x7 x8
     Curry_Prelude.C_False -> d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 x1 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1002 x3250 x3500) (d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_173 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> C_Tree x5 x1 x7 x8
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 x1 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_173 x6 x1 x2 x3 x8 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_172 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_balanceL (C_Tree x5 x6 (d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x7 x3250 x3500) x8) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1002 x3250 x3500) (d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_172 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_balanceL (C_Tree x5 x6 (nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x7 x2000 x3250 x3500) x8) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_172 x6 x1 x3 x8 x2 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_171 :: Curry_Prelude.Curry t0 => C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> t0 -> C_Color -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Tree t0
d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_balanceR (C_Tree x5 x6 x7 (d_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x8 x3250 x3500)) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1002 x3250 x3500) (d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_171 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> C_Tree t0 -> t0 -> C_Color -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_Tree t0
nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (d_C_balanceR (C_Tree x5 x6 x7 (nd_OP_updateTree_dot_upd_dot_34 x1 x2 x3 x8 x2000 x3250 x3500)) x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_171 x8 x3 x2 x1 x7 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_176 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_OP__case_176 x6 x3 x1 x2 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x6
     Curry_Prelude.C_False -> d_OP__case_175 x6 x3 x2 x8 x1 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x3 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_176 x6 x3 x1 x2 x8 x7 x1002 x3250 x3500) (d_OP__case_176 x6 x3 x1 x2 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_176 x6 x3 x1 x2 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_176 x6 x3 x1 x2 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_176 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> C_Tree t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
nd_OP__case_176 x6 x3 x1 x2 x8 x7 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x6
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_175 x6 x3 x2 x8 x1 x7 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x3 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_176 x6 x3 x1 x2 x8 x7 x1002 x3000 x3250 x3500) (nd_OP__case_176 x6 x3 x1 x2 x8 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_176 x6 x3 x1 x2 x8 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_176 x6 x3 x1 x2 x8 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_175 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_Tree t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_OP__case_175 x6 x3 x2 x8 x1 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_lookupTree x1 x2 x3 x7 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_174 x8 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_175 x6 x3 x2 x8 x1 x7 x1002 x3250 x3500) (d_OP__case_175 x6 x3 x2 x8 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_175 x6 x3 x2 x8 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_175 x6 x3 x2 x8 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_175 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> C_Tree t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
nd_OP__case_175 x6 x3 x2 x8 x1 x7 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_lookupTree x1 x2 x3 x7 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_174 x8 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_175 x6 x3 x2 x8 x1 x7 x1002 x3000 x3250 x3500) (nd_OP__case_175 x6 x3 x2 x8 x1 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_175 x6 x3 x2 x8 x1 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_175 x6 x3 x2 x8 x1 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_174 :: Curry_Prelude.Curry t0 => C_Tree t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
d_OP__case_174 x8 x3 x2 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_lookupTree x1 x2 x3 x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_174 x8 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_174 x8 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_174 x8 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_174 x8 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_174 :: Curry_Prelude.Curry t0 => C_Tree t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0
nd_OP__case_174 x8 x3 x2 x1 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_lookupTree x1 x2 x3 x8 x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_174 x8 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_174 x8 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_174 x8 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_174 x8 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_177 :: Curry_Prelude.Curry t0 => C_Tree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_177 x5 x3250 x3500 = case x5 of
     C_Empty -> Curry_Prelude.C_True
     (C_Tree x6 x7 x8 x9) -> Curry_Prelude.C_False
     (Choice_C_Tree x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_177 x1002 x3250 x3500) (d_OP__case_177 x1003 x3250 x3500)
     (Choices_C_Tree x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_177 z x3250 x3500) x1002
     (Guard_C_Tree x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_177 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Tree x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
