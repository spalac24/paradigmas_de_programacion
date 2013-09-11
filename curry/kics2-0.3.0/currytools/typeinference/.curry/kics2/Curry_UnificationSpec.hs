{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_UnificationSpec (C_Term (..), C_UnificationError (..), C_VarIdx, C_TermEq, C_TermEqs, C_Subst, d_C_showSubst, nd_C_showSubst, d_C_emptySubst, nd_C_emptySubst, d_C_extendSubst, nd_C_extendSubst, d_C_lookupSubst, nd_C_lookupSubst, d_C_applySubst, nd_C_applySubst, d_C_unify, nd_C_unify) where

import Basics
import qualified Curry_FiniteMap
import qualified Curry_Prelude
type C_VarIdx = Curry_Prelude.C_Int

type C_TermEq = Curry_Prelude.OP_Tuple2 C_Term C_Term

type C_TermEqs = Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term)

type C_Subst = Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term

data C_Term
     = C_TermVar Curry_Prelude.C_Int
     | C_TermCons (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_Term)
     | Choice_C_Term Cover ID C_Term C_Term
     | Choices_C_Term Cover ID ([C_Term])
     | Fail_C_Term Cover FailInfo
     | Guard_C_Term Cover Constraints C_Term

instance Show C_Term where
  showsPrec d (Choice_C_Term cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Term cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Term cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Term cd info) = showChar '!'
  showsPrec _ (C_TermVar x1) = (showString "(TermVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_TermCons x1 x2) = (showString "(TermCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Term where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_TermVar x1,r1) | (_,r0) <- readQualified "UnificationSpec" "TermVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_TermCons x1 x2,r2) | (_,r0) <- readQualified "UnificationSpec" "TermCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_Term where
  choiceCons = Choice_C_Term
  choicesCons = Choices_C_Term
  failCons = Fail_C_Term
  guardCons = Guard_C_Term
  try (Choice_C_Term cd i x y) = tryChoice cd i x y
  try (Choices_C_Term cd i xs) = tryChoices cd i xs
  try (Fail_C_Term cd info) = Fail cd info
  try (Guard_C_Term cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Term cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Term cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Term cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Term cd i _) = error ("UnificationSpec.Term.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Term cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Term cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Term where
  generate s c = Choices_C_Term c (freeID [1,2] s) [(C_TermVar (generate (leftSupply s) c)),(C_TermCons (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Term where
  ($!!) cont (C_TermVar x1) d cs = (((\y1 d cs -> cont (C_TermVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_TermCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TermCons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Term cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Term cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Term cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Term cd info) _ _ = failCons cd info
  ($##) cont (C_TermVar x1) d cs = (((\y1 d cs -> cont (C_TermVar y1) d cs) $## x1) d) cs
  ($##) cont (C_TermCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TermCons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Term cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Term cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Term cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Term cd info) _ _ = failCons cd info
  searchNF search cont (C_TermVar x1) = search (\y1 -> cont (C_TermVar y1)) x1
  searchNF search cont (C_TermCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_TermCons y1 y2)) x2) x1
  searchNF _ _ x = error ("UnificationSpec.Term.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Term where
  (=.=) (C_TermVar x1) (C_TermVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_TermCons x1 x2) (C_TermCons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_TermVar x1) (C_TermVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_TermCons x1 x2) (C_TermCons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_TermVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_TermCons x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Term cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Term cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Term cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Term cd i _) = error ("UnificationSpec.Term.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Term cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Term cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TermVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_TermCons x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Term cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Term cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Term cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Term cd i _) = error ("UnificationSpec.Term.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Term cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Term cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Term where
  (=?=) (Choice_C_Term cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Term cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Term cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Term cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Term cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Term cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Term cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Term cd info) _ _ = failCons cd info
  (=?=) (C_TermVar x1) (C_TermVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_TermCons x1 x2) (C_TermCons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Term cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Term cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Term cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Term cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Term cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Term cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Term cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Term cd info) _ _ = failCons cd info
  (<?=) (C_TermVar x1) (C_TermVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_TermVar _) (C_TermCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TermCons x1 x2) (C_TermCons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_UnificationError
     = C_Clash C_Term C_Term
     | C_OccurCheck Curry_Prelude.C_Int C_Term
     | Choice_C_UnificationError Cover ID C_UnificationError C_UnificationError
     | Choices_C_UnificationError Cover ID ([C_UnificationError])
     | Fail_C_UnificationError Cover FailInfo
     | Guard_C_UnificationError Cover Constraints C_UnificationError

instance Show C_UnificationError where
  showsPrec d (Choice_C_UnificationError cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_UnificationError cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_UnificationError cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_UnificationError cd info) = showChar '!'
  showsPrec _ (C_Clash x1 x2) = (showString "(Clash") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_OccurCheck x1 x2) = (showString "(OccurCheck") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_UnificationError where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Clash x1 x2,r2) | (_,r0) <- readQualified "UnificationSpec" "Clash" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_OccurCheck x1 x2,r2) | (_,r0) <- readQualified "UnificationSpec" "OccurCheck" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_UnificationError where
  choiceCons = Choice_C_UnificationError
  choicesCons = Choices_C_UnificationError
  failCons = Fail_C_UnificationError
  guardCons = Guard_C_UnificationError
  try (Choice_C_UnificationError cd i x y) = tryChoice cd i x y
  try (Choices_C_UnificationError cd i xs) = tryChoices cd i xs
  try (Fail_C_UnificationError cd info) = Fail cd info
  try (Guard_C_UnificationError cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_UnificationError cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_UnificationError cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_UnificationError cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_UnificationError cd i _) = error ("UnificationSpec.UnificationError.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_UnificationError cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_UnificationError cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_UnificationError where
  generate s c = Choices_C_UnificationError c (freeID [2,2] s) [(C_Clash (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_OccurCheck (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_UnificationError where
  ($!!) cont (C_Clash x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Clash y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_OccurCheck x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OccurCheck y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_UnificationError cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_UnificationError cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_UnificationError cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_UnificationError cd info) _ _ = failCons cd info
  ($##) cont (C_Clash x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Clash y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_OccurCheck x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OccurCheck y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_UnificationError cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_UnificationError cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_UnificationError cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_UnificationError cd info) _ _ = failCons cd info
  searchNF search cont (C_Clash x1 x2) = search (\y1 -> search (\y2 -> cont (C_Clash y1 y2)) x2) x1
  searchNF search cont (C_OccurCheck x1 x2) = search (\y1 -> search (\y2 -> cont (C_OccurCheck y1 y2)) x2) x1
  searchNF _ _ x = error ("UnificationSpec.UnificationError.searchNF: no constructor: " ++ (show x))


instance Unifiable C_UnificationError where
  (=.=) (C_Clash x1 x2) (C_Clash y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_OccurCheck x1 x2) (C_OccurCheck y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Clash x1 x2) (C_Clash y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_OccurCheck x1 x2) (C_OccurCheck y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Clash x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_OccurCheck x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_UnificationError cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_UnificationError cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_UnificationError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_UnificationError cd i _) = error ("UnificationSpec.UnificationError.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_UnificationError cd info) = [(Unsolvable info)]
  bind d i (Guard_C_UnificationError cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Clash x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_OccurCheck x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_UnificationError cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_UnificationError cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_UnificationError cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_UnificationError cd i _) = error ("UnificationSpec.UnificationError.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_UnificationError cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_UnificationError cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_UnificationError where
  (=?=) (Choice_C_UnificationError cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_UnificationError cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_UnificationError cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_UnificationError cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_UnificationError cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_UnificationError cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_UnificationError cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_UnificationError cd info) _ _ = failCons cd info
  (=?=) (C_Clash x1 x2) (C_Clash y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_OccurCheck x1 x2) (C_OccurCheck y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_UnificationError cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_UnificationError cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_UnificationError cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_UnificationError cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_UnificationError cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_UnificationError cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_UnificationError cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_UnificationError cd info) _ _ = failCons cd info
  (<?=) (C_Clash x1 x2) (C_Clash y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Clash _ _) (C_OccurCheck _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OccurCheck x1 x2) (C_OccurCheck y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_showSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSubst x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map d_OP_showSubst_dot_showOne_dot_2) Curry_FiniteMap.d_C_fmToList x3250 x3500) x3250 x3500

nd_C_showSubst :: IDSupply -> Cover -> ConstStore -> Func (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showSubst x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unlines) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_OP_showSubst_dot_showOne_dot_2))) (wrapNX id Curry_FiniteMap.nd_C_fmToList) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_OP_showSubst_dot_showOne_dot_2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showSubst_dot_showOne_dot_2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showSubst_dot_showOne_dot_2 x1002 x3250 x3500) (d_OP_showSubst_dot_showOne_dot_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showSubst_dot_showOne_dot_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showSubst_dot_showOne_dot_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_emptySubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_C_emptySubst x3250 x3500 = Curry_FiniteMap.d_C_emptyFM (acceptCs id Curry_Prelude.d_OP_lt) x3250 x3500

nd_C_emptySubst :: IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
nd_C_emptySubst x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3250 x3500))

d_C_extendSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> C_Term -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_C_extendSubst x3250 x3500 = acceptCs (acceptCs id) Curry_FiniteMap.d_C_addToFM

nd_C_extendSubst :: IDSupply -> Cover -> ConstStore -> Func (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term) (Func Curry_Prelude.C_Int (Func C_Term (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term)))
nd_C_extendSubst x3000 x3250 x3500 = wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) Curry_FiniteMap.nd_C_addToFM)

d_C_lookupSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_Term
d_C_lookupSubst x3250 x3500 = acceptCs id Curry_FiniteMap.d_C_lookupFM

nd_C_lookupSubst :: IDSupply -> Cover -> ConstStore -> Func (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term) (Func Curry_Prelude.C_Int (Curry_Prelude.C_Maybe C_Term))
nd_C_lookupSubst x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id Curry_FiniteMap.nd_C_lookupFM)

d_C_applySubst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> C_Term -> Cover -> ConstStore -> C_Term
d_C_applySubst x1 x2 x3250 x3500 = case x2 of
     (C_TermVar x3) -> Curry_Prelude.d_C_maybe x2 Curry_Prelude.d_C_id (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupSubst x3250 x3500) x1 x3250 x3500) x3 x3250 x3500) x3250 x3500
     (C_TermCons x4 x5) -> C_TermCons x4 (Curry_Prelude.d_C_map (d_C_applySubst x1) x5 x3250 x3500)
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_applySubst x1 x1002 x3250 x3500) (d_C_applySubst x1 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_applySubst x1 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_applySubst x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_applySubst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> C_Term -> IDSupply -> Cover -> ConstStore -> C_Term
nd_C_applySubst x1 x2 x3000 x3250 x3500 = case x2 of
     (C_TermVar x3) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_maybe x2 (wrapDX id Curry_Prelude.d_C_id) (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_lookupSubst x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3 x2003 x3250 x3500)))) x2005 x3250 x3500)))))
     (C_TermCons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (C_TermCons x4 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_applySubst x1)) x5 x2000 x3250 x3500)))
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_applySubst x1 x1002 x3000 x3250 x3500) (nd_C_applySubst x1 x1003 x3000 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_applySubst x1 z x3000 x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_applySubst x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substituteSingle :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 C_Term C_Term
d_C_substituteSingle x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (d_C_applySubst x1 x3 x3250 x3500) (d_C_applySubst x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substituteSingle x1 x1002 x3250 x3500) (d_C_substituteSingle x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substituteSingle x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substituteSingle x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substituteSingle :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 C_Term C_Term
nd_C_substituteSingle x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (nd_C_applySubst x1 x3 x2000 x3250 x3500) (nd_C_applySubst x1 x4 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substituteSingle x1 x1002 x3000 x3250 x3500) (nd_C_substituteSingle x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substituteSingle x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substituteSingle x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substitute :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term)
d_C_substitute x1 x2 x3250 x3500 = Curry_Prelude.d_C_map (d_C_substituteSingle x1) x2 x3250 x3500

nd_C_substitute :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term)
nd_C_substitute x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substituteSingle x1)) x2 x2000 x3250 x3500))

d_C_unify :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term)
d_C_unify x1 x3250 x3500 = Curry_Prelude.d_C_either (acceptCs id Curry_Prelude.C_Left) (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Right) d_C_eqsToSubst x3250 x3500) (d_C_unify' x1 Curry_Prelude.OP_List x3250 x3500) x3250 x3500

nd_C_unify :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term)
nd_C_unify x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_either (wrapDX id (acceptCs id Curry_Prelude.C_Left)) (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id Curry_Prelude.C_Right)) (wrapNX id nd_C_eqsToSubst) x2000 x3250 x3500) (d_C_unify' x1 Curry_Prelude.OP_List x3250 x3500) x2001 x3250 x3500)))))

d_C_eqsToSubst :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_C_eqsToSubst x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_C_emptySubst x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_14 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_eqsToSubst x1002 x3250 x3500) (d_C_eqsToSubst x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_eqsToSubst z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_eqsToSubst x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_eqsToSubst :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
nd_C_eqsToSubst x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_emptySubst x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x3 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_eqsToSubst x1002 x3000 x3250 x3500) (nd_C_eqsToSubst x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_eqsToSubst z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_eqsToSubst x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unify' :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_C_unify' x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Right x1
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_11 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unify' x1 x1002 x3250 x3500) (d_C_unify' x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unify' x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unify' x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_elim :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Int -> C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_C_elim x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_TermVar x2) x3) (Curry_Prelude.d_C_map (d_OP_elim_dot___hash_lambda3 x2 x3) x1 x3250 x3500)
      in (d_OP__case_3 x3 x2 x4 x5 (d_C_dependsOn (C_TermVar x2) x3 x3250 x3500) x3250 x3500)

d_OP_elim_dot___hash_lambda3 :: Curry_Prelude.C_Int -> C_Term -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 C_Term C_Term
d_OP_elim_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Tuple2 x4 (d_C_termSubstitute' x1 x2 x5 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_elim_dot___hash_lambda3 x1 x2 x1002 x3250 x3500) (d_OP_elim_dot___hash_lambda3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_elim_dot___hash_lambda3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_elim_dot___hash_lambda3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_termSubstitute' :: Curry_Prelude.C_Int -> C_Term -> C_Term -> Cover -> ConstStore -> C_Term
d_C_termSubstitute' x1 x2 x3 x3250 x3500 = case x3 of
     (C_TermVar x4) -> d_OP__case_1 x1 x4 x3 x2 (Curry_Prelude.d_OP_eq_eq x4 x1 x3250 x3500) x3250 x3500
     (C_TermCons x5 x6) -> C_TermCons x5 (d_C_termsSubstitute' x1 x2 x6 x3250 x3500)
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_termSubstitute' x1 x2 x1002 x3250 x3500) (d_C_termSubstitute' x1 x2 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_termSubstitute' x1 x2 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_termSubstitute' x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_termsSubstitute' :: Curry_Prelude.C_Int -> C_Term -> Curry_Prelude.OP_List C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_List C_Term
d_C_termsSubstitute' x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_map (d_C_termSubstitute' x1 x2) x3 x3250 x3500

d_C_substitute' :: Curry_Prelude.C_Int -> C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term)
d_C_substitute' x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_map (d_C_substituteSingle' x1 x2) x3 x3250 x3500

d_C_substituteSingle' :: Curry_Prelude.C_Int -> C_Term -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 C_Term C_Term
d_C_substituteSingle' x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Tuple2 (d_C_termSubstitute' x1 x2 x4 x3250 x3500) (d_C_termSubstitute' x1 x2 x5 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substituteSingle' x1 x2 x1002 x3250 x3500) (d_C_substituteSingle' x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substituteSingle' x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substituteSingle' x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dependsOn :: C_Term -> C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_dependsOn x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_and x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_not (Curry_Prelude.d_OP_eq_eq x1 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 x2 x3250 x3500) Curry_Prelude.OP_List)) x3250 x3500

d_OP_dependsOn_dot_dependsOnRecurse_dot_65 :: C_Term -> C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 x2 x3250 x3500 = case x2 of
     (C_TermVar x3) -> Curry_Prelude.d_OP_eq_eq x1 x2 x3250 x3500
     (C_TermCons x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any Curry_Prelude.d_C_id x3250 x3500) (Curry_Prelude.d_C_map (d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1) x5 x3250 x3500) x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 x1002 x3250 x3500) (d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dependsOn_dot_dependsOnRecurse_dot_65 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> C_Term -> C_Term -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Term
d_OP__case_1 x1 x4 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> d_OP__case_0 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x4 x3 x2 x1002 x3250 x3500) (d_OP__case_1 x1 x4 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x4 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x4 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: C_Term -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Term
d_OP__case_0 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x1002 x3250 x3500) (d_OP__case_0 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: C_Term -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_3 x3 x2 x4 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Left (C_OccurCheck x2 x3)
     Curry_Prelude.C_False -> d_OP__case_2 x4 x3 x2 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x2 x4 x5 x1002 x3250 x3500) (d_OP__case_3 x3 x2 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x2 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x2 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_2 x4 x3 x2 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_unify' x5 (d_C_substitute' x2 x3 x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x3 x2 x5 x1002 x3250 x3500) (d_OP__case_2 x4 x3 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x3 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x3 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_11 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_10 x6 x4 x1 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x1 x1002 x3250 x3500) (d_OP__case_11 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_10 x6 x4 x1 x5 x3250 x3500 = case x5 of
     (C_TermVar x7) -> d_OP__case_9 x7 x4 x1 x6 x3250 x3500
     (C_TermCons x11 x12) -> d_OP__case_6 x11 x5 x4 x12 x1 x6 x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x6 x4 x1 x1002 x3250 x3500) (d_OP__case_10 x6 x4 x1 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x6 x4 x1 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x6 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_6 x11 x5 x4 x12 x1 x6 x3250 x3500 = case x6 of
     (C_TermVar x13) -> d_C_elim x1 x13 x5 x4 x3250 x3500
     (C_TermCons x14 x15) -> d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 (Curry_Prelude.d_OP_eq_eq x11 x14 x3250 x3500) x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x11 x5 x4 x12 x1 x1002 x3250 x3500) (d_OP__case_6 x11 x5 x4 x12 x1 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x11 x5 x4 x12 x1 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x11 x5 x4 x12 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Term -> C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List C_Term -> Curry_Prelude.OP_List C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> d_C_unify' x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_zip x12 x15 x3250 x3500) x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 x1002 x3250 x3500) (d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x14 x11 x6 x5 x4 x15 x12 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: C_Term -> C_Term -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_4 x6 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Left (C_Clash x5 x6)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x6 x5 x1002 x3250 x3500) (d_OP__case_4 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_9 x7 x4 x1 x6 x3250 x3500 = case x6 of
     (C_TermCons x8 x9) -> d_C_elim x1 x7 x6 x4 x3250 x3500
     (C_TermVar x10) -> d_OP__case_8 x10 x7 x4 x6 x1 (Curry_Prelude.d_OP_eq_eq x7 x10 x3250 x3500) x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x7 x4 x1 x1002 x3250 x3500) (d_OP__case_9 x7 x4 x1 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x7 x4 x1 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x7 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_8 x10 x7 x4 x6 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_C_unify' x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_7 x4 x6 x7 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x10 x7 x4 x6 x1 x1002 x3250 x3500) (d_OP__case_8 x10 x7 x4 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x10 x7 x4 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x10 x7 x4 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Either C_UnificationError (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term))
d_OP__case_7 x4 x6 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_elim x1 x7 x6 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x6 x7 x1 x1002 x3250 x3500) (d_OP__case_7 x4 x6 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x6 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x6 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_OP__case_14 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_13 x5 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x3 x1002 x3250 x3500) (d_OP__case_14 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> Curry_Prelude.OP_Tuple2 C_Term C_Term -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
nd_OP__case_14 x3 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x5 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x3 x1002 x3000 x3250 x3500) (nd_OP__case_14 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_OP__case_13 x5 x3 x4 x3250 x3500 = case x4 of
     (C_TermVar x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_extendSubst x3250 x3500) (d_C_eqsToSubst x3 x3250 x3500) x3250 x3500) x6 x3250 x3500) x5 x3250 x3500
     (C_TermCons x7 x8) -> d_OP__case_12 x4 x3 x5 x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x5 x3 x1002 x3250 x3500) (d_OP__case_13 x5 x3 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x5 x3 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
nd_OP__case_13 x5 x3 x4 x3000 x3250 x3500 = case x4 of
     (C_TermVar x6) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_extendSubst x2000 x3250 x3500) (nd_C_eqsToSubst x3 x2001 x3250 x3500) x2002 x3250 x3500))))))) x6 x2005 x3250 x3500)))) x5 x2007 x3250 x3500)))))
     (C_TermCons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x4 x3 x5 x2000 x3250 x3500))
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x5 x3 x1002 x3000 x3250 x3500) (nd_OP__case_13 x5 x3 x1003 x3000 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x5 x3 z x3000 x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x5 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
d_OP__case_12 x4 x3 x5 x3250 x3500 = case x5 of
     (C_TermVar x9) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_extendSubst x3250 x3500) (d_C_eqsToSubst x3 x3250 x3500) x3250 x3500) x9 x3250 x3500) x4 x3250 x3500
     (C_TermCons x10 x11) -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.OP_Tuple2 x4 x5) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x4 x3 x1002 x3250 x3500) (d_OP__case_12 x4 x3 x1003 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x4 x3 z x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: C_Term -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Term C_Term) -> C_Term -> IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int C_Term
nd_OP__case_12 x4 x3 x5 x3000 x3250 x3500 = case x5 of
     (C_TermVar x9) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_extendSubst x2000 x3250 x3500) (nd_C_eqsToSubst x3 x2001 x3250 x3500) x2002 x3250 x3500))))))) x9 x2005 x3250 x3500)))) x4 x2007 x3250 x3500)))))
     (C_TermCons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.OP_Tuple2 x4 x5) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Choice_C_Term x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x4 x3 x1002 x3000 x3250 x3500) (nd_OP__case_12 x4 x3 x1003 x3000 x3250 x3500)
     (Choices_C_Term x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x4 x3 z x3000 x3250 x3500) x1002
     (Guard_C_Term x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x4 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Term x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
