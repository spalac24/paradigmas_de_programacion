{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_leq (C_Nat (..), d_C_add, d_C_leq) where

import Basics
import qualified Curry_Prelude
data C_Nat
     = C_Z
     | C_S C_Nat
     | Choice_C_Nat Cover ID C_Nat C_Nat
     | Choices_C_Nat Cover ID ([C_Nat])
     | Fail_C_Nat Cover FailInfo
     | Guard_C_Nat Cover Constraints C_Nat

instance Show C_Nat where
  showsPrec d (Choice_C_Nat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Nat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Nat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Nat cd info) = showChar '!'
  showsPrec _ C_Z = showString "Z"
  showsPrec _ (C_S x1) = (showString "(S") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Nat where
  readsPrec d s = (readParen False (\r -> [ (C_Z,r0) | (_,r0) <- readQualified "leq" "Z" r]) s) ++ (readParen (d > 10) (\r -> [ (C_S x1,r1) | (_,r0) <- readQualified "leq" "S" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Nat where
  choiceCons = Choice_C_Nat
  choicesCons = Choices_C_Nat
  failCons = Fail_C_Nat
  guardCons = Guard_C_Nat
  try (Choice_C_Nat cd i x y) = tryChoice cd i x y
  try (Choices_C_Nat cd i xs) = tryChoices cd i xs
  try (Fail_C_Nat cd info) = Fail cd info
  try (Guard_C_Nat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Nat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Nat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Nat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Nat cd i _) = error ("leq.Nat.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Nat cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Nat cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Nat where
  generate s c = Choices_C_Nat c (freeID [0,1] s) [C_Z,(C_S (generate (leftSupply s) c))]


instance NormalForm C_Nat where
  ($!!) cont C_Z d cs = cont C_Z d cs
  ($!!) cont (C_S x1) d cs = (((\y1 d cs -> cont (C_S y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Nat cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Nat cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Nat cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Nat cd info) _ _ = failCons cd info
  ($##) cont C_Z d cs = cont C_Z d cs
  ($##) cont (C_S x1) d cs = (((\y1 d cs -> cont (C_S y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Nat cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Nat cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Nat cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Nat cd info) _ _ = failCons cd info
  searchNF _ cont C_Z = cont C_Z
  searchNF search cont (C_S x1) = search (\y1 -> cont (C_S y1)) x1
  searchNF _ _ x = error ("leq.Nat.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Nat where
  (=.=) C_Z C_Z d cs = C_Success
  (=.=) (C_S x1) (C_S y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Z C_Z d cs = C_Success
  (=.<=) (C_S x1) (C_S y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Z = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_S x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Nat cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Nat cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Nat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Nat cd i _) = error ("leq.Nat.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Nat cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Nat cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Z = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_S x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Nat cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Nat cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Nat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Nat cd i _) = error ("leq.Nat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Nat cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Nat cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Nat where
  (=?=) (Choice_C_Nat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Nat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Nat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Nat cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Nat cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Nat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Nat cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Nat cd info) _ _ = failCons cd info
  (=?=) C_Z C_Z d cs = Curry_Prelude.C_True
  (=?=) (C_S x1) (C_S y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Nat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Nat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Nat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Nat cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Nat cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Nat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Nat cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Nat cd info) _ _ = failCons cd info
  (<?=) C_Z C_Z d cs = Curry_Prelude.C_True
  (<?=) C_Z (C_S _) _ _ = Curry_Prelude.C_True
  (<?=) (C_S x1) (C_S y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_add :: C_Nat -> C_Nat -> Cover -> ConstStore -> C_Nat
d_C_add x1 x2 x3250 x3500 = case x1 of
     C_Z -> x2
     (C_S x3) -> C_S (d_C_add x3 x2 x3250 x3500)
     (Choice_C_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_add x1002 x2 x3250 x3500) (d_C_add x1003 x2 x3250 x3500)
     (Choices_C_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_add z x2 x3250 x3500) x1002
     (Guard_C_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_add x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leq :: C_Nat -> C_Nat -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leq x1 x2 x3250 x3500 = case x1 of
     C_Z -> Curry_Prelude.C_True
     (C_S x3) -> d_OP__case_0 x3 x2 x3250 x3500
     (Choice_C_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leq x1002 x2 x3250 x3500) (d_C_leq x1003 x2 x3250 x3500)
     (Choices_C_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leq z x2 x3250 x3500) x1002
     (Guard_C_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leq x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: C_Nat -> C_Nat -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_0 x3 x2 x3250 x3500 = case x2 of
     C_Z -> Curry_Prelude.C_False
     (C_S x4) -> d_C_leq x3 x4 x3250 x3500
     (Choice_C_Nat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x1002 x3250 x3500) (d_OP__case_0 x3 x1003 x3250 x3500)
     (Choices_C_Nat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 z x3250 x3500) x1002
     (Guard_C_Nat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Nat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
