{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Base (C_NDClass (..), C_HOClass (..)) where

import Basics
import qualified Curry_Prelude
data C_NDClass
     = C_D
     | C_ND
     | Choice_C_NDClass Cover ID C_NDClass C_NDClass
     | Choices_C_NDClass Cover ID ([C_NDClass])
     | Fail_C_NDClass Cover FailInfo
     | Guard_C_NDClass Cover Constraints C_NDClass

instance Show C_NDClass where
  showsPrec d (Choice_C_NDClass cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_NDClass cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_NDClass cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_NDClass cd info) = showChar '!'
  showsPrec _ C_D = showString "D"
  showsPrec _ C_ND = showString "ND"


instance Read C_NDClass where
  readsPrec _ s = (readParen False (\r -> [ (C_D,r0) | (_,r0) <- readQualified "Base" "D" r]) s) ++ (readParen False (\r -> [ (C_ND,r0) | (_,r0) <- readQualified "Base" "ND" r]) s)


instance NonDet C_NDClass where
  choiceCons = Choice_C_NDClass
  choicesCons = Choices_C_NDClass
  failCons = Fail_C_NDClass
  guardCons = Guard_C_NDClass
  try (Choice_C_NDClass cd i x y) = tryChoice cd i x y
  try (Choices_C_NDClass cd i xs) = tryChoices cd i xs
  try (Fail_C_NDClass cd info) = Fail cd info
  try (Guard_C_NDClass cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_NDClass cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_NDClass cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_NDClass cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_NDClass cd i _) = error ("Base.NDClass.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_NDClass cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_NDClass cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_NDClass where
  generate s c = Choices_C_NDClass c (freeID [0,0] s) [C_D,C_ND]


instance NormalForm C_NDClass where
  ($!!) cont C_D d cs = cont C_D d cs
  ($!!) cont C_ND d cs = cont C_ND d cs
  ($!!) cont (Choice_C_NDClass cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_NDClass cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_NDClass cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_NDClass cd info) _ _ = failCons cd info
  ($##) cont C_D d cs = cont C_D d cs
  ($##) cont C_ND d cs = cont C_ND d cs
  ($##) cont (Choice_C_NDClass cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_NDClass cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_NDClass cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_NDClass cd info) _ _ = failCons cd info
  searchNF _ cont C_D = cont C_D
  searchNF _ cont C_ND = cont C_ND
  searchNF _ _ x = error ("Base.NDClass.searchNF: no constructor: " ++ (show x))


instance Unifiable C_NDClass where
  (=.=) C_D C_D d cs = C_Success
  (=.=) C_ND C_ND d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_D C_D d cs = C_Success
  (=.<=) C_ND C_ND d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_D = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_ND = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_NDClass cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_NDClass cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_NDClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_NDClass cd i _) = error ("Base.NDClass.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_NDClass cd info) = [(Unsolvable info)]
  bind d i (Guard_C_NDClass cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_D = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_ND = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_NDClass cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_NDClass cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_NDClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_NDClass cd i _) = error ("Base.NDClass.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_NDClass cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_NDClass cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_NDClass where
  (=?=) (Choice_C_NDClass cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_NDClass cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_NDClass cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_NDClass cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_NDClass cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_NDClass cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_NDClass cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_NDClass cd info) _ _ = failCons cd info
  (=?=) C_D C_D d cs = Curry_Prelude.C_True
  (=?=) C_ND C_ND d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_NDClass cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_NDClass cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_NDClass cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_NDClass cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_NDClass cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_NDClass cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_NDClass cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_NDClass cd info) _ _ = failCons cd info
  (<?=) C_D C_D d cs = Curry_Prelude.C_True
  (<?=) C_D C_ND _ _ = Curry_Prelude.C_True
  (<?=) C_ND C_ND d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_HOClass
     = C_FO
     | C_HO
     | Choice_C_HOClass Cover ID C_HOClass C_HOClass
     | Choices_C_HOClass Cover ID ([C_HOClass])
     | Fail_C_HOClass Cover FailInfo
     | Guard_C_HOClass Cover Constraints C_HOClass

instance Show C_HOClass where
  showsPrec d (Choice_C_HOClass cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_HOClass cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_HOClass cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_HOClass cd info) = showChar '!'
  showsPrec _ C_FO = showString "FO"
  showsPrec _ C_HO = showString "HO"


instance Read C_HOClass where
  readsPrec _ s = (readParen False (\r -> [ (C_FO,r0) | (_,r0) <- readQualified "Base" "FO" r]) s) ++ (readParen False (\r -> [ (C_HO,r0) | (_,r0) <- readQualified "Base" "HO" r]) s)


instance NonDet C_HOClass where
  choiceCons = Choice_C_HOClass
  choicesCons = Choices_C_HOClass
  failCons = Fail_C_HOClass
  guardCons = Guard_C_HOClass
  try (Choice_C_HOClass cd i x y) = tryChoice cd i x y
  try (Choices_C_HOClass cd i xs) = tryChoices cd i xs
  try (Fail_C_HOClass cd info) = Fail cd info
  try (Guard_C_HOClass cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_HOClass cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_HOClass cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_HOClass cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_HOClass cd i _) = error ("Base.HOClass.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_HOClass cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_HOClass cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_HOClass where
  generate s c = Choices_C_HOClass c (freeID [0,0] s) [C_FO,C_HO]


instance NormalForm C_HOClass where
  ($!!) cont C_FO d cs = cont C_FO d cs
  ($!!) cont C_HO d cs = cont C_HO d cs
  ($!!) cont (Choice_C_HOClass cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_HOClass cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_HOClass cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_HOClass cd info) _ _ = failCons cd info
  ($##) cont C_FO d cs = cont C_FO d cs
  ($##) cont C_HO d cs = cont C_HO d cs
  ($##) cont (Choice_C_HOClass cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_HOClass cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_HOClass cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_HOClass cd info) _ _ = failCons cd info
  searchNF _ cont C_FO = cont C_FO
  searchNF _ cont C_HO = cont C_HO
  searchNF _ _ x = error ("Base.HOClass.searchNF: no constructor: " ++ (show x))


instance Unifiable C_HOClass where
  (=.=) C_FO C_FO d cs = C_Success
  (=.=) C_HO C_HO d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_FO C_FO d cs = C_Success
  (=.<=) C_HO C_HO d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_FO = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_HO = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_HOClass cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_HOClass cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_HOClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_HOClass cd i _) = error ("Base.HOClass.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_HOClass cd info) = [(Unsolvable info)]
  bind d i (Guard_C_HOClass cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_FO = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_HO = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_HOClass cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_HOClass cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_HOClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_HOClass cd i _) = error ("Base.HOClass.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_HOClass cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_HOClass cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_HOClass where
  (=?=) (Choice_C_HOClass cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_HOClass cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_HOClass cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_HOClass cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_HOClass cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_HOClass cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_HOClass cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_HOClass cd info) _ _ = failCons cd info
  (=?=) C_FO C_FO d cs = Curry_Prelude.C_True
  (=?=) C_HO C_HO d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_HOClass cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_HOClass cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_HOClass cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_HOClass cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_HOClass cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_HOClass cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_HOClass cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_HOClass cd info) _ _ = failCons cd info
  (<?=) C_FO C_FO d cs = Curry_Prelude.C_True
  (<?=) C_FO C_HO _ _ = Curry_Prelude.C_True
  (<?=) C_HO C_HO d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False

