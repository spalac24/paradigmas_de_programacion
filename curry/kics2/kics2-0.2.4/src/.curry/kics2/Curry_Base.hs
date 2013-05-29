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
  generate s = Choices_C_NDClass defCover (freeID [0,0] s) [C_D,C_ND]


instance NormalForm C_NDClass where
  ($!!) cont C_D cs = cont C_D cs
  ($!!) cont C_ND cs = cont C_ND cs
  ($!!) cont (Choice_C_NDClass cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_NDClass cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_NDClass cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_NDClass cd info) _ = failCons cd info
  ($##) cont C_D cs = cont C_D cs
  ($##) cont C_ND cs = cont C_ND cs
  ($##) cont (Choice_C_NDClass cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_NDClass cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_NDClass cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_NDClass cd info) _ = failCons cd info
  searchNF _ cont C_D = cont C_D
  searchNF _ cont C_ND = cont C_ND
  searchNF _ _ x = error ("Base.NDClass.searchNF: no constructor: " ++ (show x))


instance Unifiable C_NDClass where
  (=.=) C_D C_D cs = C_Success
  (=.=) C_ND C_ND cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_D C_D cs = C_Success
  (=.<=) C_ND C_ND cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_D = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_ND = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_NDClass cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_NDClass cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_NDClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_NDClass cd i _) = error ("Base.NDClass.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_NDClass cd info) = [(Unsolvable info)]
  bind i (Guard_C_NDClass cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_D = [(i :=: (ChooseN 0 0))]
  lazyBind i C_ND = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_NDClass cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_NDClass cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_NDClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_NDClass cd i _) = error ("Base.NDClass.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_NDClass cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_NDClass cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_NDClass where
  (=?=) (Choice_C_NDClass cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_NDClass cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_NDClass cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_NDClass cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_NDClass cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_NDClass cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_NDClass cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_NDClass cd info) _ = failCons cd info
  (=?=) C_D C_D cs = Curry_Prelude.C_True
  (=?=) C_ND C_ND cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_NDClass cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_NDClass cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_NDClass cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_NDClass cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_NDClass cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_NDClass cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_NDClass cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_NDClass cd info) _ = failCons cd info
  (<?=) C_D C_D cs = Curry_Prelude.C_True
  (<?=) C_D C_ND _ = Curry_Prelude.C_True
  (<?=) C_ND C_ND cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_NDClass where
  cover C_D = C_D
  cover C_ND = C_ND
  cover (Choice_C_NDClass cd i x y) = Choice_C_NDClass (incCover cd) i (cover x) (cover y)
  cover (Choices_C_NDClass cd i xs) = Choices_C_NDClass (incCover cd) i (map cover xs)
  cover (Fail_C_NDClass cd info) = Fail_C_NDClass (incCover cd) info
  cover (Guard_C_NDClass cd c e) = Guard_C_NDClass (incCover cd) c (cover e)


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
  generate s = Choices_C_HOClass defCover (freeID [0,0] s) [C_FO,C_HO]


instance NormalForm C_HOClass where
  ($!!) cont C_FO cs = cont C_FO cs
  ($!!) cont C_HO cs = cont C_HO cs
  ($!!) cont (Choice_C_HOClass cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_HOClass cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_HOClass cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_HOClass cd info) _ = failCons cd info
  ($##) cont C_FO cs = cont C_FO cs
  ($##) cont C_HO cs = cont C_HO cs
  ($##) cont (Choice_C_HOClass cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_HOClass cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_HOClass cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_HOClass cd info) _ = failCons cd info
  searchNF _ cont C_FO = cont C_FO
  searchNF _ cont C_HO = cont C_HO
  searchNF _ _ x = error ("Base.HOClass.searchNF: no constructor: " ++ (show x))


instance Unifiable C_HOClass where
  (=.=) C_FO C_FO cs = C_Success
  (=.=) C_HO C_HO cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_FO C_FO cs = C_Success
  (=.<=) C_HO C_HO cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_FO = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_HO = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_HOClass cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_HOClass cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_HOClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_HOClass cd i _) = error ("Base.HOClass.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_HOClass cd info) = [(Unsolvable info)]
  bind i (Guard_C_HOClass cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_FO = [(i :=: (ChooseN 0 0))]
  lazyBind i C_HO = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_HOClass cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_HOClass cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_HOClass cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_HOClass cd i _) = error ("Base.HOClass.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_HOClass cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_HOClass cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_HOClass where
  (=?=) (Choice_C_HOClass cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_HOClass cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_HOClass cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_HOClass cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_HOClass cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_HOClass cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_HOClass cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_HOClass cd info) _ = failCons cd info
  (=?=) C_FO C_FO cs = Curry_Prelude.C_True
  (=?=) C_HO C_HO cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_HOClass cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_HOClass cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_HOClass cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_HOClass cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_HOClass cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_HOClass cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_HOClass cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_HOClass cd info) _ = failCons cd info
  (<?=) C_FO C_FO cs = Curry_Prelude.C_True
  (<?=) C_FO C_HO _ = Curry_Prelude.C_True
  (<?=) C_HO C_HO cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_HOClass where
  cover C_FO = C_FO
  cover C_HO = C_HO
  cover (Choice_C_HOClass cd i x y) = Choice_C_HOClass (incCover cd) i (cover x) (cover y)
  cover (Choices_C_HOClass cd i xs) = Choices_C_HOClass (incCover cd) i (map cover xs)
  cover (Fail_C_HOClass cd info) = Fail_C_HOClass (incCover cd) info
  cover (Guard_C_HOClass cd c e) = Guard_C_HOClass (incCover cd) c (cover e)

