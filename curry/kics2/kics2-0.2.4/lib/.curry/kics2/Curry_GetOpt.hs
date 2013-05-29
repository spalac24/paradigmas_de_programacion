{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_GetOpt (C_ArgOrder (..), C_OptDescr (..), C_ArgDescr (..), C_OptKind, d_C_usageInfo, nd_C_usageInfo, d_C_getOpt, nd_C_getOpt, d_C_getOpt', nd_C_getOpt') where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
data C_ArgOrder t0
     = C_RequireOrder
     | C_Permute
     | C_ReturnInOrder (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t0)
     | HO_C_ReturnInOrder (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0)
     | Choice_C_ArgOrder Cover ID (C_ArgOrder t0) (C_ArgOrder t0)
     | Choices_C_ArgOrder Cover ID ([C_ArgOrder t0])
     | Fail_C_ArgOrder Cover FailInfo
     | Guard_C_ArgOrder Cover Constraints (C_ArgOrder t0)

instance Show t0 => Show (C_ArgOrder t0) where
  showsPrec d (Choice_C_ArgOrder cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ArgOrder cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ArgOrder cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ArgOrder cd info) = showChar '!'
  showsPrec _ C_RequireOrder = showString "RequireOrder"
  showsPrec _ C_Permute = showString "Permute"
  showsPrec _ (C_ReturnInOrder x1) = (showString "(ReturnInOrder") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (HO_C_ReturnInOrder x1) = (showString "(ReturnInOrder") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_ArgOrder t0) where
  readsPrec d s = (readParen False (\r -> [ (C_RequireOrder,r0) | (_,r0) <- readQualified "GetOpt" "RequireOrder" r]) s) ++ ((readParen False (\r -> [ (C_Permute,r0) | (_,r0) <- readQualified "GetOpt" "Permute" r]) s) ++ (readParen (d > 10) (\r -> [ (C_ReturnInOrder x1,r1) | (_,r0) <- readQualified "GetOpt" "ReturnInOrder" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet (C_ArgOrder t0) where
  choiceCons = Choice_C_ArgOrder
  choicesCons = Choices_C_ArgOrder
  failCons = Fail_C_ArgOrder
  guardCons = Guard_C_ArgOrder
  try (Choice_C_ArgOrder cd i x y) = tryChoice cd i x y
  try (Choices_C_ArgOrder cd i xs) = tryChoices cd i xs
  try (Fail_C_ArgOrder cd info) = Fail cd info
  try (Guard_C_ArgOrder cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ArgOrder cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ArgOrder cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ArgOrder cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ArgOrder cd i _) = error ("GetOpt.ArgOrder.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ArgOrder cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ArgOrder cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_ArgOrder t0) where
  generate s = Choices_C_ArgOrder defCover (freeID [0,0,1] s) [C_RequireOrder,C_Permute,(C_ReturnInOrder (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_ArgOrder t0) where
  ($!!) cont C_RequireOrder cs = cont C_RequireOrder cs
  ($!!) cont C_Permute cs = cont C_Permute cs
  ($!!) cont (C_ReturnInOrder x1) cs = ((\y1 cs -> cont (C_ReturnInOrder y1) cs) $!! x1) cs
  ($!!) cont (HO_C_ReturnInOrder x1) cs = ((\y1 cs -> cont (HO_C_ReturnInOrder y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_ArgOrder cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_ArgOrder cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_ArgOrder cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_ArgOrder cd info) _ = failCons cd info
  ($##) cont C_RequireOrder cs = cont C_RequireOrder cs
  ($##) cont C_Permute cs = cont C_Permute cs
  ($##) cont (C_ReturnInOrder x1) cs = ((\y1 cs -> cont (C_ReturnInOrder y1) cs) $## x1) cs
  ($##) cont (HO_C_ReturnInOrder x1) cs = ((\y1 cs -> cont (HO_C_ReturnInOrder y1) cs) $## x1) cs
  ($##) cont (Choice_C_ArgOrder cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_ArgOrder cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_ArgOrder cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_ArgOrder cd info) _ = failCons cd info
  searchNF _ cont C_RequireOrder = cont C_RequireOrder
  searchNF _ cont C_Permute = cont C_Permute
  searchNF search cont (C_ReturnInOrder x1) = search (\y1 -> cont (C_ReturnInOrder y1)) x1
  searchNF search cont (HO_C_ReturnInOrder x1) = search (\y1 -> cont (HO_C_ReturnInOrder y1)) x1
  searchNF _ _ x = error ("GetOpt.ArgOrder.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ArgOrder t0) where
  (=.=) C_RequireOrder C_RequireOrder cs = C_Success
  (=.=) C_Permute C_Permute cs = C_Success
  (=.=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) cs = (x1 =:= y1) cs
  (=.=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_RequireOrder C_RequireOrder cs = C_Success
  (=.<=) C_Permute C_Permute cs = C_Success
  (=.<=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) cs = (x1 =:<= y1) cs
  (=.<=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_RequireOrder = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_Permute = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (C_ReturnInOrder x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (HO_C_ReturnInOrder x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_ArgOrder cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_ArgOrder cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_ArgOrder cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_ArgOrder cd i _) = error ("GetOpt.ArgOrder.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_ArgOrder cd info) = [(Unsolvable info)]
  bind i (Guard_C_ArgOrder cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_RequireOrder = [(i :=: (ChooseN 0 0))]
  lazyBind i C_Permute = [(i :=: (ChooseN 1 0))]
  lazyBind i (C_ReturnInOrder x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (HO_C_ReturnInOrder x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_ArgOrder cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_ArgOrder cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_ArgOrder cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_ArgOrder cd i _) = error ("GetOpt.ArgOrder.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_ArgOrder cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_ArgOrder cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ArgOrder t0) where
  (=?=) (Choice_C_ArgOrder cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_ArgOrder cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_ArgOrder cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_ArgOrder cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_ArgOrder cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_ArgOrder cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_ArgOrder cd info) _ = failCons cd info
  (=?=) C_RequireOrder C_RequireOrder cs = Curry_Prelude.C_True
  (=?=) C_Permute C_Permute cs = Curry_Prelude.C_True
  (=?=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ArgOrder cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_ArgOrder cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_ArgOrder cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_ArgOrder cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_ArgOrder cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_ArgOrder cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_ArgOrder cd info) _ = failCons cd info
  (<?=) C_RequireOrder C_RequireOrder cs = Curry_Prelude.C_True
  (<?=) C_RequireOrder C_Permute _ = Curry_Prelude.C_True
  (<?=) C_RequireOrder (C_ReturnInOrder _) _ = Curry_Prelude.C_True
  (<?=) C_RequireOrder (HO_C_ReturnInOrder _) _ = Curry_Prelude.C_True
  (<?=) C_Permute C_Permute cs = Curry_Prelude.C_True
  (<?=) C_Permute (C_ReturnInOrder _) _ = Curry_Prelude.C_True
  (<?=) C_Permute (HO_C_ReturnInOrder _) _ = Curry_Prelude.C_True
  (<?=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_ArgOrder t0) where
  cover C_RequireOrder = C_RequireOrder
  cover C_Permute = C_Permute
  cover (C_ReturnInOrder x1) = C_ReturnInOrder (cover x1)
  cover (HO_C_ReturnInOrder x1) = HO_C_ReturnInOrder (cover x1)
  cover (Choice_C_ArgOrder cd i x y) = Choice_C_ArgOrder (incCover cd) i (cover x) (cover y)
  cover (Choices_C_ArgOrder cd i xs) = Choices_C_ArgOrder (incCover cd) i (map cover xs)
  cover (Fail_C_ArgOrder cd info) = Fail_C_ArgOrder (incCover cd) info
  cover (Guard_C_ArgOrder cd c e) = Guard_C_ArgOrder (incCover cd) c (cover e)


data C_OptDescr t0
     = C_Option (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (C_ArgDescr t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_OptDescr Cover ID (C_OptDescr t0) (C_OptDescr t0)
     | Choices_C_OptDescr Cover ID ([C_OptDescr t0])
     | Fail_C_OptDescr Cover FailInfo
     | Guard_C_OptDescr Cover Constraints (C_OptDescr t0)

instance Show t0 => Show (C_OptDescr t0) where
  showsPrec d (Choice_C_OptDescr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_OptDescr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_OptDescr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_OptDescr cd info) = showChar '!'
  showsPrec _ (C_Option x1 x2 x3 x4) = (showString "(Option") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read t0 => Read (C_OptDescr t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Option x1 x2 x3 x4,r4) | (_,r0) <- readQualified "GetOpt" "Option" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet (C_OptDescr t0) where
  choiceCons = Choice_C_OptDescr
  choicesCons = Choices_C_OptDescr
  failCons = Fail_C_OptDescr
  guardCons = Guard_C_OptDescr
  try (Choice_C_OptDescr cd i x y) = tryChoice cd i x y
  try (Choices_C_OptDescr cd i xs) = tryChoices cd i xs
  try (Fail_C_OptDescr cd info) = Fail cd info
  try (Guard_C_OptDescr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_OptDescr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_OptDescr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_OptDescr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_OptDescr cd i _) = error ("GetOpt.OptDescr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_OptDescr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_OptDescr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_OptDescr t0) where
  generate s = Choices_C_OptDescr defCover (freeID [4] s) [(C_Option (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance NormalForm t0 => NormalForm (C_OptDescr t0) where
  ($!!) cont (C_Option x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_Option y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_OptDescr cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_OptDescr cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_OptDescr cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_OptDescr cd info) _ = failCons cd info
  ($##) cont (C_Option x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_Option y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_OptDescr cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_OptDescr cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_OptDescr cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_OptDescr cd info) _ = failCons cd info
  searchNF search cont (C_Option x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Option y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("GetOpt.OptDescr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_OptDescr t0) where
  (=.=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Option x2 x3 x4 x5) = ((i :=: (ChooseN 0 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (Choice_C_OptDescr cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_OptDescr cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_OptDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_OptDescr cd i _) = error ("GetOpt.OptDescr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_OptDescr cd info) = [(Unsolvable info)]
  bind i (Guard_C_OptDescr cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Option x2 x3 x4 x5) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (Choice_C_OptDescr cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_OptDescr cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_OptDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_OptDescr cd i _) = error ("GetOpt.OptDescr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_OptDescr cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_OptDescr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_OptDescr t0) where
  (=?=) (Choice_C_OptDescr cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_OptDescr cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_OptDescr cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_OptDescr cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_OptDescr cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_OptDescr cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_OptDescr cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_OptDescr cd info) _ = failCons cd info
  (=?=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (<?=) (Choice_C_OptDescr cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_OptDescr cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_OptDescr cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_OptDescr cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_OptDescr cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_OptDescr cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_OptDescr cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_OptDescr cd info) _ = failCons cd info
  (<?=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs


instance Coverable t0 => Coverable (C_OptDescr t0) where
  cover (C_Option x1 x2 x3 x4) = C_Option (cover x1) (cover x2) (cover x3) (cover x4)
  cover (Choice_C_OptDescr cd i x y) = Choice_C_OptDescr (incCover cd) i (cover x) (cover y)
  cover (Choices_C_OptDescr cd i xs) = Choices_C_OptDescr (incCover cd) i (map cover xs)
  cover (Fail_C_OptDescr cd info) = Fail_C_OptDescr (incCover cd) info
  cover (Guard_C_OptDescr cd c e) = Guard_C_OptDescr (incCover cd) c (cover e)


data C_ArgDescr t0
     = C_NoArg t0
     | C_ReqArg (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | HO_C_ReqArg (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_OptArg (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | HO_C_OptArg (Func (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_ArgDescr Cover ID (C_ArgDescr t0) (C_ArgDescr t0)
     | Choices_C_ArgDescr Cover ID ([C_ArgDescr t0])
     | Fail_C_ArgDescr Cover FailInfo
     | Guard_C_ArgDescr Cover Constraints (C_ArgDescr t0)

instance Show t0 => Show (C_ArgDescr t0) where
  showsPrec d (Choice_C_ArgDescr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ArgDescr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ArgDescr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ArgDescr cd info) = showChar '!'
  showsPrec _ (C_NoArg x1) = (showString "(NoArg") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ReqArg x1 x2) = (showString "(ReqArg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_ReqArg x1 x2) = (showString "(ReqArg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_OptArg x1 x2) = (showString "(OptArg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_OptArg x1 x2) = (showString "(OptArg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_ArgDescr t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_NoArg x1,r1) | (_,r0) <- readQualified "GetOpt" "NoArg" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_ReqArg x1 x2,r2) | (_,r0) <- readQualified "GetOpt" "ReqArg" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_OptArg x1 x2,r2) | (_,r0) <- readQualified "GetOpt" "OptArg" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


instance NonDet (C_ArgDescr t0) where
  choiceCons = Choice_C_ArgDescr
  choicesCons = Choices_C_ArgDescr
  failCons = Fail_C_ArgDescr
  guardCons = Guard_C_ArgDescr
  try (Choice_C_ArgDescr cd i x y) = tryChoice cd i x y
  try (Choices_C_ArgDescr cd i xs) = tryChoices cd i xs
  try (Fail_C_ArgDescr cd info) = Fail cd info
  try (Guard_C_ArgDescr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ArgDescr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ArgDescr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ArgDescr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ArgDescr cd i _) = error ("GetOpt.ArgDescr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ArgDescr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ArgDescr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_ArgDescr t0) where
  generate s = Choices_C_ArgDescr defCover (freeID [1,2,2] s) [(C_NoArg (generate (leftSupply s))),(C_ReqArg (generate (leftSupply s)) (generate (rightSupply s))),(C_OptArg (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm t0 => NormalForm (C_ArgDescr t0) where
  ($!!) cont (C_NoArg x1) cs = ((\y1 cs -> cont (C_NoArg y1) cs) $!! x1) cs
  ($!!) cont (C_ReqArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_ReqArg y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_ReqArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_ReqArg y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_OptArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_OptArg y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_OptArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_OptArg y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_ArgDescr cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_ArgDescr cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_ArgDescr cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_ArgDescr cd info) _ = failCons cd info
  ($##) cont (C_NoArg x1) cs = ((\y1 cs -> cont (C_NoArg y1) cs) $## x1) cs
  ($##) cont (C_ReqArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_ReqArg y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_ReqArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_ReqArg y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_OptArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_OptArg y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_OptArg x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_OptArg y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_ArgDescr cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_ArgDescr cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_ArgDescr cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_ArgDescr cd info) _ = failCons cd info
  searchNF search cont (C_NoArg x1) = search (\y1 -> cont (C_NoArg y1)) x1
  searchNF search cont (C_ReqArg x1 x2) = search (\y1 -> search (\y2 -> cont (C_ReqArg y1 y2)) x2) x1
  searchNF search cont (HO_C_ReqArg x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_ReqArg y1 y2)) x2) x1
  searchNF search cont (C_OptArg x1 x2) = search (\y1 -> search (\y2 -> cont (C_OptArg y1 y2)) x2) x1
  searchNF search cont (HO_C_OptArg x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_OptArg y1 y2)) x2) x1
  searchNF _ _ x = error ("GetOpt.ArgDescr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ArgDescr t0) where
  (=.=) (C_NoArg x1) (C_NoArg y1) cs = (x1 =:= y1) cs
  (=.=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_OptArg x1 x2) (C_OptArg y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_NoArg x1) (C_NoArg y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_OptArg x1 x2) (C_OptArg y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_NoArg x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_ReqArg x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_ReqArg x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_OptArg x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_OptArg x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_ArgDescr cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_ArgDescr cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_ArgDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_ArgDescr cd i _) = error ("GetOpt.ArgDescr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_ArgDescr cd info) = [(Unsolvable info)]
  bind i (Guard_C_ArgDescr cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_NoArg x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_ReqArg x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_ReqArg x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_OptArg x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_OptArg x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_ArgDescr cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_ArgDescr cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_ArgDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_ArgDescr cd i _) = error ("GetOpt.ArgDescr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_ArgDescr cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_ArgDescr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ArgDescr t0) where
  (=?=) (Choice_C_ArgDescr cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_ArgDescr cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_ArgDescr cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_ArgDescr cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_ArgDescr cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_ArgDescr cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_ArgDescr cd info) _ = failCons cd info
  (=?=) (C_NoArg x1) (C_NoArg y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_OptArg x1 x2) (C_OptArg y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ArgDescr cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_ArgDescr cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_ArgDescr cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_ArgDescr cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_ArgDescr cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_ArgDescr cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_ArgDescr cd info) _ = failCons cd info
  (<?=) (C_NoArg x1) (C_NoArg y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_NoArg _) (C_ReqArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (HO_C_ReqArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (HO_C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_ReqArg _ _) (C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_ReqArg _ _) (HO_C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_ReqArg _ _) (C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_ReqArg _ _) (HO_C_OptArg _ _) _ = Curry_Prelude.C_True
  (<?=) (C_OptArg x1 x2) (C_OptArg y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_ArgDescr t0) where
  cover (C_NoArg x1) = C_NoArg (cover x1)
  cover (C_ReqArg x1 x2) = C_ReqArg (cover x1) (cover x2)
  cover (HO_C_ReqArg x1 x2) = HO_C_ReqArg (cover x1) (cover x2)
  cover (C_OptArg x1 x2) = C_OptArg (cover x1) (cover x2)
  cover (HO_C_OptArg x1 x2) = HO_C_OptArg (cover x1) (cover x2)
  cover (Choice_C_ArgDescr cd i x y) = Choice_C_ArgDescr (incCover cd) i (cover x) (cover y)
  cover (Choices_C_ArgDescr cd i xs) = Choices_C_ArgDescr (incCover cd) i (map cover xs)
  cover (Fail_C_ArgDescr cd info) = Fail_C_ArgDescr (incCover cd) info
  cover (Guard_C_ArgDescr cd c e) = Guard_C_ArgDescr (incCover cd) c (cover e)


data C_OptKind t0
     = C_Opt t0
     | C_UnreqOpt (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_NonOpt (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_EndOfOpts
     | C_OptErr (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_OptKind Cover ID (C_OptKind t0) (C_OptKind t0)
     | Choices_C_OptKind Cover ID ([C_OptKind t0])
     | Fail_C_OptKind Cover FailInfo
     | Guard_C_OptKind Cover Constraints (C_OptKind t0)

instance Show t0 => Show (C_OptKind t0) where
  showsPrec d (Choice_C_OptKind cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_OptKind cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_OptKind cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_OptKind cd info) = showChar '!'
  showsPrec _ (C_Opt x1) = (showString "(Opt") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_UnreqOpt x1) = (showString "(UnreqOpt") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_NonOpt x1) = (showString "(NonOpt") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_EndOfOpts = showString "EndOfOpts"
  showsPrec _ (C_OptErr x1) = (showString "(OptErr") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read t0 => Read (C_OptKind t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Opt x1,r1) | (_,r0) <- readQualified "GetOpt" "Opt" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_UnreqOpt x1,r1) | (_,r0) <- readQualified "GetOpt" "UnreqOpt" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_NonOpt x1,r1) | (_,r0) <- readQualified "GetOpt" "NonOpt" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_EndOfOpts,r0) | (_,r0) <- readQualified "GetOpt" "EndOfOpts" r]) s) ++ (readParen (d > 10) (\r -> [ (C_OptErr x1,r1) | (_,r0) <- readQualified "GetOpt" "OptErr" r, (x1,r1) <- readsPrec 11 r0]) s))))


instance NonDet (C_OptKind t0) where
  choiceCons = Choice_C_OptKind
  choicesCons = Choices_C_OptKind
  failCons = Fail_C_OptKind
  guardCons = Guard_C_OptKind
  try (Choice_C_OptKind cd i x y) = tryChoice cd i x y
  try (Choices_C_OptKind cd i xs) = tryChoices cd i xs
  try (Fail_C_OptKind cd info) = Fail cd info
  try (Guard_C_OptKind cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_OptKind cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_OptKind cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_OptKind cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_OptKind cd i _) = error ("GetOpt.OptKind.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_OptKind cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_OptKind cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_OptKind t0) where
  generate s = Choices_C_OptKind defCover (freeID [1,1,1,0,1] s) [(C_Opt (generate (leftSupply s))),(C_UnreqOpt (generate (leftSupply s))),(C_NonOpt (generate (leftSupply s))),C_EndOfOpts,(C_OptErr (generate (leftSupply s)))]


instance NormalForm t0 => NormalForm (C_OptKind t0) where
  ($!!) cont (C_Opt x1) cs = ((\y1 cs -> cont (C_Opt y1) cs) $!! x1) cs
  ($!!) cont (C_UnreqOpt x1) cs = ((\y1 cs -> cont (C_UnreqOpt y1) cs) $!! x1) cs
  ($!!) cont (C_NonOpt x1) cs = ((\y1 cs -> cont (C_NonOpt y1) cs) $!! x1) cs
  ($!!) cont C_EndOfOpts cs = cont C_EndOfOpts cs
  ($!!) cont (C_OptErr x1) cs = ((\y1 cs -> cont (C_OptErr y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_OptKind cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_OptKind cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_OptKind cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_OptKind cd info) _ = failCons cd info
  ($##) cont (C_Opt x1) cs = ((\y1 cs -> cont (C_Opt y1) cs) $## x1) cs
  ($##) cont (C_UnreqOpt x1) cs = ((\y1 cs -> cont (C_UnreqOpt y1) cs) $## x1) cs
  ($##) cont (C_NonOpt x1) cs = ((\y1 cs -> cont (C_NonOpt y1) cs) $## x1) cs
  ($##) cont C_EndOfOpts cs = cont C_EndOfOpts cs
  ($##) cont (C_OptErr x1) cs = ((\y1 cs -> cont (C_OptErr y1) cs) $## x1) cs
  ($##) cont (Choice_C_OptKind cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_OptKind cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_OptKind cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_OptKind cd info) _ = failCons cd info
  searchNF search cont (C_Opt x1) = search (\y1 -> cont (C_Opt y1)) x1
  searchNF search cont (C_UnreqOpt x1) = search (\y1 -> cont (C_UnreqOpt y1)) x1
  searchNF search cont (C_NonOpt x1) = search (\y1 -> cont (C_NonOpt y1)) x1
  searchNF _ cont C_EndOfOpts = cont C_EndOfOpts
  searchNF search cont (C_OptErr x1) = search (\y1 -> cont (C_OptErr y1)) x1
  searchNF _ _ x = error ("GetOpt.OptKind.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_OptKind t0) where
  (=.=) (C_Opt x1) (C_Opt y1) cs = (x1 =:= y1) cs
  (=.=) (C_UnreqOpt x1) (C_UnreqOpt y1) cs = (x1 =:= y1) cs
  (=.=) (C_NonOpt x1) (C_NonOpt y1) cs = (x1 =:= y1) cs
  (=.=) C_EndOfOpts C_EndOfOpts cs = C_Success
  (=.=) (C_OptErr x1) (C_OptErr y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Opt x1) (C_Opt y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_UnreqOpt x1) (C_UnreqOpt y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_NonOpt x1) (C_NonOpt y1) cs = (x1 =:<= y1) cs
  (=.<=) C_EndOfOpts C_EndOfOpts cs = C_Success
  (=.<=) (C_OptErr x1) (C_OptErr y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Opt x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_UnreqOpt x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_NonOpt x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i C_EndOfOpts = ((i :=: (ChooseN 3 0)):(concat []))
  bind i (C_OptErr x2) = ((i :=: (ChooseN 4 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_OptKind cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_OptKind cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_OptKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_OptKind cd i _) = error ("GetOpt.OptKind.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_OptKind cd info) = [(Unsolvable info)]
  bind i (Guard_C_OptKind cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Opt x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_UnreqOpt x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_NonOpt x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i C_EndOfOpts = [(i :=: (ChooseN 3 0))]
  lazyBind i (C_OptErr x2) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_OptKind cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_OptKind cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_OptKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_OptKind cd i _) = error ("GetOpt.OptKind.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_OptKind cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_OptKind cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_OptKind t0) where
  (=?=) (Choice_C_OptKind cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_OptKind cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_OptKind cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_OptKind cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_OptKind cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_OptKind cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_OptKind cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_OptKind cd info) _ = failCons cd info
  (=?=) (C_Opt x1) (C_Opt y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_UnreqOpt x1) (C_UnreqOpt y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_NonOpt x1) (C_NonOpt y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) C_EndOfOpts C_EndOfOpts cs = Curry_Prelude.C_True
  (=?=) (C_OptErr x1) (C_OptErr y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_OptKind cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_OptKind cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_OptKind cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_OptKind cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_OptKind cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_OptKind cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_OptKind cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_OptKind cd info) _ = failCons cd info
  (<?=) (C_Opt x1) (C_Opt y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Opt _) (C_UnreqOpt _) _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) (C_NonOpt _) _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) C_EndOfOpts _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) (C_OptErr _) _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt x1) (C_UnreqOpt y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_UnreqOpt _) (C_NonOpt _) _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt _) C_EndOfOpts _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt _) (C_OptErr _) _ = Curry_Prelude.C_True
  (<?=) (C_NonOpt x1) (C_NonOpt y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_NonOpt _) C_EndOfOpts _ = Curry_Prelude.C_True
  (<?=) (C_NonOpt _) (C_OptErr _) _ = Curry_Prelude.C_True
  (<?=) C_EndOfOpts C_EndOfOpts cs = Curry_Prelude.C_True
  (<?=) C_EndOfOpts (C_OptErr _) _ = Curry_Prelude.C_True
  (<?=) (C_OptErr x1) (C_OptErr y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_OptKind t0) where
  cover (C_Opt x1) = C_Opt (cover x1)
  cover (C_UnreqOpt x1) = C_UnreqOpt (cover x1)
  cover (C_NonOpt x1) = C_NonOpt (cover x1)
  cover C_EndOfOpts = C_EndOfOpts
  cover (C_OptErr x1) = C_OptErr (cover x1)
  cover (Choice_C_OptKind cd i x y) = Choice_C_OptKind (incCover cd) i (cover x) (cover y)
  cover (Choices_C_OptKind cd i xs) = Choices_C_OptKind (incCover cd) i (map cover xs)
  cover (Fail_C_OptKind cd info) = Fail_C_OptKind (incCover cd) info
  cover (Guard_C_OptKind cd c e) = Guard_C_OptKind (incCover cd) c (cover e)


d_C_usageInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_usageInfo x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unzip3 (Curry_Prelude.d_C_concatMap d_C_fmtOpt x3500) x3500) x2 x3500
     x4 = d_OP_usageInfo_dot___hash_selFP2_hash_ss x3 x3500
     x5 = d_OP_usageInfo_dot___hash_selFP3_hash_ls x3 x3500
     x6 = d_OP_usageInfo_dot___hash_selFP4_hash_ds x3 x3500
     x7 = Curry_Prelude.d_C_zipWith3 (acceptCs (acceptCs id) d_OP_usageInfo_dot_paste_dot_2) (d_OP_usageInfo_dot_sameLen_dot_2 x4 x3500) (d_OP_usageInfo_dot_sameLen_dot_2 x5 x3500) x6 x3500
      in (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons x1 x7) x3500)

nd_C_usageInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_usageInfo x1 x2 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2004 = leftSupply x2006
          x2005 = rightSupply x2006
           in (seq x2004 (seq x2005 (let
               x3 = let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unzip3) (Curry_Prelude.nd_C_concatMap (wrapNX id nd_C_fmtOpt) x2000 x3500) x2001 x3500)))) x2 x2003 x3500)))
               x4 = d_OP_usageInfo_dot___hash_selFP2_hash_ss x3 x3500
               x5 = d_OP_usageInfo_dot___hash_selFP3_hash_ls x3 x3500
               x6 = d_OP_usageInfo_dot___hash_selFP4_hash_ds x3 x3500
               x7 = Curry_Prelude.nd_C_zipWith3 (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) d_OP_usageInfo_dot_paste_dot_2)) (d_OP_usageInfo_dot_sameLen_dot_2 x4 x3500) (d_OP_usageInfo_dot_sameLen_dot_2 x5 x3500) x6 x2005 x3500
                in (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons x1 x7) x3500))))))

d_OP_usageInfo_dot___hash_selFP2_hash_ss :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP2_hash_ss x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP2_hash_ss x1002 x3500) (d_OP_usageInfo_dot___hash_selFP2_hash_ss x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP2_hash_ss z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP2_hash_ss x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_usageInfo_dot___hash_selFP3_hash_ls :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP3_hash_ls x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP3_hash_ls x1002 x3500) (d_OP_usageInfo_dot___hash_selFP3_hash_ls x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP3_hash_ls z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP3_hash_ls x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_usageInfo_dot___hash_selFP4_hash_ds :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP4_hash_ds x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP4_hash_ds x1002 x3500) (d_OP_usageInfo_dot___hash_selFP4_hash_ds x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP4_hash_ds z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP4_hash_ds x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_usageInfo_dot_paste_dot_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_usageInfo_dot_paste_dot_2 x1 x2 x3 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3 x3500) x3500) x3500) x3500) x3500

d_OP_usageInfo_dot_flushLeft_dot_2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot_flushLeft_dot_2 x1 x2 x3500 = Curry_Prelude.d_C_map (d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 x1) x2 x3500

d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_C_take x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3500) x3500) x3500

d_OP_usageInfo_dot_sameLen_dot_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot_sameLen_dot_2 x1 x3500 = d_OP_usageInfo_dot_flushLeft_dot_2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_OP_dot d_C_maximum (Curry_Prelude.d_C_map Curry_Prelude.d_C_length) x3500) x1 x3500) x1 x3500

d_C_maximum :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_maximum x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))))))))))))) x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_C_foldl1 (acceptCs id Curry_Prelude.d_C_max) x1 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maximum x1002 x3500) (d_C_maximum x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maximum z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maximum x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fmtOpt :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_fmtOpt x1 x3500 = case x1 of
     (C_Option x2 x3 x4 x5) -> let
          x6 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.d_C_map (d_C_fmtShort x4) x2 x3500) x3500
          x7 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.d_C_map (d_C_fmtLong x4) x3 x3500) x3500
           in (d_OP__case_28 x5 x6 x7 (Curry_Prelude.d_C_lines x5 x3500) x3500)
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtOpt x1002 x3500) (d_C_fmtOpt x1003 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtOpt z x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtOpt x1002) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_fmtOpt :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_fmtOpt x1 x3000 x3500 = case x1 of
     (C_Option x2 x3 x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (let
                         x6 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_fmtShort x4)) x2 x2000 x3500) x3500
                         x7 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_fmtLong x4)) x3 x2001 x3500) x3500
                          in (nd_OP__case_28 x5 x6 x7 (Curry_Prelude.d_C_lines x5 x3500) x2002 x3500)))))))))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtOpt x1002 x3000 x3500) (nd_C_fmtOpt x1003 x3000 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtOpt z x3000 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtOpt x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_fmtOpt_dot_sepBy_dot_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_fmtOpt_dot_sepBy_dot_19 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_27 x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1002 x3500) (d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_fmtOpt_dot_sepBy_dot_19 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_fmtOpt_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_fmtOpt_dot___hash_lambda3 x1 x3500 = Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_Prelude.OP_List x1

d_C_fmtShort :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fmtShort x1 x2 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3500
     (C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500
     (C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtShort x1002 x2 x3500) (d_C_fmtShort x1003 x2 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtShort z x2 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtShort x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_fmtShort :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_fmtShort x1 x2 x3000 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3500
     (HO_C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500
     (HO_C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtShort x1002 x2 x3000 x3500) (nd_C_fmtShort x1003 x2 x3000 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtShort z x2 x3000 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtShort x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fmtLong :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fmtLong x1 x2 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2 x3500
     (C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500
     (C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtLong x1002 x2 x3500) (d_C_fmtLong x1003 x2 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtLong z x2 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtLong x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_fmtLong :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_fmtLong x1 x2 x3000 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2 x3500
     (HO_C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500
     (HO_C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtLong x1002 x2 x3000 x3500) (nd_C_fmtLong x1003 x2 x3000 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtLong z x2 x3000 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtLong x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getOpt :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getOpt x1 x2 x3 x3500 = let
     x4 = d_C_getOpt' x1 x2 x3 x3500
     x5 = d_OP_getOpt_dot___hash_selFP6_hash_os x4 x3500
     x6 = d_OP_getOpt_dot___hash_selFP7_hash_xs x4 x3500
     x7 = d_OP_getOpt_dot___hash_selFP8_hash_us x4 x3500
     x8 = d_OP_getOpt_dot___hash_selFP9_hash_es x4 x3500
      in (Curry_Prelude.OP_Tuple3 x5 x6 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_C_map d_C_errUnrec x7 x3500) x3500))

nd_C_getOpt :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getOpt x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x4 = nd_C_getOpt' x1 x2 x3 x2000 x3500
               x5 = d_OP_getOpt_dot___hash_selFP6_hash_os x4 x3500
               x6 = d_OP_getOpt_dot___hash_selFP7_hash_xs x4 x3500
               x7 = d_OP_getOpt_dot___hash_selFP8_hash_us x4 x3500
               x8 = d_OP_getOpt_dot___hash_selFP9_hash_es x4 x3500
                in (Curry_Prelude.OP_Tuple3 x5 x6 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.nd_C_map (wrapDX id d_C_errUnrec) x7 x2001 x3500) x3500)))))))

d_OP_getOpt_dot___hash_selFP6_hash_os :: Curry_Prelude.Curry t471 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t471) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List t471
d_OP_getOpt_dot___hash_selFP6_hash_os x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP6_hash_os x1002 x3500) (d_OP_getOpt_dot___hash_selFP6_hash_os x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP6_hash_os z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP6_hash_os x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt_dot___hash_selFP7_hash_xs :: Curry_Prelude.Curry t471 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t471) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP7_hash_xs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP7_hash_xs x1002 x3500) (d_OP_getOpt_dot___hash_selFP7_hash_xs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP7_hash_xs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP7_hash_xs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt_dot___hash_selFP8_hash_us :: Curry_Prelude.Curry t471 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t471) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP8_hash_us x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP8_hash_us x1002 x3500) (d_OP_getOpt_dot___hash_selFP8_hash_us x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP8_hash_us z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP8_hash_us x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt_dot___hash_selFP9_hash_es :: Curry_Prelude.Curry t471 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t471) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP9_hash_es x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP9_hash_es x1002 x3500) (d_OP_getOpt_dot___hash_selFP9_hash_es x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP9_hash_es z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP9_hash_es x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getOpt' :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getOpt' x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_C_getNext x4 x5 x2 x3500
          x7 = d_OP_getOpt'_dot___hash_selFP16_hash_opt x6 x3500
          x8 = d_OP_getOpt'_dot___hash_selFP17_hash_rest x6 x3500
          x9 = d_C_getOpt' x1 x2 x8 x3500
          x10 = d_OP_getOpt'_dot___hash_selFP12_hash_os x9 x3500
          x11 = d_OP_getOpt'_dot___hash_selFP13_hash_xs x9 x3500
          x12 = d_OP_getOpt'_dot___hash_selFP14_hash_us x9 x3500
          x13 = d_OP_getOpt'_dot___hash_selFP15_hash_es x9 x3500
           in (d_OP_getOpt'_dot_procNextOpt_dot_61 x13 x10 x8 x12 x11 x7 x1 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getOpt' x1 x2 x1002 x3500) (d_C_getOpt' x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getOpt' x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getOpt' x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_getOpt' :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getOpt' x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (let
                         x6 = nd_C_getNext x4 x5 x2 x2000 x3500
                         x7 = d_OP_getOpt'_dot___hash_selFP16_hash_opt x6 x3500
                         x8 = d_OP_getOpt'_dot___hash_selFP17_hash_rest x6 x3500
                         x9 = nd_C_getOpt' x1 x2 x8 x2001 x3500
                         x10 = d_OP_getOpt'_dot___hash_selFP12_hash_os x9 x3500
                         x11 = d_OP_getOpt'_dot___hash_selFP13_hash_xs x9 x3500
                         x12 = d_OP_getOpt'_dot___hash_selFP14_hash_us x9 x3500
                         x13 = d_OP_getOpt'_dot___hash_selFP15_hash_es x9 x3500
                          in (nd_OP_getOpt'_dot_procNextOpt_dot_61 x13 x10 x8 x12 x11 x7 x1 x2002 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getOpt' x1 x2 x1002 x3000 x3500) (nd_C_getOpt' x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getOpt' x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getOpt' x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP16_hash_opt :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple2 (C_OptKind t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> C_OptKind t456
d_OP_getOpt'_dot___hash_selFP16_hash_opt x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP16_hash_opt x1002 x3500) (d_OP_getOpt'_dot___hash_selFP16_hash_opt x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP16_hash_opt z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP16_hash_opt x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP17_hash_rest :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple2 (C_OptKind t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP17_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP17_hash_rest x1002 x3500) (d_OP_getOpt'_dot___hash_selFP17_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP17_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP17_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP12_hash_os :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List t456
d_OP_getOpt'_dot___hash_selFP12_hash_os x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP12_hash_os x1002 x3500) (d_OP_getOpt'_dot___hash_selFP12_hash_os x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP12_hash_os z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP12_hash_os x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP13_hash_xs :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP13_hash_xs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP13_hash_xs x1002 x3500) (d_OP_getOpt'_dot___hash_selFP13_hash_xs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP13_hash_xs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP13_hash_xs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP14_hash_us :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP14_hash_us x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP14_hash_us x1002 x3500) (d_OP_getOpt'_dot___hash_selFP14_hash_us x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP14_hash_us z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP14_hash_us x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot___hash_selFP15_hash_es :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP15_hash_es x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP15_hash_es x1002 x3500) (d_OP_getOpt'_dot___hash_selFP15_hash_es x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP15_hash_es z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP15_hash_es x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getOpt'_dot_procNextOpt_dot_61 :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t456 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_OptKind t456 -> C_ArgOrder t456 -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x6 x7 x3500 = case x6 of
     (C_Opt x8) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons x8 x2) x5 x4 x1
     (C_UnreqOpt x9) -> Curry_Prelude.OP_Tuple4 x2 x5 (Curry_Prelude.OP_Cons x9 x4) x1
     (C_NonOpt x10) -> d_OP__case_26 x1 x2 x3 x4 x5 x10 x7 x3500
     C_EndOfOpts -> d_OP__case_25 x3 x7 x3500
     (C_OptErr x13) -> Curry_Prelude.OP_Tuple4 x2 x5 x4 (Curry_Prelude.OP_Cons x13 x1)
     (Choice_C_OptKind x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3500) (d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1003 x7 x3500)
     (Choices_C_OptKind x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 z x7 x3500) x1002
     (Guard_C_OptKind x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7) $! (addCs x1001 x3500))
     (Fail_C_OptKind x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getOpt'_dot_procNextOpt_dot_61 :: Curry_Prelude.Curry t456 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t456 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_OptKind t456 -> C_ArgOrder t456 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t456) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x6 of
     (C_Opt x8) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons x8 x2) x5 x4 x1
     (C_UnreqOpt x9) -> Curry_Prelude.OP_Tuple4 x2 x5 (Curry_Prelude.OP_Cons x9 x4) x1
     (C_NonOpt x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x2 x3 x4 x5 x10 x7 x2000 x3500))
     C_EndOfOpts -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x3 x7 x2000 x3500))
     (C_OptErr x13) -> Curry_Prelude.OP_Tuple4 x2 x5 x4 (Curry_Prelude.OP_Cons x13 x1)
     (Choice_C_OptKind x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3000 x3500) (nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1003 x7 x3000 x3500)
     (Choices_C_OptKind x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 z x7 x3000 x3500) x1002
     (Guard_C_OptKind x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptKind x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getNext :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getNext x1 x2 x3 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = x4
           in (d_OP__case_24 x1 x2 x3 x5 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '-'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getNext x1002 x2 x3 x3500) (d_C_getNext x1003 x2 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getNext z x2 x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getNext x1002 x2 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_getNext :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getNext x1 x2 x3 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = x4
                in (nd_OP__case_24 x1 x2 x3 x5 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '-'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getNext x1002 x2 x3 x3000 x3500) (nd_C_getNext x1003 x2 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getNext z x2 x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getNext x1002 x2 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_longOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_longOpt x1 x2 x3 x3500 = let
     x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x3500) x1 x3500
     x5 = d_OP_longOpt_dot___hash_selFP19_hash_opt x4 x3500
     x6 = d_OP_longOpt_dot___hash_selFP20_hash_arg x4 x3500
     x7 = d_OP_longOpt_dot_getWith_dot_99 x5 x3 (acceptCs id Curry_Prelude.d_OP_eq_eq) x3500
     x8 = d_OP__case_20 x3 x5 x7 (Curry_Prelude.d_C_null x7 x3500) x3500
     x9 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_longOpt_dot___hash_lambda12) Curry_Prelude.OP_List x8 x3500
     x10 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x5 x3500
      in (d_OP_longOpt_dot_long_dot_99 x1 x10 x8 x9 x6 x2 x3500)

nd_C_longOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_longOpt x1 x2 x3 x3000 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2008 = leftSupply x2007
          x2009 = rightSupply x2007
           in (seq x2008 (seq x2009 (let
               x2002 = leftSupply x2008
               x2003 = rightSupply x2008
                in (seq x2002 (seq x2003 (let
                    x2004 = leftSupply x2009
                    x2010 = rightSupply x2009
                     in (seq x2004 (seq x2010 (let
                         x2005 = leftSupply x2010
                         x2006 = rightSupply x2010
                          in (seq x2005 (seq x2006 (let
                              x4 = let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '='#))) x2000 x3500) x1 x2001 x3500)))
                              x5 = d_OP_longOpt_dot___hash_selFP19_hash_opt x4 x3500
                              x6 = d_OP_longOpt_dot___hash_selFP20_hash_arg x4 x3500
                              x7 = nd_OP_longOpt_dot_getWith_dot_99 x5 x3 (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) x2003 x3500
                              x8 = nd_OP__case_20 x3 x5 x7 (Curry_Prelude.d_C_null x7 x3500) x2004 x3500
                              x9 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_longOpt_dot___hash_lambda12)) Curry_Prelude.OP_List x8 x2005 x3500
                              x10 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x5 x3500
                               in (nd_OP_longOpt_dot_long_dot_99 x1 x10 x8 x9 x6 x2 x2006 x3500)))))))))))))))

d_OP_longOpt_dot___hash_selFP19_hash_opt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_longOpt_dot___hash_selFP19_hash_opt x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_selFP19_hash_opt x1002 x3500) (d_OP_longOpt_dot___hash_selFP19_hash_opt x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_selFP19_hash_opt z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_selFP19_hash_opt x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_longOpt_dot___hash_selFP20_hash_arg :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_longOpt_dot___hash_selFP20_hash_arg x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_selFP20_hash_arg x1002 x3500) (d_OP_longOpt_dot___hash_selFP20_hash_arg x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_selFP20_hash_arg z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_selFP20_hash_arg x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_longOpt_dot_getWith_dot_99 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t173) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t173)
d_OP_longOpt_dot_getWith_dot_99 x1 x2 x3 x3500 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x3)) Curry_Prelude.OP_List x2 x3500

nd_OP_longOpt_dot_getWith_dot_99 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t173) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t173)
nd_OP_longOpt_dot_getWith_dot_99 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x3))) Curry_Prelude.OP_List x2 x2000 x3500))

d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool) -> C_OptDescr t173 -> Curry_Prelude.OP_List (C_OptDescr t173) -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t173)
d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x2 x3 x4 x3500 = let
     x5 = x3
      in (d_OP__case_19 x1 x2 x4 x5 x3500)

nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> C_OptDescr t173 -> Curry_Prelude.OP_List (C_OptDescr t173) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t173)
nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = x3
           in (nd_OP__case_19 x1 x2 x4 x5 x2000 x3500)))

d_OP_longOpt_dot___hash_lambda12 :: Curry_Prelude.Curry t173 => C_OptDescr t173 -> Curry_Prelude.OP_List (C_ArgDescr t173) -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t173)
d_OP_longOpt_dot___hash_lambda12 x1 x2 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_lambda12 x1002 x2 x3500) (d_OP_longOpt_dot___hash_lambda12 x1003 x2 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_lambda12 z x2 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_lambda12 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_longOpt_dot___hash_lambda12 :: Curry_Prelude.Curry t173 => C_OptDescr t173 -> Curry_Prelude.OP_List (C_ArgDescr t173) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t173)
nd_OP_longOpt_dot___hash_lambda12 x1 x2 x3000 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_longOpt_dot___hash_lambda12 x1002 x2 x3000 x3500) (nd_OP_longOpt_dot___hash_lambda12 x1003 x2 x3000 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_longOpt_dot___hash_lambda12 z x2 x3000 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_longOpt_dot___hash_lambda12 x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_longOpt_dot_long_dot_99 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t173) -> Curry_Prelude.OP_List (C_ArgDescr t173) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t173) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x4 x5 x6 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x1 x3500)) x6
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_17 x2 x3 x5 x6 x7 x8 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3500) (d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1003 x5 x6 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot_long_dot_99 x1 x2 x3 z x5 x6 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_longOpt_dot_long_dot_99 :: Curry_Prelude.Curry t173 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t173) -> Curry_Prelude.OP_List (C_ArgDescr t173) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t173) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x1 x3500)) x6
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x2 x3 x5 x6 x7 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3000 x3500) (nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1003 x5 x6 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 z x5 x6 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_shortOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_shortOpt x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_shortOpt_dot___hash_lambda16 x1)) Curry_Prelude.OP_List x4 x3500
     x6 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_shortOpt_dot___hash_lambda21) Curry_Prelude.OP_List x5 x3500
     x7 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
      in (d_OP_shortOpt_dot_short_dot_146 x7 x5 x6 x2 x3 x3500)

nd_C_shortOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_shortOpt x1 x2 x3 x4 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (let
                    x5 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_shortOpt_dot___hash_lambda16 x1))) Curry_Prelude.OP_List x4 x2000 x3500
                    x6 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_shortOpt_dot___hash_lambda21)) Curry_Prelude.OP_List x5 x2001 x3500
                    x7 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
                     in (nd_OP_shortOpt_dot_short_dot_146 x7 x5 x6 x2 x3 x2002 x3500)))))))))

d_OP_shortOpt_dot___hash_lambda16 :: Curry_Prelude.Curry t272 => Curry_Prelude.C_Char -> C_OptDescr t272 -> Curry_Prelude.OP_List (C_OptDescr t272) -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t272)
d_OP_shortOpt_dot___hash_lambda16 x1 x2 x3 x3500 = let
     x4 = x2
      in (d_OP__case_8 x1 x3 x4 x3500)

nd_OP_shortOpt_dot___hash_lambda16 :: Curry_Prelude.Curry t272 => Curry_Prelude.C_Char -> C_OptDescr t272 -> Curry_Prelude.OP_List (C_OptDescr t272) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t272)
nd_OP_shortOpt_dot___hash_lambda16 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = x2
           in (nd_OP__case_8 x1 x3 x4 x2000 x3500)))

d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 :: Curry_Prelude.Curry t272 => C_OptDescr t272 -> Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t272)
d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x1 x2 x3 x3500 = d_OP__case_7 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 x3 x3500) x3500

nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 :: Curry_Prelude.Curry t272 => C_OptDescr t272 -> Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t272)
nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_7 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 x3 x3500) x2000 x3500))

d_OP_shortOpt_dot___hash_lambda21 :: Curry_Prelude.Curry t272 => C_OptDescr t272 -> Curry_Prelude.OP_List (C_ArgDescr t272) -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t272)
d_OP_shortOpt_dot___hash_lambda21 x1 x2 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3500) (d_OP_shortOpt_dot___hash_lambda21 x1003 x2 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_shortOpt_dot___hash_lambda21 z x2 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_shortOpt_dot___hash_lambda21 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_shortOpt_dot___hash_lambda21 :: Curry_Prelude.Curry t272 => C_OptDescr t272 -> Curry_Prelude.OP_List (C_ArgDescr t272) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t272)
nd_OP_shortOpt_dot___hash_lambda21 x1 x2 x3000 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3000 x3500) (nd_OP_shortOpt_dot___hash_lambda21 x1003 x2 x3000 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_shortOpt_dot___hash_lambda21 z x2 x3000 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_shortOpt_dot_short_dot_146 :: Curry_Prelude.Curry t272 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t272) -> Curry_Prelude.OP_List (C_ArgDescr t272) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t272) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_shortOpt_dot_short_dot_146 x1 x2 x3 x4 x5 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_6 x1 x5 x4 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_5 x1 x2 x4 x5 x8 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3500) (d_OP_shortOpt_dot_short_dot_146 x1 x2 x1003 x4 x5 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_shortOpt_dot_short_dot_146 x1 x2 z x4 x5 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_shortOpt_dot_short_dot_146 :: Curry_Prelude.Curry t272 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t272) -> Curry_Prelude.OP_List (C_ArgDescr t272) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t272) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_shortOpt_dot_short_dot_146 x1 x2 x3 x4 x5 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x5 x4 x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x2 x4 x5 x8 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3000 x3500) (nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1003 x4 x5 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_shortOpt_dot_short_dot_146 x1 x2 z x4 x5 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_errAmbig :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_OptKind t0
d_C_errAmbig x1 x2 x3500 = let
     x3 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) x3500) x3500
      in (C_OptErr (d_C_usageInfo x3 x1 x3500))

nd_C_errAmbig :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_OptKind t0
nd_C_errAmbig x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x3 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) x3500) x3500
           in (C_OptErr (nd_C_usageInfo x3 x1 x2000 x3500))))

d_C_errReq :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_OptKind t0
d_C_errReq x1 x2 x3500 = C_OptErr (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500)

d_C_errUnrec :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_errUnrec x1 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500

d_C_errNoArg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_OptKind t0
d_C_errNoArg x1 x3500 = C_OptErr (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) x3500) x3500)

d_OP__case_5 x1 x2 x4 x5 x8 x9 x3500 = case x9 of
     Curry_Prelude.OP_List -> d_OP__case_4 x1 x4 x5 x8 x3500
     (Curry_Prelude.OP_Cons x23 x24) -> Curry_Prelude.OP_Tuple2 (d_C_errAmbig x2 x1 x3500) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x4 x5 x8 x1002 x3500) (d_OP__case_5 x1 x2 x4 x5 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 x4 x5 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x4 x5 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x4 x5 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x4 x5 x8 x2000 x3500))
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (nd_C_errAmbig x2 x1 x2000 x3500) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x4 x5 x8 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x4 x5 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 x4 x5 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x4 x5 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x4 x5 x8 x3500 = case x8 of
     (C_NoArg x10) -> d_OP__case_3 x5 x10 x4 x3500
     (C_ReqArg x13 x14) -> d_OP__case_2 x1 x5 x13 x14 x4 x3500
     (C_OptArg x19 x20) -> d_OP__case_0 x5 x19 x4 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x4 x5 x1002 x3500) (d_OP__case_4 x1 x4 x5 x1003 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x4 x5 z x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x4 x5 x8 x3000 x3500 = case x8 of
     (C_NoArg x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x5 x10 x4 x2000 x3500))
     (HO_C_ReqArg x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x5 x13 x14 x4 x2000 x3500))
     (HO_C_OptArg x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x5 x19 x4 x2000 x3500))
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_4 x1 x4 x5 x1003 x3000 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x4 x5 z x3000 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x5 x19 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x19 Curry_Prelude.C_Nothing x3500)) x5
     (Curry_Prelude.OP_Cons x21 x22) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x19 (Curry_Prelude.C_Just x4) x3500)) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x19 x1002 x3500) (d_OP__case_0 x5 x19 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 x19 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x19 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x5 x19 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x19 Curry_Prelude.C_Nothing x2000 x3500)) x5))
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x19 (Curry_Prelude.C_Just x4) x2000 x3500)) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x5 x19 x1002 x3000 x3500) (nd_OP__case_0 x5 x19 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x5 x19 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x5 x19 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x5 x13 x14 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP__case_1 x1 x13 x14 x5 x3500
     (Curry_Prelude.OP_Cons x17 x18) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x13 x4 x3500)) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x5 x13 x14 x1002 x3500) (d_OP__case_2 x1 x5 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x5 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x5 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x5 x13 x14 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x13 x14 x5 x2000 x3500))
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x13 x4 x2000 x3500)) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x5 x13 x14 x1002 x3000 x3500) (nd_OP__case_2 x1 x5 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x5 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x5 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x13 x14 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x14 x1 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x15 x16) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x13 x15 x3500)) x16
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x13 x14 x1002 x3500) (d_OP__case_1 x1 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x13 x14 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x14 x1 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x13 x15 x2000 x3500)) x16))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x13 x14 x1002 x3000 x3500) (nd_OP__case_1 x1 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x5 x10 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x10) x5
     (Curry_Prelude.OP_Cons x11 x12) -> Curry_Prelude.OP_Tuple2 (C_Opt x10) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x5 x10 x1002 x3500) (d_OP__case_3 x5 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x5 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x5 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x5 x10 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x10) x5
     (Curry_Prelude.OP_Cons x11 x12) -> Curry_Prelude.OP_Tuple2 (C_Opt x10) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x5 x10 x1002 x3000 x3500) (nd_OP__case_3 x5 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x5 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x5 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x5 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) x5
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x5 x1002 x3500) (d_OP__case_6 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x5 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) x5
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x5 x1002 x3000 x3500) (nd_OP__case_6 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x1002 x3500) (d_OP__case_7 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x3 x4 x3500 = case x4 of
     (C_Option x5 x6 x7 x8) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x4 x1) x3500) x5 x3500) x3 x3500
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x3 x1002 x3500) (d_OP__case_8 x1 x3 x1003 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x3 z x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x3 x4 x3000 x3500 = case x4 of
     (C_Option x5 x6 x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x4 x1)) x2000 x3500) x5 x2001 x3500)))) x3 x3500))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x3 x1002 x3000 x3500) (nd_OP__case_8 x1 x3 x1003 x3000 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x3 z x3000 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x2 x3 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.OP_List -> d_OP__case_16 x2 x5 x6 x7 x3500
     (Curry_Prelude.OP_Cons x22 x23) -> Curry_Prelude.OP_Tuple2 (d_C_errAmbig x3 x2 x3500) x6
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x2 x3 x5 x6 x7 x1002 x3500) (d_OP__case_17 x2 x3 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x2 x3 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x2 x3 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x2 x3 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x2 x5 x6 x7 x2000 x3500))
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (nd_C_errAmbig x3 x2 x2000 x3500) x6))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x2 x3 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_17 x2 x3 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x2 x3 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x2 x3 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x2 x5 x6 x7 x3500 = case x7 of
     (C_NoArg x9) -> d_OP__case_15 x2 x6 x9 x5 x3500
     (C_ReqArg x12 x13) -> d_OP__case_13 x2 x6 x12 x13 x5 x3500
     (C_OptArg x18 x19) -> d_OP__case_10 x6 x18 x5 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x2 x5 x6 x1002 x3500) (d_OP__case_16 x2 x5 x6 x1003 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x2 x5 x6 z x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x2 x5 x6 x7 x3000 x3500 = case x7 of
     (C_NoArg x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x2 x6 x9 x5 x2000 x3500))
     (HO_C_ReqArg x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x2 x6 x12 x13 x5 x2000 x3500))
     (HO_C_OptArg x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x6 x18 x5 x2000 x3500))
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x2 x5 x6 x1002 x3000 x3500) (nd_OP__case_16 x2 x5 x6 x1003 x3000 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x2 x5 x6 z x3000 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x6 x18 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 Curry_Prelude.C_Nothing x3500)) x6
     (Curry_Prelude.OP_Cons x20 x21) -> d_OP__case_9 x6 x18 x21 x20 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x6 x18 x1002 x3500) (d_OP__case_10 x6 x18 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x6 x18 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x6 x18 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x6 x18 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 Curry_Prelude.C_Nothing x2000 x3500)) x6))
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x6 x18 x21 x20 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x6 x18 x1002 x3000 x3500) (nd_OP__case_10 x6 x18 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x6 x18 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x6 x18 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x6 x18 x21 x20 x3500 = case x20 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 (Curry_Prelude.C_Just x21) x3500)) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 (Curry_Prelude.C_Just x21) x3500)) x6)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x6 x18 x21 x1002 x3500) (d_OP__case_9 x6 x18 x21 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x6 x18 x21 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x6 x18 x21 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x6 x18 x21 x20 x3000 x3500 = case x20 of
     (Curry_Prelude.C_Char '='#) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 (Curry_Prelude.C_Just x21) x2000 x3500)) x6))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 (Curry_Prelude.C_Just x21) x2000 x3500)) x6)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x6 x18 x21 x1002 x3000 x3500) (nd_OP__case_9 x6 x18 x21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x6 x18 x21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x6 x18 x21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x2 x6 x12 x13 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_12 x2 x12 x13 x6 x3500
     (Curry_Prelude.OP_Cons x16 x17) -> d_OP__case_11 x6 x12 x17 x16 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x2 x6 x12 x13 x1002 x3500) (d_OP__case_13 x2 x6 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x2 x6 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x2 x6 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x2 x6 x12 x13 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x2 x12 x13 x6 x2000 x3500))
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x6 x12 x17 x16 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x2 x6 x12 x13 x1002 x3000 x3500) (nd_OP__case_13 x2 x6 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x2 x6 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x2 x6 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x6 x12 x17 x16 x3500 = case x16 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x17 x3500)) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x17 x3500)) x6)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x6 x12 x17 x1002 x3500) (d_OP__case_11 x6 x12 x17 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x6 x12 x17 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x6 x12 x17 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x6 x12 x17 x16 x3000 x3500 = case x16 of
     (Curry_Prelude.C_Char '='#) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x17 x2000 x3500)) x6))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x17 x2000 x3500)) x6)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x6 x12 x17 x1002 x3000 x3500) (nd_OP__case_11 x6 x12 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x6 x12 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x6 x12 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x2 x12 x13 x6 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x13 x2 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x14 x3500)) x15
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x2 x12 x13 x1002 x3500) (d_OP__case_12 x2 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x2 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x2 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x2 x12 x13 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x13 x2 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x14 x2000 x3500)) x15))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x2 x12 x13 x1002 x3000 x3500) (nd_OP__case_12 x2 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x2 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x2 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x2 x6 x9 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x9) x6
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_14 x2 x6 x10 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x2 x6 x9 x1002 x3500) (d_OP__case_15 x2 x6 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x2 x6 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x2 x6 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x2 x6 x9 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x9) x6
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x2 x6 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x2 x6 x9 x1002 x3000 x3500) (nd_OP__case_15 x2 x6 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x2 x6 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x2 x6 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x2 x6 x10 x3500 = case x10 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3500) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3500) x6)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x2 x6 x1002 x3500) (d_OP__case_14 x2 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x2 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x2 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x2 x6 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3500) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3500) x6)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x2 x6 x1002 x3000 x3500) (nd_OP__case_14 x2 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x2 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x2 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x2 x4 x5 x3500 = case x5 of
     (C_Option x6 x7 x8 x9) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_18 x1 x2 x5 x7 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_apply (Curry_List.d_C_find (Curry_Prelude.d_C_apply x2 x1 x3500) x3500) x7 x3500) Curry_Prelude.C_Nothing x3500) x3500) x4 x3500
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x2 x4 x1002 x3500) (d_OP__case_19 x1 x2 x4 x1003 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x2 x4 z x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x2 x4 x5 x3000 x3500 = case x5 of
     (C_Option x6 x7 x8 x9) -> let
          x2006 = x3000
           in (seq x2006 (Curry_Prelude.d_OP_plus_plus (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_OP__case_18 x1 x2 x5 x7 (Curry_Prelude.d_OP_slash_eq (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_List.nd_C_find (Curry_Prelude.nd_C_apply x2 x1 x2000 x3500) x2001 x3500)))) x7 x2003 x3500)))) Curry_Prelude.C_Nothing x3500) x2005 x3500)))) x4 x3500))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_19 x1 x2 x4 x1003 x3000 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x2 x4 z x3000 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x2 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x2 x5 x7 x1002 x3500) (d_OP__case_18 x1 x2 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 x2 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x2 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x2 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x2 x5 x7 x1002 x3000 x3500) (nd_OP__case_18 x1 x2 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 x2 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x2 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x3 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP_longOpt_dot_getWith_dot_99 x5 x3 (acceptCs id Curry_List.d_C_isPrefixOf) x3500
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x3 x5 x7 x1002 x3500) (d_OP__case_20 x3 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x3 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x3 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x3 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_longOpt_dot_getWith_dot_99 x5 x3 (wrapDX (wrapDX id) (acceptCs id Curry_List.d_C_isPrefixOf)) x2000 x3500))
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x3 x5 x7 x1002 x3000 x3500) (nd_OP__case_20 x3 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x3 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x3 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x2 x3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_23 x2 x3 x5 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x2 x3 x5 x6 x1002 x3500) (d_OP__case_24 x1 x2 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x2 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x2 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x2 x3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x2 x3 x5 x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x2 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_24 x1 x2 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x2 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x2 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x2 x3 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = x7
           in (d_OP__case_22 x2 x3 x8 x9 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '-'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x2 x3 x1002 x3500) (d_OP__case_23 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x2 x3 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (let
               x9 = x7
                in (nd_OP__case_22 x2 x3 x8 x9 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '-'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x2 x3 x1002 x3000 x3500) (nd_OP__case_23 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x2 x3 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_21 x2 x3 x8 x3500
     Curry_Prelude.C_False -> d_C_shortOpt x9 x8 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x2 x3 x8 x9 x1002 x3500) (d_OP__case_22 x2 x3 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x2 x3 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x2 x3 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x2 x3 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x2 x3 x8 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_shortOpt x9 x8 x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x2 x3 x8 x9 x1002 x3000 x3500) (nd_OP__case_22 x2 x3 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x2 x3 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x2 x3 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x2 x3 x8 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 C_EndOfOpts x2
     (Curry_Prelude.OP_Cons x10 x11) -> d_C_longOpt (Curry_Prelude.OP_Cons x10 x11) x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x3 x1002 x3500) (d_OP__case_21 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x3 x8 x3000 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 C_EndOfOpts x2
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_longOpt (Curry_Prelude.OP_Cons x10 x11) x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x3 x1002 x3000 x3500) (nd_OP__case_21 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x3 x7 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (C_ReturnInOrder x12) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.d_C_map x12 x3 x3500) Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x3 x1002 x3500) (d_OP__case_25 x3 x1003 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x3 z x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x3 x7 x3000 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (HO_C_ReturnInOrder x12) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple4 (Curry_Prelude.nd_C_map x12 x3 x2000 x3500) Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List))
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x3 x1002 x3000 x3500) (nd_OP__case_25 x3 x1003 x3000 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x3 z x3000 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x1 x2 x3 x4 x5 x10 x7 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x10 x3) Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 x2 (Curry_Prelude.OP_Cons x10 x5) x4 x1
     (C_ReturnInOrder x11) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x11 x10 x3500) x2) x5 x4 x1
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x2 x3 x4 x5 x10 x1002 x3500) (d_OP__case_26 x1 x2 x3 x4 x5 x10 x1003 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x2 x3 x4 x5 x10 z x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x2 x3 x4 x5 x10 x1002) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x2 x3 x4 x5 x10 x7 x3000 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x10 x3) Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 x2 (Curry_Prelude.OP_Cons x10 x5) x4 x1
     (HO_C_ReturnInOrder x11) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x11 x10 x2000 x3500) x2) x5 x4 x1))
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x2 x3 x4 x5 x10 x1002 x3000 x3500) (nd_OP__case_26 x1 x2 x3 x4 x5 x10 x1003 x3000 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x2 x3 x4 x5 x10 z x3000 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x2 x3 x4 x5 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_OP_fmtOpt_dot_sepBy_dot_19 x1 (Curry_Prelude.OP_Cons x5 x6) x3500))) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x3 x1002 x3500) (d_OP__case_27 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_OP_fmtOpt_dot_sepBy_dot_19 x1 (Curry_Prelude.OP_Cons x5 x6) x3500))) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x3 x1002 x3000 x3500) (nd_OP__case_27 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x5 x6 x7 x10 x3500 = case x10 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 x8) (Curry_Prelude.d_C_map d_OP_fmtOpt_dot___hash_lambda3 x9 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x5 x6 x7 x1002 x3500) (d_OP__case_28 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x5 x6 x7 x10 x3000 x3500 = case x10 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 x8) (Curry_Prelude.nd_C_map (wrapDX id d_OP_fmtOpt_dot___hash_lambda3) x9 x2000 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_28 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
