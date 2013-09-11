{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_GetOpt (C_ArgOrder (..), C_OptDescr (..), C_ArgDescr (..), C_OptKind, d_C_usageInfo, nd_C_usageInfo, d_C_getOpt, nd_C_getOpt, d_C_getOpt', nd_C_getOpt') where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
data C_ArgOrder t0
     = C_RequireOrder
     | C_Permute
     | C_ReturnInOrder (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0)
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
  generate s c = Choices_C_ArgOrder c (freeID [0,0,1] s) [C_RequireOrder,C_Permute,(HO_C_ReturnInOrder (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_ArgOrder t0) where
  ($!!) cont C_RequireOrder d cs = cont C_RequireOrder d cs
  ($!!) cont C_Permute d cs = cont C_Permute d cs
  ($!!) cont (C_ReturnInOrder x1) d cs = (((\y1 d cs -> cont (C_ReturnInOrder y1) d cs) $!! x1) d) cs
  ($!!) cont (HO_C_ReturnInOrder x1) d cs = (((\y1 d cs -> cont (HO_C_ReturnInOrder y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ArgOrder cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ArgOrder cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ArgOrder cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  ($##) cont C_RequireOrder d cs = cont C_RequireOrder d cs
  ($##) cont C_Permute d cs = cont C_Permute d cs
  ($##) cont (C_ReturnInOrder x1) d cs = (((\y1 d cs -> cont (C_ReturnInOrder y1) d cs) $## x1) d) cs
  ($##) cont (HO_C_ReturnInOrder x1) d cs = (((\y1 d cs -> cont (HO_C_ReturnInOrder y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_ArgOrder cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ArgOrder cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ArgOrder cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  searchNF _ cont C_RequireOrder = cont C_RequireOrder
  searchNF _ cont C_Permute = cont C_Permute
  searchNF search cont (C_ReturnInOrder x1) = search (\y1 -> cont (C_ReturnInOrder y1)) x1
  searchNF search cont (HO_C_ReturnInOrder x1) = search (\y1 -> cont (HO_C_ReturnInOrder y1)) x1
  searchNF _ _ x = error ("GetOpt.ArgOrder.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ArgOrder t0) where
  (=.=) C_RequireOrder C_RequireOrder d cs = C_Success
  (=.=) C_Permute C_Permute d cs = C_Success
  (=.=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_RequireOrder C_RequireOrder d cs = C_Success
  (=.<=) C_Permute C_Permute d cs = C_Success
  (=.<=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_RequireOrder = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Permute = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i (C_ReturnInOrder x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (HO_C_ReturnInOrder x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_ArgOrder cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ArgOrder cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ArgOrder cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ArgOrder cd i _) = error ("GetOpt.ArgOrder.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ArgOrder cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ArgOrder cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_RequireOrder = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Permute = [(i :=: (ChooseN 1 0))]
  lazyBind cd i (C_ReturnInOrder x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (HO_C_ReturnInOrder x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_ArgOrder cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ArgOrder cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ArgOrder cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ArgOrder cd i _) = error ("GetOpt.ArgOrder.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ArgOrder cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ArgOrder cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ArgOrder t0) where
  (=?=) (Choice_C_ArgOrder cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ArgOrder cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ArgOrder cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ArgOrder cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ArgOrder cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ArgOrder cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ArgOrder cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  (=?=) C_RequireOrder C_RequireOrder d cs = Curry_Prelude.C_True
  (=?=) C_Permute C_Permute d cs = Curry_Prelude.C_True
  (=?=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ArgOrder cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ArgOrder cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ArgOrder cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ArgOrder cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ArgOrder cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ArgOrder cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ArgOrder cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ArgOrder cd info) _ _ = failCons cd info
  (<?=) C_RequireOrder C_RequireOrder d cs = Curry_Prelude.C_True
  (<?=) C_RequireOrder C_Permute _ _ = Curry_Prelude.C_True
  (<?=) C_RequireOrder (C_ReturnInOrder _) _ _ = Curry_Prelude.C_True
  (<?=) C_RequireOrder (HO_C_ReturnInOrder _) _ _ = Curry_Prelude.C_True
  (<?=) C_Permute C_Permute d cs = Curry_Prelude.C_True
  (<?=) C_Permute (C_ReturnInOrder _) _ _ = Curry_Prelude.C_True
  (<?=) C_Permute (HO_C_ReturnInOrder _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ReturnInOrder x1) (C_ReturnInOrder y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (HO_C_ReturnInOrder x1) (HO_C_ReturnInOrder y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  generate s c = Choices_C_OptDescr c (freeID [4] s) [(C_Option (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_OptDescr t0) where
  ($!!) cont (C_Option x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Option y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_OptDescr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_OptDescr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_OptDescr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_OptDescr cd info) _ _ = failCons cd info
  ($##) cont (C_Option x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Option y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_OptDescr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_OptDescr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_OptDescr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_OptDescr cd info) _ _ = failCons cd info
  searchNF search cont (C_Option x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Option y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("GetOpt.OptDescr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_OptDescr t0) where
  (=.=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Option x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_OptDescr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_OptDescr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_OptDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_OptDescr cd i _) = error ("GetOpt.OptDescr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_OptDescr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_OptDescr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Option x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_OptDescr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_OptDescr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_OptDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_OptDescr cd i _) = error ("GetOpt.OptDescr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_OptDescr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_OptDescr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_OptDescr t0) where
  (=?=) (Choice_C_OptDescr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_OptDescr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_OptDescr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_OptDescr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_OptDescr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_OptDescr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_OptDescr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_OptDescr cd info) _ _ = failCons cd info
  (=?=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_OptDescr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_OptDescr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_OptDescr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_OptDescr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_OptDescr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_OptDescr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_OptDescr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_OptDescr cd info) _ _ = failCons cd info
  (<?=) (C_Option x1 x2 x3 x4) (C_Option y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_ArgDescr t0
     = C_NoArg t0
     | C_ReqArg (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | HO_C_ReqArg (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_OptArg (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
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
  generate s c = Choices_C_ArgDescr c (freeID [1,2,2] s) [(C_NoArg (generate (leftSupply s) c)),(HO_C_ReqArg (generate (leftSupply s) c) (generate (rightSupply s) c)),(HO_C_OptArg (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_ArgDescr t0) where
  ($!!) cont (C_NoArg x1) d cs = (((\y1 d cs -> cont (C_NoArg y1) d cs) $!! x1) d) cs
  ($!!) cont (C_ReqArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ReqArg y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_ReqArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_ReqArg y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_OptArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OptArg y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_OptArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_OptArg y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ArgDescr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ArgDescr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ArgDescr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  ($##) cont (C_NoArg x1) d cs = (((\y1 d cs -> cont (C_NoArg y1) d cs) $## x1) d) cs
  ($##) cont (C_ReqArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ReqArg y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_ReqArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_ReqArg y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_OptArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_OptArg y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_OptArg x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_OptArg y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ArgDescr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ArgDescr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ArgDescr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  searchNF search cont (C_NoArg x1) = search (\y1 -> cont (C_NoArg y1)) x1
  searchNF search cont (C_ReqArg x1 x2) = search (\y1 -> search (\y2 -> cont (C_ReqArg y1 y2)) x2) x1
  searchNF search cont (HO_C_ReqArg x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_ReqArg y1 y2)) x2) x1
  searchNF search cont (C_OptArg x1 x2) = search (\y1 -> search (\y2 -> cont (C_OptArg y1 y2)) x2) x1
  searchNF search cont (HO_C_OptArg x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_OptArg y1 y2)) x2) x1
  searchNF _ _ x = error ("GetOpt.ArgDescr.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_ArgDescr t0) where
  (=.=) (C_NoArg x1) (C_NoArg y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_OptArg x1 x2) (C_OptArg y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_NoArg x1) (C_NoArg y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_OptArg x1 x2) (C_OptArg y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_NoArg x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_ReqArg x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_ReqArg x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_OptArg x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_OptArg x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_ArgDescr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ArgDescr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ArgDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ArgDescr cd i _) = error ("GetOpt.ArgDescr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ArgDescr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ArgDescr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_NoArg x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_ReqArg x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_ReqArg x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_OptArg x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_OptArg x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_ArgDescr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ArgDescr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ArgDescr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ArgDescr cd i _) = error ("GetOpt.ArgDescr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ArgDescr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ArgDescr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_ArgDescr t0) where
  (=?=) (Choice_C_ArgDescr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ArgDescr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ArgDescr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ArgDescr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ArgDescr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ArgDescr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ArgDescr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  (=?=) (C_NoArg x1) (C_NoArg y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_OptArg x1 x2) (C_OptArg y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ArgDescr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ArgDescr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ArgDescr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ArgDescr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ArgDescr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ArgDescr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ArgDescr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ArgDescr cd info) _ _ = failCons cd info
  (<?=) (C_NoArg x1) (C_NoArg y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_NoArg _) (C_ReqArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (HO_C_ReqArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_NoArg _) (HO_C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ReqArg x1 x2) (C_ReqArg y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_ReqArg _ _) (C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ReqArg _ _) (HO_C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_ReqArg x1 x2) (HO_C_ReqArg y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_ReqArg _ _) (C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_ReqArg _ _) (HO_C_OptArg _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OptArg x1 x2) (C_OptArg y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_OptArg x1 x2) (HO_C_OptArg y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


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
  generate s c = Choices_C_OptKind c (freeID [1,1,1,0,1] s) [(C_Opt (generate (leftSupply s) c)),(C_UnreqOpt (generate (leftSupply s) c)),(C_NonOpt (generate (leftSupply s) c)),C_EndOfOpts,(C_OptErr (generate (leftSupply s) c))]


instance NormalForm t0 => NormalForm (C_OptKind t0) where
  ($!!) cont (C_Opt x1) d cs = (((\y1 d cs -> cont (C_Opt y1) d cs) $!! x1) d) cs
  ($!!) cont (C_UnreqOpt x1) d cs = (((\y1 d cs -> cont (C_UnreqOpt y1) d cs) $!! x1) d) cs
  ($!!) cont (C_NonOpt x1) d cs = (((\y1 d cs -> cont (C_NonOpt y1) d cs) $!! x1) d) cs
  ($!!) cont C_EndOfOpts d cs = cont C_EndOfOpts d cs
  ($!!) cont (C_OptErr x1) d cs = (((\y1 d cs -> cont (C_OptErr y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_OptKind cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_OptKind cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_OptKind cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_OptKind cd info) _ _ = failCons cd info
  ($##) cont (C_Opt x1) d cs = (((\y1 d cs -> cont (C_Opt y1) d cs) $## x1) d) cs
  ($##) cont (C_UnreqOpt x1) d cs = (((\y1 d cs -> cont (C_UnreqOpt y1) d cs) $## x1) d) cs
  ($##) cont (C_NonOpt x1) d cs = (((\y1 d cs -> cont (C_NonOpt y1) d cs) $## x1) d) cs
  ($##) cont C_EndOfOpts d cs = cont C_EndOfOpts d cs
  ($##) cont (C_OptErr x1) d cs = (((\y1 d cs -> cont (C_OptErr y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_OptKind cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_OptKind cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_OptKind cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_OptKind cd info) _ _ = failCons cd info
  searchNF search cont (C_Opt x1) = search (\y1 -> cont (C_Opt y1)) x1
  searchNF search cont (C_UnreqOpt x1) = search (\y1 -> cont (C_UnreqOpt y1)) x1
  searchNF search cont (C_NonOpt x1) = search (\y1 -> cont (C_NonOpt y1)) x1
  searchNF _ cont C_EndOfOpts = cont C_EndOfOpts
  searchNF search cont (C_OptErr x1) = search (\y1 -> cont (C_OptErr y1)) x1
  searchNF _ _ x = error ("GetOpt.OptKind.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_OptKind t0) where
  (=.=) (C_Opt x1) (C_Opt y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_UnreqOpt x1) (C_UnreqOpt y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_NonOpt x1) (C_NonOpt y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_EndOfOpts C_EndOfOpts d cs = C_Success
  (=.=) (C_OptErr x1) (C_OptErr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Opt x1) (C_Opt y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_UnreqOpt x1) (C_UnreqOpt y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_NonOpt x1) (C_NonOpt y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_EndOfOpts C_EndOfOpts d cs = C_Success
  (=.<=) (C_OptErr x1) (C_OptErr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Opt x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_UnreqOpt x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_NonOpt x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_EndOfOpts = ((i :=: (ChooseN 3 0)):(concat []))
  bind cd i (C_OptErr x3) = ((i :=: (ChooseN 4 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_OptKind cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_OptKind cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_OptKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_OptKind cd i _) = error ("GetOpt.OptKind.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_OptKind cd info) = [(Unsolvable info)]
  bind d i (Guard_C_OptKind cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Opt x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_UnreqOpt x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_NonOpt x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_EndOfOpts = [(i :=: (ChooseN 3 0))]
  lazyBind cd i (C_OptErr x3) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_OptKind cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_OptKind cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_OptKind cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_OptKind cd i _) = error ("GetOpt.OptKind.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_OptKind cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_OptKind cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_OptKind t0) where
  (=?=) (Choice_C_OptKind cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_OptKind cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_OptKind cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_OptKind cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_OptKind cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_OptKind cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_OptKind cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_OptKind cd info) _ _ = failCons cd info
  (=?=) (C_Opt x1) (C_Opt y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_UnreqOpt x1) (C_UnreqOpt y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_NonOpt x1) (C_NonOpt y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_EndOfOpts C_EndOfOpts d cs = Curry_Prelude.C_True
  (=?=) (C_OptErr x1) (C_OptErr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_OptKind cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_OptKind cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_OptKind cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_OptKind cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_OptKind cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_OptKind cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_OptKind cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_OptKind cd info) _ _ = failCons cd info
  (<?=) (C_Opt x1) (C_Opt y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Opt _) (C_UnreqOpt _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) (C_NonOpt _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) C_EndOfOpts _ _ = Curry_Prelude.C_True
  (<?=) (C_Opt _) (C_OptErr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt x1) (C_UnreqOpt y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_UnreqOpt _) (C_NonOpt _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt _) C_EndOfOpts _ _ = Curry_Prelude.C_True
  (<?=) (C_UnreqOpt _) (C_OptErr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_NonOpt x1) (C_NonOpt y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_NonOpt _) C_EndOfOpts _ _ = Curry_Prelude.C_True
  (<?=) (C_NonOpt _) (C_OptErr _) _ _ = Curry_Prelude.C_True
  (<?=) C_EndOfOpts C_EndOfOpts d cs = Curry_Prelude.C_True
  (<?=) C_EndOfOpts (C_OptErr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_OptErr x1) (C_OptErr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_usageInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_usageInfo x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unzip3 (Curry_Prelude.d_C_concatMap d_C_fmtOpt x3250 x3500) x3250 x3500) x2 x3250 x3500
     x4 = d_OP_usageInfo_dot___hash_selFP2_hash_ss x3 x3250 x3500
     x5 = d_OP_usageInfo_dot___hash_selFP3_hash_ls x3 x3250 x3500
     x6 = d_OP_usageInfo_dot___hash_selFP4_hash_ds x3 x3250 x3500
     x7 = Curry_Prelude.d_C_zipWith3 (acceptCs (acceptCs id) d_OP_usageInfo_dot_paste_dot_2) (d_OP_usageInfo_dot_sameLen_dot_2 x4 x3250 x3500) (d_OP_usageInfo_dot_sameLen_dot_2 x5 x3250 x3500) x6 x3250 x3500
      in (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons x1 x7) x3250 x3500)

nd_C_usageInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_usageInfo x1 x2 x3000 x3250 x3500 = let
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
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unzip3) (Curry_Prelude.nd_C_concatMap (wrapNX id nd_C_fmtOpt) x2000 x3250 x3500) x2001 x3250 x3500)))) x2 x2003 x3250 x3500)))
               x4 = d_OP_usageInfo_dot___hash_selFP2_hash_ss x3 x3250 x3500
               x5 = d_OP_usageInfo_dot___hash_selFP3_hash_ls x3 x3250 x3500
               x6 = d_OP_usageInfo_dot___hash_selFP4_hash_ds x3 x3250 x3500
               x7 = Curry_Prelude.nd_C_zipWith3 (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) d_OP_usageInfo_dot_paste_dot_2)) (d_OP_usageInfo_dot_sameLen_dot_2 x4 x3250 x3500) (d_OP_usageInfo_dot_sameLen_dot_2 x5 x3250 x3500) x6 x2005 x3250 x3500
                in (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons x1 x7) x3250 x3500))))))

d_OP_usageInfo_dot___hash_selFP2_hash_ss :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP2_hash_ss x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP2_hash_ss x1002 x3250 x3500) (d_OP_usageInfo_dot___hash_selFP2_hash_ss x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP2_hash_ss z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP2_hash_ss x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_usageInfo_dot___hash_selFP3_hash_ls :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP3_hash_ls x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP3_hash_ls x1002 x3250 x3500) (d_OP_usageInfo_dot___hash_selFP3_hash_ls x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP3_hash_ls z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP3_hash_ls x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_usageInfo_dot___hash_selFP4_hash_ds :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot___hash_selFP4_hash_ds x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_usageInfo_dot___hash_selFP4_hash_ds x1002 x3250 x3500) (d_OP_usageInfo_dot___hash_selFP4_hash_ds x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_usageInfo_dot___hash_selFP4_hash_ds z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_usageInfo_dot___hash_selFP4_hash_ds x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_usageInfo_dot_paste_dot_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_usageInfo_dot_paste_dot_2 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_usageInfo_dot_flushLeft_dot_2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot_flushLeft_dot_2 x1 x2 x3250 x3500 = Curry_Prelude.d_C_map (d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 x1) x2 x3250 x3500

d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_usageInfo_dot_flushLeft_dot_2_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_C_take x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_usageInfo_dot_sameLen_dot_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_usageInfo_dot_sameLen_dot_2 x1 x3250 x3500 = d_OP_usageInfo_dot_flushLeft_dot_2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_OP_dot d_C_maximum (Curry_Prelude.d_C_map Curry_Prelude.d_C_length) x3250 x3500) x1 x3250 x3500) x1 x3250 x3500

d_C_maximum :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0
d_C_maximum x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_C_foldl1 (acceptCs id Curry_Prelude.d_C_max) x1 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maximum x1002 x3250 x3500) (d_C_maximum x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maximum z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maximum x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_fmtOpt :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_fmtOpt x1 x3250 x3500 = case x1 of
     (C_Option x2 x3 x4 x5) -> let
          x6 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.d_C_map (d_C_fmtShort x4) x2 x3250 x3500) x3250 x3500
          x7 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.d_C_map (d_C_fmtLong x4) x3 x3250 x3500) x3250 x3500
           in (d_OP__case_28 x5 x7 x6 (Curry_Prelude.d_C_lines x5 x3250 x3500) x3250 x3500)
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtOpt x1002 x3250 x3500) (d_C_fmtOpt x1003 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtOpt z x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtOpt x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_fmtOpt :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_fmtOpt x1 x3000 x3250 x3500 = case x1 of
     (C_Option x2 x3 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x6 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_fmtShort x4)) x2 x2000 x3250 x3500) x3250 x3500
                    x7 = d_OP_fmtOpt_dot_sepBy_dot_19 (Curry_Prelude.C_Char ','#) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_fmtLong x4)) x3 x2001 x3250 x3500) x3250 x3500
                     in (d_OP__case_28 x5 x7 x6 (Curry_Prelude.d_C_lines x5 x3250 x3500) x3250 x3500))))))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtOpt x1002 x3000 x3250 x3500) (nd_C_fmtOpt x1003 x3000 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtOpt z x3000 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtOpt x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_fmtOpt_dot_sepBy_dot_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_fmtOpt_dot_sepBy_dot_19 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_27 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1002 x3250 x3500) (d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_fmtOpt_dot_sepBy_dot_19 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_fmtOpt_dot_sepBy_dot_19 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_fmtOpt_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_fmtOpt_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_Prelude.OP_List x1

d_C_fmtShort :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fmtShort x1 x2 x3250 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3250 x3500
     (C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtShort x1002 x2 x3250 x3500) (d_C_fmtShort x1003 x2 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtShort z x2 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtShort x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_fmtShort :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_fmtShort x1 x2 x3000 x3250 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3250 x3500
     (HO_C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (HO_C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtShort x1002 x2 x3000 x3250 x3500) (nd_C_fmtShort x1003 x2 x3000 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtShort z x2 x3000 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtShort x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_fmtLong :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fmtLong x1 x2 x3250 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2 x3250 x3500
     (C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fmtLong x1002 x2 x3250 x3500) (d_C_fmtLong x1003 x2 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fmtLong z x2 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fmtLong x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_fmtLong :: Curry_Prelude.Curry t0 => C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_fmtLong x1 x2 x3000 x3250 x3500 = case x1 of
     (C_NoArg x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2 x3250 x3500
     (HO_C_ReqArg x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (HO_C_OptArg x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_fmtLong x1002 x2 x3000 x3250 x3500) (nd_C_fmtLong x1003 x2 x3000 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_fmtLong z x2 x3000 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_fmtLong x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getOpt :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getOpt x1 x2 x3 x3250 x3500 = let
     x4 = d_C_getOpt' x1 x2 x3 x3250 x3500
     x5 = d_OP_getOpt_dot___hash_selFP6_hash_os x4 x3250 x3500
     x6 = d_OP_getOpt_dot___hash_selFP7_hash_xs x4 x3250 x3500
     x7 = d_OP_getOpt_dot___hash_selFP8_hash_us x4 x3250 x3500
     x8 = d_OP_getOpt_dot___hash_selFP9_hash_es x4 x3250 x3500
      in (Curry_Prelude.OP_Tuple3 x5 x6 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_C_map d_C_errUnrec x7 x3250 x3500) x3250 x3500))

nd_C_getOpt :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getOpt x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x4 = nd_C_getOpt' x1 x2 x3 x2000 x3250 x3500
               x5 = d_OP_getOpt_dot___hash_selFP6_hash_os x4 x3250 x3500
               x6 = d_OP_getOpt_dot___hash_selFP7_hash_xs x4 x3250 x3500
               x7 = d_OP_getOpt_dot___hash_selFP8_hash_us x4 x3250 x3500
               x8 = d_OP_getOpt_dot___hash_selFP9_hash_es x4 x3250 x3500
                in (Curry_Prelude.OP_Tuple3 x5 x6 (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.nd_C_map (wrapDX id d_C_errUnrec) x7 x2001 x3250 x3500) x3250 x3500)))))))

d_OP_getOpt_dot___hash_selFP6_hash_os :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_getOpt_dot___hash_selFP6_hash_os x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP6_hash_os x1002 x3250 x3500) (d_OP_getOpt_dot___hash_selFP6_hash_os x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP6_hash_os z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP6_hash_os x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt_dot___hash_selFP7_hash_xs :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP7_hash_xs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP7_hash_xs x1002 x3250 x3500) (d_OP_getOpt_dot___hash_selFP7_hash_xs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP7_hash_xs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP7_hash_xs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt_dot___hash_selFP8_hash_us :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP8_hash_us x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP8_hash_us x1002 x3250 x3500) (d_OP_getOpt_dot___hash_selFP8_hash_us x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP8_hash_us z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP8_hash_us x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt_dot___hash_selFP9_hash_es :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt_dot___hash_selFP9_hash_es x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt_dot___hash_selFP9_hash_es x1002 x3250 x3500) (d_OP_getOpt_dot___hash_selFP9_hash_es x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt_dot___hash_selFP9_hash_es z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt_dot___hash_selFP9_hash_es x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getOpt' :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getOpt' x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_C_getNext x4 x5 x2 x3250 x3500
          x7 = d_OP_getOpt'_dot___hash_selFP16_hash_opt x6 x3250 x3500
          x8 = d_OP_getOpt'_dot___hash_selFP17_hash_rest x6 x3250 x3500
          x9 = d_C_getOpt' x1 x2 x8 x3250 x3500
          x10 = d_OP_getOpt'_dot___hash_selFP12_hash_os x9 x3250 x3500
          x11 = d_OP_getOpt'_dot___hash_selFP13_hash_xs x9 x3250 x3500
          x12 = d_OP_getOpt'_dot___hash_selFP14_hash_us x9 x3250 x3500
          x13 = d_OP_getOpt'_dot___hash_selFP15_hash_es x9 x3250 x3500
           in (d_OP_getOpt'_dot_procNextOpt_dot_61 x13 x10 x8 x12 x11 x7 x1 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getOpt' x1 x2 x1002 x3250 x3500) (d_C_getOpt' x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getOpt' x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getOpt' x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getOpt' :: Curry_Prelude.Curry t0 => C_ArgOrder t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getOpt' x1 x2 x3 x3000 x3250 x3500 = case x3 of
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
                         x6 = nd_C_getNext x4 x5 x2 x2000 x3250 x3500
                         x7 = d_OP_getOpt'_dot___hash_selFP16_hash_opt x6 x3250 x3500
                         x8 = d_OP_getOpt'_dot___hash_selFP17_hash_rest x6 x3250 x3500
                         x9 = nd_C_getOpt' x1 x2 x8 x2001 x3250 x3500
                         x10 = d_OP_getOpt'_dot___hash_selFP12_hash_os x9 x3250 x3500
                         x11 = d_OP_getOpt'_dot___hash_selFP13_hash_xs x9 x3250 x3500
                         x12 = d_OP_getOpt'_dot___hash_selFP14_hash_us x9 x3250 x3500
                         x13 = d_OP_getOpt'_dot___hash_selFP15_hash_es x9 x3250 x3500
                          in (nd_OP_getOpt'_dot_procNextOpt_dot_61 x13 x10 x8 x12 x11 x7 x1 x2002 x3250 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getOpt' x1 x2 x1002 x3000 x3250 x3500) (nd_C_getOpt' x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getOpt' x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getOpt' x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP16_hash_opt :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> C_OptKind t0
d_OP_getOpt'_dot___hash_selFP16_hash_opt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP16_hash_opt x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP16_hash_opt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP16_hash_opt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP16_hash_opt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP17_hash_rest :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP17_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP17_hash_rest x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP17_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP17_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP17_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP12_hash_os :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_getOpt'_dot___hash_selFP12_hash_os x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP12_hash_os x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP12_hash_os x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP12_hash_os z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP12_hash_os x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP13_hash_xs :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP13_hash_xs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP13_hash_xs x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP13_hash_xs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP13_hash_xs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP13_hash_xs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP14_hash_us :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP14_hash_us x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP14_hash_us x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP14_hash_us x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP14_hash_us z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP14_hash_us x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot___hash_selFP15_hash_es :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getOpt'_dot___hash_selFP15_hash_es x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple4 x2 x3 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot___hash_selFP15_hash_es x1002 x3250 x3500) (d_OP_getOpt'_dot___hash_selFP15_hash_es x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot___hash_selFP15_hash_es z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot___hash_selFP15_hash_es x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getOpt'_dot_procNextOpt_dot_61 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_OptKind t0 -> C_ArgOrder t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = case x6 of
     (C_Opt x8) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons x8 x2) x5 x4 x1
     (C_UnreqOpt x9) -> Curry_Prelude.OP_Tuple4 x2 x5 (Curry_Prelude.OP_Cons x9 x4) x1
     (C_NonOpt x10) -> d_OP__case_26 x1 x4 x5 x2 x10 x3 x7 x3250 x3500
     C_EndOfOpts -> d_OP__case_25 x3 x7 x3250 x3500
     (C_OptErr x13) -> Curry_Prelude.OP_Tuple4 x2 x5 x4 (Curry_Prelude.OP_Cons x13 x1)
     (Choice_C_OptKind x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3250 x3500) (d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1003 x7 x3250 x3500)
     (Choices_C_OptKind x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 z x7 x3250 x3500) x1002
     (Guard_C_OptKind x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptKind x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_getOpt'_dot_procNextOpt_dot_61 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_OptKind t0 -> C_ArgOrder t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = case x6 of
     (C_Opt x8) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons x8 x2) x5 x4 x1
     (C_UnreqOpt x9) -> Curry_Prelude.OP_Tuple4 x2 x5 (Curry_Prelude.OP_Cons x9 x4) x1
     (C_NonOpt x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x4 x5 x2 x10 x3 x7 x2000 x3250 x3500))
     C_EndOfOpts -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x3 x7 x2000 x3250 x3500))
     (C_OptErr x13) -> Curry_Prelude.OP_Tuple4 x2 x5 x4 (Curry_Prelude.OP_Cons x13 x1)
     (Choice_C_OptKind x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3000 x3250 x3500) (nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1003 x7 x3000 x3250 x3500)
     (Choices_C_OptKind x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 z x7 x3000 x3250 x3500) x1002
     (Guard_C_OptKind x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getOpt'_dot_procNextOpt_dot_61 x1 x2 x3 x4 x5 x1002 x7 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptKind x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getNext :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getNext x1 x2 x3 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = x4
           in (d_OP__case_24 x6 x2 x1 x5 x3 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getNext x1002 x2 x3 x3250 x3500) (d_C_getNext x1003 x2 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getNext z x2 x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getNext x1002 x2 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getNext :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_getNext x1 x2 x3 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = x4
                in (nd_OP__case_24 x6 x2 x1 x5 x3 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getNext x1002 x2 x3 x3000 x3250 x3500) (nd_C_getNext x1003 x2 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getNext z x2 x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getNext x1002 x2 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_longOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_longOpt x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x3250 x3500) x1 x3250 x3500
     x5 = d_OP_longOpt_dot___hash_selFP19_hash_opt x4 x3250 x3500
     x6 = d_OP_longOpt_dot___hash_selFP20_hash_arg x4 x3250 x3500
     x7 = d_OP_longOpt_dot_getWith_dot_99 x5 x3 (acceptCs id Curry_Prelude.d_OP_eq_eq) x3250 x3500
     x8 = d_OP__case_20 x7 x3 x5 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500
     x9 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_longOpt_dot___hash_lambda12) Curry_Prelude.OP_List x8 x3250 x3500
     x10 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x5 x3250 x3500
      in (d_OP_longOpt_dot_long_dot_99 x1 x10 x8 x9 x6 x2 x3250 x3500)

nd_C_longOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_longOpt x1 x2 x3 x3000 x3250 x3500 = let
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
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '='#))) x2000 x3250 x3500) x1 x2001 x3250 x3500)))
                              x5 = d_OP_longOpt_dot___hash_selFP19_hash_opt x4 x3250 x3500
                              x6 = d_OP_longOpt_dot___hash_selFP20_hash_arg x4 x3250 x3500
                              x7 = nd_OP_longOpt_dot_getWith_dot_99 x5 x3 (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) x2003 x3250 x3500
                              x8 = nd_OP__case_20 x7 x3 x5 (Curry_Prelude.d_C_null x7 x3250 x3500) x2004 x3250 x3500
                              x9 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_longOpt_dot___hash_lambda12)) Curry_Prelude.OP_List x8 x2005 x3250 x3500
                              x10 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x5 x3250 x3500
                               in (nd_OP_longOpt_dot_long_dot_99 x1 x10 x8 x9 x6 x2 x2006 x3250 x3500)))))))))))))))

d_OP_longOpt_dot___hash_selFP19_hash_opt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_longOpt_dot___hash_selFP19_hash_opt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_selFP19_hash_opt x1002 x3250 x3500) (d_OP_longOpt_dot___hash_selFP19_hash_opt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_selFP19_hash_opt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_selFP19_hash_opt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_longOpt_dot___hash_selFP20_hash_arg :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_longOpt_dot___hash_selFP20_hash_arg x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_selFP20_hash_arg x1002 x3250 x3500) (d_OP_longOpt_dot___hash_selFP20_hash_arg x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_selFP20_hash_arg z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_selFP20_hash_arg x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_longOpt_dot_getWith_dot_99 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP_longOpt_dot_getWith_dot_99 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x3)) Curry_Prelude.OP_List x2 x3250 x3500

nd_OP_longOpt_dot_getWith_dot_99 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP_longOpt_dot_getWith_dot_99 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x3))) Curry_Prelude.OP_List x2 x2000 x3250 x3500))

d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_OptDescr t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x2 x3 x4 x3250 x3500 = let
     x5 = x3
      in (d_OP__case_19 x4 x1 x2 x5 x3250 x3500)

nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> C_OptDescr t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP_longOpt_dot_getWith_dot_99_dot___hash_lambda8 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = x3
           in (nd_OP__case_19 x4 x1 x2 x5 x2000 x3250 x3500)))

d_OP_longOpt_dot___hash_lambda12 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t0)
d_OP_longOpt_dot___hash_lambda12 x1 x2 x3250 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot___hash_lambda12 x1002 x2 x3250 x3500) (d_OP_longOpt_dot___hash_lambda12 x1003 x2 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot___hash_lambda12 z x2 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot___hash_lambda12 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_longOpt_dot___hash_lambda12 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.OP_List (C_ArgDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t0)
nd_OP_longOpt_dot___hash_lambda12 x1 x2 x3000 x3250 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_longOpt_dot___hash_lambda12 x1002 x2 x3000 x3250 x3500) (nd_OP_longOpt_dot___hash_lambda12 x1003 x2 x3000 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_longOpt_dot___hash_lambda12 z x2 x3000 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_longOpt_dot___hash_lambda12 x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_longOpt_dot_long_dot_99 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x4 x5 x6 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x1 x3250 x3500)) x6
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_17 x6 x2 x3 x7 x5 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3250 x3500) (d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1003 x5 x6 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_longOpt_dot_long_dot_99 x1 x2 x3 z x5 x6 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_longOpt_dot_long_dot_99 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x1 x3250 x3500)) x6
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x6 x2 x3 x7 x5 x8 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3000 x3250 x3500) (nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1003 x5 x6 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 z x5 x6 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_longOpt_dot_long_dot_99 x1 x2 x3 x1002 x5 x6 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_shortOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_shortOpt x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_shortOpt_dot___hash_lambda16 x1)) Curry_Prelude.OP_List x4 x3250 x3500
     x6 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_shortOpt_dot___hash_lambda21) Curry_Prelude.OP_List x5 x3250 x3500
     x7 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
      in (d_OP_shortOpt_dot_short_dot_146 x7 x5 x6 x2 x3 x3250 x3500)

nd_C_shortOpt :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_shortOpt x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2000 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2000 (seq x2004 (let
               x2001 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2001 (seq x2002 (let
                    x5 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_shortOpt_dot___hash_lambda16 x1))) Curry_Prelude.OP_List x4 x2000 x3250 x3500
                    x6 = Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_shortOpt_dot___hash_lambda21)) Curry_Prelude.OP_List x5 x2001 x3250 x3500
                    x7 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
                     in (nd_OP_shortOpt_dot_short_dot_146 x7 x5 x6 x2 x3 x2002 x3250 x3500)))))))))

d_OP_shortOpt_dot___hash_lambda16 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> C_OptDescr t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP_shortOpt_dot___hash_lambda16 x1 x2 x3 x3250 x3500 = let
     x4 = x2
      in (d_OP__case_8 x3 x1 x4 x3250 x3500)

nd_OP_shortOpt_dot___hash_lambda16 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> C_OptDescr t0 -> Curry_Prelude.OP_List (C_OptDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP_shortOpt_dot___hash_lambda16 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = x2
           in (nd_OP__case_8 x3 x1 x4 x2000 x3250 x3500)))

d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x1 x2 x3 x3250 x3500 = d_OP__case_7 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq x2 x3 x3250 x3500) x3250 x3500

nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_7 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq x2 x3 x3250 x3500) x2000 x3250 x3500))

d_OP_shortOpt_dot___hash_lambda21 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t0)
d_OP_shortOpt_dot___hash_lambda21 x1 x2 x3250 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3250 x3500) (d_OP_shortOpt_dot___hash_lambda21 x1003 x2 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_shortOpt_dot___hash_lambda21 z x2 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_shortOpt_dot___hash_lambda21 :: Curry_Prelude.Curry t0 => C_OptDescr t0 -> Curry_Prelude.OP_List (C_ArgDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_ArgDescr t0)
nd_OP_shortOpt_dot___hash_lambda21 x1 x2 x3000 x3250 x3500 = case x1 of
     (C_Option x3 x4 x5 x6) -> Curry_Prelude.OP_Cons x5 x2
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3000 x3250 x3500) (nd_OP_shortOpt_dot___hash_lambda21 x1003 x2 x3000 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_shortOpt_dot___hash_lambda21 z x2 x3000 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_shortOpt_dot___hash_lambda21 x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_shortOpt_dot_short_dot_146 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_shortOpt_dot_short_dot_146 x1 x2 x3 x4 x5 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_6 x5 x1 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_5 x5 x1 x2 x8 x4 x9 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3250 x3500) (d_OP_shortOpt_dot_short_dot_146 x1 x2 x1003 x4 x5 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_shortOpt_dot_short_dot_146 x1 x2 z x4 x5 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_shortOpt_dot_short_dot_146 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_shortOpt_dot_short_dot_146 x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_6 x5 x1 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x5 x1 x2 x8 x4 x9 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3000 x3250 x3500) (nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1003 x4 x5 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_shortOpt_dot_short_dot_146 x1 x2 z x4 x5 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_shortOpt_dot_short_dot_146 x1 x2 x1002 x4 x5 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_errAmbig :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_OptKind t0
d_C_errAmbig x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500
      in (C_OptErr (d_C_usageInfo x3 x1 x3250 x3500))

nd_C_errAmbig :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> C_OptKind t0
nd_C_errAmbig x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x3 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500
           in (C_OptErr (nd_C_usageInfo x3 x1 x2000 x3250 x3500))))

d_C_errReq :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_OptKind t0
d_C_errReq x1 x2 x3250 x3500 = C_OptErr (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_C_errUnrec :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_errUnrec x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500

d_C_errNoArg :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_OptKind t0
d_C_errNoArg x1 x3250 x3500 = C_OptErr (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) x3250 x3500) x3250 x3500)

d_OP__case_5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_5 x5 x1 x2 x8 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.OP_List -> d_OP__case_4 x4 x5 x1 x8 x3250 x3500
     (Curry_Prelude.OP_Cons x23 x24) -> Curry_Prelude.OP_Tuple2 (d_C_errAmbig x2 x1 x3250 x3500) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x1 x2 x8 x4 x1002 x3250 x3500) (d_OP__case_5 x5 x1 x2 x8 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 x1 x2 x8 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x1 x2 x8 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_ArgDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_5 x5 x1 x2 x8 x4 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x4 x5 x1 x8 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (nd_C_errAmbig x2 x1 x2000 x3250 x3500) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x5 x1 x2 x8 x4 x1002 x3000 x3250 x3500) (nd_OP__case_5 x5 x1 x2 x8 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x5 x1 x2 x8 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x5 x1 x2 x8 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ArgDescr t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x4 x5 x1 x8 x3250 x3500 = case x8 of
     (C_NoArg x10) -> d_OP__case_3 x5 x10 x4 x3250 x3500
     (C_ReqArg x13 x14) -> d_OP__case_2 x5 x13 x1 x14 x4 x3250 x3500
     (C_OptArg x19 x20) -> d_OP__case_0 x5 x19 x4 x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x4 x5 x1 x1002 x3250 x3500) (d_OP__case_4 x4 x5 x1 x1003 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x4 x5 x1 z x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x4 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ArgDescr t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_4 x4 x5 x1 x8 x3000 x3250 x3500 = case x8 of
     (C_NoArg x10) -> d_OP__case_3 x5 x10 x4 x3250 x3500
     (HO_C_ReqArg x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x5 x13 x1 x14 x4 x2000 x3250 x3500))
     (HO_C_OptArg x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x5 x19 x4 x2000 x3250 x3500))
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x4 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_4 x4 x5 x1 x1003 x3000 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x4 x5 x1 z x3000 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x4 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_0 x5 x19 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x19 Curry_Prelude.C_Nothing x3250 x3500)) x5
     (Curry_Prelude.OP_Cons x21 x22) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x19 (Curry_Prelude.C_Just x4) x3250 x3500)) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x19 x1002 x3250 x3500) (d_OP__case_0 x5 x19 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 x19 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x19 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_0 x5 x19 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x19 Curry_Prelude.C_Nothing x2000 x3250 x3500)) x5))
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x19 (Curry_Prelude.C_Just x4) x2000 x3250 x3500)) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x5 x19 x1002 x3000 x3250 x3500) (nd_OP__case_0 x5 x19 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x5 x19 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x5 x19 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x5 x13 x1 x14 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP__case_1 x13 x1 x14 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x17 x18) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x13 x4 x3250 x3500)) x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x5 x13 x1 x14 x1002 x3250 x3500) (d_OP__case_2 x5 x13 x1 x14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x5 x13 x1 x14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x5 x13 x1 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_2 x5 x13 x1 x14 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x13 x1 x14 x5 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x13 x4 x2000 x3250 x3500)) x5))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x5 x13 x1 x14 x1002 x3000 x3250 x3500) (nd_OP__case_2 x5 x13 x1 x14 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x5 x13 x1 x14 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x5 x13 x1 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_1 x13 x1 x14 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x14 x1 x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x15 x16) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x13 x15 x3250 x3500)) x16
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x13 x1 x14 x1002 x3250 x3500) (d_OP__case_1 x13 x1 x14 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x13 x1 x14 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x13 x1 x14 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_1 x13 x1 x14 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x14 x1 x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x13 x15 x2000 x3250 x3500)) x16))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x13 x1 x14 x1002 x3000 x3250 x3500) (nd_OP__case_1 x13 x1 x14 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x13 x1 x14 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x13 x1 x14 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x5 x10 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x10) x5
     (Curry_Prelude.OP_Cons x11 x12) -> Curry_Prelude.OP_Tuple2 (C_Opt x10) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x5 x10 x1002 x3250 x3500) (d_OP__case_3 x5 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x5 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x5 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_6 x5 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) x5
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.OP_Tuple2 (C_UnreqOpt x1) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) x4) x5)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x5 x1 x1002 x3250 x3500) (d_OP__case_6 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> C_OptDescr t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP__case_7 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_7 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_7 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> C_OptDescr t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP__case_7 x3 x2 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_7 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.C_Char -> C_OptDescr t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP__case_8 x3 x1 x4 x3250 x3500 = case x4 of
     (C_Option x5 x6 x7 x8) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x4 x1) x3250 x3500) x5 x3250 x3500) x3 x3250 x3500
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x3 x1 x1002 x3250 x3500) (d_OP__case_8 x3 x1 x1003 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x3 x1 z x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.C_Char -> C_OptDescr t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP__case_8 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     (C_Option x5 x6 x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_shortOpt_dot___hash_lambda16_dot___hash_lambda18 x4 x1)) x2000 x3250 x3500) x5 x2001 x3250 x3500)))) x3 x3250 x3500))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_8 x3 x1 x1003 x3000 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x3 x1 z x3000 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_ArgDescr t0) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_17 x6 x2 x3 x7 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> d_OP__case_16 x5 x6 x2 x7 x3250 x3500
     (Curry_Prelude.OP_Cons x22 x23) -> Curry_Prelude.OP_Tuple2 (d_C_errAmbig x3 x2 x3250 x3500) x6
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x6 x2 x3 x7 x5 x1002 x3250 x3500) (d_OP__case_17 x6 x2 x3 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x6 x2 x3 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x6 x2 x3 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> C_ArgDescr t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_ArgDescr t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_17 x6 x2 x3 x7 x5 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x5 x6 x2 x7 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (nd_C_errAmbig x3 x2 x2000 x3250 x3500) x6))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x6 x2 x3 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_17 x6 x2 x3 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x6 x2 x3 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x6 x2 x3 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ArgDescr t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_16 x5 x6 x2 x7 x3250 x3500 = case x7 of
     (C_NoArg x9) -> d_OP__case_15 x6 x2 x9 x5 x3250 x3500
     (C_ReqArg x12 x13) -> d_OP__case_13 x6 x12 x2 x13 x5 x3250 x3500
     (C_OptArg x18 x19) -> d_OP__case_10 x6 x18 x5 x3250 x3500
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x5 x6 x2 x1002 x3250 x3500) (d_OP__case_16 x5 x6 x2 x1003 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x5 x6 x2 z x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x5 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_ArgDescr t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_16 x5 x6 x2 x7 x3000 x3250 x3500 = case x7 of
     (C_NoArg x9) -> d_OP__case_15 x6 x2 x9 x5 x3250 x3500
     (HO_C_ReqArg x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x6 x12 x2 x13 x5 x2000 x3250 x3500))
     (HO_C_OptArg x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x6 x18 x5 x2000 x3250 x3500))
     (Choice_C_ArgDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x5 x6 x2 x1002 x3000 x3250 x3500) (nd_OP__case_16 x5 x6 x2 x1003 x3000 x3250 x3500)
     (Choices_C_ArgDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x5 x6 x2 z x3000 x3250 x3500) x1002
     (Guard_C_ArgDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x5 x6 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_10 x6 x18 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 Curry_Prelude.C_Nothing x3250 x3500)) x6
     (Curry_Prelude.OP_Cons x20 x21) -> d_OP__case_9 x6 x21 x18 x20 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x6 x18 x1002 x3250 x3500) (d_OP__case_10 x6 x18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x6 x18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x6 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_10 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_10 x6 x18 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 Curry_Prelude.C_Nothing x2000 x3250 x3500)) x6))
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x6 x21 x18 x20 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x6 x18 x1002 x3000 x3250 x3500) (nd_OP__case_10 x6 x18 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x6 x18 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x6 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0) -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_9 x6 x21 x18 x20 x3250 x3500 = case x20 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 (Curry_Prelude.C_Just x21) x3250 x3500)) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x18 (Curry_Prelude.C_Just x21) x3250 x3500)) x6)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x6 x21 x18 x1002 x3250 x3500) (d_OP__case_9 x6 x21 x18 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x6 x21 x18 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x6 x21 x18 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_9 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_9 x6 x21 x18 x20 x3000 x3250 x3500 = case x20 of
     (Curry_Prelude.C_Char '='#) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 (Curry_Prelude.C_Just x21) x2000 x3250 x3500)) x6))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x18 (Curry_Prelude.C_Just x21) x2000 x3250 x3500)) x6)))] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x6 x21 x18 x1002 x3000 x3250 x3500) (nd_OP__case_9 x6 x21 x18 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x6 x21 x18 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x6 x21 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_13 x6 x12 x2 x13 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_12 x12 x2 x13 x6 x3250 x3500
     (Curry_Prelude.OP_Cons x16 x17) -> d_OP__case_11 x6 x17 x12 x16 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x6 x12 x2 x13 x1002 x3250 x3500) (d_OP__case_13 x6 x12 x2 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x6 x12 x2 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x6 x12 x2 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_13 x6 x12 x2 x13 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x12 x2 x13 x6 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x6 x17 x12 x16 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x6 x12 x2 x13 x1002 x3000 x3250 x3500) (nd_OP__case_13 x6 x12 x2 x13 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x6 x12 x2 x13 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x6 x12 x2 x13 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_11 x6 x17 x12 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x17 x3250 x3500)) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x17 x3250 x3500)) x6)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x6 x17 x12 x1002 x3250 x3500) (d_OP__case_11 x6 x17 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x6 x17 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x6 x17 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_11 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 -> Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_11 x6 x17 x12 x16 x3000 x3250 x3500 = case x16 of
     (Curry_Prelude.C_Char '='#) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x17 x2000 x3250 x3500)) x6))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x17 x2000 x3250 x3500)) x6)))] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x6 x17 x12 x1002 x3000 x3250 x3500) (nd_OP__case_11 x6 x17 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x6 x17 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x6 x17 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_12 x12 x2 x13 x6 x3250 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x13 x2 x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.d_C_apply x12 x14 x3250 x3500)) x15
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x12 x2 x13 x1002 x3250 x3500) (d_OP__case_12 x12 x2 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x12 x2 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x12 x2 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_12 x12 x2 x13 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (d_C_errReq x13 x2 x3250 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (C_Opt (Curry_Prelude.nd_C_apply x12 x14 x2000 x3250 x3500)) x15))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x12 x2 x13 x1002 x3000 x3250 x3500) (nd_OP__case_12 x12 x2 x13 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x12 x2 x13 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x12 x2 x13 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_15 x6 x2 x9 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (C_Opt x9) x6
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_14 x6 x2 x10 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x6 x2 x9 x1002 x3250 x3500) (d_OP__case_15 x6 x2 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x6 x2 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x6 x2 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_14 x6 x2 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.C_Char '='#) -> Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3250 x3500) x6
     (Curry_Prelude.CurryChar x5000) -> matchChar [('=',Curry_Prelude.OP_Tuple2 (d_C_errNoArg x2 x3250 x3500) x6)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x6 x2 x1002 x3250 x3500) (d_OP__case_14 x6 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x6 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_OptDescr t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP__case_19 x4 x1 x2 x5 x3250 x3500 = case x5 of
     (C_Option x6 x7 x8 x9) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_18 x7 x1 x2 x5 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_apply (Curry_List.d_C_find (Curry_Prelude.d_C_apply x2 x1 x3250 x3500) x3250 x3500) x7 x3250 x3500) Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500) x4 x3250 x3500
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x4 x1 x2 x1002 x3250 x3500) (d_OP__case_19 x4 x1 x2 x1003 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x4 x1 x2 z x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x4 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> C_OptDescr t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP__case_19 x4 x1 x2 x5 x3000 x3250 x3500 = case x5 of
     (C_Option x6 x7 x8 x9) -> let
          x2006 = x3000
           in (seq x2006 (Curry_Prelude.d_OP_plus_plus (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_OP__case_18 x7 x1 x2 x5 (Curry_Prelude.d_OP_slash_eq (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_List.nd_C_find (Curry_Prelude.nd_C_apply x2 x1 x2000 x3250 x3500) x2001 x3250 x3500)))) x7 x2003 x3250 x3500)))) Curry_Prelude.C_Nothing x3250 x3500) x2005 x3250 x3500)))) x4 x3250 x3500))
     (Choice_C_OptDescr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x4 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_19 x4 x1 x2 x1003 x3000 x3250 x3500)
     (Choices_C_OptDescr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x4 x1 x2 z x3000 x3250 x3500) x1002
     (Guard_C_OptDescr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x4 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_OptDescr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> C_OptDescr t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP__case_18 x7 x1 x2 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x7 x1 x2 x5 x1002 x3250 x3500) (d_OP__case_18 x7 x1 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x7 x1 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x7 x1 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) -> C_OptDescr t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP__case_18 x7 x1 x2 x5 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x7 x1 x2 x5 x1002 x3000 x3250 x3500) (nd_OP__case_18 x7 x1 x2 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x7 x1 x2 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x7 x1 x2 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
d_OP__case_20 x7 x3 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP_longOpt_dot_getWith_dot_99 x5 x3 (acceptCs id Curry_List.d_C_isPrefixOf) x3250 x3500
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x7 x3 x5 x1002 x3250 x3500) (d_OP__case_20 x7 x3 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x7 x3 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x7 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_20 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (C_OptDescr t0)
nd_OP__case_20 x7 x3 x5 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_longOpt_dot_getWith_dot_99 x5 x3 (wrapDX (wrapDX id) (acceptCs id Curry_List.d_C_isPrefixOf)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> x7
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x7 x3 x5 x1002 x3000 x3250 x3500) (nd_OP__case_20 x7 x3 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x7 x3 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x7 x3 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_24 x6 x2 x1 x5 x3 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP__case_23 x3 x2 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x6 x2 x1 x5 x3 x1002 x3250 x3500) (d_OP__case_24 x6 x2 x1 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x6 x2 x1 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x6 x2 x1 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_24 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_24 x6 x2 x1 x5 x3 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x3 x2 x5 x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (C_NonOpt x1) x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x6 x2 x1 x5 x3 x1002 x3000 x3250 x3500) (nd_OP__case_24 x6 x2 x1 x5 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x6 x2 x1 x5 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x6 x2 x1 x5 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_23 x3 x2 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = x7
           in (d_OP__case_22 x9 x3 x2 x8 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x3 x2 x1002 x3250 x3500) (d_OP__case_23 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_23 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_23 x3 x2 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (let
               x9 = x7
                in (nd_OP__case_22 x9 x3 x2 x8 (Curry_Prelude.d_OP_eq_eq x9 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x3 x2 x1002 x3000 x3250 x3500) (nd_OP__case_23 x3 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x3 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x3 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_22 x9 x3 x2 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_OP__case_21 x3 x2 x8 x3250 x3500
     Curry_Prelude.C_False -> d_C_shortOpt x9 x8 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x9 x3 x2 x8 x1002 x3250 x3500) (d_OP__case_22 x9 x3 x2 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x9 x3 x2 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x9 x3 x2 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_22 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Char -> Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_22 x9 x3 x2 x8 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x3 x2 x8 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_shortOpt x9 x8 x2 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x9 x3 x2 x8 x1002 x3000 x3250 x3500) (nd_OP__case_22 x9 x3 x2 x8 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x9 x3 x2 x8 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x9 x3 x2 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_21 x3 x2 x8 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 C_EndOfOpts x2
     (Curry_Prelude.OP_Cons x10 x11) -> d_C_longOpt (Curry_Prelude.OP_Cons x10 x11) x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x3 x2 x1002 x3250 x3500) (d_OP__case_21 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_21 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (C_OptDescr t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (C_OptKind t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_21 x3 x2 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 C_EndOfOpts x2
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_longOpt (Curry_Prelude.OP_Cons x10 x11) x2 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x3 x2 x1002 x3000 x3250 x3500) (nd_OP__case_21 x3 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x3 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x3 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_ArgOrder t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_25 x3 x7 x3250 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (C_ReturnInOrder x12) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.d_C_map x12 x3 x3250 x3500) Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x3 x1002 x3250 x3500) (d_OP__case_25 x3 x1003 x3250 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x3 z x3250 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_25 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_ArgOrder t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_25 x3 x7 x3000 x3250 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List x3 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (HO_C_ReturnInOrder x12) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple4 (Curry_Prelude.nd_C_map x12 x3 x2000 x3250 x3500) Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List))
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x3 x1002 x3000 x3250 x3500) (nd_OP__case_25 x3 x1003 x3000 x3250 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x3 z x3000 x3250 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_ArgOrder t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_26 x1 x4 x5 x2 x10 x3 x7 x3250 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x10 x3) Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 x2 (Curry_Prelude.OP_Cons x10 x5) x4 x1
     (C_ReturnInOrder x11) -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x11 x10 x3250 x3500) x2) x5 x4 x1
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x4 x5 x2 x10 x3 x1002 x3250 x3500) (d_OP__case_26 x1 x4 x5 x2 x10 x3 x1003 x3250 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x4 x5 x2 x10 x3 z x3250 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x4 x5 x2 x10 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_26 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_ArgOrder t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_26 x1 x4 x5 x2 x10 x3 x7 x3000 x3250 x3500 = case x7 of
     C_RequireOrder -> Curry_Prelude.OP_Tuple4 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x10 x3) Curry_Prelude.OP_List Curry_Prelude.OP_List
     C_Permute -> Curry_Prelude.OP_Tuple4 x2 (Curry_Prelude.OP_Cons x10 x5) x4 x1
     (HO_C_ReturnInOrder x11) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x11 x10 x2000 x3250 x3500) x2) x5 x4 x1))
     (Choice_C_ArgOrder x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x4 x5 x2 x10 x3 x1002 x3000 x3250 x3500) (nd_OP__case_26 x1 x4 x5 x2 x10 x3 x1003 x3000 x3250 x3500)
     (Choices_C_ArgOrder x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x4 x5 x2 x10 x3 z x3000 x3250 x3500) x1002
     (Guard_C_ArgOrder x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x4 x5 x2 x10 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_ArgOrder x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_27 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (d_OP_fmtOpt_dot_sepBy_dot_19 x1 (Curry_Prelude.OP_Cons x5 x6) x3250 x3500))) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x3 x1002 x3250 x3500) (d_OP__case_27 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_28 x5 x7 x6 x10 x3250 x3500 = case x10 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x6 x7 x8) (Curry_Prelude.d_C_map d_OP_fmtOpt_dot___hash_lambda3 x9 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x5 x7 x6 x1002 x3250 x3500) (d_OP__case_28 x5 x7 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x5 x7 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x5 x7 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
