{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ERD (C_ERD (..), C_Entity (..), C_Attribute (..), C_Key (..), C_Domain (..), C_Relationship (..), C_REnd (..), C_Cardinality (..), C_MaxValue (..), C_ERDName, C_EName, C_AName, C_Null, C_RName, C_Role, d_C_readERDTermFile) where

import Basics
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_Time
type C_ERDName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_EName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_AName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Null = Curry_Prelude.C_Bool

type C_RName = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Role = Curry_Prelude.OP_List Curry_Prelude.C_Char

data C_ERD
     = C_ERD (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_Entity) (Curry_Prelude.OP_List C_Relationship)
     | Choice_C_ERD Cover ID C_ERD C_ERD
     | Choices_C_ERD Cover ID ([C_ERD])
     | Fail_C_ERD Cover FailInfo
     | Guard_C_ERD Cover Constraints C_ERD

instance Show C_ERD where
  showsPrec d (Choice_C_ERD cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ERD cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ERD cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ERD cd info) = showChar '!'
  showsPrec _ (C_ERD x1 x2 x3) = (showString "(ERD") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_ERD where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_ERD x1 x2 x3,r3) | (_,r0) <- readQualified "ERD" "ERD" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_ERD where
  choiceCons = Choice_C_ERD
  choicesCons = Choices_C_ERD
  failCons = Fail_C_ERD
  guardCons = Guard_C_ERD
  try (Choice_C_ERD cd i x y) = tryChoice cd i x y
  try (Choices_C_ERD cd i xs) = tryChoices cd i xs
  try (Fail_C_ERD cd info) = Fail cd info
  try (Guard_C_ERD cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ERD cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ERD cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ERD cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ERD cd i _) = error ("ERD.ERD.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ERD cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ERD cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ERD where
  generate s c = Choices_C_ERD c (freeID [3] s) [(C_ERD (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_ERD where
  ($!!) cont (C_ERD x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ERD y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ERD cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ERD cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ERD cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ERD cd info) _ _ = failCons cd info
  ($##) cont (C_ERD x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ERD y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ERD cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ERD cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ERD cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ERD cd info) _ _ = failCons cd info
  searchNF search cont (C_ERD x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_ERD y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("ERD.ERD.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ERD where
  (=.=) (C_ERD x1 x2 x3) (C_ERD y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ERD x1 x2 x3) (C_ERD y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ERD x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_ERD cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ERD cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ERD cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ERD cd i _) = error ("ERD.ERD.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ERD cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ERD cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ERD x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_ERD cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ERD cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ERD cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ERD cd i _) = error ("ERD.ERD.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ERD cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ERD cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ERD where
  (=?=) (Choice_C_ERD cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ERD cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ERD cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ERD cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ERD cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ERD cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ERD cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ERD cd info) _ _ = failCons cd info
  (=?=) (C_ERD x1 x2 x3) (C_ERD y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_ERD cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ERD cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ERD cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ERD cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ERD cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ERD cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ERD cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ERD cd info) _ _ = failCons cd info
  (<?=) (C_ERD x1 x2 x3) (C_ERD y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_Entity
     = C_Entity (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_Attribute)
     | Choice_C_Entity Cover ID C_Entity C_Entity
     | Choices_C_Entity Cover ID ([C_Entity])
     | Fail_C_Entity Cover FailInfo
     | Guard_C_Entity Cover Constraints C_Entity

instance Show C_Entity where
  showsPrec d (Choice_C_Entity cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Entity cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Entity cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Entity cd info) = showChar '!'
  showsPrec _ (C_Entity x1 x2) = (showString "(Entity") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Entity where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Entity x1 x2,r2) | (_,r0) <- readQualified "ERD" "Entity" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_Entity where
  choiceCons = Choice_C_Entity
  choicesCons = Choices_C_Entity
  failCons = Fail_C_Entity
  guardCons = Guard_C_Entity
  try (Choice_C_Entity cd i x y) = tryChoice cd i x y
  try (Choices_C_Entity cd i xs) = tryChoices cd i xs
  try (Fail_C_Entity cd info) = Fail cd info
  try (Guard_C_Entity cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Entity cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Entity cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Entity cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Entity cd i _) = error ("ERD.Entity.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Entity cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Entity cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Entity where
  generate s c = Choices_C_Entity c (freeID [2] s) [(C_Entity (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Entity where
  ($!!) cont (C_Entity x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Entity y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Entity cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Entity cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Entity cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Entity cd info) _ _ = failCons cd info
  ($##) cont (C_Entity x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Entity y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Entity cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Entity cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Entity cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Entity cd info) _ _ = failCons cd info
  searchNF search cont (C_Entity x1 x2) = search (\y1 -> search (\y2 -> cont (C_Entity y1 y2)) x2) x1
  searchNF _ _ x = error ("ERD.Entity.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Entity where
  (=.=) (C_Entity x1 x2) (C_Entity y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Entity x1 x2) (C_Entity y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Entity x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Entity cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Entity cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Entity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Entity cd i _) = error ("ERD.Entity.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Entity cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Entity cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Entity x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Entity cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Entity cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Entity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Entity cd i _) = error ("ERD.Entity.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Entity cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Entity cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Entity where
  (=?=) (Choice_C_Entity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Entity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Entity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Entity cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Entity cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Entity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Entity cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Entity cd info) _ _ = failCons cd info
  (=?=) (C_Entity x1 x2) (C_Entity y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_Entity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Entity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Entity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Entity cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Entity cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Entity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Entity cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Entity cd info) _ _ = failCons cd info
  (<?=) (C_Entity x1 x2) (C_Entity y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_Attribute
     = C_Attribute (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Domain C_Key Curry_Prelude.C_Bool
     | Choice_C_Attribute Cover ID C_Attribute C_Attribute
     | Choices_C_Attribute Cover ID ([C_Attribute])
     | Fail_C_Attribute Cover FailInfo
     | Guard_C_Attribute Cover Constraints C_Attribute

instance Show C_Attribute where
  showsPrec d (Choice_C_Attribute cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Attribute cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Attribute cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Attribute cd info) = showChar '!'
  showsPrec _ (C_Attribute x1 x2 x3 x4) = (showString "(Attribute") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_Attribute where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Attribute x1 x2 x3 x4,r4) | (_,r0) <- readQualified "ERD" "Attribute" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet C_Attribute where
  choiceCons = Choice_C_Attribute
  choicesCons = Choices_C_Attribute
  failCons = Fail_C_Attribute
  guardCons = Guard_C_Attribute
  try (Choice_C_Attribute cd i x y) = tryChoice cd i x y
  try (Choices_C_Attribute cd i xs) = tryChoices cd i xs
  try (Fail_C_Attribute cd info) = Fail cd info
  try (Guard_C_Attribute cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Attribute cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Attribute cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Attribute cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Attribute cd i _) = error ("ERD.Attribute.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Attribute cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Attribute cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Attribute where
  generate s c = Choices_C_Attribute c (freeID [4] s) [(C_Attribute (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_Attribute where
  ($!!) cont (C_Attribute x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Attribute y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Attribute cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Attribute cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Attribute cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Attribute cd info) _ _ = failCons cd info
  ($##) cont (C_Attribute x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Attribute y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Attribute cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Attribute cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Attribute cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Attribute cd info) _ _ = failCons cd info
  searchNF search cont (C_Attribute x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Attribute y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("ERD.Attribute.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Attribute where
  (=.=) (C_Attribute x1 x2 x3 x4) (C_Attribute y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Attribute x1 x2 x3 x4) (C_Attribute y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Attribute x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_Attribute cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Attribute cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Attribute cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Attribute cd i _) = error ("ERD.Attribute.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Attribute cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Attribute cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Attribute x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_Attribute cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Attribute cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Attribute cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Attribute cd i _) = error ("ERD.Attribute.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Attribute cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Attribute cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Attribute where
  (=?=) (Choice_C_Attribute cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Attribute cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Attribute cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Attribute cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Attribute cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Attribute cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Attribute cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Attribute cd info) _ _ = failCons cd info
  (=?=) (C_Attribute x1 x2 x3 x4) (C_Attribute y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_Attribute cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Attribute cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Attribute cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Attribute cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Attribute cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Attribute cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Attribute cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Attribute cd info) _ _ = failCons cd info
  (<?=) (C_Attribute x1 x2 x3 x4) (C_Attribute y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_Key
     = C_NoKey
     | C_PKey
     | C_Unique
     | Choice_C_Key Cover ID C_Key C_Key
     | Choices_C_Key Cover ID ([C_Key])
     | Fail_C_Key Cover FailInfo
     | Guard_C_Key Cover Constraints C_Key

instance Show C_Key where
  showsPrec d (Choice_C_Key cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Key cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Key cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Key cd info) = showChar '!'
  showsPrec _ C_NoKey = showString "NoKey"
  showsPrec _ C_PKey = showString "PKey"
  showsPrec _ C_Unique = showString "Unique"


instance Read C_Key where
  readsPrec _ s = (readParen False (\r -> [ (C_NoKey,r0) | (_,r0) <- readQualified "ERD" "NoKey" r]) s) ++ ((readParen False (\r -> [ (C_PKey,r0) | (_,r0) <- readQualified "ERD" "PKey" r]) s) ++ (readParen False (\r -> [ (C_Unique,r0) | (_,r0) <- readQualified "ERD" "Unique" r]) s))


instance NonDet C_Key where
  choiceCons = Choice_C_Key
  choicesCons = Choices_C_Key
  failCons = Fail_C_Key
  guardCons = Guard_C_Key
  try (Choice_C_Key cd i x y) = tryChoice cd i x y
  try (Choices_C_Key cd i xs) = tryChoices cd i xs
  try (Fail_C_Key cd info) = Fail cd info
  try (Guard_C_Key cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Key cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Key cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Key cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Key cd i _) = error ("ERD.Key.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Key cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Key cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Key where
  generate s c = Choices_C_Key c (freeID [0,0,0] s) [C_NoKey,C_PKey,C_Unique]


instance NormalForm C_Key where
  ($!!) cont C_NoKey d cs = cont C_NoKey d cs
  ($!!) cont C_PKey d cs = cont C_PKey d cs
  ($!!) cont C_Unique d cs = cont C_Unique d cs
  ($!!) cont (Choice_C_Key cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Key cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Key cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Key cd info) _ _ = failCons cd info
  ($##) cont C_NoKey d cs = cont C_NoKey d cs
  ($##) cont C_PKey d cs = cont C_PKey d cs
  ($##) cont C_Unique d cs = cont C_Unique d cs
  ($##) cont (Choice_C_Key cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Key cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Key cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Key cd info) _ _ = failCons cd info
  searchNF _ cont C_NoKey = cont C_NoKey
  searchNF _ cont C_PKey = cont C_PKey
  searchNF _ cont C_Unique = cont C_Unique
  searchNF _ _ x = error ("ERD.Key.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Key where
  (=.=) C_NoKey C_NoKey d cs = C_Success
  (=.=) C_PKey C_PKey d cs = C_Success
  (=.=) C_Unique C_Unique d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_NoKey C_NoKey d cs = C_Success
  (=.<=) C_PKey C_PKey d cs = C_Success
  (=.<=) C_Unique C_Unique d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_NoKey = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_PKey = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_Unique = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_Key cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Key cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Key cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Key cd i _) = error ("ERD.Key.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Key cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Key cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_NoKey = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_PKey = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_Unique = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Key cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Key cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Key cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Key cd i _) = error ("ERD.Key.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Key cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Key cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Key where
  (=?=) (Choice_C_Key cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Key cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Key cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Key cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Key cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Key cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Key cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Key cd info) _ _ = failCons cd info
  (=?=) C_NoKey C_NoKey d cs = Curry_Prelude.C_True
  (=?=) C_PKey C_PKey d cs = Curry_Prelude.C_True
  (=?=) C_Unique C_Unique d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Key cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Key cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Key cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Key cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Key cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Key cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Key cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Key cd info) _ _ = failCons cd info
  (<?=) C_NoKey C_NoKey d cs = Curry_Prelude.C_True
  (<?=) C_NoKey C_PKey _ _ = Curry_Prelude.C_True
  (<?=) C_NoKey C_Unique _ _ = Curry_Prelude.C_True
  (<?=) C_PKey C_PKey d cs = Curry_Prelude.C_True
  (<?=) C_PKey C_Unique _ _ = Curry_Prelude.C_True
  (<?=) C_Unique C_Unique d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Domain
     = C_IntDom (Curry_Prelude.C_Maybe Curry_Prelude.C_Int)
     | C_FloatDom (Curry_Prelude.C_Maybe Curry_Prelude.C_Float)
     | C_CharDom (Curry_Prelude.C_Maybe Curry_Prelude.C_Char)
     | C_StringDom (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_BoolDom (Curry_Prelude.C_Maybe Curry_Prelude.C_Bool)
     | C_DateDom (Curry_Prelude.C_Maybe Curry_Time.C_CalendarTime)
     | C_UserDefined (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_KeyDom (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Domain Cover ID C_Domain C_Domain
     | Choices_C_Domain Cover ID ([C_Domain])
     | Fail_C_Domain Cover FailInfo
     | Guard_C_Domain Cover Constraints C_Domain

instance Show C_Domain where
  showsPrec d (Choice_C_Domain cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Domain cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Domain cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Domain cd info) = showChar '!'
  showsPrec _ (C_IntDom x1) = (showString "(IntDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FloatDom x1) = (showString "(FloatDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CharDom x1) = (showString "(CharDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_StringDom x1) = (showString "(StringDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_BoolDom x1) = (showString "(BoolDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_DateDom x1) = (showString "(DateDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_UserDefined x1 x2) = (showString "(UserDefined") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_KeyDom x1) = (showString "(KeyDom") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Domain where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_IntDom x1,r1) | (_,r0) <- readQualified "ERD" "IntDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FloatDom x1,r1) | (_,r0) <- readQualified "ERD" "FloatDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CharDom x1,r1) | (_,r0) <- readQualified "ERD" "CharDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_StringDom x1,r1) | (_,r0) <- readQualified "ERD" "StringDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_BoolDom x1,r1) | (_,r0) <- readQualified "ERD" "BoolDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_DateDom x1,r1) | (_,r0) <- readQualified "ERD" "DateDom" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_UserDefined x1 x2,r2) | (_,r0) <- readQualified "ERD" "UserDefined" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_KeyDom x1,r1) | (_,r0) <- readQualified "ERD" "KeyDom" r, (x1,r1) <- readsPrec 11 r0]) s)))))))


instance NonDet C_Domain where
  choiceCons = Choice_C_Domain
  choicesCons = Choices_C_Domain
  failCons = Fail_C_Domain
  guardCons = Guard_C_Domain
  try (Choice_C_Domain cd i x y) = tryChoice cd i x y
  try (Choices_C_Domain cd i xs) = tryChoices cd i xs
  try (Fail_C_Domain cd info) = Fail cd info
  try (Guard_C_Domain cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Domain cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Domain cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Domain cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Domain cd i _) = error ("ERD.Domain.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Domain cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Domain cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Domain where
  generate s c = Choices_C_Domain c (freeID [1,1,1,1,1,1,2,1] s) [(C_IntDom (generate (leftSupply s) c)),(C_FloatDom (generate (leftSupply s) c)),(C_CharDom (generate (leftSupply s) c)),(C_StringDom (generate (leftSupply s) c)),(C_BoolDom (generate (leftSupply s) c)),(C_DateDom (generate (leftSupply s) c)),(C_UserDefined (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_KeyDom (generate (leftSupply s) c))]


instance NormalForm C_Domain where
  ($!!) cont (C_IntDom x1) d cs = (((\y1 d cs -> cont (C_IntDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_FloatDom x1) d cs = (((\y1 d cs -> cont (C_FloatDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CharDom x1) d cs = (((\y1 d cs -> cont (C_CharDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_StringDom x1) d cs = (((\y1 d cs -> cont (C_StringDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_BoolDom x1) d cs = (((\y1 d cs -> cont (C_BoolDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_DateDom x1) d cs = (((\y1 d cs -> cont (C_DateDom y1) d cs) $!! x1) d) cs
  ($!!) cont (C_UserDefined x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_UserDefined y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_KeyDom x1) d cs = (((\y1 d cs -> cont (C_KeyDom y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Domain cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Domain cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Domain cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Domain cd info) _ _ = failCons cd info
  ($##) cont (C_IntDom x1) d cs = (((\y1 d cs -> cont (C_IntDom y1) d cs) $## x1) d) cs
  ($##) cont (C_FloatDom x1) d cs = (((\y1 d cs -> cont (C_FloatDom y1) d cs) $## x1) d) cs
  ($##) cont (C_CharDom x1) d cs = (((\y1 d cs -> cont (C_CharDom y1) d cs) $## x1) d) cs
  ($##) cont (C_StringDom x1) d cs = (((\y1 d cs -> cont (C_StringDom y1) d cs) $## x1) d) cs
  ($##) cont (C_BoolDom x1) d cs = (((\y1 d cs -> cont (C_BoolDom y1) d cs) $## x1) d) cs
  ($##) cont (C_DateDom x1) d cs = (((\y1 d cs -> cont (C_DateDom y1) d cs) $## x1) d) cs
  ($##) cont (C_UserDefined x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_UserDefined y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_KeyDom x1) d cs = (((\y1 d cs -> cont (C_KeyDom y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Domain cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Domain cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Domain cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Domain cd info) _ _ = failCons cd info
  searchNF search cont (C_IntDom x1) = search (\y1 -> cont (C_IntDom y1)) x1
  searchNF search cont (C_FloatDom x1) = search (\y1 -> cont (C_FloatDom y1)) x1
  searchNF search cont (C_CharDom x1) = search (\y1 -> cont (C_CharDom y1)) x1
  searchNF search cont (C_StringDom x1) = search (\y1 -> cont (C_StringDom y1)) x1
  searchNF search cont (C_BoolDom x1) = search (\y1 -> cont (C_BoolDom y1)) x1
  searchNF search cont (C_DateDom x1) = search (\y1 -> cont (C_DateDom y1)) x1
  searchNF search cont (C_UserDefined x1 x2) = search (\y1 -> search (\y2 -> cont (C_UserDefined y1 y2)) x2) x1
  searchNF search cont (C_KeyDom x1) = search (\y1 -> cont (C_KeyDom y1)) x1
  searchNF _ _ x = error ("ERD.Domain.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Domain where
  (=.=) (C_IntDom x1) (C_IntDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_FloatDom x1) (C_FloatDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CharDom x1) (C_CharDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_StringDom x1) (C_StringDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_BoolDom x1) (C_BoolDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_DateDom x1) (C_DateDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_UserDefined x1 x2) (C_UserDefined y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_KeyDom x1) (C_KeyDom y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_IntDom x1) (C_IntDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_FloatDom x1) (C_FloatDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CharDom x1) (C_CharDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_StringDom x1) (C_StringDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_BoolDom x1) (C_BoolDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_DateDom x1) (C_DateDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_UserDefined x1 x2) (C_UserDefined y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_KeyDom x1) (C_KeyDom y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_IntDom x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_FloatDom x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CharDom x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_StringDom x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_BoolDom x3) = ((i :=: (ChooseN 4 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_DateDom x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_UserDefined x3 x4) = ((i :=: (ChooseN 6 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_KeyDom x3) = ((i :=: (ChooseN 7 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Domain cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Domain cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Domain cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Domain cd i _) = error ("ERD.Domain.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Domain cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Domain cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_IntDom x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FloatDom x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CharDom x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_StringDom x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_BoolDom x3) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_DateDom x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_UserDefined x3 x4) = [(i :=: (ChooseN 6 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_KeyDom x3) = [(i :=: (ChooseN 7 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Domain cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Domain cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Domain cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Domain cd i _) = error ("ERD.Domain.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Domain cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Domain cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Domain where
  (=?=) (Choice_C_Domain cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Domain cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Domain cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Domain cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Domain cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Domain cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Domain cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Domain cd info) _ _ = failCons cd info
  (=?=) (C_IntDom x1) (C_IntDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_FloatDom x1) (C_FloatDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CharDom x1) (C_CharDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_StringDom x1) (C_StringDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_BoolDom x1) (C_BoolDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_DateDom x1) (C_DateDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_UserDefined x1 x2) (C_UserDefined y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_KeyDom x1) (C_KeyDom y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Domain cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Domain cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Domain cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Domain cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Domain cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Domain cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Domain cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Domain cd info) _ _ = failCons cd info
  (<?=) (C_IntDom x1) (C_IntDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_IntDom _) (C_FloatDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_CharDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_StringDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_BoolDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_DateDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_IntDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom x1) (C_FloatDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_FloatDom _) (C_CharDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom _) (C_StringDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom _) (C_BoolDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom _) (C_DateDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FloatDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CharDom x1) (C_CharDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CharDom _) (C_StringDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CharDom _) (C_BoolDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CharDom _) (C_DateDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CharDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CharDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_StringDom x1) (C_StringDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_StringDom _) (C_BoolDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_StringDom _) (C_DateDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_StringDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_StringDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_BoolDom x1) (C_BoolDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_BoolDom _) (C_DateDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_BoolDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_BoolDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DateDom x1) (C_DateDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_DateDom _) (C_UserDefined _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DateDom _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_UserDefined x1 x2) (C_UserDefined y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_UserDefined _ _) (C_KeyDom _) _ _ = Curry_Prelude.C_True
  (<?=) (C_KeyDom x1) (C_KeyDom y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Relationship
     = C_Relationship (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List C_REnd)
     | Choice_C_Relationship Cover ID C_Relationship C_Relationship
     | Choices_C_Relationship Cover ID ([C_Relationship])
     | Fail_C_Relationship Cover FailInfo
     | Guard_C_Relationship Cover Constraints C_Relationship

instance Show C_Relationship where
  showsPrec d (Choice_C_Relationship cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Relationship cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Relationship cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Relationship cd info) = showChar '!'
  showsPrec _ (C_Relationship x1 x2) = (showString "(Relationship") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Relationship where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Relationship x1 x2,r2) | (_,r0) <- readQualified "ERD" "Relationship" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_Relationship where
  choiceCons = Choice_C_Relationship
  choicesCons = Choices_C_Relationship
  failCons = Fail_C_Relationship
  guardCons = Guard_C_Relationship
  try (Choice_C_Relationship cd i x y) = tryChoice cd i x y
  try (Choices_C_Relationship cd i xs) = tryChoices cd i xs
  try (Fail_C_Relationship cd info) = Fail cd info
  try (Guard_C_Relationship cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Relationship cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Relationship cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Relationship cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Relationship cd i _) = error ("ERD.Relationship.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Relationship cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Relationship cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Relationship where
  generate s c = Choices_C_Relationship c (freeID [2] s) [(C_Relationship (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Relationship where
  ($!!) cont (C_Relationship x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Relationship y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Relationship cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Relationship cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Relationship cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Relationship cd info) _ _ = failCons cd info
  ($##) cont (C_Relationship x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Relationship y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Relationship cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Relationship cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Relationship cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Relationship cd info) _ _ = failCons cd info
  searchNF search cont (C_Relationship x1 x2) = search (\y1 -> search (\y2 -> cont (C_Relationship y1 y2)) x2) x1
  searchNF _ _ x = error ("ERD.Relationship.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Relationship where
  (=.=) (C_Relationship x1 x2) (C_Relationship y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Relationship x1 x2) (C_Relationship y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Relationship x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Relationship cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Relationship cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Relationship cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Relationship cd i _) = error ("ERD.Relationship.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Relationship cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Relationship cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Relationship x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Relationship cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Relationship cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Relationship cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Relationship cd i _) = error ("ERD.Relationship.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Relationship cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Relationship cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Relationship where
  (=?=) (Choice_C_Relationship cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Relationship cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Relationship cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Relationship cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Relationship cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Relationship cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Relationship cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Relationship cd info) _ _ = failCons cd info
  (=?=) (C_Relationship x1 x2) (C_Relationship y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_Relationship cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Relationship cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Relationship cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Relationship cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Relationship cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Relationship cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Relationship cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Relationship cd info) _ _ = failCons cd info
  (<?=) (C_Relationship x1 x2) (C_Relationship y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_REnd
     = C_REnd (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_Cardinality
     | Choice_C_REnd Cover ID C_REnd C_REnd
     | Choices_C_REnd Cover ID ([C_REnd])
     | Fail_C_REnd Cover FailInfo
     | Guard_C_REnd Cover Constraints C_REnd

instance Show C_REnd where
  showsPrec d (Choice_C_REnd cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_REnd cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_REnd cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_REnd cd info) = showChar '!'
  showsPrec _ (C_REnd x1 x2 x3) = (showString "(REnd") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_REnd where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_REnd x1 x2 x3,r3) | (_,r0) <- readQualified "ERD" "REnd" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_REnd where
  choiceCons = Choice_C_REnd
  choicesCons = Choices_C_REnd
  failCons = Fail_C_REnd
  guardCons = Guard_C_REnd
  try (Choice_C_REnd cd i x y) = tryChoice cd i x y
  try (Choices_C_REnd cd i xs) = tryChoices cd i xs
  try (Fail_C_REnd cd info) = Fail cd info
  try (Guard_C_REnd cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_REnd cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_REnd cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_REnd cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_REnd cd i _) = error ("ERD.REnd.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_REnd cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_REnd cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_REnd where
  generate s c = Choices_C_REnd c (freeID [3] s) [(C_REnd (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_REnd where
  ($!!) cont (C_REnd x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_REnd y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_REnd cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_REnd cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_REnd cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_REnd cd info) _ _ = failCons cd info
  ($##) cont (C_REnd x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_REnd y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_REnd cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_REnd cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_REnd cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_REnd cd info) _ _ = failCons cd info
  searchNF search cont (C_REnd x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_REnd y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("ERD.REnd.searchNF: no constructor: " ++ (show x))


instance Unifiable C_REnd where
  (=.=) (C_REnd x1 x2 x3) (C_REnd y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_REnd x1 x2 x3) (C_REnd y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_REnd x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_REnd cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_REnd cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_REnd cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_REnd cd i _) = error ("ERD.REnd.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_REnd cd info) = [(Unsolvable info)]
  bind d i (Guard_C_REnd cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_REnd x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_REnd cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_REnd cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_REnd cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_REnd cd i _) = error ("ERD.REnd.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_REnd cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_REnd cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_REnd where
  (=?=) (Choice_C_REnd cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_REnd cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_REnd cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_REnd cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_REnd cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_REnd cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_REnd cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_REnd cd info) _ _ = failCons cd info
  (=?=) (C_REnd x1 x2 x3) (C_REnd y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_REnd cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_REnd cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_REnd cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_REnd cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_REnd cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_REnd cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_REnd cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_REnd cd info) _ _ = failCons cd info
  (<?=) (C_REnd x1 x2 x3) (C_REnd y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_Cardinality
     = C_Exactly Curry_Prelude.C_Int
     | C_Between Curry_Prelude.C_Int C_MaxValue
     | C_Range Curry_Prelude.C_Int (Curry_Prelude.C_Maybe Curry_Prelude.C_Int)
     | Choice_C_Cardinality Cover ID C_Cardinality C_Cardinality
     | Choices_C_Cardinality Cover ID ([C_Cardinality])
     | Fail_C_Cardinality Cover FailInfo
     | Guard_C_Cardinality Cover Constraints C_Cardinality

instance Show C_Cardinality where
  showsPrec d (Choice_C_Cardinality cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Cardinality cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Cardinality cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Cardinality cd info) = showChar '!'
  showsPrec _ (C_Exactly x1) = (showString "(Exactly") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Between x1 x2) = (showString "(Between") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Range x1 x2) = (showString "(Range") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Cardinality where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Exactly x1,r1) | (_,r0) <- readQualified "ERD" "Exactly" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Between x1 x2,r2) | (_,r0) <- readQualified "ERD" "Between" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_Range x1 x2,r2) | (_,r0) <- readQualified "ERD" "Range" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


instance NonDet C_Cardinality where
  choiceCons = Choice_C_Cardinality
  choicesCons = Choices_C_Cardinality
  failCons = Fail_C_Cardinality
  guardCons = Guard_C_Cardinality
  try (Choice_C_Cardinality cd i x y) = tryChoice cd i x y
  try (Choices_C_Cardinality cd i xs) = tryChoices cd i xs
  try (Fail_C_Cardinality cd info) = Fail cd info
  try (Guard_C_Cardinality cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Cardinality cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Cardinality cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Cardinality cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Cardinality cd i _) = error ("ERD.Cardinality.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Cardinality cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Cardinality cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Cardinality where
  generate s c = Choices_C_Cardinality c (freeID [1,2,2] s) [(C_Exactly (generate (leftSupply s) c)),(C_Between (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Range (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Cardinality where
  ($!!) cont (C_Exactly x1) d cs = (((\y1 d cs -> cont (C_Exactly y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Between x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Between y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Range x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Range y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Cardinality cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Cardinality cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Cardinality cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Cardinality cd info) _ _ = failCons cd info
  ($##) cont (C_Exactly x1) d cs = (((\y1 d cs -> cont (C_Exactly y1) d cs) $## x1) d) cs
  ($##) cont (C_Between x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Between y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Range x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Range y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Cardinality cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Cardinality cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Cardinality cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Cardinality cd info) _ _ = failCons cd info
  searchNF search cont (C_Exactly x1) = search (\y1 -> cont (C_Exactly y1)) x1
  searchNF search cont (C_Between x1 x2) = search (\y1 -> search (\y2 -> cont (C_Between y1 y2)) x2) x1
  searchNF search cont (C_Range x1 x2) = search (\y1 -> search (\y2 -> cont (C_Range y1 y2)) x2) x1
  searchNF _ _ x = error ("ERD.Cardinality.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Cardinality where
  (=.=) (C_Exactly x1) (C_Exactly y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Between x1 x2) (C_Between y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Range x1 x2) (C_Range y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Exactly x1) (C_Exactly y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Between x1 x2) (C_Between y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Range x1 x2) (C_Range y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Exactly x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Between x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Range x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Cardinality cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Cardinality cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Cardinality cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Cardinality cd i _) = error ("ERD.Cardinality.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Cardinality cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Cardinality cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Exactly x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Between x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Range x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Cardinality cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Cardinality cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Cardinality cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Cardinality cd i _) = error ("ERD.Cardinality.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Cardinality cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Cardinality cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Cardinality where
  (=?=) (Choice_C_Cardinality cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Cardinality cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Cardinality cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Cardinality cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Cardinality cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Cardinality cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Cardinality cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Cardinality cd info) _ _ = failCons cd info
  (=?=) (C_Exactly x1) (C_Exactly y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Between x1 x2) (C_Between y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Range x1 x2) (C_Range y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Cardinality cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Cardinality cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Cardinality cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Cardinality cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Cardinality cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Cardinality cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Cardinality cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Cardinality cd info) _ _ = failCons cd info
  (<?=) (C_Exactly x1) (C_Exactly y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Exactly _) (C_Between _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Exactly _) (C_Range _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Between x1 x2) (C_Between y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Between _ _) (C_Range _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Range x1 x2) (C_Range y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_MaxValue
     = C_Max Curry_Prelude.C_Int
     | C_Infinite
     | Choice_C_MaxValue Cover ID C_MaxValue C_MaxValue
     | Choices_C_MaxValue Cover ID ([C_MaxValue])
     | Fail_C_MaxValue Cover FailInfo
     | Guard_C_MaxValue Cover Constraints C_MaxValue

instance Show C_MaxValue where
  showsPrec d (Choice_C_MaxValue cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_MaxValue cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_MaxValue cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_MaxValue cd info) = showChar '!'
  showsPrec _ (C_Max x1) = (showString "(Max") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_Infinite = showString "Infinite"


instance Read C_MaxValue where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Max x1,r1) | (_,r0) <- readQualified "ERD" "Max" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen False (\r -> [ (C_Infinite,r0) | (_,r0) <- readQualified "ERD" "Infinite" r]) s)


instance NonDet C_MaxValue where
  choiceCons = Choice_C_MaxValue
  choicesCons = Choices_C_MaxValue
  failCons = Fail_C_MaxValue
  guardCons = Guard_C_MaxValue
  try (Choice_C_MaxValue cd i x y) = tryChoice cd i x y
  try (Choices_C_MaxValue cd i xs) = tryChoices cd i xs
  try (Fail_C_MaxValue cd info) = Fail cd info
  try (Guard_C_MaxValue cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_MaxValue cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_MaxValue cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_MaxValue cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_MaxValue cd i _) = error ("ERD.MaxValue.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_MaxValue cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_MaxValue cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_MaxValue where
  generate s c = Choices_C_MaxValue c (freeID [1,0] s) [(C_Max (generate (leftSupply s) c)),C_Infinite]


instance NormalForm C_MaxValue where
  ($!!) cont (C_Max x1) d cs = (((\y1 d cs -> cont (C_Max y1) d cs) $!! x1) d) cs
  ($!!) cont C_Infinite d cs = cont C_Infinite d cs
  ($!!) cont (Choice_C_MaxValue cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_MaxValue cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_MaxValue cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_MaxValue cd info) _ _ = failCons cd info
  ($##) cont (C_Max x1) d cs = (((\y1 d cs -> cont (C_Max y1) d cs) $## x1) d) cs
  ($##) cont C_Infinite d cs = cont C_Infinite d cs
  ($##) cont (Choice_C_MaxValue cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_MaxValue cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_MaxValue cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_MaxValue cd info) _ _ = failCons cd info
  searchNF search cont (C_Max x1) = search (\y1 -> cont (C_Max y1)) x1
  searchNF _ cont C_Infinite = cont C_Infinite
  searchNF _ _ x = error ("ERD.MaxValue.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MaxValue where
  (=.=) (C_Max x1) (C_Max y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_Infinite C_Infinite d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Max x1) (C_Max y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_Infinite C_Infinite d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Max x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_Infinite = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_MaxValue cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_MaxValue cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_MaxValue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_MaxValue cd i _) = error ("ERD.MaxValue.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_MaxValue cd info) = [(Unsolvable info)]
  bind d i (Guard_C_MaxValue cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Max x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_Infinite = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_MaxValue cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_MaxValue cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_MaxValue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_MaxValue cd i _) = error ("ERD.MaxValue.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_MaxValue cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_MaxValue cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_MaxValue where
  (=?=) (Choice_C_MaxValue cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_MaxValue cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_MaxValue cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_MaxValue cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_MaxValue cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_MaxValue cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_MaxValue cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_MaxValue cd info) _ _ = failCons cd info
  (=?=) (C_Max x1) (C_Max y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_Infinite C_Infinite d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MaxValue cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_MaxValue cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_MaxValue cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_MaxValue cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_MaxValue cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_MaxValue cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_MaxValue cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_MaxValue cd info) _ _ = failCons cd info
  (<?=) (C_Max x1) (C_Max y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Max _) C_Infinite _ _ = Curry_Prelude.C_True
  (<?=) C_Infinite C_Infinite d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_readERDTermFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_ERD
d_C_readERDTermFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_OP_readERDTermFile_dot___hash_lambda1 x3250 x3500) x3250 x3500

d_OP_readERDTermFile_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_ERD
d_OP_readERDTermFile_dot___hash_lambda1 x1 x3250 x3500 = Curry_Prelude.d_C_return (d_C_updateERDTerm (Curry_ReadShowTerm.d_C_readUnqualifiedTerm (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_updateERDTerm :: C_ERD -> Cover -> ConstStore -> C_ERD
d_C_updateERDTerm x1 x3250 x3500 = case x1 of
     (C_ERD x2 x3 x4) -> C_ERD x2 x3 (Curry_Prelude.d_C_map d_OP_updateERDTerm_dot_updateRel_dot_5 x4 x3250 x3500)
     (Choice_C_ERD x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_updateERDTerm x1002 x3250 x3500) (d_C_updateERDTerm x1003 x3250 x3500)
     (Choices_C_ERD x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_updateERDTerm z x3250 x3500) x1002
     (Guard_C_ERD x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_updateERDTerm x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ERD x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateERDTerm_dot_updateCard_dot_5 :: C_Cardinality -> Cover -> ConstStore -> C_Cardinality
d_OP_updateERDTerm_dot_updateCard_dot_5 x1 x3250 x3500 = case x1 of
     (C_Exactly x2) -> C_Exactly x2
     (C_Between x3 x4) -> d_OP__case_1 x3 x4 x3250 x3500
     (C_Range x6 x7) -> d_OP_updateERDTerm_dot_updateCard_dot_5 (C_Between x6 (Curry_Prelude.d_C_maybe C_Infinite (acceptCs id C_Max) x7 x3250 x3500)) x3250 x3500
     (Choice_C_Cardinality x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateERDTerm_dot_updateCard_dot_5 x1002 x3250 x3500) (d_OP_updateERDTerm_dot_updateCard_dot_5 x1003 x3250 x3500)
     (Choices_C_Cardinality x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateERDTerm_dot_updateCard_dot_5 z x3250 x3500) x1002
     (Guard_C_Cardinality x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateERDTerm_dot_updateCard_dot_5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Cardinality x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateERDTerm_dot_updateEnd_dot_5 :: C_REnd -> Cover -> ConstStore -> C_REnd
d_OP_updateERDTerm_dot_updateEnd_dot_5 x1 x3250 x3500 = case x1 of
     (C_REnd x2 x3 x4) -> C_REnd x2 x3 (d_OP_updateERDTerm_dot_updateCard_dot_5 x4 x3250 x3500)
     (Choice_C_REnd x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateERDTerm_dot_updateEnd_dot_5 x1002 x3250 x3500) (d_OP_updateERDTerm_dot_updateEnd_dot_5 x1003 x3250 x3500)
     (Choices_C_REnd x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateERDTerm_dot_updateEnd_dot_5 z x3250 x3500) x1002
     (Guard_C_REnd x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateERDTerm_dot_updateEnd_dot_5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_REnd x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_updateERDTerm_dot_updateRel_dot_5 :: C_Relationship -> Cover -> ConstStore -> C_Relationship
d_OP_updateERDTerm_dot_updateRel_dot_5 x1 x3250 x3500 = case x1 of
     (C_Relationship x2 x3) -> C_Relationship x2 (Curry_Prelude.d_C_map d_OP_updateERDTerm_dot_updateEnd_dot_5 x3 x3250 x3500)
     (Choice_C_Relationship x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateERDTerm_dot_updateRel_dot_5 x1002 x3250 x3500) (d_OP_updateERDTerm_dot_updateRel_dot_5 x1003 x3250 x3500)
     (Choices_C_Relationship x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateERDTerm_dot_updateRel_dot_5 z x3250 x3500) x1002
     (Guard_C_Relationship x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateERDTerm_dot_updateRel_dot_5 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Relationship x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> C_MaxValue -> Cover -> ConstStore -> C_Cardinality
d_OP__case_1 x3 x4 x3250 x3500 = case x4 of
     (C_Max x5) -> d_OP__case_0 x5 x3 (Curry_Prelude.d_OP_lt_eq x3 x5 x3250 x3500) x3250 x3500
     C_Infinite -> C_Between x3 C_Infinite
     (Choice_C_MaxValue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x1002 x3250 x3500) (d_OP__case_1 x3 x1003 x3250 x3500)
     (Choices_C_MaxValue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 z x3250 x3500) x1002
     (Guard_C_MaxValue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_MaxValue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Cardinality
d_OP__case_0 x5 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> C_Between x3 (C_Max x5)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_C_show (C_Between x3 (C_Max x5)) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x3 x1002 x3250 x3500) (d_OP__case_0 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
