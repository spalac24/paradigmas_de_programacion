{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryDocCDoc (C_ModuleInfo (..), C_CurryInfo (..), C_FunctionInfo (..), C_TypeInfo (..), d_C_generateCDoc, nd_C_generateCDoc, d_C_funcComment, nd_C_funcComment, d_C_dataComment, nd_C_dataComment, d_C_flexRigid, d_C_author, d_C_consSignature) where

import Basics
import qualified Curry_CurryDocRead
import qualified Curry_FlatCurry
import qualified Curry_FlexRigid
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_CurryDocParams
import qualified Curry_List
data C_ModuleInfo
     = C_ModuleInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_ModuleInfo Cover ID C_ModuleInfo C_ModuleInfo
     | Choices_C_ModuleInfo Cover ID ([C_ModuleInfo])
     | Fail_C_ModuleInfo Cover FailInfo
     | Guard_C_ModuleInfo Cover Constraints C_ModuleInfo

instance Show C_ModuleInfo where
  showsPrec d (Choice_C_ModuleInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ModuleInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ModuleInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ModuleInfo cd info) = showChar '!'
  showsPrec _ (C_ModuleInfo x1 x2 x3) = (showString "(ModuleInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_ModuleInfo where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_ModuleInfo x1 x2 x3,r3) | (_,r0) <- readQualified "CurryDocCDoc" "ModuleInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_ModuleInfo where
  choiceCons = Choice_C_ModuleInfo
  choicesCons = Choices_C_ModuleInfo
  failCons = Fail_C_ModuleInfo
  guardCons = Guard_C_ModuleInfo
  try (Choice_C_ModuleInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_ModuleInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_ModuleInfo cd info) = Fail cd info
  try (Guard_C_ModuleInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ModuleInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ModuleInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ModuleInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ModuleInfo cd i _) = error ("CurryDocCDoc.ModuleInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ModuleInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ModuleInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ModuleInfo where
  generate s c = Choices_C_ModuleInfo c (freeID [3] s) [(C_ModuleInfo (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_ModuleInfo where
  ($!!) cont (C_ModuleInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ModuleInfo y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ModuleInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ModuleInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ModuleInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ModuleInfo cd info) _ _ = failCons cd info
  ($##) cont (C_ModuleInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_ModuleInfo y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ModuleInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ModuleInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ModuleInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ModuleInfo cd info) _ _ = failCons cd info
  searchNF search cont (C_ModuleInfo x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_ModuleInfo y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("CurryDocCDoc.ModuleInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ModuleInfo where
  (=.=) (C_ModuleInfo x1 x2 x3) (C_ModuleInfo y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_ModuleInfo x1 x2 x3) (C_ModuleInfo y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_ModuleInfo x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_ModuleInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ModuleInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ModuleInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ModuleInfo cd i _) = error ("CurryDocCDoc.ModuleInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ModuleInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ModuleInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_ModuleInfo x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_ModuleInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ModuleInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ModuleInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ModuleInfo cd i _) = error ("CurryDocCDoc.ModuleInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ModuleInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ModuleInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ModuleInfo where
  (=?=) (Choice_C_ModuleInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ModuleInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ModuleInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ModuleInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ModuleInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ModuleInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ModuleInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ModuleInfo cd info) _ _ = failCons cd info
  (=?=) (C_ModuleInfo x1 x2 x3) (C_ModuleInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_ModuleInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ModuleInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ModuleInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ModuleInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ModuleInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ModuleInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ModuleInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ModuleInfo cd info) _ _ = failCons cd info
  (<?=) (C_ModuleInfo x1 x2 x3) (C_ModuleInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_CurryInfo
     = C_CurryInfo C_ModuleInfo (Curry_Prelude.OP_List C_FunctionInfo) (Curry_Prelude.OP_List C_TypeInfo)
     | Choice_C_CurryInfo Cover ID C_CurryInfo C_CurryInfo
     | Choices_C_CurryInfo Cover ID ([C_CurryInfo])
     | Fail_C_CurryInfo Cover FailInfo
     | Guard_C_CurryInfo Cover Constraints C_CurryInfo

instance Show C_CurryInfo where
  showsPrec d (Choice_C_CurryInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CurryInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CurryInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CurryInfo cd info) = showChar '!'
  showsPrec _ (C_CurryInfo x1 x2 x3) = (showString "(CurryInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_CurryInfo where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CurryInfo x1 x2 x3,r3) | (_,r0) <- readQualified "CurryDocCDoc" "CurryInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_CurryInfo where
  choiceCons = Choice_C_CurryInfo
  choicesCons = Choices_C_CurryInfo
  failCons = Fail_C_CurryInfo
  guardCons = Guard_C_CurryInfo
  try (Choice_C_CurryInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_CurryInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_CurryInfo cd info) = Fail cd info
  try (Guard_C_CurryInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CurryInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CurryInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CurryInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CurryInfo cd i _) = error ("CurryDocCDoc.CurryInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CurryInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CurryInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CurryInfo where
  generate s c = Choices_C_CurryInfo c (freeID [3] s) [(C_CurryInfo (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_CurryInfo where
  ($!!) cont (C_CurryInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CurryInfo y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CurryInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CurryInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CurryInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CurryInfo cd info) _ _ = failCons cd info
  ($##) cont (C_CurryInfo x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CurryInfo y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CurryInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CurryInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CurryInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CurryInfo cd info) _ _ = failCons cd info
  searchNF search cont (C_CurryInfo x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_CurryInfo y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("CurryDocCDoc.CurryInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CurryInfo where
  (=.=) (C_CurryInfo x1 x2 x3) (C_CurryInfo y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CurryInfo x1 x2 x3) (C_CurryInfo y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CurryInfo x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_CurryInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CurryInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CurryInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CurryInfo cd i _) = error ("CurryDocCDoc.CurryInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CurryInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CurryInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CurryInfo x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_CurryInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CurryInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CurryInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CurryInfo cd i _) = error ("CurryDocCDoc.CurryInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CurryInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CurryInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CurryInfo where
  (=?=) (Choice_C_CurryInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CurryInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CurryInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CurryInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CurryInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CurryInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CurryInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CurryInfo cd info) _ _ = failCons cd info
  (=?=) (C_CurryInfo x1 x2 x3) (C_CurryInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_CurryInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CurryInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CurryInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CurryInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CurryInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CurryInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CurryInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CurryInfo cd info) _ _ = failCons cd info
  (<?=) (C_CurryInfo x1 x2 x3) (C_CurryInfo y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_FunctionInfo
     = C_FunctionInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_TypeExpr (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool Curry_FlexRigid.C_FlexRigidResult
     | Choice_C_FunctionInfo Cover ID C_FunctionInfo C_FunctionInfo
     | Choices_C_FunctionInfo Cover ID ([C_FunctionInfo])
     | Fail_C_FunctionInfo Cover FailInfo
     | Guard_C_FunctionInfo Cover Constraints C_FunctionInfo

instance Show C_FunctionInfo where
  showsPrec d (Choice_C_FunctionInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FunctionInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FunctionInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FunctionInfo cd info) = showChar '!'
  showsPrec _ (C_FunctionInfo x1 x2 x3 x4 x5 x6) = (showString "(FunctionInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . (showChar ')')))))))))))))


instance Read C_FunctionInfo where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_FunctionInfo x1 x2 x3 x4 x5 x6,r6) | (_,r0) <- readQualified "CurryDocCDoc" "FunctionInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5]) s


instance NonDet C_FunctionInfo where
  choiceCons = Choice_C_FunctionInfo
  choicesCons = Choices_C_FunctionInfo
  failCons = Fail_C_FunctionInfo
  guardCons = Guard_C_FunctionInfo
  try (Choice_C_FunctionInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_FunctionInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_FunctionInfo cd info) = Fail cd info
  try (Guard_C_FunctionInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FunctionInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FunctionInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FunctionInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FunctionInfo cd i _) = error ("CurryDocCDoc.FunctionInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FunctionInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FunctionInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FunctionInfo where
  generate s c = Choices_C_FunctionInfo c (freeID [6] s) [(C_FunctionInfo (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_FunctionInfo where
  ($!!) cont (C_FunctionInfo x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_FunctionInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FunctionInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FunctionInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FunctionInfo cd info) _ _ = failCons cd info
  ($##) cont (C_FunctionInfo x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_FunctionInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FunctionInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FunctionInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FunctionInfo cd info) _ _ = failCons cd info
  searchNF search cont (C_FunctionInfo x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (C_FunctionInfo y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("CurryDocCDoc.FunctionInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FunctionInfo where
  (=.=) (C_FunctionInfo x1 x2 x3 x4 x5 x6) (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_FunctionInfo x1 x2 x3 x4 x5 x6) (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_FunctionInfo x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_C_FunctionInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FunctionInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FunctionInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FunctionInfo cd i _) = error ("CurryDocCDoc.FunctionInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FunctionInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FunctionInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_FunctionInfo x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_C_FunctionInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FunctionInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FunctionInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FunctionInfo cd i _) = error ("CurryDocCDoc.FunctionInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_FunctionInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_FunctionInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_FunctionInfo where
  (=?=) (Choice_C_FunctionInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_FunctionInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_FunctionInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_FunctionInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_FunctionInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_FunctionInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_FunctionInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_FunctionInfo cd info) _ _ = failCons cd info
  (=?=) (C_FunctionInfo x1 x2 x3 x4 x5 x6) (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.=?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_FunctionInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FunctionInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FunctionInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FunctionInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FunctionInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FunctionInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FunctionInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FunctionInfo cd info) _ _ = failCons cd info
  (<?=) (C_FunctionInfo x1 x2 x3 x4 x5 x6) (C_FunctionInfo y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.<?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_TypeInfo
     = C_TypeInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr))) (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool
     | Choice_C_TypeInfo Cover ID C_TypeInfo C_TypeInfo
     | Choices_C_TypeInfo Cover ID ([C_TypeInfo])
     | Fail_C_TypeInfo Cover FailInfo
     | Guard_C_TypeInfo Cover Constraints C_TypeInfo

instance Show C_TypeInfo where
  showsPrec d (Choice_C_TypeInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TypeInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TypeInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TypeInfo cd info) = showChar '!'
  showsPrec _ (C_TypeInfo x1 x2 x3 x4 x5 x6) = (showString "(TypeInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . (showChar ')')))))))))))))


instance Read C_TypeInfo where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_TypeInfo x1 x2 x3 x4 x5 x6,r6) | (_,r0) <- readQualified "CurryDocCDoc" "TypeInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5]) s


instance NonDet C_TypeInfo where
  choiceCons = Choice_C_TypeInfo
  choicesCons = Choices_C_TypeInfo
  failCons = Fail_C_TypeInfo
  guardCons = Guard_C_TypeInfo
  try (Choice_C_TypeInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_TypeInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_TypeInfo cd info) = Fail cd info
  try (Guard_C_TypeInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TypeInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TypeInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TypeInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TypeInfo cd i _) = error ("CurryDocCDoc.TypeInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TypeInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TypeInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TypeInfo where
  generate s c = Choices_C_TypeInfo c (freeID [6] s) [(C_TypeInfo (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_TypeInfo where
  ($!!) cont (C_TypeInfo x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TypeInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TypeInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TypeInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TypeInfo cd info) _ _ = failCons cd info
  ($##) cont (C_TypeInfo x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TypeInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TypeInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TypeInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TypeInfo cd info) _ _ = failCons cd info
  searchNF search cont (C_TypeInfo x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (C_TypeInfo y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("CurryDocCDoc.TypeInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TypeInfo where
  (=.=) (C_TypeInfo x1 x2 x3 x4 x5 x6) (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_TypeInfo x1 x2 x3 x4 x5 x6) (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_TypeInfo x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_C_TypeInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TypeInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TypeInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TypeInfo cd i _) = error ("CurryDocCDoc.TypeInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TypeInfo x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_C_TypeInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeInfo cd i _) = error ("CurryDocCDoc.TypeInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TypeInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TypeInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TypeInfo where
  (=?=) (Choice_C_TypeInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TypeInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TypeInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TypeInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TypeInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TypeInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TypeInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TypeInfo cd info) _ _ = failCons cd info
  (=?=) (C_TypeInfo x1 x2 x3 x4 x5 x6) (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.=?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_TypeInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TypeInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TypeInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TypeInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TypeInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TypeInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TypeInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TypeInfo cd info) _ _ = failCons cd info
  (<?=) (C_TypeInfo x1 x2 x3 x4 x5 x6) (C_TypeInfo y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.<?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_generateCDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_CurryDocRead.C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_generateCDoc x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_FlatCurry.d_C_flatCurryFileName x1 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x5 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1 x4 x2 x3) x3250 x3500)

nd_C_generateCDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_CurryDocRead.C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_generateCDoc x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = Curry_FlatCurry.d_C_flatCurryFileName x1 x3250 x3500
           in (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x5 x3250 x3500) (wrapNX id (nd_OP_generateCDoc_dot___hash_lambda1 x4 x2 x3)) x2000 x3250 x3500)))

d_OP_generateCDoc_dot_filterT_dot_2 :: Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP_generateCDoc_dot_filterT_dot_2 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> d_OP__case_5 x3 x1 (Curry_Prelude.d_OP_eq_eq x3 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> d_OP__case_4 x7 x1 (Curry_Prelude.d_OP_eq_eq x7 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot_filterT_dot_2 x1002 x3250 x3500) (d_OP_generateCDoc_dot_filterT_dot_2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot_filterT_dot_2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot_filterT_dot_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1 :: Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Prog x5 x6 x7 x8 x9) -> let
          x10 = Curry_CurryDocRead.d_C_splitComment x2 x3250 x3500
          x11 = d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x10 x3250 x3500
          x12 = d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x10 x3250 x3500
          x13 = C_ModuleInfo x5 (d_C_author x12 x3250 x3500) x11
          x14 = Curry_Prelude.d_C_map (d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x3) (Curry_Prelude.d_C_filter d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 x8 x3250 x3500) x3250 x3500
          x15 = Curry_Prelude.d_C_map (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x3) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_generateCDoc_dot_filterT_dot_2 x3250 x3500) x7 x3250 x3500) x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_ReadShowTerm.d_C_showTerm (C_CurryInfo x13 x14 x15) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_generateCDoc_dot___hash_lambda1 :: Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Prog x5 x6 x7 x8 x9) -> let
          x2011 = x3000
           in (seq x2011 (let
               x2002 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2002 (seq x2012 (let
                    x2007 = leftSupply x2012
                    x2010 = rightSupply x2012
                     in (seq x2007 (seq x2010 (let
                         x10 = Curry_CurryDocRead.d_C_splitComment x2 x3250 x3500
                         x11 = d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x10 x3250 x3500
                         x12 = d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x10 x3250 x3500
                         x13 = C_ModuleInfo x5 (d_C_author x12 x3250 x3500) x11
                         x14 = let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x3)) (Curry_Prelude.nd_C_filter (wrapDX id d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3) x8 x2000 x3250 x3500) x2001 x3250 x3500)))
                         x15 = let
                              x2006 = leftSupply x2007
                              x2005 = rightSupply x2007
                               in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_map (wrapDX id (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x3)) (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_generateCDoc_dot_filterT_dot_2) x2003 x3250 x3500) x7 x2004 x3250 x3500)))) x2006 x3250 x3500)))
                          in (let
                              x2008 = leftSupply x2010
                              x2009 = rightSupply x2010
                               in (seq x2008 (seq x2009 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x2008 x3250 x3500) (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_return) (Curry_ReadShowTerm.d_C_showTerm (C_CurryInfo x13 x14 x15) x3250 x3500) x2009 x3250 x3500) x3250 x3500))))))))))))
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_generateCDoc_dot___hash_lambda1 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP2_hash_mCmts x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot___hash_selFP3_hash_avCmts x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 :: Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> C_FunctionInfo
d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Func x4 x5 x6 x7 x8) -> d_OP__case_3 x8 x1 x2 x7 x4 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 :: Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> C_FunctionInfo
nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Func x4 x5 x6 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x8 x1 x2 x7 x4 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_generateCDoc_dot___hash_lambda1_dot_funcInfo_dot_17 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> C_TypeInfo
d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> d_OP__case_2 x1 x5 x6 x3 x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x9 x10 x11 x12) -> d_OP__case_1 x1 x11 x12 x9 x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x1 x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 x1002 x3250 x3500) (d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_generateCDoc_dot___hash_lambda1_dot___hash_lambda3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_funcComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_funcComment x1 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst (Curry_Prelude.d_OP_dot Curry_CurryDocRead.d_C_splitComment (Curry_CurryDocRead.d_C_getFuncComment x1) x3250 x3500) x3250 x3500

nd_C_funcComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_funcComment x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_CurryDocRead.d_C_splitComment) (wrapDX id (Curry_CurryDocRead.d_C_getFuncComment x1)) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_dataComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dataComment x1 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst (Curry_Prelude.d_OP_dot Curry_CurryDocRead.d_C_splitComment (Curry_CurryDocRead.d_C_getDataComment x1) x3250 x3500) x3250 x3500

nd_C_dataComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_dataComment x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_CurryDocRead.d_C_splitComment) (wrapDX id (Curry_CurryDocRead.d_C_getDataComment x1)) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_flexRigid :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_FlexRigid.C_FlexRigidResult
d_C_flexRigid x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> Curry_FlexRigid.d_C_getFlexRigid x3 x3250 x3500
     (Curry_FlatCurry.C_External x4) -> Curry_FlexRigid.C_UnknownFR
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_flexRigid x1002 x3250 x3500) (d_C_flexRigid x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_flexRigid z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_flexRigid x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_author :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_author x1 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_concat (Curry_CurryDocRead.d_C_getCommentType (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))) x1 x3250 x3500) x3250 x3500

d_C_consSignature :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr)
d_C_consSignature x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> d_OP__case_0 x5 x2 x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_consSignature x1002 x3250 x3500) (d_C_consSignature x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_consSignature z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_consSignature x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr)
d_OP__case_0 x5 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x6 x7) x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x5 x1002 x3250 x3500) (d_OP__case_0 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_TypeInfo
d_OP__case_1 x1 x11 x12 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x13 x14) -> C_TypeInfo x14 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Cons x12 Curry_Prelude.OP_List)) Curry_Prelude.OP_List) x11 x13 (Curry_Prelude.d_C_apply (d_C_dataComment x14 x3250 x3500) x1 x3250 x3500) Curry_Prelude.C_True
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x11 x12 x1002 x3250 x3500) (d_OP__case_1 x1 x11 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x11 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x11 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_TypeInfo
d_OP__case_2 x1 x5 x6 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> C_TypeInfo x8 (Curry_Prelude.d_C_map d_C_consSignature (Curry_Prelude.d_C_filter d_OP_generateCDoc_dot___hash_lambda1_dot_typeInfo_dot_17_dot___hash_lambda2 x6 x3250 x3500) x3250 x3500) x5 x7 (Curry_Prelude.d_C_apply (d_C_dataComment x8 x3250 x3500) x1 x3250 x3500) Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x5 x6 x1002 x3250 x3500) (d_OP__case_2 x1 x5 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x5 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x5 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_FlatCurry.C_Rule -> Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_FunctionInfo
d_OP__case_3 x8 x1 x2 x7 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> C_FunctionInfo x10 x7 x9 (Curry_Prelude.d_C_apply (d_C_funcComment x10 x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_CurryDocRead.d_C_getNondetInfo x1 x3250 x3500) x4 x3250 x3500) (d_C_flexRigid x8 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x8 x1 x2 x7 x1002 x3250 x3500) (d_OP__case_3 x8 x1 x2 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x8 x1 x2 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x8 x1 x2 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_FlatCurry.C_Rule -> Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> C_FunctionInfo
nd_OP__case_3 x8 x1 x2 x7 x4 x3000 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (C_FunctionInfo x10 x7 x9 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_funcComment x10 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_CurryDocRead.nd_C_getNondetInfo x1 x2003 x3250 x3500) x4 x2004 x3250 x3500)))) (d_C_flexRigid x8 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x8 x1 x2 x7 x1002 x3000 x3250 x3500) (nd_OP__case_3 x8 x1 x2 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x8 x1 x2 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x8 x1 x2 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP__case_4 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x7 x1 x1002 x3250 x3500) (d_OP__case_4 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP__case_5 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x1 x1002 x3250 x3500) (d_OP__case_5 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
