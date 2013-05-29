{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryStringClassifier (C_Token (..), C_Tokens, d_C_isSmallComment, d_C_isBigComment, d_C_isComment, d_C_isText, d_C_isLetter, d_C_isCode, d_C_isModuleHead, d_C_isMeta, nd_C_scan, d_C_plainCode, d_C_unscan, nd_C_readScan, nd_C_testScan) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
type C_Tokens = Curry_Prelude.OP_List C_Token

data C_Token
     = C_SmallComment (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_BigComment (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Text (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Letter (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Code (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_ModuleHead (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Meta (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Token Cover ID C_Token C_Token
     | Choices_C_Token Cover ID ([C_Token])
     | Fail_C_Token Cover FailInfo
     | Guard_C_Token Cover Constraints C_Token

instance Show C_Token where
  showsPrec d (Choice_C_Token cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Token cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Token cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Token cd info) = showChar '!'
  showsPrec _ (C_SmallComment x1) = (showString "(SmallComment") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_BigComment x1) = (showString "(BigComment") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Text x1) = (showString "(Text") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Letter x1) = (showString "(Letter") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Code x1) = (showString "(Code") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ModuleHead x1) = (showString "(ModuleHead") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Meta x1) = (showString "(Meta") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Token where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_SmallComment x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "SmallComment" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_BigComment x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "BigComment" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Text x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "Text" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Letter x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "Letter" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Code x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "Code" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_ModuleHead x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "ModuleHead" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Meta x1,r1) | (_,r0) <- readQualified "CurryStringClassifier" "Meta" r, (x1,r1) <- readsPrec 11 r0]) s))))))


instance NonDet C_Token where
  choiceCons = Choice_C_Token
  choicesCons = Choices_C_Token
  failCons = Fail_C_Token
  guardCons = Guard_C_Token
  try (Choice_C_Token cd i x y) = tryChoice cd i x y
  try (Choices_C_Token cd i xs) = tryChoices cd i xs
  try (Fail_C_Token cd info) = Fail cd info
  try (Guard_C_Token cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Token cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Token cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Token cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Token cd i _) = error ("CurryStringClassifier.Token.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Token cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Token cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Token where
  generate s = Choices_C_Token defCover (freeID [1,1,1,1,1,1,1] s) [(C_SmallComment (generate (leftSupply s))),(C_BigComment (generate (leftSupply s))),(C_Text (generate (leftSupply s))),(C_Letter (generate (leftSupply s))),(C_Code (generate (leftSupply s))),(C_ModuleHead (generate (leftSupply s))),(C_Meta (generate (leftSupply s)))]


instance NormalForm C_Token where
  ($!!) cont (C_SmallComment x1) cs = ((\y1 cs -> cont (C_SmallComment y1) cs) $!! x1) cs
  ($!!) cont (C_BigComment x1) cs = ((\y1 cs -> cont (C_BigComment y1) cs) $!! x1) cs
  ($!!) cont (C_Text x1) cs = ((\y1 cs -> cont (C_Text y1) cs) $!! x1) cs
  ($!!) cont (C_Letter x1) cs = ((\y1 cs -> cont (C_Letter y1) cs) $!! x1) cs
  ($!!) cont (C_Code x1) cs = ((\y1 cs -> cont (C_Code y1) cs) $!! x1) cs
  ($!!) cont (C_ModuleHead x1) cs = ((\y1 cs -> cont (C_ModuleHead y1) cs) $!! x1) cs
  ($!!) cont (C_Meta x1) cs = ((\y1 cs -> cont (C_Meta y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Token cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Token cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Token cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Token cd info) _ = failCons cd info
  ($##) cont (C_SmallComment x1) cs = ((\y1 cs -> cont (C_SmallComment y1) cs) $## x1) cs
  ($##) cont (C_BigComment x1) cs = ((\y1 cs -> cont (C_BigComment y1) cs) $## x1) cs
  ($##) cont (C_Text x1) cs = ((\y1 cs -> cont (C_Text y1) cs) $## x1) cs
  ($##) cont (C_Letter x1) cs = ((\y1 cs -> cont (C_Letter y1) cs) $## x1) cs
  ($##) cont (C_Code x1) cs = ((\y1 cs -> cont (C_Code y1) cs) $## x1) cs
  ($##) cont (C_ModuleHead x1) cs = ((\y1 cs -> cont (C_ModuleHead y1) cs) $## x1) cs
  ($##) cont (C_Meta x1) cs = ((\y1 cs -> cont (C_Meta y1) cs) $## x1) cs
  ($##) cont (Choice_C_Token cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Token cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Token cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Token cd info) _ = failCons cd info
  searchNF search cont (C_SmallComment x1) = search (\y1 -> cont (C_SmallComment y1)) x1
  searchNF search cont (C_BigComment x1) = search (\y1 -> cont (C_BigComment y1)) x1
  searchNF search cont (C_Text x1) = search (\y1 -> cont (C_Text y1)) x1
  searchNF search cont (C_Letter x1) = search (\y1 -> cont (C_Letter y1)) x1
  searchNF search cont (C_Code x1) = search (\y1 -> cont (C_Code y1)) x1
  searchNF search cont (C_ModuleHead x1) = search (\y1 -> cont (C_ModuleHead y1)) x1
  searchNF search cont (C_Meta x1) = search (\y1 -> cont (C_Meta y1)) x1
  searchNF _ _ x = error ("CurryStringClassifier.Token.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Token where
  (=.=) (C_SmallComment x1) (C_SmallComment y1) cs = (x1 =:= y1) cs
  (=.=) (C_BigComment x1) (C_BigComment y1) cs = (x1 =:= y1) cs
  (=.=) (C_Text x1) (C_Text y1) cs = (x1 =:= y1) cs
  (=.=) (C_Letter x1) (C_Letter y1) cs = (x1 =:= y1) cs
  (=.=) (C_Code x1) (C_Code y1) cs = (x1 =:= y1) cs
  (=.=) (C_ModuleHead x1) (C_ModuleHead y1) cs = (x1 =:= y1) cs
  (=.=) (C_Meta x1) (C_Meta y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_SmallComment x1) (C_SmallComment y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_BigComment x1) (C_BigComment y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Text x1) (C_Text y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Letter x1) (C_Letter y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Code x1) (C_Code y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_ModuleHead x1) (C_ModuleHead y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Meta x1) (C_Meta y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_SmallComment x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_BigComment x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Text x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Letter x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Code x2) = ((i :=: (ChooseN 4 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_ModuleHead x2) = ((i :=: (ChooseN 5 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Meta x2) = ((i :=: (ChooseN 6 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Token cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Token cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Token cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Token cd i _) = error ("CurryStringClassifier.Token.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Token cd info) = [(Unsolvable info)]
  bind i (Guard_C_Token cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_SmallComment x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_BigComment x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Text x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Letter x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Code x2) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_ModuleHead x2) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Meta x2) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Token cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Token cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Token cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Token cd i _) = error ("CurryStringClassifier.Token.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Token cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Token cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Token where
  (=?=) (Choice_C_Token cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Token cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Token cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Token cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Token cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Token cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Token cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Token cd info) _ = failCons cd info
  (=?=) (C_SmallComment x1) (C_SmallComment y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_BigComment x1) (C_BigComment y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Text x1) (C_Text y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Letter x1) (C_Letter y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Code x1) (C_Code y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_ModuleHead x1) (C_ModuleHead y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Meta x1) (C_Meta y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Token cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Token cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Token cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Token cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Token cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Token cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Token cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Token cd info) _ = failCons cd info
  (<?=) (C_SmallComment x1) (C_SmallComment y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_SmallComment _) (C_BigComment _) _ = Curry_Prelude.C_True
  (<?=) (C_SmallComment _) (C_Text _) _ = Curry_Prelude.C_True
  (<?=) (C_SmallComment _) (C_Letter _) _ = Curry_Prelude.C_True
  (<?=) (C_SmallComment _) (C_Code _) _ = Curry_Prelude.C_True
  (<?=) (C_SmallComment _) (C_ModuleHead _) _ = Curry_Prelude.C_True
  (<?=) (C_SmallComment _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_BigComment x1) (C_BigComment y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_BigComment _) (C_Text _) _ = Curry_Prelude.C_True
  (<?=) (C_BigComment _) (C_Letter _) _ = Curry_Prelude.C_True
  (<?=) (C_BigComment _) (C_Code _) _ = Curry_Prelude.C_True
  (<?=) (C_BigComment _) (C_ModuleHead _) _ = Curry_Prelude.C_True
  (<?=) (C_BigComment _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_Text x1) (C_Text y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Text _) (C_Letter _) _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Code _) _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_ModuleHead _) _ = Curry_Prelude.C_True
  (<?=) (C_Text _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_Letter x1) (C_Letter y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Letter _) (C_Code _) _ = Curry_Prelude.C_True
  (<?=) (C_Letter _) (C_ModuleHead _) _ = Curry_Prelude.C_True
  (<?=) (C_Letter _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_Code x1) (C_Code y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Code _) (C_ModuleHead _) _ = Curry_Prelude.C_True
  (<?=) (C_Code _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_ModuleHead x1) (C_ModuleHead y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_ModuleHead _) (C_Meta _) _ = Curry_Prelude.C_True
  (<?=) (C_Meta x1) (C_Meta y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Token where
  cover (C_SmallComment x1) = C_SmallComment (cover x1)
  cover (C_BigComment x1) = C_BigComment (cover x1)
  cover (C_Text x1) = C_Text (cover x1)
  cover (C_Letter x1) = C_Letter (cover x1)
  cover (C_Code x1) = C_Code (cover x1)
  cover (C_ModuleHead x1) = C_ModuleHead (cover x1)
  cover (C_Meta x1) = C_Meta (cover x1)
  cover (Choice_C_Token cd i x y) = Choice_C_Token (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Token cd i xs) = Choices_C_Token (incCover cd) i (map cover xs)
  cover (Fail_C_Token cd info) = Fail_C_Token (incCover cd) info
  cover (Guard_C_Token cd c e) = Guard_C_Token (incCover cd) c (cover e)


d_C_isSmallComment :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSmallComment x1 x3500 = case x1 of
     (C_SmallComment x2) -> Curry_Prelude.C_True
     (C_BigComment x3) -> Curry_Prelude.C_False
     (C_Text x4) -> Curry_Prelude.C_False
     (C_Letter x5) -> Curry_Prelude.C_False
     (C_Code x6) -> Curry_Prelude.C_False
     (C_ModuleHead x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSmallComment x1002 x3500) (d_C_isSmallComment x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSmallComment z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSmallComment x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isBigComment :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isBigComment x1 x3500 = case x1 of
     (C_BigComment x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_Text x4) -> Curry_Prelude.C_False
     (C_Letter x5) -> Curry_Prelude.C_False
     (C_Code x6) -> Curry_Prelude.C_False
     (C_ModuleHead x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isBigComment x1002 x3500) (d_C_isBigComment x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isBigComment z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isBigComment x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isComment :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isComment x1 x3500 = Curry_Prelude.d_OP_bar_bar (d_C_isSmallComment x1 x3500) (d_C_isBigComment x1 x3500) x3500

d_C_isText :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isText x1 x3500 = case x1 of
     (C_Text x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_BigComment x4) -> Curry_Prelude.C_False
     (C_Letter x5) -> Curry_Prelude.C_False
     (C_Code x6) -> Curry_Prelude.C_False
     (C_ModuleHead x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isText x1002 x3500) (d_C_isText x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isText z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isText x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isLetter :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isLetter x1 x3500 = case x1 of
     (C_Letter x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_BigComment x4) -> Curry_Prelude.C_False
     (C_Text x5) -> Curry_Prelude.C_False
     (C_Code x6) -> Curry_Prelude.C_False
     (C_ModuleHead x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isLetter x1002 x3500) (d_C_isLetter x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isLetter z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isLetter x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isCode :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCode x1 x3500 = case x1 of
     (C_Code x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_BigComment x4) -> Curry_Prelude.C_False
     (C_Text x5) -> Curry_Prelude.C_False
     (C_Letter x6) -> Curry_Prelude.C_False
     (C_ModuleHead x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCode x1002 x3500) (d_C_isCode x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCode z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCode x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isModuleHead :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isModuleHead x1 x3500 = case x1 of
     (C_ModuleHead x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_BigComment x4) -> Curry_Prelude.C_False
     (C_Text x5) -> Curry_Prelude.C_False
     (C_Letter x6) -> Curry_Prelude.C_False
     (C_Code x7) -> Curry_Prelude.C_False
     (C_Meta x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isModuleHead x1002 x3500) (d_C_isModuleHead x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isModuleHead z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isModuleHead x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isMeta :: C_Token -> ConstStore -> Curry_Prelude.C_Bool
d_C_isMeta x1 x3500 = case x1 of
     (C_Meta x2) -> Curry_Prelude.C_True
     (C_SmallComment x3) -> Curry_Prelude.C_False
     (C_BigComment x4) -> Curry_Prelude.C_False
     (C_Text x5) -> Curry_Prelude.C_False
     (C_Letter x6) -> Curry_Prelude.C_False
     (C_Code x7) -> Curry_Prelude.C_False
     (C_ModuleHead x8) -> Curry_Prelude.C_False
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isMeta x1002 x3500) (d_C_isMeta x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isMeta z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isMeta x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_weaveIntoCode :: (Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token) -> Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token
d_C_weaveIntoCode x1 x2 x3500 = let
     x3 = d_C_unweaveCode x2 x3500
     x4 = d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x3 x3500
     x5 = d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x3 x3500
      in (d_C_weave (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x4 x3500) x5) x3500)

nd_C_weaveIntoCode :: Func (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token) -> Curry_Prelude.OP_List C_Token -> IDSupply -> ConstStore -> Curry_Prelude.OP_List C_Token
nd_C_weaveIntoCode x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x3 = d_C_unweaveCode x2 x3500
          x4 = d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x3 x3500
          x5 = d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x3 x3500
           in (d_C_weave (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x5) x3500)))

d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token) -> ConstStore -> Curry_Prelude.OP_List C_Token
d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x1002 x3500) (d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_weaveIntoCode_dot___hash_selFP2_hash_cs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token) -> ConstStore -> Curry_Prelude.OP_List C_Token
d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x1002 x3500) (d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_weaveIntoCode_dot___hash_selFP3_hash_ncs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unweaveCode :: Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token)
d_C_unweaveCode x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = d_C_unweaveCode x3 x3500
          x5 = d_OP_unweaveCode_dot___hash_selFP5_hash_cs x4 x3500
          x6 = d_OP_unweaveCode_dot___hash_selFP6_hash_ncs x4 x3500
           in (d_OP__case_66 x2 x5 x6 (d_C_isCode x2 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unweaveCode x1002 x3500) (d_C_unweaveCode x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unweaveCode z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unweaveCode x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unweaveCode_dot___hash_selFP5_hash_cs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token) -> ConstStore -> Curry_Prelude.OP_List C_Token
d_OP_unweaveCode_dot___hash_selFP5_hash_cs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unweaveCode_dot___hash_selFP5_hash_cs x1002 x3500) (d_OP_unweaveCode_dot___hash_selFP5_hash_cs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unweaveCode_dot___hash_selFP5_hash_cs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unweaveCode_dot___hash_selFP5_hash_cs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unweaveCode_dot___hash_selFP6_hash_ncs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_Token) (Curry_Prelude.OP_List C_Token) -> ConstStore -> Curry_Prelude.OP_List C_Token
d_OP_unweaveCode_dot___hash_selFP6_hash_ncs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unweaveCode_dot___hash_selFP6_hash_ncs x1002 x3500) (d_OP_unweaveCode_dot___hash_selFP6_hash_ncs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unweaveCode_dot___hash_selFP6_hash_ncs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unweaveCode_dot___hash_selFP6_hash_ncs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_weave :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> ConstStore -> Curry_Prelude.OP_List t0
d_C_weave x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_65 x3 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_weave x1002 x3500) (d_C_weave x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_weave z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_weave x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_scan :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List C_Token
nd_C_scan x1 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x2 = generate x2003
                in (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_modHead (wrapDX id Curry_Prelude.d_C_id) (nd_C_stateScan (Curry_Prelude.C_Int 1#) (C_Code x2) x2 x1 x2000 x3500) x2001 x3500)))))))))

nd_C_stateScan :: Curry_Prelude.C_Int -> C_Token -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_List C_Token
nd_C_stateScan x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0_stateScan x2 (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_59 x1 x3 x5 x6 x2 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_stateScan x1 x2 x3 x1002 x3000 x3500) (nd_C_stateScan x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_stateScan x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_stateScan x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0_stateScan x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_stateScan x1 x1002 x3500) (d_OP___cond_0_stateScan x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_stateScan x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_stateScan x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_stateScan x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_stateScan x1 x1002 x3000 x3500) (nd_OP___cond_0_stateScan x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_stateScan x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_stateScan x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_stateScan_dot___hash_selFP8_hash_comment :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_stateScan_dot___hash_selFP8_hash_comment x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_stateScan_dot___hash_selFP8_hash_comment x1002 x3500) (d_OP_stateScan_dot___hash_selFP8_hash_comment x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_stateScan_dot___hash_selFP8_hash_comment z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_stateScan_dot___hash_selFP8_hash_comment x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_stateScan_dot___hash_selFP9_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_stateScan_dot___hash_selFP9_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_stateScan_dot___hash_selFP9_hash_rest x1002 x3500) (d_OP_stateScan_dot___hash_selFP9_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_stateScan_dot___hash_selFP9_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_stateScan_dot___hash_selFP9_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_modHead :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token
d_C_modHead x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_21 x1 x4 x3 x3500
     Curry_Prelude.OP_List -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) Curry_Prelude.OP_List x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_modHead x1 x1002 x3500) (d_C_modHead x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_modHead x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_modHead x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_modHead :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_Token -> IDSupply -> ConstStore -> Curry_Prelude.OP_List C_Token
nd_C_modHead x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x1 x4 x3 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) Curry_Prelude.OP_List x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_modHead x1 x1002 x3000 x3500) (nd_C_modHead x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_modHead x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_modHead x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_modHead_dot___hash_lambda10 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_modHead_dot___hash_lambda10 x1 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\n'#) x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\r'#) x3500) x3500

d_C_modHeadInLine :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token
d_C_modHeadInLine x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) Curry_Prelude.OP_List x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_14 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_modHeadInLine x1 x1002 x3500) (d_C_modHeadInLine x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_modHeadInLine x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_modHeadInLine x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_modHeadInLine :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List C_Token -> IDSupply -> ConstStore -> Curry_Prelude.OP_List C_Token
nd_C_modHeadInLine x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) Curry_Prelude.OP_List x3500))
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_modHeadInLine x1 x1002 x3000 x3500) (nd_C_modHeadInLine x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_modHeadInLine x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_modHeadInLine x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_modHeadInLine_dot___hash_lambda12 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_modHeadInLine_dot___hash_lambda12 x1 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\n'#) x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\r'#) x3500) x3500

d_C_headers :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_headers x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)))))

d_C_lineBeginsWith :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_lineBeginsWith x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_length x2 x3500
      in (d_OP__case_11 x1 x2 x3 (Curry_Prelude.d_OP_lt (Curry_Prelude.d_C_length x1 x3500) x3 x3500) x3500)

d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x1002 x3500) (d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x1002 x3500) (d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isSep :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSep x1 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Char.d_C_isSpace x1 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) (d_C_infixIDs x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) Curry_Prelude.OP_List))) x3500) x3500) x3500

d_C_infixIDs :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_infixIDs x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))

d_C_delimiters :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_delimiters x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))

d_C_toBeEscaped :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_toBeEscaped x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\t'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))

d_C_maybeCode :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token
d_C_maybeCode x1 x2 x3500 = Curry_Prelude.OP_Cons (C_Code x1) x2

d_C_maybeMo :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List C_Token
d_C_maybeMo x1 x2 x3500 = d_OP__case_9 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.OP_List x3500) x3500

d_C_plainCode :: Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_plainCode x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_6 x3 x2 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_plainCode x1002 x3500) (d_C_plainCode x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_plainCode z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_plainCode x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unscan :: Curry_Prelude.OP_List C_Token -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_unscan x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_3 x3 x2 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unscan x1002 x3500) (d_C_unscan x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unscan z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unscan x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_readScan :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List C_Token)
nd_C_readScan x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapNX id nd_C_scan) x2000 x3500) x2001 x3500)))))

nd_C_testScan :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_testScan x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (wrapNX id nd_OP_testScan_dot___hash_lambda17) x2000 x3500))

nd_OP_testScan_dot___hash_lambda17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_testScan_dot___hash_lambda17 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_print (Curry_Prelude.d_OP_eq_eq (d_C_unscan (nd_C_scan x1 x2000 x3500) x3500) x1 x3500) x3500))

nd_C_testWeave :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_testWeave x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (wrapNX id nd_OP_testWeave_dot___hash_lambda18) x2000 x3500))

nd_OP_testWeave_dot___hash_lambda18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_testWeave_dot___hash_lambda18 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_print (Curry_Prelude.d_OP_eq_eq (d_C_unscan (d_C_weave (d_C_unweaveCode (nd_C_scan x1 x2000 x3500) x3500) x3500) x3500) x1 x3500) x3500))

d_OP__case_3 x3 x2 x3500 = case x2 of
     (C_ModuleHead x4) -> Curry_Prelude.d_OP_plus_plus x4 (d_OP__case_2 x3 x3500) x3500
     (C_Code x16) -> Curry_Prelude.d_OP_plus_plus x16 (d_C_unscan x3 x3500) x3500
     (C_Text x17) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_unscan x3 x3500)) x3500)
     (C_Letter x18) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.d_OP_plus_plus x18 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_unscan x3 x3500)) x3500)
     (C_BigComment x19) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x19 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_unscan x3 x3500) x3500) x3500) x3500
     (C_SmallComment x20) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x20 (d_C_unscan x3 x3500) x3500) x3500
     (C_Meta x21) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x21 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_unscan x3 x3500) x3500) x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x1002 x3500) (d_OP__case_3 x3 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x3 x2 x3000 x3500 = case x2 of
     (C_ModuleHead x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x4 (nd_OP__case_2 x3 x2000 x3500) x3500))
     (C_Code x16) -> Curry_Prelude.d_OP_plus_plus x16 (d_C_unscan x3 x3500) x3500
     (C_Text x17) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_unscan x3 x3500)) x3500)
     (C_Letter x18) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.d_OP_plus_plus x18 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_unscan x3 x3500)) x3500)
     (C_BigComment x19) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x19 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_unscan x3 x3500) x3500) x3500) x3500
     (C_SmallComment x20) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x20 (d_C_unscan x3 x3500) x3500) x3500
     (C_Meta x21) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x21 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_unscan x3 x3500) x3500) x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x3 x1002 x3000 x3500) (nd_OP__case_3 x3 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x3 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_1 x3 x6 x5 x3500
     Curry_Prelude.OP_List -> d_C_unscan x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3500) (d_OP__case_2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x3 x6 x5 x2000 x3500))
     Curry_Prelude.OP_List -> d_C_unscan x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1002 x3000 x3500) (nd_OP__case_2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x3 x6 x5 x3500 = case x5 of
     (C_Code x7) -> d_OP__case_0 x3 x6 x7 x3500
     (C_SmallComment x10) -> d_C_unscan x3 x3500
     (C_BigComment x11) -> d_C_unscan x3 x3500
     (C_Text x12) -> d_C_unscan x3 x3500
     (C_Letter x13) -> d_C_unscan x3 x3500
     (C_ModuleHead x14) -> d_C_unscan x3 x3500
     (C_Meta x15) -> d_C_unscan x3 x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x6 x1002 x3500) (d_OP__case_1 x3 x6 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x6 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x6 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x3 x6 x5 x3000 x3500 = case x5 of
     (C_Code x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x3 x6 x7 x2000 x3500))
     (C_SmallComment x10) -> d_C_unscan x3 x3500
     (C_BigComment x11) -> d_C_unscan x3 x3500
     (C_Text x12) -> d_C_unscan x3 x3500
     (C_Letter x13) -> d_C_unscan x3 x3500
     (C_ModuleHead x14) -> d_C_unscan x3 x3500
     (C_Meta x15) -> d_C_unscan x3 x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x3 x6 x1002 x3000 x3500) (nd_OP__case_1 x3 x6 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x3 x6 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x3 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_C_unscan (Curry_Prelude.OP_Cons (C_Code x9) x6) x3500
     Curry_Prelude.OP_List -> d_C_unscan x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x6 x1002 x3500) (d_OP__case_0 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_C_unscan (Curry_Prelude.OP_Cons (C_Code x9) x6) x3500
     Curry_Prelude.OP_List -> d_C_unscan x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x6 x1002 x3000 x3500) (nd_OP__case_0 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x3 x2 x3500 = case x2 of
     (C_ModuleHead x4) -> d_OP__case_5 x4 x3 x3500
     (C_Code x14) -> Curry_Prelude.d_OP_plus_plus x14 (d_C_plainCode x3 x3500) x3500
     (C_Text x15) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_plainCode x3 x3500)) x3500)
     (C_Letter x16) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_plainCode x3 x3500)) x3500)
     (C_BigComment x17) -> d_C_plainCode x3 x3500
     (C_SmallComment x18) -> d_C_plainCode x3 x3500
     (C_Meta x19) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x19 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_plainCode x3 x3500) x3500) x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x3 x1002 x3500) (d_OP__case_6 x3 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x3 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x3 x2 x3000 x3500 = case x2 of
     (C_ModuleHead x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x4 x3 x2000 x3500))
     (C_Code x14) -> Curry_Prelude.d_OP_plus_plus x14 (d_C_plainCode x3 x3500) x3500
     (C_Text x15) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_plainCode x3 x3500)) x3500)
     (C_Letter x16) -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.d_OP_plus_plus x16 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_plainCode x3 x3500)) x3500)
     (C_BigComment x17) -> d_C_plainCode x3 x3500
     (C_SmallComment x18) -> d_C_plainCode x3 x3500
     (C_Meta x19) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x19 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) (d_C_plainCode x3 x3500) x3500) x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x3 x1002 x3000 x3500) (nd_OP__case_6 x3 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x3 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x4 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_4 x3 x4 x6 x5 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x4 x1002 x3500) (d_OP__case_5 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x4 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x3 x4 x6 x5 x2000 x3500))
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x4 x1002 x3000 x3500) (nd_OP__case_5 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x3 x4 x6 x5 x3500 = case x5 of
     (C_Code x7) -> Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 1#) x7 x3500) (d_C_plainCode x6 x3500) x3500) x3500
     (C_SmallComment x8) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_BigComment x9) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Text x10) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Letter x11) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_ModuleHead x12) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Meta x13) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x4 x6 x1002 x3500) (d_OP__case_4 x3 x4 x6 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 x4 x6 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x3 x4 x6 x5 x3000 x3500 = case x5 of
     (C_Code x7) -> Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 1#) x7 x3500) (d_C_plainCode x6 x3500) x3500) x3500
     (C_SmallComment x8) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_BigComment x9) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Text x10) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Letter x11) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_ModuleHead x12) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (C_Meta x13) -> Curry_Prelude.d_OP_plus_plus x4 (d_C_plainCode x3 x3500) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_4 x3 x4 x6 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x3 x4 x6 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_ModuleHead x1) (d_OP__case_8 x2 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x1002 x3500) (d_OP__case_9 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_ModuleHead x1) (nd_OP__case_8 x2 x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_7 x2 x4 x3 x3500
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1002 x3500) (d_OP__case_8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x2 x4 x3 x2000 x3500))
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1002 x3000 x3500) (nd_OP__case_8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x2 x4 x3 x3500 = case x3 of
     (C_Code x5) -> Curry_Prelude.OP_Cons (C_Code (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x5)) x4
     (C_SmallComment x6) -> x2
     (C_BigComment x7) -> x2
     (C_Text x8) -> x2
     (C_Letter x9) -> x2
     (C_ModuleHead x10) -> x2
     (C_Meta x11) -> x2
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x4 x1002 x3500) (d_OP__case_7 x2 x4 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 x4 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x2 x4 x3 x3000 x3500 = case x3 of
     (C_Code x5) -> Curry_Prelude.OP_Cons (C_Code (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x5)) x4
     (C_SmallComment x6) -> x2
     (C_BigComment x7) -> x2
     (C_Text x8) -> x2
     (C_Letter x9) -> x2
     (C_ModuleHead x10) -> x2
     (C_Meta x11) -> x2
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x2 x4 x1002 x3000 x3500) (nd_OP__case_7 x2 x4 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x2 x4 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> d_OP__case_10 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x2 x3 x1002 x3500) (d_OP__case_11 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_11 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 x2 x3500) (let
          x4 = Curry_Prelude.d_C_splitAt (Curry_Prelude.d_C_length x2 x3500) x1 x3500
          x5 = d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x4 x3500
          x6 = d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x4 x3500
           in (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 x5 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x6 x3500) (d_C_isSep (Curry_Prelude.d_C_head x6 x3500) x3500) x3500) x3500)) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x1002 x3500) (d_OP__case_10 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 x2 x3500) (let
          x4 = Curry_Prelude.d_C_splitAt (Curry_Prelude.d_C_length x2 x3500) x1 x3500
          x5 = d_OP_lineBeginsWith_dot___hash_selFP11_hash_s' x4 x3500
          x6 = d_OP_lineBeginsWith_dot___hash_selFP12_hash_rest x4 x3500
           in (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 x5 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x6 x3500) (d_C_isSep (Curry_Prelude.d_C_head x6 x3500) x3500) x3500) x3500)) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x4 x3 x3500 = case x3 of
     (C_Code x5) -> d_OP__case_13 x1 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break d_OP_modHeadInLine_dot___hash_lambda12 x3500) x5 x3500) x3500
     (C_BigComment x10) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_BigComment x10) (d_C_modHeadInLine Curry_Prelude.d_C_id x4 x3500)) x3500
     (C_SmallComment x11) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_SmallComment x11) (d_C_modHeadInLine Curry_Prelude.d_C_id x4 x3500)) x3500
     (C_Meta x12) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Meta x12) x4) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x4 x1002 x3500) (d_OP__case_14 x1 x4 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x4 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x4 x3 x3000 x3500 = case x3 of
     (C_Code x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_13 x1 x4 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapDX id d_OP_modHeadInLine_dot___hash_lambda12) x2000 x3500) x5 x2001 x3500)))) x2003 x3500)))))
     (C_BigComment x10) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_BigComment x10) (nd_C_modHeadInLine (wrapDX id Curry_Prelude.d_C_id) x4 x2001 x3500)) x3500)))))
     (C_SmallComment x11) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_SmallComment x11) (nd_C_modHeadInLine (wrapDX id Curry_Prelude.d_C_id) x4 x2001 x3500)) x3500)))))
     (C_Meta x12) -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_Meta x12) x4) x3500))
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x4 x1002 x3000 x3500) (nd_OP__case_14 x1 x4 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x4 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x4 x5 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_12 x1 x4 x5 x6 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x4 x5 x1002 x3500) (d_OP__case_13 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x4 x5 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x4 x5 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_13 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x4 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_C_modHead (Curry_Prelude.d_OP_dot x1 (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_plus_plus x6) (acceptCs id (Curry_Prelude.OP_Cons x8)) x3500) x3500) (Curry_Prelude.OP_Cons (C_Code x9) x4) x3500
     Curry_Prelude.OP_List -> d_C_modHead x1 (Curry_Prelude.OP_Cons (C_Code x5) x4) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x4 x5 x6 x1002 x3500) (d_OP__case_12 x1 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x4 x5 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_modHead (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot x1 (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_plus_plus x6)) (wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x8))) x2000 x3500) x2001 x3500)))) (Curry_Prelude.OP_Cons (C_Code x9) x4) x2003 x3500)))))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_modHead x1 (Curry_Prelude.OP_Cons (C_Code x5) x4) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_12 x1 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x4 x3 x3500 = case x3 of
     (C_Code x5) -> d_OP__case_20 x1 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break d_OP_modHead_dot___hash_lambda10 x3500) x5 x3500) x3500
     (C_BigComment x14) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_BigComment x14) (d_C_modHead Curry_Prelude.d_C_id x4 x3500)) x3500
     (C_SmallComment x15) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_SmallComment x15) (d_C_modHead Curry_Prelude.d_C_id x4 x3500)) x3500
     (C_Meta x16) -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Meta x16) x4) x3500
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x4 x1002 x3500) (d_OP__case_21 x1 x4 x1003 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x4 z x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x4 x3 x3000 x3500 = case x3 of
     (C_Code x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_20 x1 x4 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapDX id d_OP_modHead_dot___hash_lambda10) x2000 x3500) x5 x2001 x3500)))) x2003 x3500)))))
     (C_BigComment x14) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_BigComment x14) (nd_C_modHead (wrapDX id Curry_Prelude.d_C_id) x4 x2001 x3500)) x3500)))))
     (C_SmallComment x15) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_SmallComment x15) (nd_C_modHead (wrapDX id Curry_Prelude.d_C_id) x4 x2001 x3500)) x3500)))))
     (C_Meta x16) -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_Meta x16) x4) x3500))
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x4 x1002 x3000 x3500) (nd_OP__case_21 x1 x4 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x4 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x4 x5 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_19 x1 x4 x5 x7 x6 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x4 x5 x1002 x3500) (d_OP__case_20 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x4 x5 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x1 x4 x5 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_20 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x4 x5 x7 x6 x3500 = case x6 of
     Curry_Prelude.OP_List -> d_OP__case_18 x1 x4 x7 x3500
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_17 x1 x4 x5 x10 x11 x7 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x4 x5 x7 x1002 x3500) (d_OP__case_19 x1 x4 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x4 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x4 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x4 x5 x7 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x4 x7 x2000 x3500))
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x1 x4 x5 x10 x11 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x4 x5 x7 x1002 x3000 x3500) (nd_OP__case_19 x1 x4 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x4 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x4 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x4 x5 x10 x11 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_16 x1 x4 x5 x10 x11 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_C_lineBeginsWith (Curry_Prelude.OP_Cons x10 x11)) x3500) (d_C_headers x3500) x3500) x3500
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_C_lineBeginsWith (Curry_Prelude.OP_Cons x10 x11)) x3500) (d_C_headers x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x4 x5 x10 x11 x1002 x3500) (d_OP__case_17 x1 x4 x5 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x4 x5 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x4 x5 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x4 x5 x10 x11 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_16 x1 x4 x5 x10 x11 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_any (wrapDX id (d_C_lineBeginsWith (Curry_Prelude.OP_Cons x10 x11))) x2000 x3500) (d_C_headers x3500) x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_any (wrapDX id (d_C_lineBeginsWith (Curry_Prelude.OP_Cons x10 x11))) x2000 x3500) (d_C_headers x3500) x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x4 x5 x10 x11 x1002 x3000 x3500) (nd_OP__case_17 x1 x4 x5 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x4 x5 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x4 x5 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_modHead (Curry_Prelude.d_OP_dot x1 (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x10 x11)) (acceptCs id (Curry_Prelude.OP_Cons x12)) x3500) x3500) (Curry_Prelude.OP_Cons (C_Code x13) x4) x3500
     Curry_Prelude.C_False -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Code x5) x4) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1002 x3500) (d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_modHead (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot x1 (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x10 x11))) (wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x12))) x2000 x3500) x2001 x3500)))) (Curry_Prelude.OP_Cons (C_Code x13) x4) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_Code x5) x4) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x4 x5 x10 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x4 x5 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_ModuleHead (Curry_Prelude.d_C_apply x1 (Curry_Prelude.OP_Cons x10 x11) x3500)) (d_C_modHeadInLine Curry_Prelude.d_C_id x4 x3500)
     Curry_Prelude.C_False -> d_C_maybeMo (Curry_Prelude.d_C_apply x1 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Code x5) x4) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x4 x5 x10 x11 x1002 x3500) (d_OP__case_16 x1 x4 x5 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 x4 x5 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x4 x5 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x4 x5 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (C_ModuleHead (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.OP_Cons x10 x11) x2000 x3500)) (nd_C_modHeadInLine (wrapDX id Curry_Prelude.d_C_id) x4 x2001 x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (d_C_maybeMo (Curry_Prelude.nd_C_apply x1 Curry_Prelude.OP_List x2000 x3500) (Curry_Prelude.OP_Cons (C_Code x5) x4) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x4 x5 x10 x11 x1002 x3000 x3500) (nd_OP__case_16 x1 x4 x5 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 x4 x5 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x4 x5 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x4 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_C_modHead x1 x4 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> d_C_modHead (Curry_Prelude.d_OP_dot x1 (acceptCs id (Curry_Prelude.OP_Cons x8)) x3500) (Curry_Prelude.OP_Cons (C_Code x9) x4) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x4 x1002 x3500) (d_OP__case_18 x1 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x4 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_modHead x1 x4 x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_modHead (Curry_Prelude.nd_OP_dot x1 (wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x8))) x2000 x3500) (Curry_Prelude.OP_Cons (C_Code x9) x4) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x4 x1002 x3000 x3500) (nd_OP__case_18 x1 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_59 x1 x3 x5 x6 x2 x3000 x3500 = case x2 of
     (C_Code x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_58 x1 x3 x5 x7 x6 x2000 x3500))
     (C_Text x34) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x1 x3 x5 x34 x6 x2000 x3500))
     (C_BigComment x38) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x1 x3 x5 x38 x6 x2000 x3500))
     (C_Meta x42) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x3 x5 x42 x6 x2000 x3500))
     (Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_59 x1 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_59 x1 x3 x5 x6 x1003 x3000 x3500)
     (Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_59 x1 x3 x5 x6 z x3000 x3500) x1002
     (Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_59 x1 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x3 x5 x42 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0__case_25 (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x43 x44) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x45 = generate x2001
                     in (nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '+'#) x3500) (Curry_Prelude.d_OP_eq_eq x43 (Curry_Prelude.C_Char '}'#) x3500) x3500) x2000 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x3 x5 x42 x1002 x3000 x3500) (nd_OP__case_25 x1 x3 x5 x42 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x3 x5 x42 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x3 x5 x42 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0__case_25 x1 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_25 x1002 x3500) (d_OP___cond_0__case_25 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_25 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_25 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__case_25 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__case_25 x1002 x3000 x3500) (nd_OP___cond_0__case_25 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__case_25 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__case_25 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 x46 x3000 x3500 = case x46 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Meta x42) (nd_C_stateScan x1 (C_Code x45) x45 x44 x2000 x3500)) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '\n'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 x1002 x3000 x3500) (nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x3 x5 x42 x43 x44 x45 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 x46 x3000 x3500 = case x46 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x45) x3500) (nd_C_stateScan (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500) (C_Meta x42) x45 (Curry_Prelude.OP_Cons x43 x44) x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 x1002 x3000 x3500) (nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x3 x5 x42 x43 x44 x45 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 x46 x3000 x3500 = case x46 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x45) x3500) (nd_C_stateScan x1 (C_Meta x42) x45 (Curry_Prelude.OP_Cons x43 x44) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 x1002 x3000 x3500) (nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x3 x5 x42 x43 x44 x45 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x3 x5 x38 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0__case_29 (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x39 x40) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x41 = generate x2001
                     in (nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '-'#) x3500) (Curry_Prelude.d_OP_eq_eq x39 (Curry_Prelude.C_Char '}'#) x3500) x3500) x2000 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x3 x5 x38 x1002 x3000 x3500) (nd_OP__case_29 x1 x3 x5 x38 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x3 x5 x38 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x3 x5 x38 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0__case_29 x1 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_29 x1002 x3500) (d_OP___cond_0__case_29 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_29 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__case_29 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__case_29 x1002 x3000 x3500) (nd_OP___cond_0__case_29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__case_29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__case_29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 x42 x3000 x3500 = case x42 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_BigComment x38) (nd_C_stateScan x1 (C_Code x41) x41 x40 x2000 x3500)) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '\n'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 x1002 x3000 x3500) (nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x3 x5 x38 x39 x40 x41 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 x42 x3000 x3500 = case x42 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x41) x3500) (nd_C_stateScan (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500) (C_BigComment x38) x41 (Curry_Prelude.OP_Cons x39 x40) x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 x1002 x3000 x3500) (nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x3 x5 x38 x39 x40 x41 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 x42 x3000 x3500 = case x42 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x41) x3500) (nd_C_stateScan x1 (C_BigComment x38) x41 (Curry_Prelude.OP_Cons x39 x40) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 x1002 x3000 x3500) (nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x3 x5 x38 x39 x40 x41 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x3 x5 x34 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0__case_35 x5 x34 (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x35 x36) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x37 = generate x2001
                     in (nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '"'#) x3500) x2000 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x3 x5 x34 x1002 x3000 x3500) (nd_OP__case_35 x1 x3 x5 x34 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x3 x5 x34 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x3 x5 x34 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0__case_35 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_Success -> d_OP__case_34 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '"'#) x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_35 x1 x2 x1002 x3500) (d_OP___cond_0__case_35 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_35 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_35 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__case_35 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_Success -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '"'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__case_35 x1 x2 x1002 x3000 x3500) (nd_OP___cond_0__case_35 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__case_35 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__case_35 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 x38 x3000 x3500 = case x38 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (Curry_Prelude.OP_Cons (C_Text x34) (nd_C_stateScan x1 (C_Code x37) x37 (Curry_Prelude.OP_Cons x35 x36) x2000 x3500)) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '\\'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 x1002 x3000 x3500) (nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x3 x5 x34 x35 x36 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 x38 x3000 x3500 = case x38 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 (Curry_Prelude.OP_Cons x35 x37)) x3500) (nd_C_stateScan x1 (C_Text x34) x37 x36 x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem x5 x2000 x3500) (d_C_toBeEscaped x3500) x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 x1002 x3000 x3500) (nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x1 x3 x5 x34 x35 x36 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 x38 x3000 x3500 = case x38 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500)) x3500) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 x1002 x3000 x3500) (nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x3 x5 x34 x35 x36 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 x38 x3000 x3500 = case x38 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x37) x3500) (nd_C_stateScan x1 (C_Text x34) x37 (Curry_Prelude.OP_Cons x35 x36) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 x1002 x3000 x3500) (nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1 x3 x5 x34 x35 x36 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x5 x34 x35 x3500 = case x35 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_Text x34) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x5 x34 x1002 x3500) (d_OP__case_34 x5 x34 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x5 x34 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x5 x34 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x5 x34 x35 x3000 x3500 = case x35 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_Text x34) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x5 x34 x1002 x3000 x3500) (nd_OP__case_34 x5 x34 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x5 x34 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x5 x34 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_58 x1 x3 x5 x7 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0__case_58 x7 (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x10 = generate x2001
                     in (nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '"'#) x3500) x2000 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x1 x3 x5 x7 x1002 x3000 x3500) (nd_OP__case_58 x1 x3 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 x1 x3 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x1 x3 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0__case_58 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> d_C_maybeCode x1 Curry_Prelude.OP_List x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_58 x1 x1002 x3500) (d_OP___cond_0__case_58 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_58 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_58 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__case_58 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> d_C_maybeCode x1 Curry_Prelude.OP_List x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__case_58 x1 x1002 x3000 x3500) (nd_OP___cond_0__case_58 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__case_58 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__case_58 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (d_C_maybeCode x7 (nd_C_stateScan x1 (C_Text x10) x10 (Curry_Prelude.OP_Cons x8 x9) x2000 x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '-'#) x3500) (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '-'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x11 = Curry_Prelude.nd_C_span (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char '\n'#))) x9 x2000 x3500
                    x12 = d_OP_stateScan_dot___hash_selFP8_hash_comment x11 x3500
                    x13 = d_OP_stateScan_dot___hash_selFP9_hash_rest x11 x3500
                     in (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (d_C_maybeCode x7 (Curry_Prelude.OP_Cons (C_SmallComment x12) (nd_C_stateScan x1 (C_Code x10) x10 x13 x2001 x3500)) x3500) x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '{'#) x3500) (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '-'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (d_C_maybeCode x7 (nd_C_stateScan x1 (C_BigComment x10) x10 x9 x2000 x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '{'#) x3500) (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '+'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 Curry_Prelude.OP_List x3500) (d_C_maybeCode x7 (nd_C_stateScan x1 (C_Meta x10) x10 x9 x2000 x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char '\''#) x3500) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem x5 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_infixIDs x3500) (d_C_delimiters x3500) x3500) x2001 x3500)))) x3500) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_54 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id (d_C_maybeCode x7)) (nd_OP__case_52 x1 x10 x9 x2000 x3500) x2001 x3500)))) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '\n'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_53 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x10) x3500) (nd_C_stateScan (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500) (C_Code x7) x10 (Curry_Prelude.OP_Cons x8 x9) x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.OP_Cons x5 x10) x3500) (nd_C_stateScan x1 (C_Code x7) x10 (Curry_Prelude.OP_Cons x8 x9) x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x3 x5 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_52 x1 x10 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (let
               x16 = x14
                in (nd_OP__case_51 x1 x10 x14 x15 x16 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char '\\'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_52 x1 x10 x1002 x3000 x3500) (nd_OP__case_52 x1 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_52 x1 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_52 x1 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_51 x1 x10 x14 x15 x16 x17 x3000 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_50 x1 x10 x14 x15 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x1 x10 x16 x15 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x1 x10 x14 x15 x16 x1002 x3000 x3500) (nd_OP__case_51 x1 x10 x14 x15 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x1 x10 x14 x15 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x1 x10 x14 x15 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x10 x16 x15 x3000 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x31 x32) -> let
          x2000 = x3000
           in (seq x2000 (let
               x33 = x31
                in (nd_OP__case_38 x1 x10 x16 x32 x33 (Curry_Prelude.d_OP_eq_eq x33 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x10 x16 x1002 x3000 x3500) (nd_OP__case_39 x1 x10 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 x10 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x10 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x10 x16 x32 x33 x34 x3000 x3500 = case x34 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons x16 Curry_Prelude.OP_List)) (nd_C_stateScan x1 (C_Code x10) x10 x32 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x10 x16 x32 x33 x1002 x3000 x3500) (nd_OP__case_38 x1 x10 x16 x32 x33 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x10 x16 x32 x33 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x10 x16 x32 x33 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_50 x1 x10 x14 x15 x3000 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_49 x1 x10 x14 x17 x18 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_50 x1 x10 x14 x1002 x3000 x3500) (nd_OP__case_50 x1 x10 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_50 x1 x10 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_50 x1 x10 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_49 x1 x10 x14 x17 x18 x3000 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x19 x20) -> let
          x2000 = x3000
           in (seq x2000 (let
               x21 = x19
                in (nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 (Curry_Prelude.d_OP_eq_eq x21 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (let
               x30 = x17
                in (nd_OP__case_40 x1 x10 x14 x30 (Curry_Prelude.d_OP_eq_eq x30 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_49 x1 x10 x14 x17 x1002 x3000 x3500) (nd_OP__case_49 x1 x10 x14 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_49 x1 x10 x14 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_49 x1 x10 x14 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x10 x14 x30 x31 x3000 x3500 = case x31 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons x14 Curry_Prelude.OP_List)) (nd_C_stateScan x1 (C_Code x10) x10 Curry_Prelude.OP_List x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x10 x14 x30 x1002 x3000 x3500) (nd_OP__case_40 x1 x10 x14 x30 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x10 x14 x30 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x10 x14 x30 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 x22 x3000 x3500 = case x22 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons x17 Curry_Prelude.OP_List))) (nd_C_stateScan x1 (C_Code x10) x10 x20 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x1 x10 x14 x17 x18 x21 x20 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 x1002 x3000 x3500) (nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_48 x1 x10 x14 x17 x18 x20 x21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x1 x10 x14 x17 x18 x21 x20 x3000 x3500 = case x20 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 x23 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (let
               x29 = x17
                in (nd_OP__case_41 x1 x10 x14 x18 x29 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x1 x10 x14 x17 x18 x21 x1002 x3000 x3500) (nd_OP__case_47 x1 x10 x14 x17 x18 x21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x1 x10 x14 x17 x18 x21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x1 x10 x14 x17 x18 x21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x10 x14 x18 x29 x30 x3000 x3500 = case x30 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons x14 Curry_Prelude.OP_List)) (nd_C_stateScan x1 (C_Code x10) x10 x18 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x10 x14 x18 x29 x1002 x3000 x3500) (nd_OP__case_41 x1 x10 x14 x18 x29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x10 x14 x18 x29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x10 x14 x18 x29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 x23 x3000 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (let
               x26 = x24
                in (nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (let
               x28 = x17
                in (nd_OP__case_42 x1 x10 x14 x18 x28 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 x1002 x3000 x3500) (nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x1 x10 x14 x17 x18 x21 x22 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x10 x14 x18 x28 x29 x3000 x3500 = case x29 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons x14 Curry_Prelude.OP_List)) (nd_C_stateScan x1 (C_Code x10) x10 x18 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x10 x14 x18 x28 x1002 x3000 x3500) (nd_OP__case_42 x1 x10 x14 x18 x28 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x10 x14 x18 x28 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x10 x14 x18 x28 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 x28 x3000 x3500 = case x28 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_44 x1 x10 x17 x21 x22 x25 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_all (wrapDX id Curry_Char.d_C_isDigit) x2000 x3500) (Curry_Prelude.OP_Cons x17 (Curry_Prelude.OP_Cons x21 (Curry_Prelude.OP_Cons x22 Curry_Prelude.OP_List))) x2001 x3500)))) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x27 = x17
                in (nd_OP__case_43 x1 x10 x14 x18 x27 (Curry_Prelude.d_OP_eq_eq x27 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 x1002 x3000 x3500) (nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x1 x10 x14 x17 x18 x21 x22 x25 x26 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x1 x10 x14 x18 x27 x28 x3000 x3500 = case x28 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons x14 Curry_Prelude.OP_List)) (nd_C_stateScan x1 (C_Code x10) x10 x18 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x1 x10 x14 x18 x27 x1002 x3000 x3500) (nd_OP__case_43 x1 x10 x14 x18 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x1 x10 x14 x18 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x1 x10 x14 x18 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x1 x10 x17 x21 x22 x25 x26 x3000 x3500 = case x26 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (C_Letter (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons x17 (Curry_Prelude.OP_Cons x21 (Curry_Prelude.OP_Cons x22 Curry_Prelude.OP_List))))) (nd_C_stateScan x1 (C_Code x10) x10 x25 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x1 x10 x17 x21 x22 x25 x1002 x3000 x3500) (nd_OP__case_44 x1 x10 x17 x21 x22 x25 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x1 x10 x17 x21 x22 x25 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x1 x10 x17 x21 x22 x25 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_65 x3 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_64 x3 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_62 x3 x8 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x3 x1002 x3500) (d_OP__case_65 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_65 x3 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_64 x3 x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x3 x8 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_65 x3 x1002 x3000 x3500) (nd_OP__case_65 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_65 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_65 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_62 x3 x8 x9 x3500 = case x9 of
     Curry_Prelude.OP_List -> d_OP__case_61 x8 x9 x3 x3500
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_60 x8 x12 x13 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x3 x8 x1002 x3500) (d_OP__case_62 x3 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x3 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x3 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_62 x3 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_61 x8 x9 x3 x2000 x3500))
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x8 x12 x13 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_62 x3 x8 x1002 x3000 x3500) (nd_OP__case_62 x3 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_62 x3 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_62 x3 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_60 x8 x12 x13 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.OP_Cons x8 (Curry_Prelude.OP_Cons x14 (d_C_weave (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x12 x13) x15) x3500))
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x8 x12 x13 x1002 x3500) (d_OP__case_60 x8 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x8 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x8 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_60 x8 x12 x13 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x14 x15) -> Curry_Prelude.OP_Cons x8 (Curry_Prelude.OP_Cons x14 (d_C_weave (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x12 x13) x15) x3500))
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_60 x8 x12 x13 x1002 x3000 x3500) (nd_OP__case_60 x8 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_60 x8 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_60 x8 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_61 x8 x9 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x8 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x10 x11) -> Curry_Prelude.OP_Cons x8 (Curry_Prelude.OP_Cons x10 (d_C_weave (Curry_Prelude.OP_Tuple2 x9 x11) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x8 x9 x1002 x3500) (d_OP__case_61 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_61 x8 x9 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x8 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x10 x11) -> Curry_Prelude.OP_Cons x8 (Curry_Prelude.OP_Cons x10 (d_C_weave (Curry_Prelude.OP_Tuple2 x9 x11) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_61 x8 x9 x1002 x3000 x3500) (nd_OP__case_61 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_61 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_61 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_64 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_63 x4 x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x1002 x3500) (d_OP__case_64 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_64 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_63 x4 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_64 x1002 x3000 x3500) (nd_OP__case_64 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_64 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_64 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_63 x4 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x4 x1002 x3500) (d_OP__case_63 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_63 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_63 x4 x1002 x3000 x3500) (nd_OP__case_63 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_63 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_63 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_66 x2 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x5) x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons x2 x6)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x2 x5 x6 x1002 x3500) (d_OP__case_66 x2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_66 x2 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x5) x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons x2 x6)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_66 x2 x5 x6 x1002 x3000 x3500) (nd_OP__case_66 x2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_66 x2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_66 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
