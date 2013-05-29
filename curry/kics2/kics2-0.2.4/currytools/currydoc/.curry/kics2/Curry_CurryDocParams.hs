{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryDocParams (C_DocType (..), C_DocParams (..), d_C_docType, d_C_setDocType, d_C_withIndex, d_C_setIndex, d_C_withMarkdown, d_C_setMarkDown, d_C_defaultCurryDocParams) where

import Basics
import qualified Curry_Prelude
import qualified Curry_System
data C_DocType
     = C_HtmlDoc
     | C_TexDoc
     | C_CDoc
     | Choice_C_DocType Cover ID C_DocType C_DocType
     | Choices_C_DocType Cover ID ([C_DocType])
     | Fail_C_DocType Cover FailInfo
     | Guard_C_DocType Cover Constraints C_DocType

instance Show C_DocType where
  showsPrec d (Choice_C_DocType cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_DocType cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_DocType cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_DocType cd info) = showChar '!'
  showsPrec _ C_HtmlDoc = showString "HtmlDoc"
  showsPrec _ C_TexDoc = showString "TexDoc"
  showsPrec _ C_CDoc = showString "CDoc"


instance Read C_DocType where
  readsPrec _ s = (readParen False (\r -> [ (C_HtmlDoc,r0) | (_,r0) <- readQualified "CurryDocParams" "HtmlDoc" r]) s) ++ ((readParen False (\r -> [ (C_TexDoc,r0) | (_,r0) <- readQualified "CurryDocParams" "TexDoc" r]) s) ++ (readParen False (\r -> [ (C_CDoc,r0) | (_,r0) <- readQualified "CurryDocParams" "CDoc" r]) s))


instance NonDet C_DocType where
  choiceCons = Choice_C_DocType
  choicesCons = Choices_C_DocType
  failCons = Fail_C_DocType
  guardCons = Guard_C_DocType
  try (Choice_C_DocType cd i x y) = tryChoice cd i x y
  try (Choices_C_DocType cd i xs) = tryChoices cd i xs
  try (Fail_C_DocType cd info) = Fail cd info
  try (Guard_C_DocType cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_DocType cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_DocType cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_DocType cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_DocType cd i _) = error ("CurryDocParams.DocType.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_DocType cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_DocType cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_DocType where
  generate s = Choices_C_DocType defCover (freeID [0,0,0] s) [C_HtmlDoc,C_TexDoc,C_CDoc]


instance NormalForm C_DocType where
  ($!!) cont C_HtmlDoc cs = cont C_HtmlDoc cs
  ($!!) cont C_TexDoc cs = cont C_TexDoc cs
  ($!!) cont C_CDoc cs = cont C_CDoc cs
  ($!!) cont (Choice_C_DocType cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_DocType cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_DocType cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_DocType cd info) _ = failCons cd info
  ($##) cont C_HtmlDoc cs = cont C_HtmlDoc cs
  ($##) cont C_TexDoc cs = cont C_TexDoc cs
  ($##) cont C_CDoc cs = cont C_CDoc cs
  ($##) cont (Choice_C_DocType cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_DocType cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_DocType cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_DocType cd info) _ = failCons cd info
  searchNF _ cont C_HtmlDoc = cont C_HtmlDoc
  searchNF _ cont C_TexDoc = cont C_TexDoc
  searchNF _ cont C_CDoc = cont C_CDoc
  searchNF _ _ x = error ("CurryDocParams.DocType.searchNF: no constructor: " ++ (show x))


instance Unifiable C_DocType where
  (=.=) C_HtmlDoc C_HtmlDoc cs = C_Success
  (=.=) C_TexDoc C_TexDoc cs = C_Success
  (=.=) C_CDoc C_CDoc cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_HtmlDoc C_HtmlDoc cs = C_Success
  (=.<=) C_TexDoc C_TexDoc cs = C_Success
  (=.<=) C_CDoc C_CDoc cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_HtmlDoc = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_TexDoc = ((i :=: (ChooseN 1 0)):(concat []))
  bind i C_CDoc = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (Choice_C_DocType cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_DocType cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_DocType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_DocType cd i _) = error ("CurryDocParams.DocType.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_DocType cd info) = [(Unsolvable info)]
  bind i (Guard_C_DocType cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_HtmlDoc = [(i :=: (ChooseN 0 0))]
  lazyBind i C_TexDoc = [(i :=: (ChooseN 1 0))]
  lazyBind i C_CDoc = [(i :=: (ChooseN 2 0))]
  lazyBind i (Choice_C_DocType cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_DocType cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_DocType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_DocType cd i _) = error ("CurryDocParams.DocType.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_DocType cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_DocType cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_DocType where
  (=?=) (Choice_C_DocType cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_DocType cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_DocType cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_DocType cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_DocType cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_DocType cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_DocType cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_DocType cd info) _ = failCons cd info
  (=?=) C_HtmlDoc C_HtmlDoc cs = Curry_Prelude.C_True
  (=?=) C_TexDoc C_TexDoc cs = Curry_Prelude.C_True
  (=?=) C_CDoc C_CDoc cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_DocType cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_DocType cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_DocType cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_DocType cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_DocType cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_DocType cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_DocType cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_DocType cd info) _ = failCons cd info
  (<?=) C_HtmlDoc C_HtmlDoc cs = Curry_Prelude.C_True
  (<?=) C_HtmlDoc C_TexDoc _ = Curry_Prelude.C_True
  (<?=) C_HtmlDoc C_CDoc _ = Curry_Prelude.C_True
  (<?=) C_TexDoc C_TexDoc cs = Curry_Prelude.C_True
  (<?=) C_TexDoc C_CDoc _ = Curry_Prelude.C_True
  (<?=) C_CDoc C_CDoc cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_DocType where
  cover C_HtmlDoc = C_HtmlDoc
  cover C_TexDoc = C_TexDoc
  cover C_CDoc = C_CDoc
  cover (Choice_C_DocType cd i x y) = Choice_C_DocType (incCover cd) i (cover x) (cover y)
  cover (Choices_C_DocType cd i xs) = Choices_C_DocType (incCover cd) i (map cover xs)
  cover (Fail_C_DocType cd info) = Fail_C_DocType (incCover cd) info
  cover (Guard_C_DocType cd c e) = Guard_C_DocType (incCover cd) c (cover e)


data C_DocParams
     = C_DocParams C_DocType Curry_Prelude.C_Bool Curry_Prelude.C_Bool
     | Choice_C_DocParams Cover ID C_DocParams C_DocParams
     | Choices_C_DocParams Cover ID ([C_DocParams])
     | Fail_C_DocParams Cover FailInfo
     | Guard_C_DocParams Cover Constraints C_DocParams

instance Show C_DocParams where
  showsPrec d (Choice_C_DocParams cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_DocParams cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_DocParams cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_DocParams cd info) = showChar '!'
  showsPrec _ (C_DocParams x1 x2 x3) = (showString "(DocParams") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_DocParams where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_DocParams x1 x2 x3,r3) | (_,r0) <- readQualified "CurryDocParams" "DocParams" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_DocParams where
  choiceCons = Choice_C_DocParams
  choicesCons = Choices_C_DocParams
  failCons = Fail_C_DocParams
  guardCons = Guard_C_DocParams
  try (Choice_C_DocParams cd i x y) = tryChoice cd i x y
  try (Choices_C_DocParams cd i xs) = tryChoices cd i xs
  try (Fail_C_DocParams cd info) = Fail cd info
  try (Guard_C_DocParams cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_DocParams cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_DocParams cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_DocParams cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_DocParams cd i _) = error ("CurryDocParams.DocParams.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_DocParams cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_DocParams cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_DocParams where
  generate s = Choices_C_DocParams defCover (freeID [3] s) [(C_DocParams (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_DocParams where
  ($!!) cont (C_DocParams x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DocParams y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_DocParams cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_DocParams cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_DocParams cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_DocParams cd info) _ = failCons cd info
  ($##) cont (C_DocParams x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DocParams y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_DocParams cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_DocParams cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_DocParams cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_DocParams cd info) _ = failCons cd info
  searchNF search cont (C_DocParams x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_DocParams y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("CurryDocParams.DocParams.searchNF: no constructor: " ++ (show x))


instance Unifiable C_DocParams where
  (=.=) (C_DocParams x1 x2 x3) (C_DocParams y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_DocParams x1 x2 x3) (C_DocParams y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_DocParams x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_DocParams cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_DocParams cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_DocParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_DocParams cd i _) = error ("CurryDocParams.DocParams.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_DocParams cd info) = [(Unsolvable info)]
  bind i (Guard_C_DocParams cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_DocParams x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_DocParams cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_DocParams cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_DocParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_DocParams cd i _) = error ("CurryDocParams.DocParams.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_DocParams cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_DocParams cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_DocParams where
  (=?=) (Choice_C_DocParams cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_DocParams cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_DocParams cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_DocParams cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_DocParams cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_DocParams cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_DocParams cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_DocParams cd info) _ = failCons cd info
  (=?=) (C_DocParams x1 x2 x3) (C_DocParams y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_DocParams cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_DocParams cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_DocParams cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_DocParams cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_DocParams cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_DocParams cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_DocParams cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_DocParams cd info) _ = failCons cd info
  (<?=) (C_DocParams x1 x2 x3) (C_DocParams y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_DocParams where
  cover (C_DocParams x1 x2 x3) = C_DocParams (cover x1) (cover x2) (cover x3)
  cover (Choice_C_DocParams cd i x y) = Choice_C_DocParams (incCover cd) i (cover x) (cover y)
  cover (Choices_C_DocParams cd i xs) = Choices_C_DocParams (incCover cd) i (map cover xs)
  cover (Fail_C_DocParams cd info) = Fail_C_DocParams (incCover cd) info
  cover (Guard_C_DocParams cd c e) = Guard_C_DocParams (incCover cd) c (cover e)


d_C_docType :: C_DocParams -> ConstStore -> C_DocType
d_C_docType x1 x3500 = case x1 of
     (C_DocParams x2 x3 x4) -> x2
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_docType x1002 x3500) (d_C_docType x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_docType z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_docType x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setDocType :: C_DocType -> C_DocParams -> ConstStore -> C_DocParams
d_C_setDocType x1 x2 x3500 = case x2 of
     (C_DocParams x3 x4 x5) -> C_DocParams x1 (Curry_Prelude.d_OP_eq_eq x1 C_HtmlDoc x3500) x5
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setDocType x1 x1002 x3500) (d_C_setDocType x1 x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setDocType x1 z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setDocType x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_withIndex :: C_DocParams -> ConstStore -> Curry_Prelude.C_Bool
d_C_withIndex x1 x3500 = case x1 of
     (C_DocParams x2 x3 x4) -> x3
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_withIndex x1002 x3500) (d_C_withIndex x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_withIndex z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_withIndex x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setIndex :: Curry_Prelude.C_Bool -> C_DocParams -> ConstStore -> C_DocParams
d_C_setIndex x1 x2 x3500 = case x2 of
     (C_DocParams x3 x4 x5) -> C_DocParams x3 x1 x5
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setIndex x1 x1002 x3500) (d_C_setIndex x1 x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setIndex x1 z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setIndex x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_withMarkdown :: C_DocParams -> ConstStore -> Curry_Prelude.C_Bool
d_C_withMarkdown x1 x3500 = case x1 of
     (C_DocParams x2 x3 x4) -> x4
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_withMarkdown x1002 x3500) (d_C_withMarkdown x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_withMarkdown z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_withMarkdown x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setMarkDown :: Curry_Prelude.C_Bool -> C_DocParams -> ConstStore -> C_DocParams
d_C_setMarkDown x1 x2 x3500 = case x2 of
     (C_DocParams x3 x4 x5) -> C_DocParams x3 x4 x1
     (Choice_C_DocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_setMarkDown x1 x1002 x3500) (d_C_setMarkDown x1 x1003 x3500)
     (Choices_C_DocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_setMarkDown x1 z x3500) x1002
     (Guard_C_DocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_setMarkDown x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_DocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_defaultCurryDocParams :: ConstStore -> C_DocParams
d_C_defaultCurryDocParams x3500 = C_DocParams C_HtmlDoc Curry_Prelude.C_True Curry_Prelude.C_True
