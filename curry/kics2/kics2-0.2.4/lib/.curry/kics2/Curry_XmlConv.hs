{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_XmlConv (C_XmlReads, C_XmlShows, C_XElemConv, C_XAttrConv, C_XPrimConv, C_XOptConv, C_XRepConv, C_Repeatable, C_NotRepeatable, C_Elem, C_NoElem, C_XmlConv, d_C_xmlReads, nd_C_xmlReads, d_C_xmlShows, nd_C_xmlShows, d_C_xmlRead, nd_C_xmlRead, d_C_xmlShow, nd_C_xmlShow, d_C_int, nd_C_int, d_C_float, nd_C_float, d_C_char, nd_C_char, nd_C_string, nd_OP_bang, d_C_element, nd_C_element, d_C_empty, nd_C_empty, d_C_attr, nd_C_attr, d_C_adapt, nd_C_adapt, nd_C_opt, nd_C_rep, d_C_aInt, nd_C_aInt, d_C_aFloat, nd_C_aFloat, d_C_aChar, nd_C_aChar, d_C_aString, nd_C_aString, d_C_aBool, nd_C_aBool, d_C_eInt, nd_C_eInt, d_C_eFloat, nd_C_eFloat, d_C_eChar, nd_C_eChar, nd_C_eString, nd_C_eBool, d_C_eEmpty, nd_C_eEmpty, nd_C_eOpt, nd_C_eRep, nd_C_seq1, nd_C_repSeq1, nd_C_eSeq1, nd_C_eRepSeq1, nd_C_seq2, nd_C_repSeq2, nd_C_eSeq2, nd_C_eRepSeq2, nd_C_seq3, nd_C_repSeq3, nd_C_eSeq3, nd_C_eRepSeq3, nd_C_seq4, nd_C_repSeq4, nd_C_eSeq4, nd_C_eRepSeq4, nd_C_seq5, nd_C_repSeq5, nd_C_eSeq5, nd_C_eRepSeq5, nd_C_seq6, nd_C_repSeq6, nd_C_eSeq6, nd_C_eRepSeq6) where

import Basics
import qualified Curry_Prelude
import qualified Curry_Read
import qualified Curry_ReadShowTerm
import qualified Curry_XML
type C_Attrs = Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))

type C_Childs = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)

type C_XmlReads t0 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))

type C_XmlShows t0 = t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)

type C_ValConv t0 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t0) (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_XElemConv t0 = C_XmlConv C_Repeatable C_Elem t0

type C_XAttrConv t0 = C_XmlConv C_NotRepeatable C_NoElem t0

type C_XPrimConv t0 = C_XmlConv C_NotRepeatable C_NoElem t0

type C_XOptConv t0 = C_XmlConv C_NotRepeatable C_NoElem t0

type C_XRepConv t0 = C_XmlConv C_NotRepeatable C_NoElem t0

type C_XSeqConv t0 = C_XmlConv C_NotRepeatable C_NoElem t0

data C_Repeatable
     = C_Repeatable
     | Choice_C_Repeatable Cover ID C_Repeatable C_Repeatable
     | Choices_C_Repeatable Cover ID ([C_Repeatable])
     | Fail_C_Repeatable Cover FailInfo
     | Guard_C_Repeatable Cover Constraints C_Repeatable

instance Show C_Repeatable where
  showsPrec d (Choice_C_Repeatable cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Repeatable cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Repeatable cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Repeatable cd info) = showChar '!'
  showsPrec _ C_Repeatable = showString "Repeatable"


instance Read C_Repeatable where
  readsPrec _ s = readParen False (\r -> [ (C_Repeatable,r0) | (_,r0) <- readQualified "XmlConv" "Repeatable" r]) s


instance NonDet C_Repeatable where
  choiceCons = Choice_C_Repeatable
  choicesCons = Choices_C_Repeatable
  failCons = Fail_C_Repeatable
  guardCons = Guard_C_Repeatable
  try (Choice_C_Repeatable cd i x y) = tryChoice cd i x y
  try (Choices_C_Repeatable cd i xs) = tryChoices cd i xs
  try (Fail_C_Repeatable cd info) = Fail cd info
  try (Guard_C_Repeatable cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Repeatable cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Repeatable cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Repeatable cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Repeatable cd i _) = error ("XmlConv.Repeatable.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Repeatable cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Repeatable cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Repeatable where
  generate s = Choices_C_Repeatable defCover (freeID [0] s) [C_Repeatable]


instance NormalForm C_Repeatable where
  ($!!) cont C_Repeatable cs = cont C_Repeatable cs
  ($!!) cont (Choice_C_Repeatable cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Repeatable cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Repeatable cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Repeatable cd info) _ = failCons cd info
  ($##) cont C_Repeatable cs = cont C_Repeatable cs
  ($##) cont (Choice_C_Repeatable cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Repeatable cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Repeatable cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Repeatable cd info) _ = failCons cd info
  searchNF _ cont C_Repeatable = cont C_Repeatable
  searchNF _ _ x = error ("XmlConv.Repeatable.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Repeatable where
  (=.=) C_Repeatable C_Repeatable cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Repeatable C_Repeatable cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Repeatable = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Repeatable cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Repeatable cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Repeatable cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Repeatable cd i _) = error ("XmlConv.Repeatable.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Repeatable cd info) = [(Unsolvable info)]
  bind i (Guard_C_Repeatable cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Repeatable = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Repeatable cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Repeatable cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Repeatable cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Repeatable cd i _) = error ("XmlConv.Repeatable.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Repeatable cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Repeatable cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Repeatable where
  (=?=) (Choice_C_Repeatable cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Repeatable cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Repeatable cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Repeatable cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Repeatable cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Repeatable cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Repeatable cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Repeatable cd info) _ = failCons cd info
  (=?=) C_Repeatable C_Repeatable cs = Curry_Prelude.C_True
  (<?=) (Choice_C_Repeatable cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Repeatable cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Repeatable cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Repeatable cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Repeatable cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Repeatable cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Repeatable cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Repeatable cd info) _ = failCons cd info
  (<?=) C_Repeatable C_Repeatable cs = Curry_Prelude.C_True


instance Coverable C_Repeatable where
  cover C_Repeatable = C_Repeatable
  cover (Choice_C_Repeatable cd i x y) = Choice_C_Repeatable (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Repeatable cd i xs) = Choices_C_Repeatable (incCover cd) i (map cover xs)
  cover (Fail_C_Repeatable cd info) = Fail_C_Repeatable (incCover cd) info
  cover (Guard_C_Repeatable cd c e) = Guard_C_Repeatable (incCover cd) c (cover e)


data C_NotRepeatable
     = C_NotRepeatable
     | Choice_C_NotRepeatable Cover ID C_NotRepeatable C_NotRepeatable
     | Choices_C_NotRepeatable Cover ID ([C_NotRepeatable])
     | Fail_C_NotRepeatable Cover FailInfo
     | Guard_C_NotRepeatable Cover Constraints C_NotRepeatable

instance Show C_NotRepeatable where
  showsPrec d (Choice_C_NotRepeatable cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_NotRepeatable cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_NotRepeatable cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_NotRepeatable cd info) = showChar '!'
  showsPrec _ C_NotRepeatable = showString "NotRepeatable"


instance Read C_NotRepeatable where
  readsPrec _ s = readParen False (\r -> [ (C_NotRepeatable,r0) | (_,r0) <- readQualified "XmlConv" "NotRepeatable" r]) s


instance NonDet C_NotRepeatable where
  choiceCons = Choice_C_NotRepeatable
  choicesCons = Choices_C_NotRepeatable
  failCons = Fail_C_NotRepeatable
  guardCons = Guard_C_NotRepeatable
  try (Choice_C_NotRepeatable cd i x y) = tryChoice cd i x y
  try (Choices_C_NotRepeatable cd i xs) = tryChoices cd i xs
  try (Fail_C_NotRepeatable cd info) = Fail cd info
  try (Guard_C_NotRepeatable cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_NotRepeatable cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_NotRepeatable cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_NotRepeatable cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_NotRepeatable cd i _) = error ("XmlConv.NotRepeatable.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_NotRepeatable cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_NotRepeatable cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_NotRepeatable where
  generate s = Choices_C_NotRepeatable defCover (freeID [0] s) [C_NotRepeatable]


instance NormalForm C_NotRepeatable where
  ($!!) cont C_NotRepeatable cs = cont C_NotRepeatable cs
  ($!!) cont (Choice_C_NotRepeatable cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_NotRepeatable cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_NotRepeatable cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_NotRepeatable cd info) _ = failCons cd info
  ($##) cont C_NotRepeatable cs = cont C_NotRepeatable cs
  ($##) cont (Choice_C_NotRepeatable cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_NotRepeatable cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_NotRepeatable cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_NotRepeatable cd info) _ = failCons cd info
  searchNF _ cont C_NotRepeatable = cont C_NotRepeatable
  searchNF _ _ x = error ("XmlConv.NotRepeatable.searchNF: no constructor: " ++ (show x))


instance Unifiable C_NotRepeatable where
  (=.=) C_NotRepeatable C_NotRepeatable cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_NotRepeatable C_NotRepeatable cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_NotRepeatable = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_NotRepeatable cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_NotRepeatable cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_NotRepeatable cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_NotRepeatable cd i _) = error ("XmlConv.NotRepeatable.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_NotRepeatable cd info) = [(Unsolvable info)]
  bind i (Guard_C_NotRepeatable cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_NotRepeatable = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_NotRepeatable cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_NotRepeatable cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_NotRepeatable cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_NotRepeatable cd i _) = error ("XmlConv.NotRepeatable.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_NotRepeatable cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_NotRepeatable cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_NotRepeatable where
  (=?=) (Choice_C_NotRepeatable cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_NotRepeatable cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_NotRepeatable cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_NotRepeatable cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_NotRepeatable cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_NotRepeatable cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_NotRepeatable cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_NotRepeatable cd info) _ = failCons cd info
  (=?=) C_NotRepeatable C_NotRepeatable cs = Curry_Prelude.C_True
  (<?=) (Choice_C_NotRepeatable cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_NotRepeatable cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_NotRepeatable cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_NotRepeatable cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_NotRepeatable cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_NotRepeatable cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_NotRepeatable cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_NotRepeatable cd info) _ = failCons cd info
  (<?=) C_NotRepeatable C_NotRepeatable cs = Curry_Prelude.C_True


instance Coverable C_NotRepeatable where
  cover C_NotRepeatable = C_NotRepeatable
  cover (Choice_C_NotRepeatable cd i x y) = Choice_C_NotRepeatable (incCover cd) i (cover x) (cover y)
  cover (Choices_C_NotRepeatable cd i xs) = Choices_C_NotRepeatable (incCover cd) i (map cover xs)
  cover (Fail_C_NotRepeatable cd info) = Fail_C_NotRepeatable (incCover cd) info
  cover (Guard_C_NotRepeatable cd c e) = Guard_C_NotRepeatable (incCover cd) c (cover e)


data C_Elem
     = C_Elem
     | Choice_C_Elem Cover ID C_Elem C_Elem
     | Choices_C_Elem Cover ID ([C_Elem])
     | Fail_C_Elem Cover FailInfo
     | Guard_C_Elem Cover Constraints C_Elem

instance Show C_Elem where
  showsPrec d (Choice_C_Elem cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Elem cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Elem cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Elem cd info) = showChar '!'
  showsPrec _ C_Elem = showString "Elem"


instance Read C_Elem where
  readsPrec _ s = readParen False (\r -> [ (C_Elem,r0) | (_,r0) <- readQualified "XmlConv" "Elem" r]) s


instance NonDet C_Elem where
  choiceCons = Choice_C_Elem
  choicesCons = Choices_C_Elem
  failCons = Fail_C_Elem
  guardCons = Guard_C_Elem
  try (Choice_C_Elem cd i x y) = tryChoice cd i x y
  try (Choices_C_Elem cd i xs) = tryChoices cd i xs
  try (Fail_C_Elem cd info) = Fail cd info
  try (Guard_C_Elem cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Elem cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Elem cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Elem cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Elem cd i _) = error ("XmlConv.Elem.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Elem cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Elem cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Elem where
  generate s = Choices_C_Elem defCover (freeID [0] s) [C_Elem]


instance NormalForm C_Elem where
  ($!!) cont C_Elem cs = cont C_Elem cs
  ($!!) cont (Choice_C_Elem cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Elem cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Elem cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Elem cd info) _ = failCons cd info
  ($##) cont C_Elem cs = cont C_Elem cs
  ($##) cont (Choice_C_Elem cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Elem cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Elem cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Elem cd info) _ = failCons cd info
  searchNF _ cont C_Elem = cont C_Elem
  searchNF _ _ x = error ("XmlConv.Elem.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Elem where
  (=.=) C_Elem C_Elem cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Elem C_Elem cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Elem = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Elem cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Elem cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Elem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Elem cd i _) = error ("XmlConv.Elem.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Elem cd info) = [(Unsolvable info)]
  bind i (Guard_C_Elem cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Elem = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Elem cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Elem cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Elem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Elem cd i _) = error ("XmlConv.Elem.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Elem cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Elem cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Elem where
  (=?=) (Choice_C_Elem cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Elem cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Elem cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Elem cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Elem cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Elem cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Elem cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Elem cd info) _ = failCons cd info
  (=?=) C_Elem C_Elem cs = Curry_Prelude.C_True
  (<?=) (Choice_C_Elem cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Elem cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Elem cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Elem cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Elem cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Elem cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Elem cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Elem cd info) _ = failCons cd info
  (<?=) C_Elem C_Elem cs = Curry_Prelude.C_True


instance Coverable C_Elem where
  cover C_Elem = C_Elem
  cover (Choice_C_Elem cd i x y) = Choice_C_Elem (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Elem cd i xs) = Choices_C_Elem (incCover cd) i (map cover xs)
  cover (Fail_C_Elem cd info) = Fail_C_Elem (incCover cd) info
  cover (Guard_C_Elem cd c e) = Guard_C_Elem (incCover cd) c (cover e)


data C_NoElem
     = C_NoElem
     | Choice_C_NoElem Cover ID C_NoElem C_NoElem
     | Choices_C_NoElem Cover ID ([C_NoElem])
     | Fail_C_NoElem Cover FailInfo
     | Guard_C_NoElem Cover Constraints C_NoElem

instance Show C_NoElem where
  showsPrec d (Choice_C_NoElem cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_NoElem cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_NoElem cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_NoElem cd info) = showChar '!'
  showsPrec _ C_NoElem = showString "NoElem"


instance Read C_NoElem where
  readsPrec _ s = readParen False (\r -> [ (C_NoElem,r0) | (_,r0) <- readQualified "XmlConv" "NoElem" r]) s


instance NonDet C_NoElem where
  choiceCons = Choice_C_NoElem
  choicesCons = Choices_C_NoElem
  failCons = Fail_C_NoElem
  guardCons = Guard_C_NoElem
  try (Choice_C_NoElem cd i x y) = tryChoice cd i x y
  try (Choices_C_NoElem cd i xs) = tryChoices cd i xs
  try (Fail_C_NoElem cd info) = Fail cd info
  try (Guard_C_NoElem cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_NoElem cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_NoElem cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_NoElem cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_NoElem cd i _) = error ("XmlConv.NoElem.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_NoElem cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_NoElem cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_NoElem where
  generate s = Choices_C_NoElem defCover (freeID [0] s) [C_NoElem]


instance NormalForm C_NoElem where
  ($!!) cont C_NoElem cs = cont C_NoElem cs
  ($!!) cont (Choice_C_NoElem cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_NoElem cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_NoElem cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_NoElem cd info) _ = failCons cd info
  ($##) cont C_NoElem cs = cont C_NoElem cs
  ($##) cont (Choice_C_NoElem cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_NoElem cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_NoElem cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_NoElem cd info) _ = failCons cd info
  searchNF _ cont C_NoElem = cont C_NoElem
  searchNF _ _ x = error ("XmlConv.NoElem.searchNF: no constructor: " ++ (show x))


instance Unifiable C_NoElem where
  (=.=) C_NoElem C_NoElem cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_NoElem C_NoElem cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_NoElem = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_NoElem cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_NoElem cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_NoElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_NoElem cd i _) = error ("XmlConv.NoElem.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_NoElem cd info) = [(Unsolvable info)]
  bind i (Guard_C_NoElem cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_NoElem = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_NoElem cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_NoElem cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_NoElem cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_NoElem cd i _) = error ("XmlConv.NoElem.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_NoElem cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_NoElem cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_NoElem where
  (=?=) (Choice_C_NoElem cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_NoElem cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_NoElem cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_NoElem cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_NoElem cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_NoElem cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_NoElem cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_NoElem cd info) _ = failCons cd info
  (=?=) C_NoElem C_NoElem cs = Curry_Prelude.C_True
  (<?=) (Choice_C_NoElem cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_NoElem cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_NoElem cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_NoElem cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_NoElem cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_NoElem cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_NoElem cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_NoElem cd info) _ = failCons cd info
  (<?=) C_NoElem C_NoElem cs = Curry_Prelude.C_True


instance Coverable C_NoElem where
  cover C_NoElem = C_NoElem
  cover (Choice_C_NoElem cd i x y) = Choice_C_NoElem (incCover cd) i (cover x) (cover y)
  cover (Choices_C_NoElem cd i xs) = Choices_C_NoElem (incCover cd) i (map cover xs)
  cover (Fail_C_NoElem cd info) = Fail_C_NoElem (incCover cd) info
  cover (Guard_C_NoElem cd c e) = Guard_C_NoElem (incCover cd) c (cover e)


data C_XmlConv t0 t1 t2
     = C_Conv (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) (t2 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
     | HO_C_Conv (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))) (Func t2 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))))
     | Choice_C_XmlConv Cover ID (C_XmlConv t0 t1 t2) (C_XmlConv t0 t1 t2)
     | Choices_C_XmlConv Cover ID ([C_XmlConv t0 t1 t2])
     | Fail_C_XmlConv Cover FailInfo
     | Guard_C_XmlConv Cover Constraints (C_XmlConv t0 t1 t2)

instance (Show t0,Show t1,Show t2) => Show (C_XmlConv t0 t1 t2) where
  showsPrec d (Choice_C_XmlConv cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_XmlConv cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_XmlConv cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_XmlConv cd info) = showChar '!'
  showsPrec _ (C_Conv x1 x2) = (showString "(Conv") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_Conv x1 x2) = (showString "(Conv") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance (Read t0,Read t1,Read t2) => Read (C_XmlConv t0 t1 t2) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Conv x1 x2,r2) | (_,r0) <- readQualified "XmlConv" "Conv" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet (C_XmlConv t0 t1 t2) where
  choiceCons = Choice_C_XmlConv
  choicesCons = Choices_C_XmlConv
  failCons = Fail_C_XmlConv
  guardCons = Guard_C_XmlConv
  try (Choice_C_XmlConv cd i x y) = tryChoice cd i x y
  try (Choices_C_XmlConv cd i xs) = tryChoices cd i xs
  try (Fail_C_XmlConv cd info) = Fail cd info
  try (Guard_C_XmlConv cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_XmlConv cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_XmlConv cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_XmlConv cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_XmlConv cd i _) = error ("XmlConv.XmlConv.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_XmlConv cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_XmlConv cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance (Generable t0,Generable t1,Generable t2) => Generable (C_XmlConv t0 t1 t2) where
  generate s = Choices_C_XmlConv defCover (freeID [2] s) [(C_Conv (generate (leftSupply s)) (generate (rightSupply s)))]


instance (NormalForm t0,NormalForm t1,NormalForm t2) => NormalForm (C_XmlConv t0 t1 t2) where
  ($!!) cont (C_Conv x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Conv y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_Conv x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_Conv y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_XmlConv cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_XmlConv cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_XmlConv cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_XmlConv cd info) _ = failCons cd info
  ($##) cont (C_Conv x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Conv y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_Conv x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_Conv y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_XmlConv cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_XmlConv cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_XmlConv cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_XmlConv cd info) _ = failCons cd info
  searchNF search cont (C_Conv x1 x2) = search (\y1 -> search (\y2 -> cont (C_Conv y1 y2)) x2) x1
  searchNF search cont (HO_C_Conv x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_Conv y1 y2)) x2) x1
  searchNF _ _ x = error ("XmlConv.XmlConv.searchNF: no constructor: " ++ (show x))


instance (Unifiable t0,Unifiable t1,Unifiable t2) => Unifiable (C_XmlConv t0 t1 t2) where
  (=.=) (C_Conv x1 x2) (C_Conv y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_Conv x1 x2) (HO_C_Conv y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Conv x1 x2) (C_Conv y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_Conv x1 x2) (HO_C_Conv y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Conv x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_Conv x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_XmlConv cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_XmlConv cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_XmlConv cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_XmlConv cd i _) = error ("XmlConv.XmlConv.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_XmlConv cd info) = [(Unsolvable info)]
  bind i (Guard_C_XmlConv cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Conv x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_Conv x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_XmlConv cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_XmlConv cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_XmlConv cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_XmlConv cd i _) = error ("XmlConv.XmlConv.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_XmlConv cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_XmlConv cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.Curry (C_XmlConv t0 t1 t2) where
  (=?=) (Choice_C_XmlConv cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_XmlConv cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_XmlConv cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_XmlConv cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_XmlConv cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_XmlConv cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_XmlConv cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_XmlConv cd info) _ = failCons cd info
  (=?=) (C_Conv x1 x2) (C_Conv y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_Conv x1 x2) (HO_C_Conv y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (<?=) (Choice_C_XmlConv cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_XmlConv cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_XmlConv cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_XmlConv cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_XmlConv cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_XmlConv cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_XmlConv cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_XmlConv cd info) _ = failCons cd info
  (<?=) (C_Conv x1 x2) (C_Conv y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_Conv x1 x2) (HO_C_Conv y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs


instance (Coverable t0,Coverable t1,Coverable t2) => Coverable (C_XmlConv t0 t1 t2) where
  cover (C_Conv x1 x2) = C_Conv (cover x1) (cover x2)
  cover (HO_C_Conv x1 x2) = HO_C_Conv (cover x1) (cover x2)
  cover (Choice_C_XmlConv cd i x y) = Choice_C_XmlConv (incCover cd) i (cover x) (cover y)
  cover (Choices_C_XmlConv cd i xs) = Choices_C_XmlConv (incCover cd) i (map cover xs)
  cover (Fail_C_XmlConv cd info) = Fail_C_XmlConv (incCover cd) info
  cover (Guard_C_XmlConv cd c e) = Guard_C_XmlConv (incCover cd) c (cover e)


d_OP_slash_gt_eq :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_slash_gt_eq x1 x2 x3500 = d_OP_slash_gt_eq_dot___hash_lambda1 x2 x1

nd_OP_slash_gt_eq :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Func t0 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_slash_gt_eq x1 x2 x3000 x3500 = wrapNX id (nd_OP_slash_gt_eq_dot___hash_lambda1 x2 x1)

d_OP_slash_gt_eq_dot___hash_lambda1 :: (Curry_Prelude.Curry t9,Curry_Prelude.Curry t2) => (t9 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t9 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_slash_gt_eq_dot___hash_lambda1 x1 x2 x3 x3500 = d_OP__case_35 x1 x2 x3 (Curry_Prelude.d_C_apply x2 x3 x3500) x3500

nd_OP_slash_gt_eq_dot___hash_lambda1 :: (Curry_Prelude.Curry t9,Curry_Prelude.Curry t2) => Func t9 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))) -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t9 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_slash_gt_eq_dot___hash_lambda1 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_35 x1 x2 x3 (Curry_Prelude.nd_C_apply x2 x3 x2000 x3500) x2001 x3500)))))

d_C_ret :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_C_ret x1 x2 x3500 = Curry_Prelude.OP_Tuple2 x1 x2

d_C_xmlReads :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_C_xmlReads x1 x3500 = case x1 of
     (C_Conv x2 x3) -> x2
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_xmlReads x1002 x3500) (d_C_xmlReads x1003 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_xmlReads z x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_xmlReads x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_xmlReads :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_C_xmlReads x1 x3000 x3500 = case x1 of
     (HO_C_Conv x2 x3) -> x2
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_xmlReads x1002 x3000 x3500) (nd_C_xmlReads x1003 x3000 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_xmlReads z x3000 x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_xmlReads x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xmlShows :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> ConstStore -> t2 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
d_C_xmlShows x1 x3500 = case x1 of
     (C_Conv x2 x3) -> x3
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_xmlShows x1002 x3500) (d_C_xmlShows x1003 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_xmlShows z x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_xmlShows x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_xmlShows :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> Func t2 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_C_xmlShows x1 x3000 x3500 = case x1 of
     (HO_C_Conv x2 x3) -> x3
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_xmlShows x1002 x3000 x3500) (nd_C_xmlShows x1003 x3000 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_xmlShows z x3000 x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_xmlShows x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xmlRead :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_XmlConv t0 C_Elem t1 -> Curry_XML.C_XmlExp -> ConstStore -> t1
d_C_xmlRead x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (d_C_xmlReads x1 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List)) x3500
      in (d_OP_xmlRead_dot___hash_selFP2_hash_a x3 x3500)

nd_C_xmlRead :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_XmlConv t0 C_Elem t1 -> Curry_XML.C_XmlExp -> IDSupply -> ConstStore -> t1
nd_C_xmlRead x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x3 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlReads x1 x2000 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List)) x2001 x3500)))
           in (d_OP_xmlRead_dot___hash_selFP2_hash_a x3 x3500)))

d_OP_xmlRead_dot___hash_selFP2_hash_a :: (Curry_Prelude.Curry t1501,Curry_Prelude.Curry t1502,Curry_Prelude.Curry t43) => Curry_Prelude.OP_Tuple2 t43 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1501) (Curry_Prelude.OP_List t1502)) -> ConstStore -> t43
d_OP_xmlRead_dot___hash_selFP2_hash_a x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_34 x2 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_xmlRead_dot___hash_selFP2_hash_a x1002 x3500) (d_OP_xmlRead_dot___hash_selFP2_hash_a x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_xmlRead_dot___hash_selFP2_hash_a z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_xmlRead_dot___hash_selFP2_hash_a x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xmlShow :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_XmlConv t0 C_Elem t1 -> t1 -> ConstStore -> Curry_XML.C_XmlExp
d_C_xmlShow x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x3500
      in (d_OP_xmlShow_dot___hash_selFP4_hash_x x3 x3500)

nd_C_xmlShow :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_XmlConv t0 C_Elem t1 -> t1 -> IDSupply -> ConstStore -> Curry_XML.C_XmlExp
nd_C_xmlShow x1 x2 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x3 = let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x2003 x3500)))
           in (d_OP_xmlShow_dot___hash_selFP4_hash_x x3 x3500)))

d_OP_xmlShow_dot___hash_selFP4_hash_x :: Curry_Prelude.Curry t1504 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1504) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_XML.C_XmlExp
d_OP_xmlShow_dot___hash_selFP4_hash_x x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_31 x3 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_xmlShow_dot___hash_selFP4_hash_x x1002 x3500) (d_OP_xmlShow_dot___hash_selFP4_hash_x x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_xmlShow_dot___hash_selFP4_hash_x z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_xmlShow_dot___hash_selFP4_hash_x x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_int_ :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int) (Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_int_ x3500 = Curry_Prelude.OP_Tuple2 Curry_Read.d_C_readInt Curry_Prelude.d_C_show

nd_C_int_ :: IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Int) (Func Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_int_ x3000 x3500 = Curry_Prelude.OP_Tuple2 (wrapDX id Curry_Read.d_C_readInt) (wrapDX id Curry_Prelude.d_C_show)

d_C_float_ :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Float) (Curry_Prelude.C_Float -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_float_ x3500 = Curry_Prelude.OP_Tuple2 Curry_ReadShowTerm.d_C_readQTerm Curry_Prelude.d_C_show

nd_C_float_ :: IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Float) (Func Curry_Prelude.C_Float (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_float_ x3000 x3500 = Curry_Prelude.OP_Tuple2 (wrapDX id Curry_ReadShowTerm.d_C_readQTerm) (wrapDX id Curry_Prelude.d_C_show)

d_C_char_ :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Char) (Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_char_ x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.d_C_head (Curry_Prelude.d_C_flip (acceptCs (acceptCs id) Curry_Prelude.OP_Cons) Curry_Prelude.OP_List)

nd_C_char_ :: IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Char) (Func Curry_Prelude.C_Char (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_char_ x3000 x3500 = Curry_Prelude.OP_Tuple2 (wrapDX id Curry_Prelude.d_C_head) (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs (acceptCs id) Curry_Prelude.OP_Cons)) Curry_Prelude.OP_List))

d_C_string_ :: ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_string_ x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.d_C_id Curry_Prelude.d_C_id

nd_C_string_ :: IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_string_ x3000 x3500 = Curry_Prelude.OP_Tuple2 (wrapDX id Curry_Prelude.d_C_id) (wrapDX id Curry_Prelude.d_C_id)

d_C_bool_ :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool) (Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_bool_ x1 x2 x3500 = Curry_Prelude.OP_Tuple2 (d_OP_bool__dot_readBool_dot_29 x2 x1) (d_OP_bool__dot_showBool_dot_29 x2 x1)

nd_C_bool_ :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool) (Func Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_bool_ x1 x2 x3000 x3500 = Curry_Prelude.OP_Tuple2 (wrapDX id (d_OP_bool__dot_readBool_dot_29 x2 x1)) (wrapDX id (d_OP_bool__dot_showBool_dot_29 x2 x1))

d_OP_bool__dot_fromJust_dot_29 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> ConstStore -> t0
d_OP_bool__dot_fromJust_dot_29 x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> x2
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bool__dot_fromJust_dot_29 x1002 x3500) (d_OP_bool__dot_fromJust_dot_29 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bool__dot_fromJust_dot_29 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bool__dot_fromJust_dot_29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_bool__dot_readBool_dot_29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_bool__dot_readBool_dot_29 x1 x2 x3 x3500 = d_OP_bool__dot_fromJust_dot_29 (Curry_Prelude.d_C_lookup x3 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.C_True) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.C_False) Curry_Prelude.OP_List)) x3500) x3500

d_OP_bool__dot_showBool_dot_29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_bool__dot_showBool_dot_29 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bool__dot_showBool_dot_29 x1 x2 x1002 x3500) (d_OP_bool__dot_showBool_dot_29 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bool__dot_showBool_dot_29 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bool__dot_showBool_dot_29 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_val_ :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t0) (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
d_C_val_ x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> C_Conv (d_OP_val__dot_rd_dot_37 x2) (acceptCs id (d_OP_val__dot_sh_dot_37 x3))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_val_ x1002 x3500) (d_C_val_ x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_val_ z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_val_ x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_val_ :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
nd_C_val_ x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> HO_C_Conv (wrapNX id (nd_OP_val__dot_rd_dot_37 x2)) (wrapDX (wrapNX id) (acceptCs id (nd_OP_val__dot_sh_dot_37 x3)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_val_ x1002 x3000 x3500) (nd_C_val_ x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_val_ z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_val_ x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_val__dot_rd_dot_37 :: (Curry_Prelude.Curry t91,Curry_Prelude.Curry t0) => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t91) -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t91 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_val__dot_rd_dot_37 x1 x2 x3500 = let
     x3 = d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x2 x3500
     x4 = d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x2 x3500
     x5 = d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x2 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x4 x3500) (Curry_Prelude.OP_Tuple2 x3 x5))

nd_OP_val__dot_rd_dot_37 :: (Curry_Prelude.Curry t91,Curry_Prelude.Curry t0) => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t91 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t91 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_val__dot_rd_dot_37 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x3 = d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x2 x3500
          x4 = d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x2 x3500
          x5 = d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x2 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) (Curry_Prelude.OP_Tuple2 x3 x5))))

d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs :: Curry_Prelude.Curry t84 => Curry_Prelude.OP_Tuple2 t84 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t84
d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_28 x2 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x1002 x3500) (d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_val__dot_rd_dot_37_dot___hash_selFP6_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a :: Curry_Prelude.Curry t84 => Curry_Prelude.OP_Tuple2 t84 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_26 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x1002 x3500) (d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_val__dot_rd_dot_37_dot___hash_selFP7_hash_a x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems :: Curry_Prelude.Curry t84 => Curry_Prelude.OP_Tuple2 t84 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_24 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x1002 x3500) (d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_val__dot_rd_dot_37_dot___hash_selFP8_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_val__dot_sh_dot_37 :: (Curry_Prelude.Curry t91,Curry_Prelude.Curry t0) => (t91 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t91 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
d_OP_val__dot_sh_dot_37 x1 x2 x3 x3500 = let
     x4 = d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x3 x3500
     x5 = d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x3 x3500
      in (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Cons (Curry_XML.C_XText (Curry_Prelude.d_C_apply x1 x2 x3500)) x5))

nd_OP_val__dot_sh_dot_37 :: (Curry_Prelude.Curry t91,Curry_Prelude.Curry t0) => Func t91 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t91 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
nd_OP_val__dot_sh_dot_37 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x3 x3500
          x5 = d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x3 x3500
           in (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Cons (Curry_XML.C_XText (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500)) x5))))

d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs :: Curry_Prelude.Curry t93 => Curry_Prelude.OP_Tuple2 t93 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t93
d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x1002 x3500) (d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_val__dot_sh_dot_37_dot___hash_selFP10_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems :: Curry_Prelude.Curry t93 => Curry_Prelude.OP_Tuple2 t93 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x1002 x3500) (d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_val__dot_sh_dot_37_dot___hash_selFP11_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_int :: ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Int
d_C_int x3500 = d_C_val_ (d_C_int_ x3500) x3500

nd_C_int :: IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Int
nd_C_int x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_val_ (nd_C_int_ x2000 x3500) x2001 x3500)))))

d_C_float :: ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Float
d_C_float x3500 = d_C_val_ (d_C_float_ x3500) x3500

nd_C_float :: IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Float
nd_C_float x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_val_ (nd_C_float_ x2000 x3500) x2001 x3500)))))

d_C_char :: ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Char
d_C_char x3500 = d_C_val_ (d_C_char_ x3500) x3500

nd_C_char :: IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Char
nd_C_char x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_val_ (nd_C_char_ x2000 x3500) x2001 x3500)))))

nd_C_string :: IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_string x3000 x3500 = HO_C_Conv (wrapNX id nd_OP_string_dot_rd_dot_51) (wrapDX (wrapDX id) (acceptCs id d_OP_string_dot_sh_dot_51))

nd_OP_string_dot_rd_dot_51 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_string_dot_rd_dot_51 x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2 = d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs x1 x3500
          x3 = d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems x1 x3500
           in (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1) (nd_OP__case_22 x2 x3 x2000 x3500) x2001 x3500))))))

d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs :: Curry_Prelude.Curry t108 => Curry_Prelude.OP_Tuple2 t108 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t108
d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs x1002 x3500) (d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string_dot_rd_dot_51_dot___hash_selFP13_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems :: Curry_Prelude.Curry t108 => Curry_Prelude.OP_Tuple2 t108 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems x1002 x3500) (d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string_dot_rd_dot_51_dot___hash_selFP14_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_string_dot_sh_dot_51 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
d_OP_string_dot_sh_dot_51 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs x2 x3500
          x6 = d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems x2 x3500
           in (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons (Curry_XML.C_XText x1) x6))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string_dot_sh_dot_51 x1002 x2 x3500) (d_OP_string_dot_sh_dot_51 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string_dot_sh_dot_51 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string_dot_sh_dot_51 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs :: Curry_Prelude.Curry t122 => Curry_Prelude.OP_Tuple2 t122 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t122
d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs x1002 x3500) (d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string_dot_sh_dot_51_dot___hash_selFP16_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems :: Curry_Prelude.Curry t122 => Curry_Prelude.OP_Tuple2 t122 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems x1002 x3500) (d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_string_dot_sh_dot_51_dot___hash_selFP17_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_bang :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> C_XmlConv t0 t1 t2
nd_OP_bang x1 x2 x3000 x3500 = case x1 of
     (HO_C_Conv x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x3 x4 x2 x2000 x3500))
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_bang x1002 x2 x3000 x3500) (nd_OP_bang x1003 x2 x3000 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_bang z x2 x3000 x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_bang x1002 x2 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_bang_dot_rd_dot_65 :: Curry_Prelude.Curry t147 => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t147 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t147 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t147 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_bang_dot_rd_dot_65 x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_apply x2 x3 x2001 x3500) x2002 x3500))))))))

nd_OP_bang_dot_sh_dot_65 :: Curry_Prelude.Curry t147 => Func t147 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> Func t147 (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))) -> t147 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_bang_dot_sh_dot_65 x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_apply x2 x3 x2001 x3500) x2002 x3500))))))))

d_C_element :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t0 t1 t2 -> ConstStore -> C_XmlConv C_Repeatable C_Elem t2
d_C_element x1 x2 x3500 = C_Conv (d_OP_element_dot_rd_dot_71 x1 x2) (acceptCs id (d_OP_element_dot_sh_dot_71 x1 x2))

nd_C_element :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t2
nd_C_element x1 x2 x3000 x3500 = HO_C_Conv (wrapNX id (nd_OP_element_dot_rd_dot_71 x1 x2)) (wrapDX (wrapNX id) (acceptCs id (nd_OP_element_dot_sh_dot_71 x1 x2)))

d_OP_element_dot_rd_dot_71 :: (Curry_Prelude.Curry t168,Curry_Prelude.Curry t169,Curry_Prelude.Curry t176,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t168 t169 t176 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t176 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_element_dot_rd_dot_71 x1 x2 x3 x3500 = let
     x4 = d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x3 x3500
     x5 = d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x3 x3500
     x6 = d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x3 x3500
     x7 = d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x3 x3500
     x8 = d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x3 x3500
      in (d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_eq_eq x5 x1 x3500) x3500)

nd_OP_element_dot_rd_dot_71 :: (Curry_Prelude.Curry t168,Curry_Prelude.Curry t169,Curry_Prelude.Curry t176,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t168 t169 t176 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t176 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_element_dot_rd_dot_71 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x3 x3500
          x5 = d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x3 x3500
          x6 = d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x3 x3500
          x7 = d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x3 x3500
          x8 = d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x3 x3500
           in (nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_eq_eq x5 x1 x3500) x2000 x3500)))

d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs :: Curry_Prelude.Curry t159 => Curry_Prelude.OP_Tuple2 t159 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t159
d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_14 x2 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x1002 x3500) (d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_rd_dot_71_dot___hash_selFP19_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName :: Curry_Prelude.Curry t159 => Curry_Prelude.OP_Tuple2 t159 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_12 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x1002 x3500) (d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_rd_dot_71_dot___hash_selFP20_hash_myName x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs :: Curry_Prelude.Curry t159 => Curry_Prelude.OP_Tuple2 t159 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_10 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x1002 x3500) (d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_rd_dot_71_dot___hash_selFP21_hash_myAttrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems :: Curry_Prelude.Curry t159 => Curry_Prelude.OP_Tuple2 t159 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_8 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x1002 x3500) (d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_rd_dot_71_dot___hash_selFP22_hash_myElems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems :: Curry_Prelude.Curry t159 => Curry_Prelude.OP_Tuple2 t159 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_6 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x1002 x3500) (d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_rd_dot_71_dot___hash_selFP23_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_sh_dot_71 :: (Curry_Prelude.Curry t168,Curry_Prelude.Curry t169,Curry_Prelude.Curry t176,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t168 t169 t176 -> t176 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
d_OP_element_dot_sh_dot_71 x1 x2 x3 x4 x3500 = let
     x5 = d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x4 x3500
     x6 = d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x4 x3500
      in (d_OP__case_4 x1 x2 x3 x5 x6 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_xmlShows x2 x3500) x3 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x3500) x3500)

nd_OP_element_dot_sh_dot_71 :: (Curry_Prelude.Curry t168,Curry_Prelude.Curry t169,Curry_Prelude.Curry t176,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t168 t169 t176 -> t176 -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
nd_OP_element_dot_sh_dot_71 x1 x2 x3 x4 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x5 = d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x4 x3500
          x6 = d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x4 x3500
           in (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_OP__case_4 x1 x2 x3 x5 x6 (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x2 x2000 x3500) x3 x2001 x3500)))) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x2003 x3500)))) x2005 x3500))))))

d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs :: Curry_Prelude.Curry t178 => Curry_Prelude.OP_Tuple2 t178 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> t178
d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x1002 x3500) (d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_sh_dot_71_dot___hash_selFP25_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems :: Curry_Prelude.Curry t178 => Curry_Prelude.OP_Tuple2 t178 (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List Curry_XML.C_XmlExp
d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x1002 x3500) (d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_element_dot_sh_dot_71_dot___hash_selFP26_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_empty :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
d_C_empty x1 x3500 = let
     x2 = d_C_ret x1
      in (C_Conv x2 (acceptCs id (d_OP_empty_dot_sh_dot_83 x1)))

nd_C_empty :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
nd_C_empty x1 x3000 x3500 = let
     x2 = wrapDX id (d_C_ret x1)
      in (HO_C_Conv x2 (wrapDX (wrapDX id) (acceptCs id (d_OP_empty_dot_sh_dot_83 x1))))

d_OP_empty_dot_sh_dot_83 :: (Curry_Prelude.Curry t195,Curry_Prelude.Curry t0) => t195 -> t195 -> t0 -> ConstStore -> t0
d_OP_empty_dot_sh_dot_83 x1 x2 x3 x3500 = d_OP___cond_0_empty_dot_sh_dot_83 x3 (Curry_Prelude.d_OP_eq_colon_eq x2 x1 x3500) x3500

d_OP___cond_0_empty_dot_sh_dot_83 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_empty_dot_sh_dot_83 x1 x1002 x3500) (d_OP___cond_0_empty_dot_sh_dot_83 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_empty_dot_sh_dot_83 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_empty_dot_sh_dot_83 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_empty_dot_sh_dot_83 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_empty_dot_sh_dot_83 x1 x1002 x3000 x3500) (nd_OP___cond_0_empty_dot_sh_dot_83 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_empty_dot_sh_dot_83 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_empty_dot_sh_dot_83 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_attr :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t0) (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
d_C_attr x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> C_Conv (d_OP_attr_dot_rd_dot_88 x1 x3) (acceptCs id (d_OP_attr_dot_sh_dot_88 x1 x4))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_attr x1 x1002 x3500) (d_C_attr x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_attr x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_attr x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_attr :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem t0
nd_C_attr x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> HO_C_Conv (wrapNX id (nd_OP_attr_dot_rd_dot_88 x1 x3)) (wrapDX (wrapNX id) (acceptCs id (nd_OP_attr_dot_sh_dot_88 x1 x4)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_attr x1 x1002 x3000 x3500) (nd_C_attr x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_attr x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_attr x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_rd_dot_88 :: (Curry_Prelude.Curry t238,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> t238) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t238 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0)
d_OP_attr_dot_rd_dot_88 x1 x2 x3 x3500 = let
     x4 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x3 x3500
     x5 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x3 x3500
     x6 = d_C_exposeBy (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_eq_eq x1) Curry_Prelude.d_C_fst x3500) x4 x3500
     x7 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x6 x3500
     x8 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x6 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x2 x7 x3500) (Curry_Prelude.OP_Tuple2 x8 x5))

nd_OP_attr_dot_rd_dot_88 :: (Curry_Prelude.Curry t238,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) t238 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t238 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0)
nd_OP_attr_dot_rd_dot_88 x1 x2 x3 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x4 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x3 x3500
               x5 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x3 x3500
               x6 = let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_exposeBy (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_eq_eq x1)) (wrapDX id Curry_Prelude.d_C_fst) x2000 x3500) x4 x2001 x3500)))
               x7 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x6 x3500
               x8 = d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x6 x3500
                in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x2 x7 x2003 x3500) (Curry_Prelude.OP_Tuple2 x8 x5)))))))

d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs :: Curry_Prelude.Curry t224 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t224 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x1002 x3500) (d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_rd_dot_88_dot___hash_selFP31_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems :: Curry_Prelude.Curry t224 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t224 -> ConstStore -> t224
d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x1002 x3500) (d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_rd_dot_88_dot___hash_selFP32_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_3 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x1002 x3500) (d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_rd_dot_88_dot___hash_selFP29_hash_value x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_2 x3 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x1002 x3500) (d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_rd_dot_88_dot___hash_selFP30_hash_attrs' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_sh_dot_88 :: (Curry_Prelude.Curry t238,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (t238 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t238 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0
d_OP_attr_dot_sh_dot_88 x1 x2 x3 x4 x3500 = let
     x5 = d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x4 x3500
     x6 = d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x4 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_apply x2 x3 x3500)) x5) x6)

nd_OP_attr_dot_sh_dot_88 :: (Curry_Prelude.Curry t238,Curry_Prelude.Curry t0) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t238 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t238 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t0
nd_OP_attr_dot_sh_dot_88 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x4 x3500
          x6 = d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x4 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.nd_C_apply x2 x3 x2000 x3500)) x5) x6)))

d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs :: Curry_Prelude.Curry t241 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t241 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x1002 x3500) (d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_sh_dot_88_dot___hash_selFP34_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems :: Curry_Prelude.Curry t241 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) t241 -> ConstStore -> t241
d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x1002 x3500) (d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_attr_dot_sh_dot_88_dot___hash_selFP35_hash_elems x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_exposeBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t0)
d_C_exposeBy x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = d_C_exposeBy x1 x4 x3500
          x6 = d_OP_exposeBy_dot___hash_selFP37_hash_y x5 x3500
          x7 = d_OP_exposeBy_dot___hash_selFP38_hash_ys x5 x3500
           in (d_OP__case_1 x1 x3 x4 x6 x7 (Curry_Prelude.d_C_apply x1 x3 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_exposeBy x1 x1002 x3500) (d_C_exposeBy x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_exposeBy x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_exposeBy x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_exposeBy :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t0)
nd_C_exposeBy x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x5 = nd_C_exposeBy x1 x4 x2000 x3500
                    x6 = d_OP_exposeBy_dot___hash_selFP37_hash_y x5 x3500
                    x7 = d_OP_exposeBy_dot___hash_selFP38_hash_ys x5 x3500
                     in (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (nd_OP__case_1 x1 x3 x4 x6 x7 (Curry_Prelude.nd_C_apply x1 x3 x2001 x3500) x2002 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_exposeBy x1 x1002 x3000 x3500) (nd_C_exposeBy x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_exposeBy x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_exposeBy x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_exposeBy_dot___hash_selFP37_hash_y :: Curry_Prelude.Curry t211 => Curry_Prelude.OP_Tuple2 t211 (Curry_Prelude.OP_List t211) -> ConstStore -> t211
d_OP_exposeBy_dot___hash_selFP37_hash_y x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_exposeBy_dot___hash_selFP37_hash_y x1002 x3500) (d_OP_exposeBy_dot___hash_selFP37_hash_y x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_exposeBy_dot___hash_selFP37_hash_y z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_exposeBy_dot___hash_selFP37_hash_y x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_exposeBy_dot___hash_selFP38_hash_ys :: Curry_Prelude.Curry t211 => Curry_Prelude.OP_Tuple2 t211 (Curry_Prelude.OP_List t211) -> ConstStore -> Curry_Prelude.OP_List t211
d_OP_exposeBy_dot___hash_selFP38_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_exposeBy_dot___hash_selFP38_hash_ys x1002 x3500) (d_OP_exposeBy_dot___hash_selFP38_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_exposeBy_dot___hash_selFP38_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_exposeBy_dot___hash_selFP38_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_adapt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 (t0 -> ConstStore -> t1) (t1 -> ConstStore -> t0) -> C_XmlConv t2 t3 t0 -> ConstStore -> C_XmlConv t2 t3 t1
d_C_adapt x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_adapt x1002 x2 x3500) (d_C_adapt x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_adapt z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_adapt x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_adapt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 (Func t0 t1) (Func t1 t0) -> C_XmlConv t2 t3 t0 -> IDSupply -> ConstStore -> C_XmlConv t2 t3 t1
nd_C_adapt x1 x2 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x3 x4 x2 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_adapt x1002 x2 x3000 x3500) (nd_C_adapt x1003 x2 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_adapt z x2 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_adapt x1002 x2 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_opt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.C_Maybe t2)
nd_C_opt x1 x3000 x3500 = HO_C_Conv (wrapNX id (nd_OP_opt_dot_rd_dot_105 x1)) (wrapNX id (nd_OP_opt_dot_sh_dot_105 x1))

nd_OP_opt_dot_rd_dot_105 :: (Curry_Prelude.Curry t286,Curry_Prelude.Curry t287,Curry_Prelude.Curry t298) => C_XmlConv t286 t287 t298 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Maybe t298) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_opt_dot_rd_dot_105 x1 x2 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (Curry_Prelude.nd_OP_qmark (d_C_ret Curry_Prelude.C_Nothing x2 x3500) (let
               x2005 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2005 (seq x2003 (Curry_Prelude.nd_C_apply (let
                    x2002 = leftSupply x2003
                    x2004 = rightSupply x2003
                     in (seq x2002 (seq x2004 (let
                         x2000 = leftSupply x2004
                         x2001 = rightSupply x2004
                          in (seq x2000 (seq x2001 (nd_OP_slash_gt_eq (nd_C_xmlReads x1 x2000 x3500) (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id d_C_ret)) (wrapDX id (acceptCs id Curry_Prelude.C_Just)) x2001 x3500) x2002 x3500))))))) x2 x2005 x3500)))) x2007 x3500)))))

d_OP_opt_dot_sh_dot_105 :: (Curry_Prelude.Curry t286,Curry_Prelude.Curry t287,Curry_Prelude.Curry t298) => C_XmlConv t286 t287 t298 -> Curry_Prelude.C_Maybe t298 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)
d_OP_opt_dot_sh_dot_105 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_id
     (Curry_Prelude.C_Just x3) -> Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x3 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_opt_dot_sh_dot_105 x1 x1002 x3500) (d_OP_opt_dot_sh_dot_105 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_opt_dot_sh_dot_105 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_opt_dot_sh_dot_105 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_opt_dot_sh_dot_105 :: (Curry_Prelude.Curry t286,Curry_Prelude.Curry t287,Curry_Prelude.Curry t298) => C_XmlConv t286 t287 t298 -> Curry_Prelude.C_Maybe t298 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_opt_dot_sh_dot_105 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.C_Just x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x3 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_opt_dot_sh_dot_105 x1 x1002 x3000 x3500) (nd_OP_opt_dot_sh_dot_105 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_opt_dot_sh_dot_105 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_opt_dot_sh_dot_105 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_rep :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => C_XmlConv C_Repeatable t0 t1 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t1)
nd_C_rep x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id Curry_Prelude.nd_OP_dot)) (wrapDX id Curry_Prelude.d_C_id))) (wrapNX id (Curry_Prelude.nd_C_map (nd_C_xmlShows x1 x2000 x3500))) x2001 x3500)))
           in (HO_C_Conv (wrapNX id (nd_OP_rep_dot_rd_dot_113 x1)) x2)))

nd_OP_rep_dot_rd_dot_113 :: (Curry_Prelude.Curry t316,Curry_Prelude.Curry t318) => C_XmlConv C_Repeatable t316 t318 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t318) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_rep_dot_rd_dot_113 x1 x2 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_qmark (d_C_ret Curry_Prelude.OP_List x2 x3500) (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x1 x2000 x3500) (wrapNX id (nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6 x1)) x2001 x3500)))) x2 x2003 x3500)))) x2005 x3500)))))

nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6 :: (Curry_Prelude.Curry t316,Curry_Prelude.Curry t318) => C_XmlConv C_Repeatable t316 t318 -> t318 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t318) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP_slash_gt_eq (wrapNX id (nd_OP_rep_dot_rd_dot_113 x1)) (wrapNX id (nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6_dot___hash_lambda7 x2)) x2000 x3500))

d_OP_rep_dot_rd_dot_113_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t318 => t318 -> Curry_Prelude.OP_List t318 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t318) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_rep_dot_rd_dot_113_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3500 = d_C_ret (Curry_Prelude.OP_Cons x1 x2)

nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t318 => t318 -> Curry_Prelude.OP_List t318 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t318) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_rep_dot_rd_dot_113_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3000 x3500 = wrapDX id (d_C_ret (Curry_Prelude.OP_Cons x1 x2))

d_C_aInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Int
d_C_aInt x1 x3500 = d_C_attr x1 (d_C_int_ x3500) x3500

nd_C_aInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Int
nd_C_aInt x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_attr x1 (nd_C_int_ x2000 x3500) x2001 x3500)))))

d_C_aFloat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Float
d_C_aFloat x1 x3500 = d_C_attr x1 (d_C_float_ x3500) x3500

nd_C_aFloat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Float
nd_C_aFloat x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_attr x1 (nd_C_float_ x2000 x3500) x2001 x3500)))))

d_C_aChar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Char
d_C_aChar x1 x3500 = d_C_attr x1 (d_C_char_ x3500) x3500

nd_C_aChar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Char
nd_C_aChar x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_attr x1 (nd_C_char_ x2000 x3500) x2001 x3500)))))

d_C_aString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_aString x1 x3500 = d_C_attr x1 (d_C_string_ x3500) x3500

nd_C_aString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_aString x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_attr x1 (nd_C_string_ x2000 x3500) x2001 x3500)))))

d_C_aBool :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Bool
d_C_aBool x1 x2 x3 x3500 = d_C_attr x1 (d_C_bool_ x2 x3 x3500) x3500

nd_C_aBool :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem Curry_Prelude.C_Bool
nd_C_aBool x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_attr x1 (nd_C_bool_ x2 x3 x2000 x3500) x2001 x3500)))))

d_C_eInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Int
d_C_eInt x1 x3500 = d_C_element x1 (d_C_int x3500) x3500

nd_C_eInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Int
nd_C_eInt x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_int x2000 x3500) x2001 x3500)))))

d_C_eFloat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Float
d_C_eFloat x1 x3500 = d_C_element x1 (d_C_float x3500) x3500

nd_C_eFloat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Float
nd_C_eFloat x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_float x2000 x3500) x2001 x3500)))))

d_C_eChar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Char
d_C_eChar x1 x3500 = d_C_element x1 (d_C_char x3500) x3500

nd_C_eChar :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Char
nd_C_eChar x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_char x2000 x3500) x2001 x3500)))))

nd_C_eString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_eString x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_string x2000 x3500) x2001 x3500)))))

nd_C_eBool :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem Curry_Prelude.C_Bool
nd_C_eBool x1 x2 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (nd_OP_bang (nd_C_eEmpty x1 Curry_Prelude.C_True x2000 x3500) (nd_C_eEmpty x2 Curry_Prelude.C_False x2001 x3500) x2002 x3500))))))))

d_C_eEmpty :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> ConstStore -> C_XmlConv C_Repeatable C_Elem t0
d_C_eEmpty x1 x2 x3500 = d_C_element x1 (d_C_empty x2 x3500) x3500

nd_C_eEmpty :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t0
nd_C_eEmpty x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_empty x2 x2000 x3500) x2001 x3500)))))

nd_C_eOpt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv t0 t1 t2 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.C_Maybe t2)
nd_C_eOpt x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_opt x2 x2000 x3500) x2001 x3500)))))

nd_C_eRep :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlConv C_Repeatable t0 t1 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t1)
nd_C_eRep x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_rep x2 x2000 x3500) x2001 x3500)))))

nd_C_seq1 :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => Func t0 t1 -> C_XmlConv t2 t3 t0 -> IDSupply -> ConstStore -> C_XmlConv t2 C_NoElem t1
nd_C_seq1 x1 x2 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x3 = let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id d_C_ret)) x1 x2001 x3500) x2002 x3500))))))
           in (HO_C_Conv x3 (wrapNX id (nd_OP_seq1_dot_sh_dot_146 x1 x2)))))

d_OP_seq1_dot_cf_dot_146 :: (Curry_Prelude.Curry t425,Curry_Prelude.Curry t413) => (t425 -> ConstStore -> t413) -> t425 -> ConstStore -> t413
d_OP_seq1_dot_cf_dot_146 x1 x2 x3500 = Curry_Prelude.d_C_apply x1 x2 x3500

nd_OP_seq1_dot_cf_dot_146 :: (Curry_Prelude.Curry t425,Curry_Prelude.Curry t413) => Func t425 t413 -> t425 -> IDSupply -> ConstStore -> t413
nd_OP_seq1_dot_cf_dot_146 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500))

nd_OP_seq1_dot_sh_dot_146 :: (Curry_Prelude.Curry t417,Curry_Prelude.Curry t418,Curry_Prelude.Curry t425,Curry_Prelude.Curry t413) => Func t425 t413 -> C_XmlConv t417 t418 t425 -> t413 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq1_dot_sh_dot_146 x1 x2 x3 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x4 = generate x2003
                in (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP___cond_0_seq1_dot_sh_dot_146 x2 x4 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq1_dot_cf_dot_146 x1 x4 x2000 x3500) x3 x3500) x2001 x3500)))))))))

d_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1002 x3500) (d_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_Success -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1002 x3000 x3500) (nd_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq1_dot_sh_dot_146 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_repSeq1 :: (Curry_Prelude.Curry t2,Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> C_XmlConv C_Repeatable t2 t0 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t1)
nd_C_repSeq1 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq1 x1 x2 x2000 x3500) x2001 x3500)))))

nd_C_eSeq1 :: (Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 t1 -> C_XmlConv t2 t3 t0 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t1
nd_C_eSeq1 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_seq1 x2 x3 x2000 x3500) x2001 x3500)))))

nd_C_eRepSeq1 :: (Curry_Prelude.Curry t2,Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 t1 -> C_XmlConv C_Repeatable t2 t0 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t1)
nd_C_eRepSeq1 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq1 x2 x3 x2000 x3500) x2001 x3500)))))

nd_C_seq2_ :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t7,Curry_Prelude.Curry t2) => Func t0 (Func t1 t2) -> C_XmlConv t3 t4 t0 -> C_XmlConv t5 t6 t1 -> IDSupply -> ConstStore -> C_XmlConv t7 C_NoElem t2
nd_C_seq2_ x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x4 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq2__dot___hash_lambda9 x1 x3)) x2001 x3500)))
           in (HO_C_Conv x4 (wrapNX id (nd_OP_seq2__dot_sh_dot_159 x1 x2 x3)))))

d_OP_seq2__dot_cf_dot_159 :: (Curry_Prelude.Curry t502,Curry_Prelude.Curry t503,Curry_Prelude.Curry t486) => (t502 -> ConstStore -> t503 -> ConstStore -> t486) -> t502 -> t503 -> ConstStore -> t486
d_OP_seq2__dot_cf_dot_159 x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500

nd_OP_seq2__dot_cf_dot_159 :: (Curry_Prelude.Curry t502,Curry_Prelude.Curry t503,Curry_Prelude.Curry t486) => Func t502 (Func t503 t486) -> t502 -> t503 -> IDSupply -> ConstStore -> t486
nd_OP_seq2__dot_cf_dot_159 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))))

d_OP_seq2__dot___hash_lambda9 :: (Curry_Prelude.Curry t496,Curry_Prelude.Curry t497,Curry_Prelude.Curry t503,Curry_Prelude.Curry t502,Curry_Prelude.Curry t486) => (t502 -> ConstStore -> t503 -> ConstStore -> t486) -> C_XmlConv t496 t497 t503 -> t502 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t486 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq2__dot___hash_lambda9 x1 x2 x3 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x2 x3500) (d_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 x3 x1) x3500

nd_OP_seq2__dot___hash_lambda9 :: (Curry_Prelude.Curry t496,Curry_Prelude.Curry t497,Curry_Prelude.Curry t503,Curry_Prelude.Curry t502,Curry_Prelude.Curry t486) => Func t502 (Func t503 t486) -> C_XmlConv t496 t497 t503 -> t502 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t486 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq2__dot___hash_lambda9 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 x3 x1)) x2001 x3500)))))

d_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 :: (Curry_Prelude.Curry t502,Curry_Prelude.Curry t503,Curry_Prelude.Curry t486) => t502 -> (t502 -> ConstStore -> t503 -> ConstStore -> t486) -> t503 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t486 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x3500 = d_C_ret (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x1 x3500) x3 x3500)

nd_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 :: (Curry_Prelude.Curry t502,Curry_Prelude.Curry t503,Curry_Prelude.Curry t486) => t502 -> Func t502 (Func t503 t486) -> t503 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t486 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq2__dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (wrapDX id (d_C_ret (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x1 x2000 x3500) x3 x2001 x3500)))))))

nd_OP_seq2__dot_sh_dot_159 :: (Curry_Prelude.Curry t490,Curry_Prelude.Curry t491,Curry_Prelude.Curry t502,Curry_Prelude.Curry t496,Curry_Prelude.Curry t497,Curry_Prelude.Curry t503,Curry_Prelude.Curry t486) => Func t502 (Func t503 t486) -> C_XmlConv t490 t491 t502 -> C_XmlConv t496 t497 t503 -> t486 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq2__dot_sh_dot_159 x1 x2 x3 x4 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2002 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2002 (seq x2006 (let
               x2003 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2003 (seq x2004 (let
                    x5 = generate x2003
                    x6 = generate x2004
                     in (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (nd_OP___cond_0_seq2__dot_sh_dot_159 x2 x5 x3 x6 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq2__dot_cf_dot_159 x1 x5 x6 x2000 x3500) x4 x3500) x2001 x3500))))))))))))

d_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.d_C_apply (d_C_xmlShows x3 x3500) x4 x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1002 x3500) (d_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_Success -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2002 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2002 (seq x2005 (Curry_Prelude.nd_OP_dot (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x3 x2003 x3500) x4 x2004 x3500)))) x2006 x3500))))))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq2__dot_sh_dot_159 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_seq2 :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => IDSupply -> ConstStore -> Func (Func t0 (Func t1 t2)) (Func (C_XmlConv t3 t4 t0) (Func (C_XmlConv t5 t6 t1) (C_XmlConv C_NotRepeatable C_NoElem t2)))
nd_C_seq2 x3000 x3500 = wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) nd_C_seq2_)

nd_C_repSeq2 :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t0,Curry_Prelude.Curry t4,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 (Func t1 t2) -> C_XmlConv C_Repeatable t3 t0 -> C_XmlConv C_Repeatable t4 t1 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t2)
nd_C_repSeq2 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq2_ x1 x2 x3 x2000 x3500) x2001 x3500)))))

nd_C_eSeq2 :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t4,Curry_Prelude.Curry t0,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 t2) -> C_XmlConv t3 t4 t0 -> C_XmlConv t5 t6 t1 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t2
nd_C_eSeq2 x1 x2 x3 x4 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (nd_C_element x1 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_seq2 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))) x2007 x3500)))))

nd_C_eRepSeq2 :: (Curry_Prelude.Curry t3,Curry_Prelude.Curry t0,Curry_Prelude.Curry t4,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 t2) -> C_XmlConv C_Repeatable t3 t0 -> C_XmlConv C_Repeatable t4 t1 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t2)
nd_C_eRepSeq2 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq2 x2 x3 x4 x2000 x3500) x2001 x3500)))))

nd_C_seq3_ :: (Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t2,Curry_Prelude.Curry t10,Curry_Prelude.Curry t3) => Func t0 (Func t1 (Func t2 t3)) -> C_XmlConv t4 t5 t0 -> C_XmlConv t6 t7 t1 -> C_XmlConv t8 t9 t2 -> IDSupply -> ConstStore -> C_XmlConv t10 C_NoElem t3
nd_C_seq3_ x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x5 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq3__dot___hash_lambda12 x1 x3 x4)) x2001 x3500)))
           in (HO_C_Conv x5 (wrapNX id (nd_OP_seq3__dot_sh_dot_176 x1 x2 x3 x4)))))

d_OP_seq3__dot_cf_dot_176 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t634,Curry_Prelude.Curry t635,Curry_Prelude.Curry t611) => (t633 -> ConstStore -> t634 -> ConstStore -> t635 -> ConstStore -> t611) -> t633 -> t634 -> t635 -> ConstStore -> t611
d_OP_seq3__dot_cf_dot_176 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500) x4 x3500

nd_OP_seq3__dot_cf_dot_176 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t634,Curry_Prelude.Curry t635,Curry_Prelude.Curry t611) => Func t633 (Func t634 (Func t635 t611)) -> t633 -> t634 -> t635 -> IDSupply -> ConstStore -> t611
nd_OP_seq3__dot_cf_dot_176 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))) x4 x2003 x3500)))))

d_OP_seq3__dot___hash_lambda12 :: (Curry_Prelude.Curry t621,Curry_Prelude.Curry t622,Curry_Prelude.Curry t634,Curry_Prelude.Curry t627,Curry_Prelude.Curry t628,Curry_Prelude.Curry t635,Curry_Prelude.Curry t633,Curry_Prelude.Curry t611) => (t633 -> ConstStore -> t634 -> ConstStore -> t635 -> ConstStore -> t611) -> C_XmlConv t621 t622 t634 -> C_XmlConv t627 t628 t635 -> t633 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq3__dot___hash_lambda12 x1 x2 x3 x4 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x2 x3500) (d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 x4 x1 x3) x3500

nd_OP_seq3__dot___hash_lambda12 :: (Curry_Prelude.Curry t621,Curry_Prelude.Curry t622,Curry_Prelude.Curry t634,Curry_Prelude.Curry t627,Curry_Prelude.Curry t628,Curry_Prelude.Curry t635,Curry_Prelude.Curry t633,Curry_Prelude.Curry t611) => Func t633 (Func t634 (Func t635 t611)) -> C_XmlConv t621 t622 t634 -> C_XmlConv t627 t628 t635 -> t633 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq3__dot___hash_lambda12 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 x4 x1 x3)) x2001 x3500)))))

d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t627,Curry_Prelude.Curry t628,Curry_Prelude.Curry t635,Curry_Prelude.Curry t634,Curry_Prelude.Curry t611) => t633 -> (t633 -> ConstStore -> t634 -> ConstStore -> t635 -> ConstStore -> t611) -> C_XmlConv t627 t628 t635 -> t634 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3 x4 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x3 x3500) (d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x4 x2) x3500

nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t627,Curry_Prelude.Curry t628,Curry_Prelude.Curry t635,Curry_Prelude.Curry t634,Curry_Prelude.Curry t611) => t633 -> Func t633 (Func t634 (Func t635 t611)) -> C_XmlConv t627 t628 t635 -> t634 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x3 x2000 x3500) (wrapNX id (nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x4 x2)) x2001 x3500)))))

d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t634,Curry_Prelude.Curry t635,Curry_Prelude.Curry t611) => t633 -> t634 -> (t633 -> ConstStore -> t634 -> ConstStore -> t635 -> ConstStore -> t611) -> t635 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x2 x3 x4 x3500 = d_C_ret (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 x1 x3500) x2 x3500) x4 x3500)

nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 :: (Curry_Prelude.Curry t633,Curry_Prelude.Curry t634,Curry_Prelude.Curry t635,Curry_Prelude.Curry t611) => t633 -> t634 -> Func t633 (Func t634 (Func t635 t611)) -> t635 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t611 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq3__dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (wrapDX id (d_C_ret (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 x1 x2000 x3500) x2 x2001 x3500)))) x4 x2003 x3500)))))))

nd_OP_seq3__dot_sh_dot_176 :: (Curry_Prelude.Curry t615,Curry_Prelude.Curry t616,Curry_Prelude.Curry t633,Curry_Prelude.Curry t621,Curry_Prelude.Curry t622,Curry_Prelude.Curry t634,Curry_Prelude.Curry t627,Curry_Prelude.Curry t628,Curry_Prelude.Curry t635,Curry_Prelude.Curry t611) => Func t633 (Func t634 (Func t635 t611)) -> C_XmlConv t615 t616 t633 -> C_XmlConv t621 t622 t634 -> C_XmlConv t627 t628 t635 -> t611 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2007 = leftSupply x2006
          x2008 = rightSupply x2006
           in (seq x2007 (seq x2008 (let
               x2002 = leftSupply x2007
               x2003 = rightSupply x2007
                in (seq x2002 (seq x2003 (let
                    x2004 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2004 (seq x2005 (let
                         x6 = generate x2003
                         x7 = generate x2004
                         x8 = generate x2005
                          in (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (nd_OP___cond_0_seq3__dot_sh_dot_176 x2 x6 x3 x7 x4 x8 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq3__dot_cf_dot_176 x1 x6 x7 x8 x2000 x3500) x5 x3500) x2001 x3500)))))))))))))))

d_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x3 x3500) x4 x3500) (Curry_Prelude.d_C_apply (d_C_xmlShows x5 x3500) x6 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_Success -> let
          x2013 = x3000
           in (seq x2013 (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2002 = leftSupply x2014
                    x2010 = rightSupply x2014
                     in (seq x2002 (seq x2010 (Curry_Prelude.nd_OP_dot (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (let
                         x2009 = leftSupply x2010
                         x2011 = rightSupply x2010
                          in (seq x2009 (seq x2011 (let
                              x2005 = leftSupply x2011
                              x2008 = rightSupply x2011
                               in (seq x2005 (seq x2008 (Curry_Prelude.nd_OP_dot (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x3 x2003 x3500) x4 x2004 x3500)))) (let
                                   x2007 = leftSupply x2008
                                   x2006 = rightSupply x2008
                                    in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x5 x2006 x3500) x6 x2007 x3500)))) x2009 x3500))))))) x2012 x3500))))))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq3__dot_sh_dot_176 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_seq3 :: (Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => IDSupply -> ConstStore -> Func (Func t0 (Func t1 (Func t2 t3))) (Func (C_XmlConv t4 t5 t0) (Func (C_XmlConv t6 t7 t1) (Func (C_XmlConv t8 t9 t2) (C_XmlConv C_NotRepeatable C_NoElem t3))))
nd_C_seq3 x3000 x3500 = wrapDX (wrapDX (wrapDX (wrapNX id))) (acceptCs (acceptCs (acceptCs id)) nd_C_seq3_)

nd_C_repSeq3 :: (Curry_Prelude.Curry t4,Curry_Prelude.Curry t0,Curry_Prelude.Curry t5,Curry_Prelude.Curry t1,Curry_Prelude.Curry t6,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Func t0 (Func t1 (Func t2 t3)) -> C_XmlConv C_Repeatable t4 t0 -> C_XmlConv C_Repeatable t5 t1 -> C_XmlConv C_Repeatable t6 t2 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t3)
nd_C_repSeq3 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq3_ x1 x2 x3 x4 x2000 x3500) x2001 x3500)))))

nd_C_eSeq3 :: (Curry_Prelude.Curry t4,Curry_Prelude.Curry t5,Curry_Prelude.Curry t0,Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 t3)) -> C_XmlConv t4 t5 t0 -> C_XmlConv t6 t7 t1 -> C_XmlConv t8 t9 t2 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t3
nd_C_eSeq3 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (nd_C_element x1 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_seq3 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))) x5 x2007 x3500)))) x2009 x3500)))))

nd_C_eRepSeq3 :: (Curry_Prelude.Curry t4,Curry_Prelude.Curry t0,Curry_Prelude.Curry t5,Curry_Prelude.Curry t1,Curry_Prelude.Curry t6,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 t3)) -> C_XmlConv C_Repeatable t4 t0 -> C_XmlConv C_Repeatable t5 t1 -> C_XmlConv C_Repeatable t6 t2 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t3)
nd_C_eRepSeq3 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq3 x2 x3 x4 x5 x2000 x3500) x2001 x3500)))))

nd_C_seq4_ :: (Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t0,Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t1,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t2,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t3,Curry_Prelude.Curry t13,Curry_Prelude.Curry t4) => Func t0 (Func t1 (Func t2 (Func t3 t4))) -> C_XmlConv t5 t6 t0 -> C_XmlConv t7 t8 t1 -> C_XmlConv t9 t10 t2 -> C_XmlConv t11 t12 t3 -> IDSupply -> ConstStore -> C_XmlConv t13 C_NoElem t4
nd_C_seq4_ x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x6 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq4__dot___hash_lambda16 x1 x3 x4 x5)) x2001 x3500)))
           in (HO_C_Conv x6 (wrapNX id (nd_OP_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5)))))

d_OP_seq4__dot_cf_dot_194 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t810,Curry_Prelude.Curry t811,Curry_Prelude.Curry t780) => (t808 -> ConstStore -> t809 -> ConstStore -> t810 -> ConstStore -> t811 -> ConstStore -> t780) -> t808 -> t809 -> t810 -> t811 -> ConstStore -> t780
d_OP_seq4__dot_cf_dot_194 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500) x4 x3500) x5 x3500

nd_OP_seq4__dot_cf_dot_194 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t810,Curry_Prelude.Curry t811,Curry_Prelude.Curry t780) => Func t808 (Func t809 (Func t810 (Func t811 t780))) -> t808 -> t809 -> t810 -> t811 -> IDSupply -> ConstStore -> t780
nd_OP_seq4__dot_cf_dot_194 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))) x4 x2003 x3500)))) x5 x2005 x3500)))))

d_OP_seq4__dot___hash_lambda16 :: (Curry_Prelude.Curry t790,Curry_Prelude.Curry t791,Curry_Prelude.Curry t809,Curry_Prelude.Curry t796,Curry_Prelude.Curry t797,Curry_Prelude.Curry t810,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t808,Curry_Prelude.Curry t780) => (t808 -> ConstStore -> t809 -> ConstStore -> t810 -> ConstStore -> t811 -> ConstStore -> t780) -> C_XmlConv t790 t791 t809 -> C_XmlConv t796 t797 t810 -> C_XmlConv t802 t803 t811 -> t808 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq4__dot___hash_lambda16 x1 x2 x3 x4 x5 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x2 x3500) (d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 x5 x1 x3 x4) x3500

nd_OP_seq4__dot___hash_lambda16 :: (Curry_Prelude.Curry t790,Curry_Prelude.Curry t791,Curry_Prelude.Curry t809,Curry_Prelude.Curry t796,Curry_Prelude.Curry t797,Curry_Prelude.Curry t810,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t808,Curry_Prelude.Curry t780) => Func t808 (Func t809 (Func t810 (Func t811 t780))) -> C_XmlConv t790 t791 t809 -> C_XmlConv t796 t797 t810 -> C_XmlConv t802 t803 t811 -> t808 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq4__dot___hash_lambda16 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 x5 x1 x3 x4)) x2001 x3500)))))

d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t796,Curry_Prelude.Curry t797,Curry_Prelude.Curry t810,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t809,Curry_Prelude.Curry t780) => t808 -> (t808 -> ConstStore -> t809 -> ConstStore -> t810 -> ConstStore -> t811 -> ConstStore -> t780) -> C_XmlConv t796 t797 t810 -> C_XmlConv t802 t803 t811 -> t809 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 x1 x2 x3 x4 x5 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x3 x3500) (d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x5 x2 x4) x3500

nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t796,Curry_Prelude.Curry t797,Curry_Prelude.Curry t810,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t809,Curry_Prelude.Curry t780) => t808 -> Func t808 (Func t809 (Func t810 (Func t811 t780))) -> C_XmlConv t796 t797 t810 -> C_XmlConv t802 t803 t811 -> t809 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x3 x2000 x3500) (wrapNX id (nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x5 x2 x4)) x2001 x3500)))))

d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t810,Curry_Prelude.Curry t780) => t808 -> t809 -> (t808 -> ConstStore -> t809 -> ConstStore -> t810 -> ConstStore -> t811 -> ConstStore -> t780) -> C_XmlConv t802 t803 t811 -> t810 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x4 x3500) (d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x5 x3) x3500

nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t810,Curry_Prelude.Curry t780) => t808 -> t809 -> Func t808 (Func t809 (Func t810 (Func t811 t780))) -> C_XmlConv t802 t803 t811 -> t810 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x4 x2000 x3500) (wrapNX id (nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x5 x3)) x2001 x3500)))))

d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t810,Curry_Prelude.Curry t811,Curry_Prelude.Curry t780) => t808 -> t809 -> t810 -> (t808 -> ConstStore -> t809 -> ConstStore -> t810 -> ConstStore -> t811 -> ConstStore -> t780) -> t811 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x3 x4 x5 x3500 = d_C_ret (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x4 x1 x3500) x2 x3500) x3 x3500) x5 x3500)

nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 :: (Curry_Prelude.Curry t808,Curry_Prelude.Curry t809,Curry_Prelude.Curry t810,Curry_Prelude.Curry t811,Curry_Prelude.Curry t780) => t808 -> t809 -> t810 -> Func t808 (Func t809 (Func t810 (Func t811 t780))) -> t811 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t780 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq4__dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (wrapDX id (d_C_ret (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x4 x1 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x5 x2005 x3500)))))))

nd_OP_seq4__dot_sh_dot_194 :: (Curry_Prelude.Curry t784,Curry_Prelude.Curry t785,Curry_Prelude.Curry t808,Curry_Prelude.Curry t790,Curry_Prelude.Curry t791,Curry_Prelude.Curry t809,Curry_Prelude.Curry t796,Curry_Prelude.Curry t797,Curry_Prelude.Curry t810,Curry_Prelude.Curry t802,Curry_Prelude.Curry t803,Curry_Prelude.Curry t811,Curry_Prelude.Curry t780) => Func t808 (Func t809 (Func t810 (Func t811 t780))) -> C_XmlConv t784 t785 t808 -> C_XmlConv t790 t791 t809 -> C_XmlConv t796 t797 t810 -> C_XmlConv t802 t803 t811 -> t780 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
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
                              x7 = generate x2003
                              x8 = generate x2004
                              x9 = generate x2005
                              x10 = generate x2006
                               in (let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (nd_OP___cond_0_seq4__dot_sh_dot_194 x2 x7 x3 x8 x4 x9 x5 x10 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq4__dot_cf_dot_194 x1 x7 x8 x9 x10 x2000 x3500) x6 x3500) x2001 x3500))))))))))))))))))

d_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x3 x3500) x4 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x5 x3500) x6 x3500) (Curry_Prelude.d_C_apply (d_C_xmlShows x7 x3500) x8 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3500) (d_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_Success -> let
          x2019 = x3000
           in (seq x2019 (let
               x2018 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2018 (seq x2020 (let
                    x2002 = leftSupply x2020
                    x2016 = rightSupply x2020
                     in (seq x2002 (seq x2016 (Curry_Prelude.nd_OP_dot (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (let
                         x2015 = leftSupply x2016
                         x2017 = rightSupply x2016
                          in (seq x2015 (seq x2017 (let
                              x2005 = leftSupply x2017
                              x2013 = rightSupply x2017
                               in (seq x2005 (seq x2013 (Curry_Prelude.nd_OP_dot (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x3 x2003 x3500) x4 x2004 x3500)))) (let
                                   x2012 = leftSupply x2013
                                   x2014 = rightSupply x2013
                                    in (seq x2012 (seq x2014 (let
                                        x2008 = leftSupply x2014
                                        x2011 = rightSupply x2014
                                         in (seq x2008 (seq x2011 (Curry_Prelude.nd_OP_dot (let
                                             x2007 = leftSupply x2008
                                             x2006 = rightSupply x2008
                                              in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x5 x2006 x3500) x6 x2007 x3500)))) (let
                                             x2010 = leftSupply x2011
                                             x2009 = rightSupply x2011
                                              in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x7 x2009 x3500) x8 x2010 x3500)))) x2012 x3500))))))) x2015 x3500))))))) x2018 x3500))))))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq4__dot_sh_dot_194 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_seq4 :: (Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t0,Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t1,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t2,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => IDSupply -> ConstStore -> Func (Func t0 (Func t1 (Func t2 (Func t3 t4)))) (Func (C_XmlConv t5 t6 t0) (Func (C_XmlConv t7 t8 t1) (Func (C_XmlConv t9 t10 t2) (Func (C_XmlConv t11 t12 t3) (C_XmlConv C_NotRepeatable C_NoElem t4)))))
nd_C_seq4 x3000 x3500 = wrapDX (wrapDX (wrapDX (wrapDX (wrapNX id)))) (acceptCs (acceptCs (acceptCs (acceptCs id))) nd_C_seq4_)

nd_C_repSeq4 :: (Curry_Prelude.Curry t5,Curry_Prelude.Curry t0,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t7,Curry_Prelude.Curry t2,Curry_Prelude.Curry t8,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => Func t0 (Func t1 (Func t2 (Func t3 t4))) -> C_XmlConv C_Repeatable t5 t0 -> C_XmlConv C_Repeatable t6 t1 -> C_XmlConv C_Repeatable t7 t2 -> C_XmlConv C_Repeatable t8 t3 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t4)
nd_C_repSeq4 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq4_ x1 x2 x3 x4 x5 x2000 x3500) x2001 x3500)))))

nd_C_eSeq4 :: (Curry_Prelude.Curry t5,Curry_Prelude.Curry t6,Curry_Prelude.Curry t0,Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t1,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t2,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 t4))) -> C_XmlConv t5 t6 t0 -> C_XmlConv t7 t8 t1 -> C_XmlConv t9 t10 t2 -> C_XmlConv t11 t12 t3 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t4
nd_C_eSeq4 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2012 = x3000
      in (seq x2012 (let
          x2011 = leftSupply x2012
          x2010 = rightSupply x2012
           in (seq x2011 (seq x2010 (nd_C_element x1 (let
               x2009 = leftSupply x2010
               x2008 = rightSupply x2010
                in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
                    x2007 = leftSupply x2008
                    x2006 = rightSupply x2008
                     in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                         x2005 = leftSupply x2006
                         x2004 = rightSupply x2006
                          in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_seq4 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))) x5 x2007 x3500)))) x6 x2009 x3500)))) x2011 x3500)))))

nd_C_eRepSeq4 :: (Curry_Prelude.Curry t5,Curry_Prelude.Curry t0,Curry_Prelude.Curry t6,Curry_Prelude.Curry t1,Curry_Prelude.Curry t7,Curry_Prelude.Curry t2,Curry_Prelude.Curry t8,Curry_Prelude.Curry t3,Curry_Prelude.Curry t4) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 t4))) -> C_XmlConv C_Repeatable t5 t0 -> C_XmlConv C_Repeatable t6 t1 -> C_XmlConv C_Repeatable t7 t2 -> C_XmlConv C_Repeatable t8 t3 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t4)
nd_C_eRepSeq4 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq4 x2 x3 x4 x5 x6 x2000 x3500) x2001 x3500)))))

nd_C_seq5_ :: (Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t0,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t1,Curry_Prelude.Curry t10,Curry_Prelude.Curry t11,Curry_Prelude.Curry t2,Curry_Prelude.Curry t12,Curry_Prelude.Curry t13,Curry_Prelude.Curry t3,Curry_Prelude.Curry t14,Curry_Prelude.Curry t15,Curry_Prelude.Curry t4,Curry_Prelude.Curry t16,Curry_Prelude.Curry t5) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5)))) -> C_XmlConv t6 t7 t0 -> C_XmlConv t8 t9 t1 -> C_XmlConv t10 t11 t2 -> C_XmlConv t12 t13 t3 -> C_XmlConv t14 t15 t4 -> IDSupply -> ConstStore -> C_XmlConv t16 C_NoElem t5
nd_C_seq5_ x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x7 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq5__dot___hash_lambda21 x1 x3 x4 x5 x6)) x2001 x3500)))
           in (HO_C_Conv x7 (wrapNX id (nd_OP_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6)))))

d_OP_seq5__dot_cf_dot_213 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t993) => (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> t1027 -> t1028 -> t1029 -> t1030 -> t1031 -> ConstStore -> t993
d_OP_seq5__dot_cf_dot_213 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500) x4 x3500) x5 x3500) x6 x3500

nd_OP_seq5__dot_cf_dot_213 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t993) => Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> t1027 -> t1028 -> t1029 -> t1030 -> t1031 -> IDSupply -> ConstStore -> t993
nd_OP_seq5__dot_cf_dot_213 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))) x4 x2003 x3500)))) x5 x2005 x3500)))) x6 x2007 x3500)))))

d_OP_seq5__dot___hash_lambda21 :: (Curry_Prelude.Curry t1003,Curry_Prelude.Curry t1004,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1009,Curry_Prelude.Curry t1010,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1027,Curry_Prelude.Curry t993) => (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> C_XmlConv t1003 t1004 t1028 -> C_XmlConv t1009 t1010 t1029 -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1027 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq5__dot___hash_lambda21 x1 x2 x3 x4 x5 x6 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x2 x3500) (d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 x6 x1 x3 x4 x5) x3500

nd_OP_seq5__dot___hash_lambda21 :: (Curry_Prelude.Curry t1003,Curry_Prelude.Curry t1004,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1009,Curry_Prelude.Curry t1010,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1027,Curry_Prelude.Curry t993) => Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> C_XmlConv t1003 t1004 t1028 -> C_XmlConv t1009 t1010 t1029 -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1027 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq5__dot___hash_lambda21 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 x6 x1 x3 x4 x5)) x2001 x3500)))))

d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1009,Curry_Prelude.Curry t1010,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t993) => t1027 -> (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> C_XmlConv t1009 t1010 t1029 -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1028 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 x1 x2 x3 x4 x5 x6 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x3 x3500) (d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 x1 x6 x2 x4 x5) x3500

nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1009,Curry_Prelude.Curry t1010,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t993) => t1027 -> Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> C_XmlConv t1009 t1010 t1029 -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1028 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x3 x2000 x3500) (wrapNX id (nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 x1 x6 x2 x4 x5)) x2001 x3500)))))

d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t993) => t1027 -> t1028 -> (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1029 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 x1 x2 x3 x4 x5 x6 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x4 x3500) (d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x6 x3 x5) x3500

nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t993) => t1027 -> t1028 -> Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t1029 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x4 x2000 x3500) (wrapNX id (nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x6 x3 x5)) x2001 x3500)))))

d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t993) => t1027 -> t1028 -> t1029 -> (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> C_XmlConv t1021 t1022 t1031 -> t1030 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x3 x4 x5 x6 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x5 x3500) (d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3 x4 x6) x3500

nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t993) => t1027 -> t1028 -> t1029 -> Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> C_XmlConv t1021 t1022 t1031 -> t1030 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x5 x2000 x3500) (wrapNX id (nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3 x4 x6)) x2001 x3500)))))

d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t993) => t1027 -> t1028 -> t1029 -> (t1027 -> ConstStore -> t1028 -> ConstStore -> t1029 -> ConstStore -> t1030 -> ConstStore -> t1031 -> ConstStore -> t993) -> t1030 -> t1031 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3 x4 x5 x6 x3500 = d_C_ret (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x4 x1 x3500) x2 x3500) x3 x3500) x5 x3500) x6 x3500)

nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 :: (Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t993) => t1027 -> t1028 -> t1029 -> Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> t1030 -> t1031 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t993 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq5__dot___hash_lambda21_dot___hash_lambda22_dot___hash_lambda23_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (wrapDX id (d_C_ret (let
          x2007 = leftSupply x2008
          x2006 = rightSupply x2008
           in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x4 x1 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x5 x2005 x3500)))) x6 x2007 x3500)))))))

nd_OP_seq5__dot_sh_dot_213 :: (Curry_Prelude.Curry t997,Curry_Prelude.Curry t998,Curry_Prelude.Curry t1027,Curry_Prelude.Curry t1003,Curry_Prelude.Curry t1004,Curry_Prelude.Curry t1028,Curry_Prelude.Curry t1009,Curry_Prelude.Curry t1010,Curry_Prelude.Curry t1029,Curry_Prelude.Curry t1015,Curry_Prelude.Curry t1016,Curry_Prelude.Curry t1030,Curry_Prelude.Curry t1021,Curry_Prelude.Curry t1022,Curry_Prelude.Curry t1031,Curry_Prelude.Curry t993) => Func t1027 (Func t1028 (Func t1029 (Func t1030 (Func t1031 t993)))) -> C_XmlConv t997 t998 t1027 -> C_XmlConv t1003 t1004 t1028 -> C_XmlConv t1009 t1010 t1029 -> C_XmlConv t1015 t1016 t1030 -> C_XmlConv t1021 t1022 t1031 -> t993 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2009 = leftSupply x2008
          x2011 = rightSupply x2008
           in (seq x2009 (seq x2011 (let
               x2002 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2002 (seq x2010 (let
                    x2003 = leftSupply x2010
                    x2004 = rightSupply x2010
                     in (seq x2003 (seq x2004 (let
                         x2005 = leftSupply x2011
                         x2012 = rightSupply x2011
                          in (seq x2005 (seq x2012 (let
                              x2006 = leftSupply x2012
                              x2007 = rightSupply x2012
                               in (seq x2006 (seq x2007 (let
                                   x8 = generate x2003
                                   x9 = generate x2004
                                   x10 = generate x2005
                                   x11 = generate x2006
                                   x12 = generate x2007
                                    in (let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (nd_OP___cond_0_seq5__dot_sh_dot_213 x2 x8 x3 x9 x4 x10 x5 x11 x6 x12 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq5__dot_cf_dot_213 x1 x8 x9 x10 x11 x12 x2000 x3500) x7 x3500) x2001 x3500)))))))))))))))))))))

d_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x3 x3500) x4 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x5 x3500) x6 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x7 x3500) x8 x3500) (Curry_Prelude.d_C_apply (d_C_xmlShows x9 x3500) x10 x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1002 x3500) (d_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_Success -> let
          x2025 = x3000
           in (seq x2025 (let
               x2024 = leftSupply x2025
               x2026 = rightSupply x2025
                in (seq x2024 (seq x2026 (let
                    x2002 = leftSupply x2026
                    x2022 = rightSupply x2026
                     in (seq x2002 (seq x2022 (Curry_Prelude.nd_OP_dot (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (let
                         x2021 = leftSupply x2022
                         x2023 = rightSupply x2022
                          in (seq x2021 (seq x2023 (let
                              x2005 = leftSupply x2023
                              x2019 = rightSupply x2023
                               in (seq x2005 (seq x2019 (Curry_Prelude.nd_OP_dot (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x3 x2003 x3500) x4 x2004 x3500)))) (let
                                   x2018 = leftSupply x2019
                                   x2020 = rightSupply x2019
                                    in (seq x2018 (seq x2020 (let
                                        x2008 = leftSupply x2020
                                        x2016 = rightSupply x2020
                                         in (seq x2008 (seq x2016 (Curry_Prelude.nd_OP_dot (let
                                             x2007 = leftSupply x2008
                                             x2006 = rightSupply x2008
                                              in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x5 x2006 x3500) x6 x2007 x3500)))) (let
                                             x2015 = leftSupply x2016
                                             x2017 = rightSupply x2016
                                              in (seq x2015 (seq x2017 (let
                                                  x2011 = leftSupply x2017
                                                  x2014 = rightSupply x2017
                                                   in (seq x2011 (seq x2014 (Curry_Prelude.nd_OP_dot (let
                                                       x2010 = leftSupply x2011
                                                       x2009 = rightSupply x2011
                                                        in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x7 x2009 x3500) x8 x2010 x3500)))) (let
                                                       x2013 = leftSupply x2014
                                                       x2012 = rightSupply x2014
                                                        in (seq x2013 (seq x2012 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x9 x2012 x3500) x10 x2013 x3500)))) x2015 x3500))))))) x2018 x3500))))))) x2021 x3500))))))) x2024 x3500))))))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1002 x3000 x3500) (nd_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq5__dot_sh_dot_213 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_seq5 :: (Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t0,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t1,Curry_Prelude.Curry t10,Curry_Prelude.Curry t11,Curry_Prelude.Curry t2,Curry_Prelude.Curry t12,Curry_Prelude.Curry t13,Curry_Prelude.Curry t3,Curry_Prelude.Curry t14,Curry_Prelude.Curry t15,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => IDSupply -> ConstStore -> Func (Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5))))) (Func (C_XmlConv t6 t7 t0) (Func (C_XmlConv t8 t9 t1) (Func (C_XmlConv t10 t11 t2) (Func (C_XmlConv t12 t13 t3) (Func (C_XmlConv t14 t15 t4) (C_XmlConv C_NotRepeatable C_NoElem t5))))))
nd_C_seq5 x3000 x3500 = wrapDX (wrapDX (wrapDX (wrapDX (wrapDX (wrapNX id))))) (acceptCs (acceptCs (acceptCs (acceptCs (acceptCs id)))) nd_C_seq5_)

nd_C_repSeq5 :: (Curry_Prelude.Curry t6,Curry_Prelude.Curry t0,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t8,Curry_Prelude.Curry t2,Curry_Prelude.Curry t9,Curry_Prelude.Curry t3,Curry_Prelude.Curry t10,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5)))) -> C_XmlConv C_Repeatable t6 t0 -> C_XmlConv C_Repeatable t7 t1 -> C_XmlConv C_Repeatable t8 t2 -> C_XmlConv C_Repeatable t9 t3 -> C_XmlConv C_Repeatable t10 t4 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t5)
nd_C_repSeq5 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq5_ x1 x2 x3 x4 x5 x6 x2000 x3500) x2001 x3500)))))

nd_C_eSeq5 :: (Curry_Prelude.Curry t6,Curry_Prelude.Curry t7,Curry_Prelude.Curry t0,Curry_Prelude.Curry t8,Curry_Prelude.Curry t9,Curry_Prelude.Curry t1,Curry_Prelude.Curry t10,Curry_Prelude.Curry t11,Curry_Prelude.Curry t2,Curry_Prelude.Curry t12,Curry_Prelude.Curry t13,Curry_Prelude.Curry t3,Curry_Prelude.Curry t14,Curry_Prelude.Curry t15,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5)))) -> C_XmlConv t6 t7 t0 -> C_XmlConv t8 t9 t1 -> C_XmlConv t10 t11 t2 -> C_XmlConv t12 t13 t3 -> C_XmlConv t14 t15 t4 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t5
nd_C_eSeq5 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2014 = x3000
      in (seq x2014 (let
          x2013 = leftSupply x2014
          x2012 = rightSupply x2014
           in (seq x2013 (seq x2012 (nd_C_element x1 (let
               x2011 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2011 (seq x2010 (Curry_Prelude.nd_C_apply (let
                    x2009 = leftSupply x2010
                    x2008 = rightSupply x2010
                     in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
                         x2007 = leftSupply x2008
                         x2006 = rightSupply x2008
                          in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                              x2005 = leftSupply x2006
                              x2004 = rightSupply x2006
                               in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                                   x2003 = leftSupply x2004
                                   x2002 = rightSupply x2004
                                    in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_seq5 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))) x5 x2007 x3500)))) x6 x2009 x3500)))) x7 x2011 x3500)))) x2013 x3500)))))

nd_C_eRepSeq5 :: (Curry_Prelude.Curry t6,Curry_Prelude.Curry t0,Curry_Prelude.Curry t7,Curry_Prelude.Curry t1,Curry_Prelude.Curry t8,Curry_Prelude.Curry t2,Curry_Prelude.Curry t9,Curry_Prelude.Curry t3,Curry_Prelude.Curry t10,Curry_Prelude.Curry t4,Curry_Prelude.Curry t5) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 (Func t4 t5)))) -> C_XmlConv C_Repeatable t6 t0 -> C_XmlConv C_Repeatable t7 t1 -> C_XmlConv C_Repeatable t8 t2 -> C_XmlConv C_Repeatable t9 t3 -> C_XmlConv C_Repeatable t10 t4 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t5)
nd_C_eRepSeq5 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq5 x2 x3 x4 x5 x6 x7 x2000 x3500) x2001 x3500)))))

nd_C_seq6_ :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t0,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t1,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t2,Curry_Prelude.Curry t13,Curry_Prelude.Curry t14,Curry_Prelude.Curry t3,Curry_Prelude.Curry t15,Curry_Prelude.Curry t16,Curry_Prelude.Curry t4,Curry_Prelude.Curry t17,Curry_Prelude.Curry t18,Curry_Prelude.Curry t5,Curry_Prelude.Curry t19,Curry_Prelude.Curry t6) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6))))) -> C_XmlConv t7 t8 t0 -> C_XmlConv t9 t10 t1 -> C_XmlConv t11 t12 t2 -> C_XmlConv t13 t14 t3 -> C_XmlConv t15 t16 t4 -> C_XmlConv t17 t18 t5 -> IDSupply -> ConstStore -> C_XmlConv t19 C_NoElem t6
nd_C_seq6_ x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x8 = let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27 x1 x3 x4 x5 x6 x7)) x2001 x3500)))
           in (HO_C_Conv x8 (wrapNX id (nd_OP_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7)))))

d_OP_seq6__dot_cf_dot_233 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1250) => (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> t1290 -> t1291 -> t1292 -> t1293 -> t1294 -> t1295 -> ConstStore -> t1250
d_OP_seq6__dot_cf_dot_233 x1 x2 x3 x4 x5 x6 x7 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500) x4 x3500) x5 x3500) x6 x3500) x7 x3500

nd_OP_seq6__dot_cf_dot_233 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1250) => Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> t1290 -> t1291 -> t1292 -> t1293 -> t1294 -> t1295 -> IDSupply -> ConstStore -> t1250
nd_OP_seq6__dot_cf_dot_233 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))) x4 x2003 x3500)))) x5 x2005 x3500)))) x6 x2007 x3500)))) x7 x2009 x3500)))))

d_OP_seq6__dot___hash_lambda27 :: (Curry_Prelude.Curry t1260,Curry_Prelude.Curry t1261,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1266,Curry_Prelude.Curry t1267,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1250) => (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> C_XmlConv t1260 t1261 t1291 -> C_XmlConv t1266 t1267 t1292 -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1290 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x2 x3500) (d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 x7 x1 x3 x4 x5 x6) x3500

nd_OP_seq6__dot___hash_lambda27 :: (Curry_Prelude.Curry t1260,Curry_Prelude.Curry t1261,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1266,Curry_Prelude.Curry t1267,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1250) => Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> C_XmlConv t1260 t1261 t1291 -> C_XmlConv t1266 t1267 t1292 -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1290 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x2 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 x7 x1 x3 x4 x5 x6)) x2001 x3500)))))

d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1266,Curry_Prelude.Curry t1267,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1250) => t1290 -> (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> C_XmlConv t1266 t1267 t1292 -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1291 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x3 x3500) (d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x7 x2 x4 x5 x6) x3500

nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1266,Curry_Prelude.Curry t1267,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1250) => t1290 -> Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> C_XmlConv t1266 t1267 t1292 -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1291 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x3 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x7 x2 x4 x5 x6)) x2001 x3500)))))

d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1292 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x4 x3500) (d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x7 x3 x5 x6) x3500

nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1292 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x4 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x7 x3 x5 x6)) x2001 x3500)))))

d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1293 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x5 x3500) (d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 x1 x2 x3 x4 x7 x6) x3500

nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1293 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x5 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 x1 x2 x3 x4 x7 x6)) x2001 x3500)))))

d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> t1293 -> C_XmlConv t1284 t1285 t1295 -> t1294 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 x1 x2 x3 x4 x5 x6 x7 x3500 = d_OP_slash_gt_eq (d_C_xmlReads x6 x3500) (d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x5 x7) x3500

nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> t1293 -> C_XmlConv t1284 t1285 t1295 -> t1294 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq (nd_C_xmlReads x6 x2000 x3500) (wrapNX id (nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x5 x7)) x2001 x3500)))))

d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> (t1290 -> ConstStore -> t1291 -> ConstStore -> t1292 -> ConstStore -> t1293 -> ConstStore -> t1294 -> ConstStore -> t1295 -> ConstStore -> t1250) -> t1293 -> t1294 -> t1295 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp) -> ConstStore -> Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
d_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x5 x6 x7 x3500 = d_C_ret (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x4 x1 x3500) x2 x3500) x3 x3500) x5 x3500) x6 x3500) x7 x3500)

nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 :: (Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1250) => t1290 -> t1291 -> t1292 -> Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> t1293 -> t1294 -> t1295 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 t1250 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)))
nd_OP_seq6__dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29_dot___hash_lambda30_dot___hash_lambda31_dot___hash_lambda32 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (wrapDX id (d_C_ret (let
          x2009 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x4 x1 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x5 x2005 x3500)))) x6 x2007 x3500)))) x7 x2009 x3500)))))))

nd_OP_seq6__dot_sh_dot_233 :: (Curry_Prelude.Curry t1254,Curry_Prelude.Curry t1255,Curry_Prelude.Curry t1290,Curry_Prelude.Curry t1260,Curry_Prelude.Curry t1261,Curry_Prelude.Curry t1291,Curry_Prelude.Curry t1266,Curry_Prelude.Curry t1267,Curry_Prelude.Curry t1292,Curry_Prelude.Curry t1272,Curry_Prelude.Curry t1273,Curry_Prelude.Curry t1293,Curry_Prelude.Curry t1278,Curry_Prelude.Curry t1279,Curry_Prelude.Curry t1294,Curry_Prelude.Curry t1284,Curry_Prelude.Curry t1285,Curry_Prelude.Curry t1295,Curry_Prelude.Curry t1250) => Func t1290 (Func t1291 (Func t1292 (Func t1293 (Func t1294 (Func t1295 t1250))))) -> C_XmlConv t1254 t1255 t1290 -> C_XmlConv t1260 t1261 t1291 -> C_XmlConv t1266 t1267 t1292 -> C_XmlConv t1272 t1273 t1293 -> C_XmlConv t1278 t1279 t1294 -> C_XmlConv t1284 t1285 t1295 -> t1250 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_XML.C_XmlExp))
nd_OP_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2010 = leftSupply x2009
          x2012 = rightSupply x2009
           in (seq x2010 (seq x2012 (let
               x2002 = leftSupply x2010
               x2011 = rightSupply x2010
                in (seq x2002 (seq x2011 (let
                    x2003 = leftSupply x2011
                    x2004 = rightSupply x2011
                     in (seq x2003 (seq x2004 (let
                         x2013 = leftSupply x2012
                         x2014 = rightSupply x2012
                          in (seq x2013 (seq x2014 (let
                              x2005 = leftSupply x2013
                              x2006 = rightSupply x2013
                               in (seq x2005 (seq x2006 (let
                                   x2007 = leftSupply x2014
                                   x2008 = rightSupply x2014
                                    in (seq x2007 (seq x2008 (let
                                        x9 = generate x2003
                                        x10 = generate x2004
                                        x11 = generate x2005
                                        x12 = generate x2006
                                        x13 = generate x2007
                                        x14 = generate x2008
                                         in (let
                                             x2001 = leftSupply x2002
                                             x2000 = rightSupply x2002
                                              in (seq x2001 (seq x2000 (nd_OP___cond_0_seq6__dot_sh_dot_233 x2 x9 x3 x10 x4 x11 x5 x12 x6 x13 x7 x14 (Curry_Prelude.d_OP_eq_colon_lt_eq (nd_OP_seq6__dot_cf_dot_233 x1 x9 x10 x11 x12 x13 x14 x2000 x3500) x8 x3500) x2001 x3500))))))))))))))))))))))))

d_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x3500 = case x13 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x1 x3500) x2 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x3 x3500) x4 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x5 x3500) x6 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x7 x3500) x8 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (d_C_xmlShows x9 x3500) x10 x3500) (Curry_Prelude.d_C_apply (d_C_xmlShows x11 x3500) x12 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1002 x3500) (d_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_Success -> let
          x2031 = x3000
           in (seq x2031 (let
               x2030 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2030 (seq x2032 (let
                    x2002 = leftSupply x2032
                    x2028 = rightSupply x2032
                     in (seq x2002 (seq x2028 (Curry_Prelude.nd_OP_dot (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x1 x2000 x3500) x2 x2001 x3500)))) (let
                         x2027 = leftSupply x2028
                         x2029 = rightSupply x2028
                          in (seq x2027 (seq x2029 (let
                              x2005 = leftSupply x2029
                              x2025 = rightSupply x2029
                               in (seq x2005 (seq x2025 (Curry_Prelude.nd_OP_dot (let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x3 x2003 x3500) x4 x2004 x3500)))) (let
                                   x2024 = leftSupply x2025
                                   x2026 = rightSupply x2025
                                    in (seq x2024 (seq x2026 (let
                                        x2008 = leftSupply x2026
                                        x2022 = rightSupply x2026
                                         in (seq x2008 (seq x2022 (Curry_Prelude.nd_OP_dot (let
                                             x2007 = leftSupply x2008
                                             x2006 = rightSupply x2008
                                              in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x5 x2006 x3500) x6 x2007 x3500)))) (let
                                             x2021 = leftSupply x2022
                                             x2023 = rightSupply x2022
                                              in (seq x2021 (seq x2023 (let
                                                  x2011 = leftSupply x2023
                                                  x2019 = rightSupply x2023
                                                   in (seq x2011 (seq x2019 (Curry_Prelude.nd_OP_dot (let
                                                       x2010 = leftSupply x2011
                                                       x2009 = rightSupply x2011
                                                        in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x7 x2009 x3500) x8 x2010 x3500)))) (let
                                                       x2018 = leftSupply x2019
                                                       x2020 = rightSupply x2019
                                                        in (seq x2018 (seq x2020 (let
                                                            x2014 = leftSupply x2020
                                                            x2017 = rightSupply x2020
                                                             in (seq x2014 (seq x2017 (Curry_Prelude.nd_OP_dot (let
                                                                 x2013 = leftSupply x2014
                                                                 x2012 = rightSupply x2014
                                                                  in (seq x2013 (seq x2012 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x9 x2012 x3500) x10 x2013 x3500)))) (let
                                                                 x2016 = leftSupply x2017
                                                                 x2015 = rightSupply x2017
                                                                  in (seq x2016 (seq x2015 (Curry_Prelude.nd_C_apply (nd_C_xmlShows x11 x2015 x3500) x12 x2016 x3500)))) x2018 x3500))))))) x2021 x3500))))))) x2024 x3500))))))) x2027 x3500))))))) x2030 x3500))))))))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1002 x3000 x3500) (nd_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_seq6__dot_sh_dot_233 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_seq6 :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t0,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t1,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t2,Curry_Prelude.Curry t13,Curry_Prelude.Curry t14,Curry_Prelude.Curry t3,Curry_Prelude.Curry t15,Curry_Prelude.Curry t16,Curry_Prelude.Curry t4,Curry_Prelude.Curry t17,Curry_Prelude.Curry t18,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => IDSupply -> ConstStore -> Func (Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6)))))) (Func (C_XmlConv t7 t8 t0) (Func (C_XmlConv t9 t10 t1) (Func (C_XmlConv t11 t12 t2) (Func (C_XmlConv t13 t14 t3) (Func (C_XmlConv t15 t16 t4) (Func (C_XmlConv t17 t18 t5) (C_XmlConv C_NotRepeatable C_NoElem t6)))))))
nd_C_seq6 x3000 x3500 = wrapDX (wrapDX (wrapDX (wrapDX (wrapDX (wrapDX (wrapNX id)))))) (acceptCs (acceptCs (acceptCs (acceptCs (acceptCs (acceptCs id))))) nd_C_seq6_)

nd_C_repSeq6 :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t0,Curry_Prelude.Curry t8,Curry_Prelude.Curry t1,Curry_Prelude.Curry t9,Curry_Prelude.Curry t2,Curry_Prelude.Curry t10,Curry_Prelude.Curry t3,Curry_Prelude.Curry t11,Curry_Prelude.Curry t4,Curry_Prelude.Curry t12,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6))))) -> C_XmlConv C_Repeatable t7 t0 -> C_XmlConv C_Repeatable t8 t1 -> C_XmlConv C_Repeatable t9 t2 -> C_XmlConv C_Repeatable t10 t3 -> C_XmlConv C_Repeatable t11 t4 -> C_XmlConv C_Repeatable t12 t5 -> IDSupply -> ConstStore -> C_XmlConv C_NotRepeatable C_NoElem (Curry_Prelude.OP_List t6)
nd_C_repSeq6 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_rep (nd_C_seq6_ x1 x2 x3 x4 x5 x6 x7 x2000 x3500) x2001 x3500)))))

nd_C_eSeq6 :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t8,Curry_Prelude.Curry t0,Curry_Prelude.Curry t9,Curry_Prelude.Curry t10,Curry_Prelude.Curry t1,Curry_Prelude.Curry t11,Curry_Prelude.Curry t12,Curry_Prelude.Curry t2,Curry_Prelude.Curry t13,Curry_Prelude.Curry t14,Curry_Prelude.Curry t3,Curry_Prelude.Curry t15,Curry_Prelude.Curry t16,Curry_Prelude.Curry t4,Curry_Prelude.Curry t17,Curry_Prelude.Curry t18,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6))))) -> C_XmlConv t7 t8 t0 -> C_XmlConv t9 t10 t1 -> C_XmlConv t11 t12 t2 -> C_XmlConv t13 t14 t3 -> C_XmlConv t15 t16 t4 -> C_XmlConv t17 t18 t5 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem t6
nd_C_eSeq6 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2016 = x3000
      in (seq x2016 (let
          x2015 = leftSupply x2016
          x2014 = rightSupply x2016
           in (seq x2015 (seq x2014 (nd_C_element x1 (let
               x2013 = leftSupply x2014
               x2012 = rightSupply x2014
                in (seq x2013 (seq x2012 (Curry_Prelude.nd_C_apply (let
                    x2011 = leftSupply x2012
                    x2010 = rightSupply x2012
                     in (seq x2011 (seq x2010 (Curry_Prelude.nd_C_apply (let
                         x2009 = leftSupply x2010
                         x2008 = rightSupply x2010
                          in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (let
                              x2007 = leftSupply x2008
                              x2006 = rightSupply x2008
                               in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                                   x2005 = leftSupply x2006
                                   x2004 = rightSupply x2006
                                    in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                                        x2003 = leftSupply x2004
                                        x2002 = rightSupply x2004
                                         in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                             x2001 = leftSupply x2002
                                             x2000 = rightSupply x2002
                                              in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_seq6 x2000 x3500) x2 x2001 x3500)))) x3 x2003 x3500)))) x4 x2005 x3500)))) x5 x2007 x3500)))) x6 x2009 x3500)))) x7 x2011 x3500)))) x8 x2013 x3500)))) x2015 x3500)))))

nd_C_eRepSeq6 :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t0,Curry_Prelude.Curry t8,Curry_Prelude.Curry t1,Curry_Prelude.Curry t9,Curry_Prelude.Curry t2,Curry_Prelude.Curry t10,Curry_Prelude.Curry t3,Curry_Prelude.Curry t11,Curry_Prelude.Curry t4,Curry_Prelude.Curry t12,Curry_Prelude.Curry t5,Curry_Prelude.Curry t6) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func t0 (Func t1 (Func t2 (Func t3 (Func t4 (Func t5 t6))))) -> C_XmlConv C_Repeatable t7 t0 -> C_XmlConv C_Repeatable t8 t1 -> C_XmlConv C_Repeatable t9 t2 -> C_XmlConv C_Repeatable t10 t3 -> C_XmlConv C_Repeatable t11 t4 -> C_XmlConv C_Repeatable t12 t5 -> IDSupply -> ConstStore -> C_XmlConv C_Repeatable C_Elem (Curry_Prelude.OP_List t6)
nd_C_eRepSeq6 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_element x1 (nd_C_repSeq6 x2 x3 x4 x5 x6 x7 x8 x2000 x3500) x2001 x3500)))))

d_OP__case_0 x3 x4 x2 x3500 = case x2 of
     (C_Conv x5 x6) -> let
          x7 = d_OP_slash_gt_eq x5 (Curry_Prelude.d_OP_dot (acceptCs id d_C_ret) x3 x3500) x3500
          x8 = Curry_Prelude.d_OP_dot x6 x4 x3500
           in (C_Conv x7 x8)
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x2 x3000 x3500 = case x2 of
     (HO_C_Conv x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x7 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (nd_OP_slash_gt_eq x5 (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id d_C_ret)) x3 x2000 x3500) x2001 x3500)))
                    x8 = Curry_Prelude.nd_OP_dot x6 x4 x2003 x3500
                     in (HO_C_Conv x7 x8))))))
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x3 x4
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Cons x3 x7)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x6 x7 x1002 x3500) (d_OP__case_1 x1 x3 x4 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x3 x4
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Cons x3 x7)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x6 x7 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x1002 x3500) (d_OP__case_2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x1002 x3000 x3500) (nd_OP__case_2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3500) (d_OP__case_3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x5
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1002 x3000 x3500) (nd_OP__case_3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x5 x6 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons (Curry_XML.C_XElem x1 x7 x8) x6)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x5 x6 x1002 x3500) (d_OP__case_4 x1 x2 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x5 x6 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons (Curry_XML.C_XElem x1 x7 x8) x6)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_5 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3500) (d_OP__case_6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1002 x3000 x3500) (nd_OP__case_6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x5 x4 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x5
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x1002 x3500) (d_OP__case_5 x5 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x5 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x5
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x5 x1002 x3000 x3500) (nd_OP__case_5 x5 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x5 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_7 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1002 x3500) (d_OP__case_8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1002 x3000 x3500) (nd_OP__case_8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x4 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x8
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3500) (d_OP__case_7 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x8
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1002 x3000 x3500) (nd_OP__case_7 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_9 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1002 x3500) (d_OP__case_10 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1002 x3000 x3500) (nd_OP__case_10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x4 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x7
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1002 x3500) (d_OP__case_9 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x7
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1002 x3000 x3500) (nd_OP__case_9 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_11 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1002 x3500) (d_OP__case_12 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1002 x3000 x3500) (nd_OP__case_12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x4 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x6
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1002 x3500) (d_OP__case_11 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x6
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1002 x3000 x3500) (nd_OP__case_11 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_13 x2 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x2 x1002 x3500) (d_OP__case_14 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x2 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x2 x1002 x3000 x3500) (nd_OP__case_14 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x2 x4 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x2
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x2 x1002 x3500) (d_OP__case_13 x2 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x2 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x2 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x2 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XElem x6 x7 x8) -> x2
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x2 x1002 x3000 x3500) (nd_OP__case_13 x2 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x2 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_18 x2 x4 x6 x7 x8 (Curry_Prelude.d_C_apply (d_C_xmlReads x2 x3500) (Curry_Prelude.OP_Tuple2 x6 x7) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1002 x3500) (d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_18 x2 x4 x6 x7 x8 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_xmlReads x2 x2000 x3500) (Curry_Prelude.OP_Tuple2 x6 x7) x2001 x3500)))) x2003 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x2 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x2 x4 x6 x7 x8 x11 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> d_OP__case_17 x4 x8 x9 x10 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x2 x4 x6 x7 x8 x1002 x3500) (d_OP__case_18 x2 x4 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x2 x4 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x2 x4 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x2 x4 x6 x7 x8 x11 x3000 x3500 = case x11 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x4 x8 x9 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x2 x4 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_18 x2 x4 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x2 x4 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x2 x4 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x4 x8 x9 x10 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> d_OP__case_16 x4 x8 x9 x12 x11 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x4 x8 x9 x1002 x3500) (d_OP__case_17 x4 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x4 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x4 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x4 x8 x9 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x4 x8 x9 x12 x11 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x4 x8 x9 x1002 x3000 x3500) (nd_OP__case_17 x4 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x4 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x4 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x4 x8 x9 x12 x11 x3500 = case x11 of
     Curry_Prelude.OP_List -> d_OP__case_15 x4 x8 x9 x12 x3500
     (Curry_Prelude.OP_Cons x15 x16) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x4 x8 x9 x12 x1002 x3500) (d_OP__case_16 x4 x8 x9 x12 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x4 x8 x9 x12 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x4 x8 x9 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x4 x8 x9 x12 x11 x3000 x3500 = case x11 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x4 x8 x9 x12 x2000 x3500))
     (Curry_Prelude.OP_Cons x15 x16) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x4 x8 x9 x12 x1002 x3000 x3500) (nd_OP__case_16 x4 x8 x9 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x4 x8 x9 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x4 x8 x9 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x4 x8 x9 x12 x3500 = case x12 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Tuple2 x4 x8)
     (Curry_Prelude.OP_Cons x13 x14) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x4 x8 x9 x1002 x3500) (d_OP__case_15 x4 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x4 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x4 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x4 x8 x9 x12 x3000 x3500 = case x12 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Tuple2 x4 x8)
     (Curry_Prelude.OP_Cons x13 x14) -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x4 x8 x9 x1002 x3000 x3500) (nd_OP__case_15 x4 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x4 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x4 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x3 x4 x2 x3000 x3500 = case x2 of
     (HO_C_Conv x5 x6) -> HO_C_Conv (wrapNX id (nd_OP_bang_dot_rd_dot_65 x3 x5)) (wrapNX id (nd_OP_bang_dot_sh_dot_65 x4 x6))
     (Choice_C_XmlConv x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x3 x4 x1002 x3000 x3500) (nd_OP__case_20 x3 x4 x1003 x3000 x3500)
     (Choices_C_XmlConv x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x3 x4 z x3000 x3500) x1002
     (Guard_C_XmlConv x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlConv x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_21 x2 x5 x4 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x2 x1002 x3500) (d_OP__case_22 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x2 x5 x4 x2000 x3500))
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x2 x1002 x3000 x3500) (nd_OP__case_22 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x2 x5 x4 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x2 x5)
     (Curry_XML.C_XElem x7 x8 x9) -> Curry_Prelude.d_C_failed x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x5 x1002 x3500) (d_OP__case_21 x2 x5 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 x5 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> Curry_Prelude.OP_Tuple2 x6 (Curry_Prelude.OP_Tuple2 x2 x5)
     (Curry_XML.C_XElem x7 x8 x9) -> Curry_Prelude.d_C_failed x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x5 x1002 x3000 x3500) (nd_OP__case_21 x2 x5 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 x5 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_23 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1002 x3500) (d_OP__case_24 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1002 x3000 x3500) (nd_OP__case_24 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x5 x4 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x5
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x5 x1002 x3500) (d_OP__case_23 x5 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x5 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x5 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x5 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x5
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x5 x1002 x3000 x3500) (nd_OP__case_23 x5 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x5 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_25 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1002 x3500) (d_OP__case_26 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1002 x3000 x3500) (nd_OP__case_26 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x4 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x6
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1002 x3500) (d_OP__case_25 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x6
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1002 x3000 x3500) (nd_OP__case_25 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_27 x2 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x1002 x3500) (d_OP__case_28 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x2 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x2 x1002 x3000 x3500) (nd_OP__case_28 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x2 x4 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x2
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x2 x1002 x3500) (d_OP__case_27 x2 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x2 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x2 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x2 x4 x3000 x3500 = case x4 of
     (Curry_XML.C_XText x6) -> x2
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x2 x1002 x3000 x3500) (nd_OP__case_27 x2 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x2 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x3 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_30 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x3 x1002 x3500) (d_OP__case_31 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x3 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x3 x1002 x3000 x3500) (nd_OP__case_31 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_29 x4 x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1002 x3500) (d_OP__case_30 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x4 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1002 x3000 x3500) (nd_OP__case_30 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x4 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x4 x1002 x3500) (d_OP__case_29 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x4 x1002 x3000 x3500) (nd_OP__case_29 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_33 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x2 x1002 x3500) (d_OP__case_34 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_33 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x2 x1002 x3000 x3500) (nd_OP__case_34 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x2 x5 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP__case_32 x2 x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x2 x5 x1002 x3500) (d_OP__case_33 x2 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x2 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x2 x5 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x2 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x2 x5 x1002 x3000 x3500) (nd_OP__case_33 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x2 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x2 x1002 x3500) (d_OP__case_32 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x2 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x2 x1002 x3000 x3500) (nd_OP__case_32 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x1 x2 x3 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1 x2 x3 x1002 x3500) (d_OP__case_35 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x2 x3 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x5 x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_35 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
