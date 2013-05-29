{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_XML (C_XmlExp (..), C_Encoding (..), C_XmlDocParams (..), d_C_tagOf, d_C_elemsOf, d_C_textOf, nd_C_textOf, d_C_textOfXml, nd_C_textOfXml, d_C_xtxt, d_C_xml, d_C_writeXmlFile, d_C_writeXmlFileWithParams, d_C_showXmlDoc, d_C_showXmlDocWithParams, d_C_readXmlFile, d_C_readUnsafeXmlFile, d_C_readFileWithXmlDocs, d_C_parseXmlString, d_C_updateXmlFile, nd_C_updateXmlFile) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
import qualified Curry_Read
import qualified Curry_List
data C_XmlExp
     = C_XText (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_XElem (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List C_XmlExp)
     | Choice_C_XmlExp Cover ID C_XmlExp C_XmlExp
     | Choices_C_XmlExp Cover ID ([C_XmlExp])
     | Fail_C_XmlExp Cover FailInfo
     | Guard_C_XmlExp Cover Constraints C_XmlExp

instance Show C_XmlExp where
  showsPrec d (Choice_C_XmlExp cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_XmlExp cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_XmlExp cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_XmlExp cd info) = showChar '!'
  showsPrec _ (C_XText x1) = (showString "(XText") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_XElem x1 x2 x3) = (showString "(XElem") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_XmlExp where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_XText x1,r1) | (_,r0) <- readQualified "XML" "XText" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_XElem x1 x2 x3,r3) | (_,r0) <- readQualified "XML" "XElem" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s)


instance NonDet C_XmlExp where
  choiceCons = Choice_C_XmlExp
  choicesCons = Choices_C_XmlExp
  failCons = Fail_C_XmlExp
  guardCons = Guard_C_XmlExp
  try (Choice_C_XmlExp cd i x y) = tryChoice cd i x y
  try (Choices_C_XmlExp cd i xs) = tryChoices cd i xs
  try (Fail_C_XmlExp cd info) = Fail cd info
  try (Guard_C_XmlExp cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_XmlExp cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_XmlExp cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_XmlExp cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_XmlExp cd i _) = error ("XML.XmlExp.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_XmlExp cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_XmlExp cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_XmlExp where
  generate s = Choices_C_XmlExp defCover (freeID [1,3] s) [(C_XText (generate (leftSupply s))),(C_XElem (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_XmlExp where
  ($!!) cont (C_XText x1) cs = ((\y1 cs -> cont (C_XText y1) cs) $!! x1) cs
  ($!!) cont (C_XElem x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_XElem y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_XmlExp cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_XmlExp cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_XmlExp cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_XmlExp cd info) _ = failCons cd info
  ($##) cont (C_XText x1) cs = ((\y1 cs -> cont (C_XText y1) cs) $## x1) cs
  ($##) cont (C_XElem x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_XElem y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_XmlExp cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_XmlExp cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_XmlExp cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_XmlExp cd info) _ = failCons cd info
  searchNF search cont (C_XText x1) = search (\y1 -> cont (C_XText y1)) x1
  searchNF search cont (C_XElem x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_XElem y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("XML.XmlExp.searchNF: no constructor: " ++ (show x))


instance Unifiable C_XmlExp where
  (=.=) (C_XText x1) (C_XText y1) cs = (x1 =:= y1) cs
  (=.=) (C_XElem x1 x2 x3) (C_XElem y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_XText x1) (C_XText y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_XElem x1 x2 x3) (C_XElem y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_XText x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_XElem x2 x3 x4) = ((i :=: (ChooseN 1 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_XmlExp cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_XmlExp cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_XmlExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_XmlExp cd i _) = error ("XML.XmlExp.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_XmlExp cd info) = [(Unsolvable info)]
  bind i (Guard_C_XmlExp cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_XText x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_XElem x2 x3 x4) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_XmlExp cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_XmlExp cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_XmlExp cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_XmlExp cd i _) = error ("XML.XmlExp.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_XmlExp cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_XmlExp cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_XmlExp where
  (=?=) (Choice_C_XmlExp cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_XmlExp cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_XmlExp cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_XmlExp cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_XmlExp cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_XmlExp cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_XmlExp cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_XmlExp cd info) _ = failCons cd info
  (=?=) (C_XText x1) (C_XText y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_XElem x1 x2 x3) (C_XElem y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_XmlExp cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_XmlExp cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_XmlExp cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_XmlExp cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_XmlExp cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_XmlExp cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_XmlExp cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_XmlExp cd info) _ = failCons cd info
  (<?=) (C_XText x1) (C_XText y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_XText _) (C_XElem _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_XElem x1 x2 x3) (C_XElem y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_XmlExp where
  cover (C_XText x1) = C_XText (cover x1)
  cover (C_XElem x1 x2 x3) = C_XElem (cover x1) (cover x2) (cover x3)
  cover (Choice_C_XmlExp cd i x y) = Choice_C_XmlExp (incCover cd) i (cover x) (cover y)
  cover (Choices_C_XmlExp cd i xs) = Choices_C_XmlExp (incCover cd) i (map cover xs)
  cover (Fail_C_XmlExp cd info) = Fail_C_XmlExp (incCover cd) info
  cover (Guard_C_XmlExp cd c e) = Guard_C_XmlExp (incCover cd) c (cover e)


data C_Encoding
     = C_StandardEnc
     | C_Iso88591Enc
     | Choice_C_Encoding Cover ID C_Encoding C_Encoding
     | Choices_C_Encoding Cover ID ([C_Encoding])
     | Fail_C_Encoding Cover FailInfo
     | Guard_C_Encoding Cover Constraints C_Encoding

instance Show C_Encoding where
  showsPrec d (Choice_C_Encoding cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Encoding cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Encoding cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Encoding cd info) = showChar '!'
  showsPrec _ C_StandardEnc = showString "StandardEnc"
  showsPrec _ C_Iso88591Enc = showString "Iso88591Enc"


instance Read C_Encoding where
  readsPrec _ s = (readParen False (\r -> [ (C_StandardEnc,r0) | (_,r0) <- readQualified "XML" "StandardEnc" r]) s) ++ (readParen False (\r -> [ (C_Iso88591Enc,r0) | (_,r0) <- readQualified "XML" "Iso88591Enc" r]) s)


instance NonDet C_Encoding where
  choiceCons = Choice_C_Encoding
  choicesCons = Choices_C_Encoding
  failCons = Fail_C_Encoding
  guardCons = Guard_C_Encoding
  try (Choice_C_Encoding cd i x y) = tryChoice cd i x y
  try (Choices_C_Encoding cd i xs) = tryChoices cd i xs
  try (Fail_C_Encoding cd info) = Fail cd info
  try (Guard_C_Encoding cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Encoding cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Encoding cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Encoding cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Encoding cd i _) = error ("XML.Encoding.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Encoding cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Encoding cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Encoding where
  generate s = Choices_C_Encoding defCover (freeID [0,0] s) [C_StandardEnc,C_Iso88591Enc]


instance NormalForm C_Encoding where
  ($!!) cont C_StandardEnc cs = cont C_StandardEnc cs
  ($!!) cont C_Iso88591Enc cs = cont C_Iso88591Enc cs
  ($!!) cont (Choice_C_Encoding cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Encoding cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Encoding cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Encoding cd info) _ = failCons cd info
  ($##) cont C_StandardEnc cs = cont C_StandardEnc cs
  ($##) cont C_Iso88591Enc cs = cont C_Iso88591Enc cs
  ($##) cont (Choice_C_Encoding cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Encoding cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Encoding cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Encoding cd info) _ = failCons cd info
  searchNF _ cont C_StandardEnc = cont C_StandardEnc
  searchNF _ cont C_Iso88591Enc = cont C_Iso88591Enc
  searchNF _ _ x = error ("XML.Encoding.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Encoding where
  (=.=) C_StandardEnc C_StandardEnc cs = C_Success
  (=.=) C_Iso88591Enc C_Iso88591Enc cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_StandardEnc C_StandardEnc cs = C_Success
  (=.<=) C_Iso88591Enc C_Iso88591Enc cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_StandardEnc = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_Iso88591Enc = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_Encoding cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Encoding cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Encoding cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Encoding cd i _) = error ("XML.Encoding.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Encoding cd info) = [(Unsolvable info)]
  bind i (Guard_C_Encoding cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_StandardEnc = [(i :=: (ChooseN 0 0))]
  lazyBind i C_Iso88591Enc = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_Encoding cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Encoding cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Encoding cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Encoding cd i _) = error ("XML.Encoding.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Encoding cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Encoding cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Encoding where
  (=?=) (Choice_C_Encoding cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Encoding cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Encoding cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Encoding cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Encoding cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Encoding cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Encoding cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Encoding cd info) _ = failCons cd info
  (=?=) C_StandardEnc C_StandardEnc cs = Curry_Prelude.C_True
  (=?=) C_Iso88591Enc C_Iso88591Enc cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Encoding cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Encoding cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Encoding cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Encoding cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Encoding cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Encoding cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Encoding cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Encoding cd info) _ = failCons cd info
  (<?=) C_StandardEnc C_StandardEnc cs = Curry_Prelude.C_True
  (<?=) C_StandardEnc C_Iso88591Enc _ = Curry_Prelude.C_True
  (<?=) C_Iso88591Enc C_Iso88591Enc cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Encoding where
  cover C_StandardEnc = C_StandardEnc
  cover C_Iso88591Enc = C_Iso88591Enc
  cover (Choice_C_Encoding cd i x y) = Choice_C_Encoding (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Encoding cd i xs) = Choices_C_Encoding (incCover cd) i (map cover xs)
  cover (Fail_C_Encoding cd info) = Fail_C_Encoding (incCover cd) info
  cover (Guard_C_Encoding cd c e) = Guard_C_Encoding (incCover cd) c (cover e)


data C_XmlDocParams
     = C_Enc C_Encoding
     | C_DtdUrl (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_XmlDocParams Cover ID C_XmlDocParams C_XmlDocParams
     | Choices_C_XmlDocParams Cover ID ([C_XmlDocParams])
     | Fail_C_XmlDocParams Cover FailInfo
     | Guard_C_XmlDocParams Cover Constraints C_XmlDocParams

instance Show C_XmlDocParams where
  showsPrec d (Choice_C_XmlDocParams cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_XmlDocParams cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_XmlDocParams cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_XmlDocParams cd info) = showChar '!'
  showsPrec _ (C_Enc x1) = (showString "(Enc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_DtdUrl x1) = (showString "(DtdUrl") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_XmlDocParams where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Enc x1,r1) | (_,r0) <- readQualified "XML" "Enc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_DtdUrl x1,r1) | (_,r0) <- readQualified "XML" "DtdUrl" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_XmlDocParams where
  choiceCons = Choice_C_XmlDocParams
  choicesCons = Choices_C_XmlDocParams
  failCons = Fail_C_XmlDocParams
  guardCons = Guard_C_XmlDocParams
  try (Choice_C_XmlDocParams cd i x y) = tryChoice cd i x y
  try (Choices_C_XmlDocParams cd i xs) = tryChoices cd i xs
  try (Fail_C_XmlDocParams cd info) = Fail cd info
  try (Guard_C_XmlDocParams cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_XmlDocParams cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_XmlDocParams cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_XmlDocParams cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_XmlDocParams cd i _) = error ("XML.XmlDocParams.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_XmlDocParams cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_XmlDocParams cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_XmlDocParams where
  generate s = Choices_C_XmlDocParams defCover (freeID [1,1] s) [(C_Enc (generate (leftSupply s))),(C_DtdUrl (generate (leftSupply s)))]


instance NormalForm C_XmlDocParams where
  ($!!) cont (C_Enc x1) cs = ((\y1 cs -> cont (C_Enc y1) cs) $!! x1) cs
  ($!!) cont (C_DtdUrl x1) cs = ((\y1 cs -> cont (C_DtdUrl y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_XmlDocParams cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_XmlDocParams cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_XmlDocParams cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_XmlDocParams cd info) _ = failCons cd info
  ($##) cont (C_Enc x1) cs = ((\y1 cs -> cont (C_Enc y1) cs) $## x1) cs
  ($##) cont (C_DtdUrl x1) cs = ((\y1 cs -> cont (C_DtdUrl y1) cs) $## x1) cs
  ($##) cont (Choice_C_XmlDocParams cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_XmlDocParams cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_XmlDocParams cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_XmlDocParams cd info) _ = failCons cd info
  searchNF search cont (C_Enc x1) = search (\y1 -> cont (C_Enc y1)) x1
  searchNF search cont (C_DtdUrl x1) = search (\y1 -> cont (C_DtdUrl y1)) x1
  searchNF _ _ x = error ("XML.XmlDocParams.searchNF: no constructor: " ++ (show x))


instance Unifiable C_XmlDocParams where
  (=.=) (C_Enc x1) (C_Enc y1) cs = (x1 =:= y1) cs
  (=.=) (C_DtdUrl x1) (C_DtdUrl y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_Enc x1) (C_Enc y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_DtdUrl x1) (C_DtdUrl y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_Enc x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_DtdUrl x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_XmlDocParams cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_XmlDocParams cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_XmlDocParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_XmlDocParams cd i _) = error ("XML.XmlDocParams.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_XmlDocParams cd info) = [(Unsolvable info)]
  bind i (Guard_C_XmlDocParams cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_Enc x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_DtdUrl x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_XmlDocParams cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_XmlDocParams cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_XmlDocParams cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_XmlDocParams cd i _) = error ("XML.XmlDocParams.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_XmlDocParams cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_XmlDocParams cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_XmlDocParams where
  (=?=) (Choice_C_XmlDocParams cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_XmlDocParams cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_XmlDocParams cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_XmlDocParams cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_XmlDocParams cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_XmlDocParams cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_XmlDocParams cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_XmlDocParams cd info) _ = failCons cd info
  (=?=) (C_Enc x1) (C_Enc y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_DtdUrl x1) (C_DtdUrl y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_XmlDocParams cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_XmlDocParams cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_XmlDocParams cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_XmlDocParams cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_XmlDocParams cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_XmlDocParams cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_XmlDocParams cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_XmlDocParams cd info) _ = failCons cd info
  (<?=) (C_Enc x1) (C_Enc y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Enc _) (C_DtdUrl _) _ = Curry_Prelude.C_True
  (<?=) (C_DtdUrl x1) (C_DtdUrl y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_XmlDocParams where
  cover (C_Enc x1) = C_Enc (cover x1)
  cover (C_DtdUrl x1) = C_DtdUrl (cover x1)
  cover (Choice_C_XmlDocParams cd i x y) = Choice_C_XmlDocParams (incCover cd) i (cover x) (cover y)
  cover (Choices_C_XmlDocParams cd i xs) = Choices_C_XmlDocParams (incCover cd) i (map cover xs)
  cover (Fail_C_XmlDocParams cd info) = Fail_C_XmlDocParams (incCover cd) info
  cover (Guard_C_XmlDocParams cd c e) = Guard_C_XmlDocParams (incCover cd) c (cover e)


d_C_encoding2Attribute :: C_Encoding -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_encoding2Attribute x1 x3500 = case x1 of
     C_StandardEnc -> Curry_Prelude.OP_List
     C_Iso88591Enc -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '8'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '8'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '5'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '9'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))
     (Choice_C_Encoding x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_encoding2Attribute x1002 x3500) (d_C_encoding2Attribute x1003 x3500)
     (Choices_C_Encoding x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_encoding2Attribute z x3500) x1002
     (Guard_C_Encoding x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_encoding2Attribute x1002) $! (addCs x1001 x3500))
     (Fail_C_Encoding x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_encoding2EncFunc :: C_Encoding -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_encoding2EncFunc x1 x3500 = case x1 of
     C_StandardEnc -> d_C_standardEncoding
     C_Iso88591Enc -> d_C_iso88591Encoding
     (Choice_C_Encoding x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_encoding2EncFunc x1002 x3500) (d_C_encoding2EncFunc x1003 x3500)
     (Choices_C_Encoding x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_encoding2EncFunc z x3500) x1002
     (Guard_C_Encoding x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_encoding2EncFunc x1002) $! (addCs x1001 x3500))
     (Fail_C_Encoding x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_encoding2EncFunc :: C_Encoding -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_encoding2EncFunc x1 x3000 x3500 = case x1 of
     C_StandardEnc -> wrapDX id d_C_standardEncoding
     C_Iso88591Enc -> wrapDX id d_C_iso88591Encoding
     (Choice_C_Encoding x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_encoding2EncFunc x1002 x3000 x3500) (nd_C_encoding2EncFunc x1003 x3000 x3500)
     (Choices_C_Encoding x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_encoding2EncFunc z x3000 x3500) x1002
     (Guard_C_Encoding x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_encoding2EncFunc x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Encoding x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_standardEncoding :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_standardEncoding x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_68 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '<'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_standardEncoding x1002 x3500) (d_C_standardEncoding x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_standardEncoding z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_standardEncoding x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_iso88591Encoding :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_iso88591Encoding x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_60 x2 x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.d_C_ord x2 x3500) x3500) (d_C_iso88591list x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_iso88591Encoding x1002 x3500) (d_C_iso88591Encoding x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_iso88591Encoding z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_iso88591Encoding x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_iso88591list :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_iso88591list x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 192#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 193#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 194#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 195#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 196#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 197#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 198#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 199#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 200#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 201#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 202#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 203#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 204#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 205#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 207#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 208#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 209#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 210#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 211#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 212#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 214#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 216#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 217#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 218#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 219#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 220#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 221#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 224#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 225#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 228#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 229#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 226#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 227#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 230#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 231#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 233#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 232#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 235#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 234#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 236#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 237#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 239#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 240#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 241#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 248#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 246#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 242#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 243#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 244#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 245#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 250#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 249#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 252#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 251#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 253#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 255#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))

d_C_lookupEncoding :: Curry_Prelude.OP_List C_XmlDocParams -> ConstStore -> C_Encoding
d_C_lookupEncoding x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_59 x3 x2 x3500
     Curry_Prelude.OP_List -> C_StandardEnc
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupEncoding x1002 x3500) (d_C_lookupEncoding x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupEncoding z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupEncoding x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lookupDtdUrl :: Curry_Prelude.OP_List C_XmlDocParams -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_lookupDtdUrl x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_58 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupDtdUrl x1002 x3500) (d_C_lookupDtdUrl x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupDtdUrl z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupDtdUrl x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_hasDtdUrl :: Curry_Prelude.OP_List C_XmlDocParams -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasDtdUrl x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_57 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasDtdUrl x1002 x3500) (d_C_hasDtdUrl x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasDtdUrl z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasDtdUrl x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tagOf :: C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_tagOf x1 x3500 = case x1 of
     (C_XElem x2 x3 x4) -> x2
     (C_XText x5) -> Curry_Prelude.OP_List
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tagOf x1002 x3500) (d_C_tagOf x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tagOf z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tagOf x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_elemsOf :: C_XmlExp -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_elemsOf x1 x3500 = case x1 of
     (C_XElem x2 x3 x4) -> x4
     (C_XText x5) -> Curry_Prelude.OP_List
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_elemsOf x1002 x3500) (d_C_elemsOf x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_elemsOf z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_elemsOf x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_textOf :: ConstStore -> Curry_Prelude.OP_List C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_textOf x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unwords (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3500)) (Curry_Prelude.d_C_map d_OP_textOf_dot_textOfXmlExp_dot_115) x3500) x3500

nd_C_textOf :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_textOf x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unwords) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_filter (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (wrapDX id Curry_Prelude.d_C_null) x2000 x3500))) (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_OP_textOf_dot_textOfXmlExp_dot_115))) x2001 x3500)))) x2003 x3500)))))

d_OP_textOf_dot_textOfXmlExp_dot_115 :: C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_textOf_dot_textOfXmlExp_dot_115 x1 x3500 = case x1 of
     (C_XText x2) -> x2
     (C_XElem x3 x4 x5) -> Curry_Prelude.d_C_apply (d_C_textOf x3500) x5 x3500
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_textOf_dot_textOfXmlExp_dot_115 x1002 x3500) (d_OP_textOf_dot_textOfXmlExp_dot_115 x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_textOf_dot_textOfXmlExp_dot_115 z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_textOf_dot_textOfXmlExp_dot_115 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_textOfXml :: ConstStore -> Curry_Prelude.OP_List C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_textOfXml x3500 = d_C_textOf x3500

nd_C_textOfXml :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_textOfXml x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_textOf x2000 x3500))

d_C_xtxt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> C_XmlExp
d_C_xtxt x1 x3500 = C_XText x1

d_C_xml :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_XmlExp -> ConstStore -> C_XmlExp
d_C_xml x1 x2 x3500 = C_XElem x1 Curry_Prelude.OP_List x2

d_C_writeXmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_XmlExp -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeXmlFile x1 x2 x3500 = d_C_writeXmlFileWithParams x1 (Curry_Prelude.OP_Cons (C_Enc C_StandardEnc) Curry_Prelude.OP_List) x2 x3500

d_C_writeXmlFileWithParams :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_XmlDocParams -> C_XmlExp -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeXmlFileWithParams x1 x2 x3 x3500 = Curry_Prelude.d_C_writeFile x1 (d_C_showXmlDocWithParams x2 x3 x3500) x3500

d_C_showXmlDoc :: C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showXmlDoc x1 x3500 = d_C_showXmlDocWithParams Curry_Prelude.OP_List x1 x3500

d_C_showXmlDocWithParams :: Curry_Prelude.OP_List C_XmlDocParams -> C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showXmlDocWithParams x1 x2 x3500 = case x2 of
     (C_XElem x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '1'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_encoding2Attribute (d_C_lookupEncoding x1 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_56 x1 (d_C_hasDtdUrl x1 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_55 x1 x3 (d_C_hasDtdUrl x1 x3500) x3500) (d_C_showXmlExp (Curry_Prelude.C_Int 0#) (d_C_encoding2EncFunc (d_C_lookupEncoding x1 x3500) x3500) (C_XElem x3 x4 x5) x3500) x3500) x3500) x3500) x3500) x3500) x3500
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showXmlDocWithParams x1 x1002 x3500) (d_C_showXmlDocWithParams x1 x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showXmlDocWithParams x1 z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showXmlDocWithParams x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showXmlExp :: Curry_Prelude.C_Int -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showXmlExp x1 x2 x3 x3500 = case x3 of
     (C_XText x4) -> Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x2 x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500
     (C_XElem x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showXmlOpenTag x5 x6 x2 x3500) (d_OP__case_54 x1 x2 x5 x7 (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.OP_List x3500) x3500) x3500) x3500
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showXmlExp x1 x2 x1002 x3500) (d_C_showXmlExp x1 x2 x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showXmlExp x1 x2 z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showXmlExp x1 x2 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_showXmlExp :: Curry_Prelude.C_Int -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_XmlExp -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showXmlExp x1 x2 x3 x3000 x3500 = case x3 of
     (C_XText x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x2 x4 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500) x3500))
     (C_XElem x5 x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (nd_C_showXmlOpenTag x5 x6 x2 x2000 x3500) (nd_OP__case_54 x1 x2 x5 x7 (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.OP_List x3500) x2001 x3500) x3500)))) x3500))
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showXmlExp x1 x2 x1002 x3000 x3500) (nd_C_showXmlExp x1 x2 x1003 x3000 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showXmlExp x1 x2 z x3000 x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showXmlExp x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_showXmlExp_dot___hash_selFP2_hash_s :: Curry_Prelude.OP_List C_XmlExp -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showXmlExp_dot___hash_selFP2_hash_s x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_52 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showXmlExp_dot___hash_selFP2_hash_s x1002 x3500) (d_OP_showXmlExp_dot___hash_selFP2_hash_s x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showXmlExp_dot___hash_selFP2_hash_s z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showXmlExp_dot___hash_selFP2_hash_s x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xtab :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_xtab x1 x3500 = Curry_Prelude.d_C_take x1 (Curry_Prelude.d_C_repeat (Curry_Prelude.C_Char ' '#) x3500) x3500

d_C_showXmlOpenTag :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) -> (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showXmlOpenTag x1 x2 x3 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_concat (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (d_OP_showXmlOpenTag_dot_attr2string_dot_148 x3) x3500) x2 x3500) x3500) x3500) x3500

nd_C_showXmlOpenTag :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) -> Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showXmlOpenTag x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_concat (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_map (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (wrapNX id (nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x3)) x2000 x3500) x2 x2001 x3500)))) x3500) x3500) x3500))

d_OP_showXmlOpenTag_dot_attr2string_dot_148 :: Curry_Prelude.Curry t158 => (t158 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t158 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1002 x3500) (d_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_showXmlOpenTag_dot_attr2string_dot_148 :: Curry_Prelude.Curry t158 => Func t158 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t158 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1002 x3000 x3500) (nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showXmlOpenTag_dot_attr2string_dot_148 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showXmlExps :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_XmlExp -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showXmlExps x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showXmlExp x1 x3) x3500) x2 x3500

nd_C_showXmlExps :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List C_XmlExp -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showXmlExps x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_C_showXmlExp x1 x3)) x2000 x3500) x2 x2001 x3500)))))

d_C_isXText :: C_XmlExp -> ConstStore -> Curry_Prelude.C_Bool
d_C_isXText x1 x3500 = case x1 of
     (C_XText x2) -> Curry_Prelude.C_True
     (C_XElem x3 x4 x5) -> Curry_Prelude.C_False
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isXText x1002 x3500) (d_C_isXText x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isXText z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isXText x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xmlUnquoteSpecials :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_xmlUnquoteSpecials x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_50 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '&'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_xmlUnquoteSpecials x1002 x3500) (d_C_xmlUnquoteSpecials x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_xmlUnquoteSpecials z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_xmlUnquoteSpecials x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x1002 x3500) (d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x1002 x3500) (d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_xmlUnquoteSpecial :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_xmlUnquoteSpecial x1 x2 x3500 = d_OP__case_48 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)) x3500) x3500

d_C_unquoteUnicode :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_unquoteUnicode x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_35 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '#'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unquoteUnicode x1002 x3500) (d_C_unquoteUnicode x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unquoteUnicode z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unquoteUnicode x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_readXmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO C_XmlExp
d_C_readXmlFile x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (d_OP_readXmlFile_dot___hash_lambda2 x1) x3500

d_OP_readXmlFile_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO C_XmlExp
d_OP_readXmlFile_dot___hash_lambda2 x1 x2 x3500 = let
     x3 = d_C_parseXmlString x2 x3500
      in (d_OP__case_31 x1 x3 (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.OP_List x3500) x3500)

d_C_readUnsafeXmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_XmlExp)
d_C_readUnsafeXmlFile x1 x3500 = Curry_Prelude.d_C_catch (Curry_Prelude.d_OP_gt_gt_eq (d_C_readXmlFile x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Just) x3500) x3500) d_OP_readUnsafeXmlFile_dot___hash_lambda3 x3500

d_OP_readUnsafeXmlFile_dot___hash_lambda3 :: Curry_Prelude.Curry t260 => Curry_Prelude.C_IOError -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t260)
d_OP_readUnsafeXmlFile_dot___hash_lambda3 x1 x3500 = Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500

d_C_showXmlFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_showXmlFile x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readXmlFile x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_putStr d_C_showXmlDoc x3500) x3500

d_C_readFileWithXmlDocs :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List C_XmlExp)
d_C_readFileWithXmlDocs x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return d_C_parseXmlString x3500) x3500

d_C_parseXmlString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_parseXmlString x1 x3500 = Curry_Prelude.d_C_fst (d_C_parseXmlTokens (d_C_scanXmlString x1 x3500) Curry_Prelude.C_Nothing x3500) x3500

d_C_parseXmlTokens :: Curry_Prelude.OP_List C_XmlExp -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp)
d_C_parseXmlTokens x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_29 x2 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_28 x2 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parseXmlTokens x1002 x2 x3500) (d_C_parseXmlTokens x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parseXmlTokens z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parseXmlTokens x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_XmlExp) (Curry_Prelude.OP_List C_XmlExp) -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x1002 x3500) (d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlString :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlString x1 x3500 = d_OP_scanXmlString_dot_scanXml_dot_209 (Curry_Prelude.d_C_apply (d_C_dropBlanks x3500) x1 x3500) x3500

d_OP_scanXmlString_dot_scanXml_dot_209 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_OP_scanXmlString_dot_scanXml_dot_209 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_22 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '<'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlString_dot_scanXml_dot_209 x1002 x3500) (d_OP_scanXmlString_dot_scanXml_dot_209 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlString_dot_scanXml_dot_209 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlString_dot_scanXml_dot_209 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x1002 x3500) (d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x1002 x3500) (d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlText :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_scanXmlText x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_21 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '<'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanXmlText x1002 x3500) (d_C_scanXmlText x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanXmlText z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanXmlText x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlText_dot___hash_selFP25_hash_txt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlText_dot___hash_selFP25_hash_txt x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlText_dot___hash_selFP25_hash_txt x1002 x3500) (d_OP_scanXmlText_dot___hash_selFP25_hash_txt x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlText_dot___hash_selFP25_hash_txt z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlText_dot___hash_selFP25_hash_txt x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlText_dot___hash_selFP26_hash_rem :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlText_dot___hash_selFP26_hash_rem x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlText_dot___hash_selFP26_hash_rem x1002 x3500) (d_OP_scanXmlText_dot___hash_selFP26_hash_rem x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlText_dot___hash_selFP26_hash_rem z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlText_dot___hash_selFP26_hash_rem x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlText_dot___hash_selFP28_hash_txt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlText_dot___hash_selFP28_hash_txt x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlText_dot___hash_selFP28_hash_txt x1002 x3500) (d_OP_scanXmlText_dot___hash_selFP28_hash_txt x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlText_dot___hash_selFP28_hash_txt z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlText_dot___hash_selFP28_hash_txt x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlText_dot___hash_selFP29_hash_rem :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlText_dot___hash_selFP29_hash_rem x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlText_dot___hash_selFP29_hash_rem x1002 x3500) (d_OP_scanXmlText_dot___hash_selFP29_hash_rem x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlText_dot___hash_selFP29_hash_rem z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlText_dot___hash_selFP29_hash_rem x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlElem :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlElem x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_17 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '!'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanXmlElem x1002 x3500) (d_C_scanXmlElem x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanXmlElem z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanXmlElem x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlElemName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlElemName x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) Curry_Prelude.OP_List Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_13 x1 x3 x4 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '>'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanXmlElemName x1 x1002 x3500) (d_C_scanXmlElemName x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanXmlElemName x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanXmlElemName x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x1002 x3500) (d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x1002 x3500) (d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlComment x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_8 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '-'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanXmlComment x1002 x3500) (d_C_scanXmlComment x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanXmlComment z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanXmlComment x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlCData :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlCData x1 x3500 = let
     x2 = d_C_dropCData x1 x3500
      in (d_OP__case_7 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x2 x3500) (Curry_Prelude.C_Char '>'#) x3500) x3500)

d_C_dropCData :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dropCData x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_6 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '['#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dropCData x1002 x3500) (d_C_dropCData x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dropCData z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dropCData x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanXmlProcInstr :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List C_XmlExp
d_C_scanXmlProcInstr x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_3 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '?'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x3 x3500) (Curry_Prelude.C_Char '>'#) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanXmlProcInstr x1002 x3500) (d_C_scanXmlProcInstr x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanXmlProcInstr z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanXmlProcInstr x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_parseAttrs :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_parseAttrs x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_2 x2 x3 (Curry_Char.d_C_isAlpha x2 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_parseAttrs x1002 x3500) (d_C_parseAttrs x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_parseAttrs z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_parseAttrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP40_hash_name :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_parseAttrs_dot___hash_selFP40_hash_name x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP40_hash_name x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP40_hash_name x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP40_hash_name z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP40_hash_name x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP38_hash_value :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_parseAttrs_dot___hash_selFP38_hash_value x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP38_hash_value x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP38_hash_value x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP38_hash_value z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP38_hash_value x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x1002 x3500) (d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dropBlanks :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dropBlanks x3500 = Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace

nd_C_dropBlanks :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_dropBlanks x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id Curry_Char.d_C_isSpace))

d_C_splitAtChar :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_C_splitAtChar x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_0 x1 x3 x4 (Curry_Prelude.d_OP_eq_eq x3 x1 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitAtChar x1 x1002 x3500) (d_C_splitAtChar x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitAtChar x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitAtChar x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitAtChar_dot___hash_selFP43_hash_first :: Curry_Prelude.Curry t259 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t259) (Curry_Prelude.OP_List t259) -> ConstStore -> Curry_Prelude.OP_List t259
d_OP_splitAtChar_dot___hash_selFP43_hash_first x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAtChar_dot___hash_selFP43_hash_first x1002 x3500) (d_OP_splitAtChar_dot___hash_selFP43_hash_first x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAtChar_dot___hash_selFP43_hash_first z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAtChar_dot___hash_selFP43_hash_first x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitAtChar_dot___hash_selFP44_hash_rest :: Curry_Prelude.Curry t259 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t259) (Curry_Prelude.OP_List t259) -> ConstStore -> Curry_Prelude.OP_List t259
d_OP_splitAtChar_dot___hash_selFP44_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitAtChar_dot___hash_selFP44_hash_rest x1002 x3500) (d_OP_splitAtChar_dot___hash_selFP44_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitAtChar_dot___hash_selFP44_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitAtChar_dot___hash_selFP44_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_updateXmlFile :: (C_XmlExp -> ConstStore -> C_XmlExp) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_updateXmlFile x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readXmlFile x2 x3500) (d_OP_updateXmlFile_dot___hash_lambda4 x2 x1) x3500

nd_C_updateXmlFile :: Func C_XmlExp C_XmlExp -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_updateXmlFile x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_readXmlFile x2 x3500) (wrapNX id (nd_OP_updateXmlFile_dot___hash_lambda4 x2 x1)) x2000 x3500))

d_OP_updateXmlFile_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (C_XmlExp -> ConstStore -> C_XmlExp) -> C_XmlExp -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateXmlFile_dot___hash_lambda4 x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar_bang_bang (d_C_writeXmlFile x1) (Curry_Prelude.d_C_apply x2 x3 x3500) x3500

nd_OP_updateXmlFile_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func C_XmlExp C_XmlExp -> C_XmlExp -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_updateXmlFile_dot___hash_lambda4 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar_bang_bang (wrapDX id (d_C_writeXmlFile x1)) (Curry_Prelude.nd_C_apply x2 x3 x2000 x3500) x2001 x3500)))))

d_OP__case_0 x1 x3 x4 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x5 = d_C_splitAtChar x1 x4 x3500
          x6 = d_OP_splitAtChar_dot___hash_selFP43_hash_first x5 x3500
          x7 = d_OP_splitAtChar_dot___hash_selFP44_hash_rest x5 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x3 x6) x7)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x3 x4 x1002 x3500) (d_OP__case_0 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3 x4 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x5 = d_C_splitAtChar x1 x4 x3500
          x6 = d_OP_splitAtChar_dot___hash_selFP43_hash_first x5 x3500
          x7 = d_OP_splitAtChar_dot___hash_selFP44_hash_rest x5 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x3 x6) x7)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x2 x3 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x4 = d_C_splitAtChar (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons x2 x3) x3500
          x5 = d_OP_parseAttrs_dot___hash_selFP40_hash_name x4 x3500
          x6 = d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x4 x3500
          x7 = d_C_splitAtChar (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_C_tail x6 x3500) x3500
          x8 = d_OP_parseAttrs_dot___hash_selFP38_hash_value x7 x3500
          x9 = d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x7 x3500
          x10 = d_C_parseAttrs (Curry_Prelude.d_C_apply (d_C_dropBlanks x3500) x9 x3500) x3500
          x11 = d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x10 x3500
          x12 = d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x10 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 (d_C_xmlUnquoteSpecials x8 x3500)) x11) x12)
     Curry_Prelude.C_False -> d_OP__case_1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x3 x1002 x3500) (d_OP__case_2 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x2 x3 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x4 = d_C_splitAtChar (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons x2 x3) x3500
               x5 = d_OP_parseAttrs_dot___hash_selFP40_hash_name x4 x3500
               x6 = d_OP_parseAttrs_dot___hash_selFP41_hash_rest1 x4 x3500
               x7 = d_C_splitAtChar (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_C_tail x6 x3500) x3500
               x8 = d_OP_parseAttrs_dot___hash_selFP38_hash_value x7 x3500
               x9 = d_OP_parseAttrs_dot___hash_selFP39_hash_rest2 x7 x3500
               x10 = d_C_parseAttrs (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_dropBlanks x2000 x3500) x9 x2001 x3500)))) x3500
               x11 = d_OP_parseAttrs_dot___hash_selFP36_hash_rem_attrs x10 x3500
               x12 = d_OP_parseAttrs_dot___hash_selFP37_hash_rem_inp x10 x3500
                in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x5 (d_C_xmlUnquoteSpecials x8 x3500)) x11) x12)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x2 x3 x1002 x3000 x3500) (nd_OP__case_2 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 x3)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x3 x1002 x3500) (d_OP__case_1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 x3)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x3 x1002 x3000 x3500) (nd_OP__case_1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_tail x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlProcInstr x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x3 x1002 x3500) (d_OP__case_3 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_tail x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlProcInstr x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x2 x3 x1002 x3000 x3500) (nd_OP__case_3 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_tail (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char ']'#)) x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_5 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '>'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x2 x3 x1002 x3500) (d_OP__case_6 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_tail (Curry_Prelude.nd_C_dropWhile (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char ']'#))) x3 x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '>'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x2 x3 x1002 x3000 x3500) (nd_OP__case_6 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 x3
     Curry_Prelude.C_False -> d_OP__case_4 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x3 x1002 x3500) (d_OP__case_5 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 x3
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x3 x1002 x3000 x3500) (nd_OP__case_5 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_dropCData x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x1002 x3500) (d_OP__case_4 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_dropCData x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x3 x1002 x3000 x3500) (nd_OP__case_4 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_tail x2 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlCData x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x1002 x3500) (d_OP__case_7 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_tail x2 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlCData x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x2 x1002 x3000 x3500) (nd_OP__case_7 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlComment x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x3 x1002 x3500) (d_OP__case_8 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlComment x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x3 x1002 x3000 x3500) (nd_OP__case_8 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) Curry_Prelude.OP_List Curry_Prelude.OP_List) (d_C_scanXmlString x4 x3500)
     Curry_Prelude.C_False -> d_OP__case_12 x1 x3 x4 (Curry_Char.d_C_isSpace x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x3 x4 x1002 x3500) (d_OP__case_13 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) Curry_Prelude.OP_List Curry_Prelude.OP_List) (d_C_scanXmlString x4 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x3 x4 (Curry_Char.d_C_isSpace x3 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_13 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x3 x4 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x5 = d_C_parseAttrs (Curry_Prelude.d_C_apply (d_C_dropBlanks x3500) x4 x3500) x3500
          x6 = d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x5 x3500
          x7 = d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x5 x3500
           in (d_OP__case_11 x1 x6 x7 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x7 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_10 x1 x3 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '/'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x4 x3500) (Curry_Prelude.C_Char '>'#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x3 x4 x1002 x3500) (d_OP__case_12 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x3 x4 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x5 = d_C_parseAttrs (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_dropBlanks x2000 x3500) x4 x2001 x3500)))) x3500
                    x6 = d_OP_scanXmlElemName_dot___hash_selFP31_hash_attrs x5 x3500
                    x7 = d_OP_scanXmlElemName_dot___hash_selFP32_hash_rest x5 x3500
                     in (nd_OP__case_11 x1 x6 x7 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x7 x3500) (Curry_Prelude.C_Char '/'#) x3500) x2003 x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x3 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '/'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x4 x3500) (Curry_Prelude.C_Char '>'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_12 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem x1 Curry_Prelude.OP_List Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_tail x4 x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_9 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x3 x4 x1002 x3500) (d_OP__case_10 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem x1 Curry_Prelude.OP_List Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_tail x4 x3500) x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_10 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_scanXmlElemName (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3500) x4 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x3 x4 x1002 x3500) (d_OP__case_9 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_scanXmlElemName (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3500) x4 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_9 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem x1 x6 Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x7 x3500) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) x6 Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_tail x7 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x6 x7 x1002 x3500) (d_OP__case_11 x1 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (C_XElem x1 x6 Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x7 x3500) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) x1) x6 Curry_Prelude.OP_List) (d_C_scanXmlString (Curry_Prelude.d_C_tail x7 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x6 x7 x1002 x3000 x3500) (nd_OP__case_11 x1 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP__case_16 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_15 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '?'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x2 x3 x1002 x3500) (d_OP__case_17 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x3500) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '?'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x2 x3 x1002 x3000 x3500) (nd_OP__case_17 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlProcInstr x3 x3500
     Curry_Prelude.C_False -> d_OP__case_14 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x2 x3 x1002 x3500) (d_OP__case_15 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlProcInstr x3 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x2 x3 x1002 x3000 x3500) (nd_OP__case_15 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlElemName (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x2 x3 x1002 x3500) (d_OP__case_14 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlElemName (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x2 x3 x1002 x3000 x3500) (nd_OP__case_14 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlComment (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlCData x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x1002 x3500) (d_OP__case_16 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_C_scanXmlComment (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_scanXmlCData x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x3 x1002 x3000 x3500) (nd_OP__case_16 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 x3)
     Curry_Prelude.C_False -> d_OP__case_20 x2 x3 (Curry_Char.d_C_isSpace x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x3 x1002 x3500) (d_OP__case_21 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.OP_Cons x2 x3)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x2 x3 (Curry_Char.d_C_isSpace x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x3 x1002 x3000 x3500) (nd_OP__case_21 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x2 x3 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = d_C_scanXmlText (Curry_Prelude.d_C_apply (d_C_dropBlanks x3500) x3 x3500) x3500
          x5 = d_OP_scanXmlText_dot___hash_selFP25_hash_txt x4 x3500
          x6 = d_OP_scanXmlText_dot___hash_selFP26_hash_rem x4 x3500
           in (Curry_Prelude.OP_Tuple2 (d_OP__case_19 x5 (Curry_Prelude.d_C_null x5 x3500) x3500) x6)
     Curry_Prelude.C_False -> d_OP__case_18 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x2 x3 x1002 x3500) (d_OP__case_20 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x2 x3 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x4 = d_C_scanXmlText (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_dropBlanks x2000 x3500) x3 x2001 x3500)))) x3500
                    x5 = d_OP_scanXmlText_dot___hash_selFP25_hash_txt x4 x3500
                    x6 = d_OP_scanXmlText_dot___hash_selFP26_hash_rem x4 x3500
                     in (Curry_Prelude.OP_Tuple2 (nd_OP__case_19 x5 (Curry_Prelude.d_C_null x5 x3500) x2003 x3500) x6))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x2 x3 x1002 x3000 x3500) (nd_OP__case_20 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x2 x3 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x7 = d_C_scanXmlText x3 x3500
          x8 = d_OP_scanXmlText_dot___hash_selFP28_hash_txt x7 x3500
          x9 = d_OP_scanXmlText_dot___hash_selFP29_hash_rem x7 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x8) x9)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x2 x3 x1002 x3500) (d_OP__case_18 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x2 x3 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x7 = d_C_scanXmlText x3 x3500
          x8 = d_OP_scanXmlText_dot___hash_selFP28_hash_txt x7 x3500
          x9 = d_OP_scanXmlText_dot___hash_selFP29_hash_rem x7 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x8) x9)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x2 x3 x1002 x3000 x3500) (nd_OP__case_18 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x5 x1002 x3500) (d_OP__case_19 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x5 x1002 x3000 x3500) (nd_OP__case_19 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x2 x3 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_scanXmlElem x3 x3500
     Curry_Prelude.C_False -> let
          x4 = d_C_scanXmlText (Curry_Prelude.OP_Cons x2 x3) x3500
          x5 = d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x4 x3500
          x6 = d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x4 x3500
           in (Curry_Prelude.OP_Cons (C_XText x5) (d_OP_scanXmlString_dot_scanXml_dot_209 x6 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x2 x3 x1002 x3500) (d_OP__case_22 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x2 x3 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_scanXmlElem x3 x3500
     Curry_Prelude.C_False -> let
          x4 = d_C_scanXmlText (Curry_Prelude.OP_Cons x2 x3) x3500
          x5 = d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP22_hash_initxt x4 x3500
          x6 = d_OP_scanXmlString_dot_scanXml_dot_209_dot___hash_selFP23_hash_remtag x4 x3500
           in (Curry_Prelude.OP_Cons (C_XText x5) (d_OP_scanXmlString_dot_scanXml_dot_209 x6 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x2 x3 x1002 x3000 x3500) (nd_OP__case_22 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x2 x4 x3 x3500 = case x3 of
     (C_XText x5) -> let
          x6 = d_C_parseXmlTokens x4 x2 x3500
          x7 = d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x6 x3500
          x8 = d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x6 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XText (d_C_xmlUnquoteSpecials x5 x3500)) x7) x8)
     (C_XElem x9 x10 x11) -> d_OP__case_27 x2 x4 x10 x11 x9 x3500
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x4 x1002 x3500) (d_OP__case_28 x2 x4 x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 x4 z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x2 x4 x3 x3000 x3500 = case x3 of
     (C_XText x5) -> let
          x6 = d_C_parseXmlTokens x4 x2 x3500
          x7 = d_OP_parseXmlTokens_dot___hash_selFP7_hash_xexps x6 x3500
          x8 = d_OP_parseXmlTokens_dot___hash_selFP8_hash_rem_xtokens x6 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XText (d_C_xmlUnquoteSpecials x5 x3500)) x7) x8)
     (C_XElem x9 x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x2 x4 x10 x11 x9 x2000 x3500))
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x2 x4 x1002 x3000 x3500) (nd_OP__case_28 x2 x4 x1003 x3000 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x2 x4 z x3000 x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x2 x4 x10 x11 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_26 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '<'#) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x13 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x2 x4 x10 x11 x1002 x3500) (d_OP__case_27 x2 x4 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x2 x4 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x2 x4 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x2 x4 x10 x11 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '<'#) x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x13 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x2 x4 x10 x11 x1002 x3000 x3500) (nd_OP__case_27 x2 x4 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x2 x4 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x2 x4 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x2 x4 x10 x11 x12 x13 x20 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x14 = d_C_parseXmlTokens x4 (Curry_Prelude.C_Just x13) x3500
          x15 = d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x14 x3500
          x16 = d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x14 x3500
          x17 = d_C_parseXmlTokens x16 x2 x3500
          x18 = d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x17 x3500
          x19 = d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x17 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem x13 x10 x15) x18) x19)
     Curry_Prelude.C_False -> d_OP__case_25 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '<'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x13 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x2 x4 x10 x11 x12 x13 x1002 x3500) (d_OP__case_26 x2 x4 x10 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x2 x4 x10 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x2 x4 x10 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x2 x4 x10 x11 x12 x13 x20 x3000 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x14 = d_C_parseXmlTokens x4 (Curry_Prelude.C_Just x13) x3500
          x15 = d_OP_parseXmlTokens_dot___hash_selFP13_hash_xexps1 x14 x3500
          x16 = d_OP_parseXmlTokens_dot___hash_selFP14_hash_xtokens1 x14 x3500
          x17 = d_C_parseXmlTokens x16 x2 x3500
          x18 = d_OP_parseXmlTokens_dot___hash_selFP11_hash_xexps x17 x3500
          x19 = d_OP_parseXmlTokens_dot___hash_selFP12_hash_rem_xtokens x17 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem x13 x10 x15) x18) x19)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char '<'#) x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x13 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x2 x4 x10 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_26 x2 x4 x10 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x2 x4 x10 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x2 x4 x10 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x2 x4 x10 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_24 x2 x4 x10 x11 x13 (Curry_Prelude.d_C_maybe Curry_Prelude.C_False (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.d_C_tail x13 x3500)) x2 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_23 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x2 x4 x10 x11 x12 x13 x1002 x3500) (d_OP__case_25 x2 x4 x10 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x2 x4 x10 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x2 x4 x10 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x2 x4 x10 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_24 x2 x4 x10 x11 x13 (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.d_C_tail x13 x3500))) x2 x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x2 x4 x10 x11 x12 x13 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x2 x4 x10 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_25 x2 x4 x10 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x2 x4 x10 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x2 x4 x10 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x2 x4 x10 x11 x12 x13 x26 x3500 = case x26 of
     Curry_Prelude.C_True -> let
          x23 = d_C_parseXmlTokens x4 x2 x3500
          x24 = d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x23 x3500
          x25 = d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x23 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons x12 x13) x10 x11) x24) x25)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x2 x4 x10 x11 x12 x13 x1002 x3500) (d_OP__case_23 x2 x4 x10 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x2 x4 x10 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x2 x4 x10 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x2 x4 x10 x11 x12 x13 x26 x3000 x3500 = case x26 of
     Curry_Prelude.C_True -> let
          x23 = d_C_parseXmlTokens x4 x2 x3500
          x24 = d_OP_parseXmlTokens_dot___hash_selFP19_hash_xexps x23 x3500
          x25 = d_OP_parseXmlTokens_dot___hash_selFP20_hash_rem_xtokens x23 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem (Curry_Prelude.OP_Cons x12 x13) x10 x11) x24) x25)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x2 x4 x10 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_23 x2 x4 x10 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x2 x4 x10 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x2 x4 x10 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x2 x4 x10 x11 x13 x23 x3500 = case x23 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x20 = d_C_parseXmlTokens x4 x2 x3500
          x21 = d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x20 x3500
          x22 = d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x20 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem x13 x10 x11) x21) x22)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x2 x4 x10 x11 x13 x1002 x3500) (d_OP__case_24 x2 x4 x10 x11 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x2 x4 x10 x11 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x2 x4 x10 x11 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x2 x4 x10 x11 x13 x23 x3000 x3500 = case x23 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x4
     Curry_Prelude.C_False -> let
          x20 = d_C_parseXmlTokens x4 x2 x3500
          x21 = d_OP_parseXmlTokens_dot___hash_selFP16_hash_xexps x20 x3500
          x22 = d_OP_parseXmlTokens_dot___hash_selFP17_hash_rem_xtokens x20 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (C_XElem x13 x10 x11) x21) x22)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x2 x4 x10 x11 x13 x1002 x3000 x3500) (nd_OP__case_24 x2 x4 x10 x11 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x2 x4 x10 x11 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x2 x4 x10 x11 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x2 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1002 x3500) (d_OP__case_29 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1002 x3000 x3500) (nd_OP__case_29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_30 x1 x3 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_tail x3 x3500) Curry_Prelude.OP_List x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x3 x1002 x3500) (d_OP__case_31 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x3500) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x1 x3 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_tail x3 x3500) Curry_Prelude.OP_List x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x3 x1002 x3000 x3500) (nd_OP__case_31 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_head x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x3 x1002 x3500) (d_OP__case_30 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.d_C_head x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1 x3 x1002 x3000 x3500) (nd_OP__case_30 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP__case_34 x3 x3500
     Curry_Prelude.C_False -> d_OP__case_32 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x2 x3 x1002 x3500) (d_OP__case_35 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x2 x3 x1002 x3000 x3500) (nd_OP__case_35 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 x3) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x2 x3 x1002 x3500) (d_OP__case_32 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 x3) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x2 x3 x1002 x3000 x3500) (nd_OP__case_32 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = x4
           in (d_OP__case_33 x3 x5 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char 'x'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readInt x3 x3500) x3500) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1002 x3500) (d_OP__case_34 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = x4
                in (nd_OP__case_33 x3 x5 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char 'x'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readInt x3 x3500) x3500) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1002 x3000 x3500) (nd_OP__case_34 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readHex x5 x3500) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readInt x3 x3500) x3500) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x3 x5 x6 x1002 x3500) (d_OP__case_33 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readHex x5 x3500) x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Read.d_C_readInt x3 x3500) x3500) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_33 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_48 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_47 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1 x2 x1002 x3500) (d_OP__case_48 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_48 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_48 x1 x2 x1002 x3000 x3500) (nd_OP__case_48 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_48 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_48 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_47 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_46 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x1 x2 x1002 x3500) (d_OP__case_47 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x1 x2 x1002 x3000 x3500) (nd_OP__case_47 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_46 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_45 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x1 x2 x1002 x3500) (d_OP__case_46 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_45 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x1 x2 x1002 x3000 x3500) (nd_OP__case_46 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_45 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_44 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x1 x2 x1002 x3500) (d_OP__case_45 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_44 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x1 x2 x1002 x3000 x3500) (nd_OP__case_45 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_44 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_43 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1 x2 x1002 x3500) (d_OP__case_44 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_43 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x1 x2 x1002 x3000 x3500) (nd_OP__case_44 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_43 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 228#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_42 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x1 x2 x1002 x3500) (d_OP__case_43 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 228#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_42 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x1 x2 x1002 x3000 x3500) (nd_OP__case_43 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 246#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_41 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x2 x1002 x3500) (d_OP__case_42 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 246#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_41 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x2 x1002 x3000 x3500) (nd_OP__case_42 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 252#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_40 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x1 x2 x1002 x3500) (d_OP__case_41 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 252#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_40 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x2 x1002 x3000 x3500) (nd_OP__case_41 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 196#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_39 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1 x2 x1002 x3500) (d_OP__case_40 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 196#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x2 x1002 x3000 x3500) (nd_OP__case_40 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 214#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_38 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x2 x1002 x3500) (d_OP__case_39 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 214#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_38 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x2 x1002 x3000 x3500) (nd_OP__case_39 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 220#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_37 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x2 x1002 x3500) (d_OP__case_38 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 220#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_37 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x2 x1002 x3000 x3500) (nd_OP__case_38 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 223#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> d_OP__case_36 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x2 x1002 x3500) (d_OP__case_37 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char (nonAsciiChr 223#)) (d_C_xmlUnquoteSpecials x2 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x2 x1002 x3000 x3500) (nd_OP__case_37 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_unquoteUnicode x1 x3500) (d_C_xmlUnquoteSpecials x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x2 x1002 x3500) (d_OP__case_36 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_C_unquoteUnicode x1 x3500) (d_C_xmlUnquoteSpecials x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x2 x1002 x3000 x3500) (nd_OP__case_36 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_50 x2 x3 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = d_C_splitAtChar (Curry_Prelude.C_Char ';'#) x3 x3500
          x5 = d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x4 x3500
          x6 = d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x4 x3500
           in (d_C_xmlUnquoteSpecial x5 x6 x3500)
     Curry_Prelude.C_False -> d_OP__case_49 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x2 x3 x1002 x3500) (d_OP__case_50 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_50 x2 x3 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = d_C_splitAtChar (Curry_Prelude.C_Char ';'#) x3 x3500
          x5 = d_OP_xmlUnquoteSpecials_dot___hash_selFP4_hash_special x4 x3500
          x6 = d_OP_xmlUnquoteSpecials_dot___hash_selFP5_hash_rest x4 x3500
           in (d_C_xmlUnquoteSpecial x5 x6 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_49 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_50 x2 x3 x1002 x3000 x3500) (nd_OP__case_50 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_50 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_50 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_49 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_xmlUnquoteSpecials x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x2 x3 x1002 x3500) (d_OP__case_49 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_49 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_xmlUnquoteSpecials x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_49 x2 x3 x1002 x3000 x3500) (nd_OP__case_49 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_49 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_49 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_52 x3 x2 x3500 = case x2 of
     (C_XText x4) -> d_OP__case_51 x4 x3 x3500
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x3 x1002 x3500) (d_OP__case_52 x3 x1003 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x3 z x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_52 x3 x2 x3000 x3500 = case x2 of
     (C_XText x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_51 x4 x3 x2000 x3500))
     (Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_52 x3 x1002 x3000 x3500) (nd_OP__case_52 x3 x1003 x3000 x3500)
     (Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_52 x3 z x3000 x3500) x1002
     (Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_52 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_51 x4 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x4 x1002 x3500) (d_OP__case_51 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_51 x4 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x4 x1002 x3000 x3500) (nd_OP__case_51 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_54 x1 x2 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> d_OP__case_53 x1 x2 x5 x7 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3500) (Curry_Prelude.C_Int 1#) x3500) (d_C_isXText (Curry_Prelude.d_C_head x7 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x1 x2 x5 x7 x1002 x3500) (d_OP__case_54 x1 x2 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x1 x2 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x1 x2 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_54 x1 x2 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_53 x1 x2 x5 x7 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3500) (Curry_Prelude.C_Int 1#) x3500) (d_C_isXText (Curry_Prelude.d_C_head x7 x3500) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_54 x1 x2 x5 x7 x1002 x3000 x3500) (nd_OP__case_54 x1 x2 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_54 x1 x2 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_54 x1 x2 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_53 x1 x2 x5 x7 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x8 = d_OP_showXmlExp_dot___hash_selFP2_hash_s x7 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x2 x8 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_C_showXmlExps (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500) x7 x2 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x1 x2 x5 x7 x1002 x3500) (d_OP__case_53 x1 x2 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x1 x2 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x1 x2 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_53 x1 x2 x5 x7 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x8 = d_OP_showXmlExp_dot___hash_selFP2_hash_s x7 x3500
                in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x2 x8 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (nd_C_showXmlExps (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500) x7 x2 x2000 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_xtab x1 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_53 x1 x2 x5 x7 x1002 x3000 x3500) (nd_OP__case_53 x1 x2 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_53 x1 x2 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_53 x1 x2 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_55 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_lookupDtdUrl x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x1 x3 x1002 x3500) (d_OP__case_55 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_55 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'Y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_lookupDtdUrl x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x1 x3 x1002 x3000 x3500) (nd_OP__case_55 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_56 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x1 x1002 x3500) (d_OP__case_56 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_56 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x1 x1002 x3000 x3500) (nd_OP__case_56 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_57 x3 x2 x3500 = case x2 of
     (C_DtdUrl x4) -> Curry_Prelude.C_True
     (C_Enc x5) -> d_C_hasDtdUrl x3 x3500
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x3 x1002 x3500) (d_OP__case_57 x3 x1003 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x3 z x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_57 x3 x2 x3000 x3500 = case x2 of
     (C_DtdUrl x4) -> Curry_Prelude.C_True
     (C_Enc x5) -> d_C_hasDtdUrl x3 x3500
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x3 x1002 x3000 x3500) (nd_OP__case_57 x3 x1003 x3000 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x3 z x3000 x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_58 x3 x2 x3500 = case x2 of
     (C_Enc x4) -> d_C_lookupDtdUrl x3 x3500
     (C_DtdUrl x5) -> x5
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x3 x1002 x3500) (d_OP__case_58 x3 x1003 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x3 z x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_58 x3 x2 x3000 x3500 = case x2 of
     (C_Enc x4) -> d_C_lookupDtdUrl x3 x3500
     (C_DtdUrl x5) -> x5
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x3 x1002 x3000 x3500) (nd_OP__case_58 x3 x1003 x3000 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 x3 z x3000 x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_59 x3 x2 x3500 = case x2 of
     (C_Enc x4) -> x4
     (C_DtdUrl x5) -> d_C_lookupEncoding x3 x3500
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x3 x1002 x3500) (d_OP__case_59 x3 x1003 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x3 z x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_59 x3 x2 x3000 x3500 = case x2 of
     (C_Enc x4) -> x4
     (C_DtdUrl x5) -> d_C_lookupEncoding x3 x3500
     (Choice_C_XmlDocParams x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_59 x3 x1002 x3000 x3500) (nd_OP__case_59 x3 x1003 x3000 x3500)
     (Choices_C_XmlDocParams x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_59 x3 z x3000 x3500) x1002
     (Guard_C_XmlDocParams x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_59 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_XmlDocParams x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_60 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_iso88591Encoding x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_C_standardEncoding (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3500) (d_C_iso88591Encoding x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x2 x3 x1002 x3500) (d_OP__case_60 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_60 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_iso88591Encoding x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_C_standardEncoding (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3500) (d_C_iso88591Encoding x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_60 x2 x3 x1002 x3000 x3500) (nd_OP__case_60 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_60 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_60 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_68 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_67 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '>'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x2 x3 x1002 x3500) (d_OP__case_68 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_68 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_67 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '>'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_68 x2 x3 x1002 x3000 x3500) (nd_OP__case_68 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_68 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_68 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_67 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_66 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '&'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x2 x3 x1002 x3500) (d_OP__case_67 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_67 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_66 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '&'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_67 x2 x3 x1002 x3000 x3500) (nd_OP__case_67 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_67 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_67 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_66 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_65 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x2 x3 x1002 x3500) (d_OP__case_66 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_66 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_65 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '"'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_66 x2 x3 x1002 x3000 x3500) (nd_OP__case_66 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_66 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_66 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_65 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_64 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\''#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x2 x3 x1002 x3500) (d_OP__case_65 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_65 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_64 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '\''#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_65 x2 x3 x1002 x3000 x3500) (nd_OP__case_65 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_65 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_65 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_64 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_63 x2 x3 (Curry_Prelude.d_OP_lt (Curry_Prelude.d_C_ord x2 x3500) (Curry_Prelude.C_Int 32#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x2 x3 x1002 x3500) (d_OP__case_64 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_64 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List)))))) (d_C_standardEncoding x3 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_63 x2 x3 (Curry_Prelude.d_OP_lt (Curry_Prelude.d_C_ord x2 x3500) (Curry_Prelude.C_Int 32#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_64 x2 x3 x1002 x3000 x3500) (nd_OP__case_64 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_64 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_64 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_63 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_C_ord x2 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) (d_C_standardEncoding x3 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_62 x2 x3 (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_ord x2 x3500) (Curry_Prelude.C_Int 127#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x2 x3 x1002 x3500) (d_OP__case_63 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_63 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_C_ord x2 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) (d_C_standardEncoding x3 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x2 x3 (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_ord x2 x3500) (Curry_Prelude.C_Int 127#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_63 x2 x3 x1002 x3000 x3500) (nd_OP__case_63 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_63 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_63 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_62 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_C_ord x2 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) (d_C_standardEncoding x3 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_61 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x2 x3 x1002 x3500) (d_OP__case_62 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_62 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_C_ord x2 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) Curry_Prelude.OP_List) (d_C_standardEncoding x3 x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_61 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_62 x2 x3 x1002 x3000 x3500) (nd_OP__case_62 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_62 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_62 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_61 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_standardEncoding x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x2 x3 x1002 x3500) (d_OP__case_61 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_61 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (d_C_standardEncoding x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_61 x2 x3 x1002 x3000 x3500) (nd_OP__case_61 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_61 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_61 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
