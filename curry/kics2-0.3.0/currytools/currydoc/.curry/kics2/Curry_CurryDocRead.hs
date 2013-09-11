{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryDocRead (C_SourceLine (..), C_AnaInfo (..), d_C_readComments, d_C_classifyLine, d_C_getFirstId, d_C_isIdChar, d_C_groupLines, d_C_groupProgLines, d_C_groupComment, d_C_skipFuncDefs, d_C_skipDataDefs, d_C_getFuncComment, d_C_getConsComment, d_C_getDataComment, d_C_getCommentType, d_C_splitComment, d_C_splitCommentMain, d_C_splitCommentParams, d_C_getNondetInfo, nd_C_getNondetInfo, d_C_getCompleteInfo, nd_C_getCompleteInfo, d_C_getIndetInfo, nd_C_getIndetInfo, d_C_getOpCompleteInfo, nd_C_getOpCompleteInfo, d_C_getFunctionInfo, d_C_isFunctionType, d_C_skipWhiteSpace, nd_C_skipWhiteSpace, d_C_isWhiteSpace, d_C_showId, d_C_brackets, d_C_getLastName, nd_C_getLastName) where

import Basics
import qualified Curry_Char
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_TotallyDefined
data C_SourceLine
     = C_Comment (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_FuncDef (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_DataDef (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_ModDef
     | C_OtherLine
     | Choice_C_SourceLine Cover ID C_SourceLine C_SourceLine
     | Choices_C_SourceLine Cover ID ([C_SourceLine])
     | Fail_C_SourceLine Cover FailInfo
     | Guard_C_SourceLine Cover Constraints C_SourceLine

instance Show C_SourceLine where
  showsPrec d (Choice_C_SourceLine cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_SourceLine cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_SourceLine cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_SourceLine cd info) = showChar '!'
  showsPrec _ (C_Comment x1) = (showString "(Comment") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FuncDef x1) = (showString "(FuncDef") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_DataDef x1) = (showString "(DataDef") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_ModDef = showString "ModDef"
  showsPrec _ C_OtherLine = showString "OtherLine"


instance Read C_SourceLine where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Comment x1,r1) | (_,r0) <- readQualified "CurryDocRead" "Comment" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FuncDef x1,r1) | (_,r0) <- readQualified "CurryDocRead" "FuncDef" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_DataDef x1,r1) | (_,r0) <- readQualified "CurryDocRead" "DataDef" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_ModDef,r0) | (_,r0) <- readQualified "CurryDocRead" "ModDef" r]) s) ++ (readParen False (\r -> [ (C_OtherLine,r0) | (_,r0) <- readQualified "CurryDocRead" "OtherLine" r]) s))))


instance NonDet C_SourceLine where
  choiceCons = Choice_C_SourceLine
  choicesCons = Choices_C_SourceLine
  failCons = Fail_C_SourceLine
  guardCons = Guard_C_SourceLine
  try (Choice_C_SourceLine cd i x y) = tryChoice cd i x y
  try (Choices_C_SourceLine cd i xs) = tryChoices cd i xs
  try (Fail_C_SourceLine cd info) = Fail cd info
  try (Guard_C_SourceLine cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_SourceLine cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_SourceLine cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_SourceLine cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_SourceLine cd i _) = error ("CurryDocRead.SourceLine.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_SourceLine cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_SourceLine cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_SourceLine where
  generate s c = Choices_C_SourceLine c (freeID [1,1,1,0,0] s) [(C_Comment (generate (leftSupply s) c)),(C_FuncDef (generate (leftSupply s) c)),(C_DataDef (generate (leftSupply s) c)),C_ModDef,C_OtherLine]


instance NormalForm C_SourceLine where
  ($!!) cont (C_Comment x1) d cs = (((\y1 d cs -> cont (C_Comment y1) d cs) $!! x1) d) cs
  ($!!) cont (C_FuncDef x1) d cs = (((\y1 d cs -> cont (C_FuncDef y1) d cs) $!! x1) d) cs
  ($!!) cont (C_DataDef x1) d cs = (((\y1 d cs -> cont (C_DataDef y1) d cs) $!! x1) d) cs
  ($!!) cont C_ModDef d cs = cont C_ModDef d cs
  ($!!) cont C_OtherLine d cs = cont C_OtherLine d cs
  ($!!) cont (Choice_C_SourceLine cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_SourceLine cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_SourceLine cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_SourceLine cd info) _ _ = failCons cd info
  ($##) cont (C_Comment x1) d cs = (((\y1 d cs -> cont (C_Comment y1) d cs) $## x1) d) cs
  ($##) cont (C_FuncDef x1) d cs = (((\y1 d cs -> cont (C_FuncDef y1) d cs) $## x1) d) cs
  ($##) cont (C_DataDef x1) d cs = (((\y1 d cs -> cont (C_DataDef y1) d cs) $## x1) d) cs
  ($##) cont C_ModDef d cs = cont C_ModDef d cs
  ($##) cont C_OtherLine d cs = cont C_OtherLine d cs
  ($##) cont (Choice_C_SourceLine cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_SourceLine cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_SourceLine cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_SourceLine cd info) _ _ = failCons cd info
  searchNF search cont (C_Comment x1) = search (\y1 -> cont (C_Comment y1)) x1
  searchNF search cont (C_FuncDef x1) = search (\y1 -> cont (C_FuncDef y1)) x1
  searchNF search cont (C_DataDef x1) = search (\y1 -> cont (C_DataDef y1)) x1
  searchNF _ cont C_ModDef = cont C_ModDef
  searchNF _ cont C_OtherLine = cont C_OtherLine
  searchNF _ _ x = error ("CurryDocRead.SourceLine.searchNF: no constructor: " ++ (show x))


instance Unifiable C_SourceLine where
  (=.=) (C_Comment x1) (C_Comment y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_FuncDef x1) (C_FuncDef y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_DataDef x1) (C_DataDef y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_ModDef C_ModDef d cs = C_Success
  (=.=) C_OtherLine C_OtherLine d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Comment x1) (C_Comment y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_FuncDef x1) (C_FuncDef y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_DataDef x1) (C_DataDef y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_ModDef C_ModDef d cs = C_Success
  (=.<=) C_OtherLine C_OtherLine d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Comment x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_FuncDef x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_DataDef x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_ModDef = ((i :=: (ChooseN 3 0)):(concat []))
  bind cd i C_OtherLine = ((i :=: (ChooseN 4 0)):(concat []))
  bind d i (Choice_C_SourceLine cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_SourceLine cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_SourceLine cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_SourceLine cd i _) = error ("CurryDocRead.SourceLine.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_SourceLine cd info) = [(Unsolvable info)]
  bind d i (Guard_C_SourceLine cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Comment x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FuncDef x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_DataDef x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_ModDef = [(i :=: (ChooseN 3 0))]
  lazyBind cd i C_OtherLine = [(i :=: (ChooseN 4 0))]
  lazyBind d i (Choice_C_SourceLine cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_SourceLine cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_SourceLine cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_SourceLine cd i _) = error ("CurryDocRead.SourceLine.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_SourceLine cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_SourceLine cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_SourceLine where
  (=?=) (Choice_C_SourceLine cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_SourceLine cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_SourceLine cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_SourceLine cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_SourceLine cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_SourceLine cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_SourceLine cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_SourceLine cd info) _ _ = failCons cd info
  (=?=) (C_Comment x1) (C_Comment y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_FuncDef x1) (C_FuncDef y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_DataDef x1) (C_DataDef y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_ModDef C_ModDef d cs = Curry_Prelude.C_True
  (=?=) C_OtherLine C_OtherLine d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_SourceLine cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_SourceLine cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_SourceLine cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_SourceLine cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_SourceLine cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_SourceLine cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_SourceLine cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_SourceLine cd info) _ _ = failCons cd info
  (<?=) (C_Comment x1) (C_Comment y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Comment _) (C_FuncDef _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comment _) (C_DataDef _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comment _) C_ModDef _ _ = Curry_Prelude.C_True
  (<?=) (C_Comment _) C_OtherLine _ _ = Curry_Prelude.C_True
  (<?=) (C_FuncDef x1) (C_FuncDef y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_FuncDef _) (C_DataDef _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FuncDef _) C_ModDef _ _ = Curry_Prelude.C_True
  (<?=) (C_FuncDef _) C_OtherLine _ _ = Curry_Prelude.C_True
  (<?=) (C_DataDef x1) (C_DataDef y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_DataDef _) C_ModDef _ _ = Curry_Prelude.C_True
  (<?=) (C_DataDef _) C_OtherLine _ _ = Curry_Prelude.C_True
  (<?=) C_ModDef C_ModDef d cs = Curry_Prelude.C_True
  (<?=) C_ModDef C_OtherLine _ _ = Curry_Prelude.C_True
  (<?=) C_OtherLine C_OtherLine d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_AnaInfo
     = C_AnaInfo (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_TotallyDefined.C_Completeness) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool)
     | HO_C_AnaInfo (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_TotallyDefined.C_Completeness) (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
     | Choice_C_AnaInfo Cover ID C_AnaInfo C_AnaInfo
     | Choices_C_AnaInfo Cover ID ([C_AnaInfo])
     | Fail_C_AnaInfo Cover FailInfo
     | Guard_C_AnaInfo Cover Constraints C_AnaInfo

instance Show C_AnaInfo where
  showsPrec d (Choice_C_AnaInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AnaInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AnaInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AnaInfo cd info) = showChar '!'
  showsPrec _ (C_AnaInfo x1 x2 x3 x4) = (showString "(AnaInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (HO_C_AnaInfo x1 x2 x3 x4) = (showString "(AnaInfo") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_AnaInfo where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_AnaInfo x1 x2 x3 x4,r4) | (_,r0) <- readQualified "CurryDocRead" "AnaInfo" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet C_AnaInfo where
  choiceCons = Choice_C_AnaInfo
  choicesCons = Choices_C_AnaInfo
  failCons = Fail_C_AnaInfo
  guardCons = Guard_C_AnaInfo
  try (Choice_C_AnaInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_AnaInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_AnaInfo cd info) = Fail cd info
  try (Guard_C_AnaInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AnaInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AnaInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AnaInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AnaInfo cd i _) = error ("CurryDocRead.AnaInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AnaInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AnaInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_AnaInfo where
  generate s c = Choices_C_AnaInfo c (freeID [4] s) [(HO_C_AnaInfo (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_AnaInfo where
  ($!!) cont (C_AnaInfo x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_AnaInfo y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_AnaInfo x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_AnaInfo y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_AnaInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AnaInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AnaInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AnaInfo cd info) _ _ = failCons cd info
  ($##) cont (C_AnaInfo x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_AnaInfo y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_AnaInfo x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_AnaInfo y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_AnaInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AnaInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AnaInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AnaInfo cd info) _ _ = failCons cd info
  searchNF search cont (C_AnaInfo x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_AnaInfo y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (HO_C_AnaInfo x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (HO_C_AnaInfo y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("CurryDocRead.AnaInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_AnaInfo where
  (=.=) (C_AnaInfo x1 x2 x3 x4) (C_AnaInfo y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_AnaInfo x1 x2 x3 x4) (HO_C_AnaInfo y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_AnaInfo x1 x2 x3 x4) (C_AnaInfo y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_AnaInfo x1 x2 x3 x4) (HO_C_AnaInfo y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_AnaInfo x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (HO_C_AnaInfo x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_AnaInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AnaInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AnaInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AnaInfo cd i _) = error ("CurryDocRead.AnaInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AnaInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AnaInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_AnaInfo x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (HO_C_AnaInfo x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_AnaInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AnaInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AnaInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AnaInfo cd i _) = error ("CurryDocRead.AnaInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AnaInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AnaInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_AnaInfo where
  (=?=) (Choice_C_AnaInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AnaInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AnaInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AnaInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AnaInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AnaInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AnaInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AnaInfo cd info) _ _ = failCons cd info
  (=?=) (C_AnaInfo x1 x2 x3 x4) (C_AnaInfo y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (HO_C_AnaInfo x1 x2 x3 x4) (HO_C_AnaInfo y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_AnaInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AnaInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AnaInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AnaInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AnaInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AnaInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AnaInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AnaInfo cd info) _ _ = failCons cd info
  (<?=) (C_AnaInfo x1 x2 x3 x4) (C_AnaInfo y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_AnaInfo x1 x2 x3 x4) (HO_C_AnaInfo y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_readComments :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_readComments x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_OP_readComments_dot___hash_lambda1 x3250 x3500

d_OP_readComments_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_readComments_dot___hash_lambda1 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot d_C_groupLines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) C_OtherLine)) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map d_C_classifyLine) Curry_Prelude.d_C_lines x3250 x3500) x3250 x3500) x3250 x3500) x1 x3250 x3500) x3250 x3500

d_C_classifyLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> C_SourceLine
d_C_classifyLine x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_takeWhile d_C_isIdChar) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#))) (Curry_Prelude.d_C_dropWhile d_C_isIdChar) x3250 x3500) x3250 x3500
      in (d_OP__case_29 x1 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 3#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List))) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3250 x3500) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 3#) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_C_getFirstId :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getFirstId x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_22 x2 x3 (Curry_Char.d_C_isAlpha x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getFirstId x1002 x3250 x3500) (d_C_getFirstId x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getFirstId z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getFirstId x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isIdChar :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isIdChar x1 x3250 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Char.d_C_isAlphaNum x1 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '_'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\''#) x3250 x3500) x3250 x3500) x3250 x3500

d_C_groupLines :: Curry_Prelude.OP_List C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_groupLines x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) C_ModDef) x3250 x3500) x1 x3250 x3500
     x3 = d_OP_groupLines_dot___hash_selFP2_hash_modcmts x2 x3250 x3500
     x4 = d_OP_groupLines_dot___hash_selFP3_hash_progcmts x2 x3250 x3500
      in (d_OP__case_19 x4 x3 x1 (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.OP_List x3250 x3500) x3250 x3500)

d_OP_groupLines_dot_getComment_dot_22 :: C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_groupLines_dot_getComment_dot_22 x1 x3250 x3500 = case x1 of
     (C_Comment x2) -> Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500
     (C_FuncDef x3) -> Curry_Prelude.OP_List
     (C_DataDef x4) -> Curry_Prelude.OP_List
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_groupLines_dot_getComment_dot_22 x1002 x3250 x3500) (d_OP_groupLines_dot_getComment_dot_22 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_groupLines_dot_getComment_dot_22 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_groupLines_dot_getComment_dot_22 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_groupLines_dot___hash_selFP2_hash_modcmts :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_SourceLine) (Curry_Prelude.OP_List C_SourceLine) -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceLine
d_OP_groupLines_dot___hash_selFP2_hash_modcmts x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_groupLines_dot___hash_selFP2_hash_modcmts x1002 x3250 x3500) (d_OP_groupLines_dot___hash_selFP2_hash_modcmts x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_groupLines_dot___hash_selFP2_hash_modcmts z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_groupLines_dot___hash_selFP2_hash_modcmts x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_groupLines_dot___hash_selFP3_hash_progcmts :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List C_SourceLine) (Curry_Prelude.OP_List C_SourceLine) -> Cover -> ConstStore -> Curry_Prelude.OP_List C_SourceLine
d_OP_groupLines_dot___hash_selFP3_hash_progcmts x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_groupLines_dot___hash_selFP3_hash_progcmts x1002 x3250 x3500) (d_OP_groupLines_dot___hash_selFP3_hash_progcmts x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_groupLines_dot___hash_selFP3_hash_progcmts z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_groupLines_dot___hash_selFP3_hash_progcmts x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_groupProgLines :: Curry_Prelude.OP_List C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_groupProgLines x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_18 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_groupProgLines x1002 x3250 x3500) (d_C_groupProgLines x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_groupProgLines z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_groupProgLines x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_groupComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_groupComment x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_17 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_groupComment x1 x1002 x3250 x3500) (d_C_groupComment x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_groupComment x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_groupComment x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_skipFuncDefs :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_skipFuncDefs x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_16 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_skipFuncDefs x1 x1002 x3250 x3500) (d_C_skipFuncDefs x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_skipFuncDefs x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_skipFuncDefs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_skipDataDefs :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_skipDataDefs x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_14 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_skipDataDefs x1 x1002 x3250 x3500) (d_C_skipDataDefs x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_skipDataDefs x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_skipDataDefs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getFuncComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getFuncComment x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_12 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getFuncComment x1 x1002 x3250 x3500) (d_C_getFuncComment x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getFuncComment x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getFuncComment x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getConsComment :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getConsComment x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_C_span d_C_isIdChar x3 x3250 x3500
          x6 = d_OP_getConsComment_dot___hash_selFP8_hash_consname x5 x3250 x3500
          x7 = d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt x5 x3250 x3500
           in (d_OP__case_9 x2 x6 x4 x3 x7 (Curry_Prelude.d_OP_eq_eq x6 x2 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getConsComment x1002 x2 x3250 x3500) (d_C_getConsComment x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getConsComment z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getConsComment x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getConsComment_dot___hash_selFP8_hash_consname :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_getConsComment_dot___hash_selFP8_hash_consname x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getConsComment_dot___hash_selFP8_hash_consname x1002 x3250 x3500) (d_OP_getConsComment_dot___hash_selFP8_hash_consname x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getConsComment_dot___hash_selFP8_hash_consname z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getConsComment_dot___hash_selFP8_hash_consname x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt x1002 x3250 x3500) (d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getConsComment_dot___hash_selFP9_hash_rconscmt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getConsComment_dot___hash_selFP6_hash_conscall :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_getConsComment_dot___hash_selFP6_hash_conscall x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getConsComment_dot___hash_selFP6_hash_conscall x1002 x3250 x3500) (d_OP_getConsComment_dot___hash_selFP6_hash_conscall x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getConsComment_dot___hash_selFP6_hash_conscall z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getConsComment_dot___hash_selFP6_hash_conscall x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getConsComment_dot___hash_selFP7_hash_callcmt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_getConsComment_dot___hash_selFP7_hash_callcmt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getConsComment_dot___hash_selFP7_hash_callcmt x1002 x3250 x3500) (d_OP_getConsComment_dot___hash_selFP7_hash_callcmt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getConsComment_dot___hash_selFP7_hash_callcmt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getConsComment_dot___hash_selFP7_hash_callcmt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getDataComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getDataComment x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_7 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getDataComment x1 x1002 x3250 x3500) (d_C_getDataComment x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getDataComment x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getDataComment x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getCommentType :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
d_C_getCommentType x1 x2 x3250 x3500 = Curry_Prelude.d_C_map Curry_Prelude.d_C_snd (Curry_Prelude.d_C_filter (d_OP_getCommentType_dot___hash_lambda2 x1) x2 x3250 x3500) x3250 x3500

d_OP_getCommentType_dot___hash_lambda2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getCommentType_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x2 x3250 x3500) x1 x3250 x3500

d_C_splitComment :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_splitComment x1 x3250 x3500 = d_C_splitCommentMain (Curry_Prelude.d_C_lines x1 x3250 x3500) x3250 x3500

d_C_splitCommentMain :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_splitCommentMain x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x2 Curry_Prelude.OP_List x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x2 x3250 x3500) (Curry_Prelude.C_Char '@'#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitCommentMain x1002 x3250 x3500) (d_C_splitCommentMain x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitCommentMain z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitCommentMain x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt x1002 x3250 x3500) (d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitCommentMain_dot___hash_selFP12_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_splitCommentMain_dot___hash_selFP12_hash_rest x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitCommentMain_dot___hash_selFP12_hash_rest x1002 x3250 x3500) (d_OP_splitCommentMain_dot___hash_selFP12_hash_rest x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitCommentMain_dot___hash_selFP12_hash_rest z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitCommentMain_dot___hash_selFP12_hash_rest x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_splitCommentParams :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_splitCommentParams x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_apply (d_C_skipWhiteSpace x3250 x3500) x2 x3250 x3500)) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_3 x4 x5 x2 x1 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.OP_List x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x4 x3250 x3500) (Curry_Prelude.C_Char '@'#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitCommentParams x1 x2 x1002 x3250 x3500) (d_C_splitCommentParams x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitCommentParams x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitCommentParams x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getNondetInfo :: C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_getNondetInfo x1 x3250 x3500 = case x1 of
     (C_AnaInfo x2 x3 x4 x5) -> x2
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getNondetInfo x1002 x3250 x3500) (d_C_getNondetInfo x1003 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getNondetInfo z x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getNondetInfo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getNondetInfo :: C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool
nd_C_getNondetInfo x1 x3000 x3250 x3500 = case x1 of
     (HO_C_AnaInfo x2 x3 x4 x5) -> x2
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getNondetInfo x1002 x3000 x3250 x3500) (nd_C_getNondetInfo x1003 x3000 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getNondetInfo z x3000 x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getNondetInfo x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getCompleteInfo :: C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_TotallyDefined.C_Completeness
d_C_getCompleteInfo x1 x3250 x3500 = case x1 of
     (C_AnaInfo x2 x3 x4 x5) -> x3
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getCompleteInfo x1002 x3250 x3500) (d_C_getCompleteInfo x1003 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getCompleteInfo z x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getCompleteInfo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getCompleteInfo :: C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_TotallyDefined.C_Completeness
nd_C_getCompleteInfo x1 x3000 x3250 x3500 = case x1 of
     (HO_C_AnaInfo x2 x3 x4 x5) -> x3
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getCompleteInfo x1002 x3000 x3250 x3500) (nd_C_getCompleteInfo x1003 x3000 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getCompleteInfo z x3000 x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getCompleteInfo x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getIndetInfo :: C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_getIndetInfo x1 x3250 x3500 = case x1 of
     (C_AnaInfo x2 x3 x4 x5) -> x4
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getIndetInfo x1002 x3250 x3500) (d_C_getIndetInfo x1003 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getIndetInfo z x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getIndetInfo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getIndetInfo :: C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool
nd_C_getIndetInfo x1 x3000 x3250 x3500 = case x1 of
     (HO_C_AnaInfo x2 x3 x4 x5) -> x4
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getIndetInfo x1002 x3000 x3250 x3500) (nd_C_getIndetInfo x1003 x3000 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getIndetInfo z x3000 x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getIndetInfo x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getOpCompleteInfo :: C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_getOpCompleteInfo x1 x3250 x3500 = case x1 of
     (C_AnaInfo x2 x3 x4 x5) -> x5
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getOpCompleteInfo x1002 x3250 x3500) (d_C_getOpCompleteInfo x1003 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getOpCompleteInfo z x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getOpCompleteInfo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_getOpCompleteInfo :: C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool
nd_C_getOpCompleteInfo x1 x3000 x3250 x3500 = case x1 of
     (HO_C_AnaInfo x2 x3 x4 x5) -> x5
     (Choice_C_AnaInfo x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getOpCompleteInfo x1002 x3000 x3250 x3500) (nd_C_getOpCompleteInfo x1003 x3000 x3250 x3500)
     (Choices_C_AnaInfo x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getOpCompleteInfo z x3000 x3250 x3500) x1002
     (Guard_C_AnaInfo x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getOpCompleteInfo x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_AnaInfo x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getFunctionInfo :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0
d_C_getFunctionInfo x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x2 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getFunctionInfo x1002 x2 x3250 x3500) (d_C_getFunctionInfo x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getFunctionInfo z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getFunctionInfo x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isFunctionType :: Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFunctionType x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFunctionType x1002 x3250 x3500) (d_C_isFunctionType x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFunctionType z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFunctionType x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_skipWhiteSpace :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_skipWhiteSpace x3250 x3500 = Curry_Prelude.d_C_dropWhile d_C_isWhiteSpace

nd_C_skipWhiteSpace :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_skipWhiteSpace x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id d_C_isWhiteSpace))

d_C_isWhiteSpace :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isWhiteSpace x1 x3250 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\n'#) x3250 x3500) x3250 x3500

d_C_showId :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showId x1 x3250 x3500 = d_OP__case_0 x1 (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_brackets :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_brackets x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.C_False -> x2
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_brackets x1002 x2 x3250 x3500) (d_C_brackets x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_brackets z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_brackets x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getLastName :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getLastName x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_takeWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '/'#))) (Curry_Prelude.d_C_reverse x3250 x3500) x3250 x3500) x3250 x3500

nd_C_getLastName :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_getLastName x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_takeWhile (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Prelude.C_Char '/'#))))) (Curry_Prelude.nd_C_reverse x2001 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) x1) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3250 x3500) (d_OP__case_0 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0 -> Cover -> ConstStore -> t0
d_OP__case_2 x2 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_1 x2 x5 x4 x6 (Curry_Prelude.d_OP_eq_eq x5 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x4 x1002 x3250 x3500) (d_OP__case_2 x2 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> t0
d_OP__case_1 x2 x5 x4 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_getFunctionInfo x4 x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x5 x4 x6 x1002 x3250 x3500) (d_OP__case_1 x2 x5 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x5 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x5 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x4 x5 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_C_splitCommentParams x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x4) x3250 x3500) x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_apply (d_C_skipWhiteSpace x3250 x3500) x2 x3250 x3500)) (d_C_splitCommentParams (Curry_Prelude.d_C_takeWhile Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500) x5 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x4 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_3 x4 x5 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x4 x5 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x4 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_4 x2 x3 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x4 = d_C_splitCommentMain x3 x3250 x3500
          x5 = d_OP_splitCommentMain_dot___hash_selFP11_hash_maincmt x4 x3250 x3500
          x6 = d_OP_splitCommentMain_dot___hash_selFP12_hash_rest x4 x3250 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) x5) x3250 x3500) x6)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (d_C_splitCommentParams (Curry_Prelude.d_C_takeWhile Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_tail x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_tail x2 x3250 x3500) x3250 x3500) x3 x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x3 x1002 x3250 x3500) (d_OP__case_4 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_7 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_6 x4 x1 x6 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x1 x1002 x3250 x3500) (d_OP__case_7 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_6 x4 x1 x6 x5 x3250 x3500 = case x5 of
     (C_DataDef x7) -> d_OP__case_5 x7 x1 x4 x6 (Curry_Prelude.d_OP_eq_eq x1 x7 x3250 x3500) x3250 x3500
     (C_FuncDef x8) -> d_C_getDataComment x1 x4 x3250 x3500
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x4 x1 x6 x1002 x3250 x3500) (d_OP__case_6 x4 x1 x6 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x4 x1 x6 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x4 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_5 x7 x1 x4 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_getDataComment x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x7 x1 x4 x6 x1002 x3250 x3500) (d_OP__case_5 x7 x1 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x7 x1 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x7 x1 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_9 x2 x6 x4 x3 x7 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x8 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '-'#)) x3250 x3500) x3 x3250 x3500
          x9 = d_OP_getConsComment_dot___hash_selFP6_hash_conscall x8 x3250 x3500
          x10 = d_OP_getConsComment_dot___hash_selFP7_hash_callcmt x8 x3250 x3500
           in (Curry_Prelude.C_Just (d_OP__case_8 x10 x9 x7 x6 (Curry_Prelude.d_C_null x10 x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> d_C_getConsComment x4 x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x2 x6 x4 x3 x7 x1002 x3250 x3500) (d_OP__case_9 x2 x6 x4 x3 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x2 x6 x4 x3 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x2 x6 x4 x3 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_8 x10 x9 x7 x6 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x6 x7
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x9 x10
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x10 x9 x7 x6 x1002 x3250 x3500) (d_OP__case_8 x10 x9 x7 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x10 x9 x7 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x10 x9 x7 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_12 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_11 x4 x1 x6 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x4 x1 x1002 x3250 x3500) (d_OP__case_12 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_11 x4 x1 x6 x5 x3250 x3500 = case x5 of
     (C_FuncDef x7) -> d_OP__case_10 x7 x1 x4 x6 (Curry_Prelude.d_OP_eq_eq x1 x7 x3250 x3500) x3250 x3500
     (C_DataDef x8) -> d_C_getFuncComment x1 x4 x3250 x3500
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x1 x6 x1002 x3250 x3500) (d_OP__case_11 x4 x1 x6 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x1 x6 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_10 x7 x1 x4 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> d_C_getFuncComment x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x7 x1 x4 x6 x1002 x3250 x3500) (d_OP__case_10 x7 x1 x4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x7 x1 x4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x7 x1 x4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_14 x1 x4 x3 x3250 x3500 = case x3 of
     (C_Comment x5) -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_Comment x5) x4) x3250 x3500
     (C_FuncDef x6) -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_FuncDef x6) x4) x3250 x3500
     (C_DataDef x7) -> d_OP__case_13 x7 x1 x4 (Curry_Prelude.d_OP_eq_eq x1 x7 x3250 x3500) x3250 x3500
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x4 x1002 x3250 x3500) (d_OP__case_14 x1 x4 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x4 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_13 x7 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_skipDataDefs x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_DataDef x7) x4) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_13 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_16 x1 x4 x3 x3250 x3500 = case x3 of
     (C_Comment x5) -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_Comment x5) x4) x3250 x3500
     (C_DataDef x6) -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_DataDef x6) x4) x3250 x3500
     (C_FuncDef x7) -> d_OP__case_15 x7 x1 x4 (Curry_Prelude.d_OP_eq_eq x1 x7 x3250 x3500) x3250 x3500
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x4 x1002 x3250 x3500) (d_OP__case_16 x1 x4 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 x4 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_15 x7 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_skipFuncDefs x1 x4 x3250 x3500
     Curry_Prelude.C_False -> d_C_groupProgLines (Curry_Prelude.OP_Cons (C_FuncDef x7) x4) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_15 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_17 x4 x1 x3 x3250 x3500 = case x3 of
     (C_Comment x5) -> d_C_groupComment (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x4 x3250 x3500
     (C_FuncDef x6) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_FuncDef x6) x1) (d_C_skipFuncDefs x6 x4 x3250 x3500)
     (C_DataDef x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_DataDef x7) x1) (d_C_skipDataDefs x7 x4 x3250 x3500)
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x4 x1 x1002 x3250 x3500) (d_OP__case_17 x4 x1 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x4 x1 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List C_SourceLine -> C_SourceLine -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_18 x3 x2 x3250 x3500 = case x2 of
     (C_Comment x4) -> d_C_groupComment x4 x3 x3250 x3500
     (C_FuncDef x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_FuncDef x5) Curry_Prelude.OP_List) (d_C_skipFuncDefs x5 x3 x3250 x3500)
     (C_DataDef x6) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (C_DataDef x6) Curry_Prelude.OP_List) (d_C_skipDataDefs x6 x3 x3250 x3500)
     (Choice_C_SourceLine x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x3 x1002 x3250 x3500) (d_OP__case_18 x3 x1003 x3250 x3500)
     (Choices_C_SourceLine x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x3 z x3250 x3500) x1002
     (Guard_C_SourceLine x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_SourceLine x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.OP_List C_SourceLine -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_19 x4 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (d_C_groupProgLines x1 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_groupLines_dot_getComment_dot_22 x3250 x3500) x3 x3250 x3500) (d_C_groupProgLines (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) C_ModDef) (Curry_Prelude.d_C_tail x4 x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_19 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_22 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_takeWhile d_C_isIdChar (Curry_Prelude.OP_Cons x2 x3) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_21 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '('#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x2 x3 x1002 x3250 x3500) (d_OP__case_22 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_21 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_takeWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char ')'#)) x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_20 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x3 x1002 x3250 x3500) (d_OP__case_21 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_20 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1002 x3250 x3500) (d_OP__case_20 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_29 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_Comment Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_28 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 4#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x2 x1002 x3250 x3500) (d_OP__case_29 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_29 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_Comment Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 4#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_29 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_28 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_Comment (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 4#) x1 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_27 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x2 x1002 x3250 x3500) (d_OP__case_28 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_28 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_28 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_Comment (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 4#) x1 x3250 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_28 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_27 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_ModDef
     Curry_Prelude.C_False -> d_OP__case_26 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x2 x1002 x3250 x3500) (d_OP__case_27 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_27 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_27 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_ModDef
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 7#) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_27 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_26 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_ModDef
     Curry_Prelude.C_False -> d_OP__case_25 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x2 x1002 x3250 x3500) (d_OP__case_26 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_26 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_26 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_ModDef
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_26 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_25 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x3 = d_C_getFirstId x1 x3250 x3500
           in (d_OP__case_24 x3 x1 x2 (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.OP_List x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x2 x1002 x3250 x3500) (d_OP__case_25 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_25 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_25 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x3 = d_C_getFirstId x1 x3250 x3500
                in (nd_OP__case_24 x3 x1 x2 (Curry_Prelude.d_OP_eq_eq x3 Curry_Prelude.OP_List x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_25 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_24 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> C_OtherLine
     Curry_Prelude.C_False -> d_OP__case_23 x3 x1 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_24 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_24 x3 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> C_OtherLine
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x3 x1 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_24 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_SourceLine
d_OP__case_23 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> C_DataDef (Curry_Prelude.d_C_apply x2 x1 x3250 x3500)
     Curry_Prelude.C_False -> C_FuncDef x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_23 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> C_SourceLine
nd_OP__case_23 x3 x1 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (C_DataDef (Curry_Prelude.nd_C_apply x2 x1 x2000 x3250 x3500)))
     Curry_Prelude.C_False -> C_FuncDef x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x3 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_23 x3 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x3 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x3 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
