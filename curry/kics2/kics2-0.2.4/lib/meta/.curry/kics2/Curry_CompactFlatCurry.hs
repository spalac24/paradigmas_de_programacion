{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CompactFlatCurry (C_Option (..), C_RequiredSpec, d_C_requires, d_C_alwaysRequired, d_C_defaultRequired, d_C_generateCompactFlatCurryFile, d_C_computeCompactFlatCurry) where

import Basics
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
import qualified Curry_SetRBT
import qualified Curry_Sort
import qualified Curry_TableRBT
import qualified Curry_Time
import qualified Curry_XML
data C_Option
     = C_Verbose
     | C_Main (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_Exports
     | C_InitFuncs (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | C_Required (Curry_Prelude.OP_List C_RequiredSpec)
     | C_Import (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Option Cover ID C_Option C_Option
     | Choices_C_Option Cover ID ([C_Option])
     | Fail_C_Option Cover FailInfo
     | Guard_C_Option Cover Constraints C_Option

instance Show C_Option where
  showsPrec d (Choice_C_Option cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Option cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Option cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Option cd info) = showChar '!'
  showsPrec _ C_Verbose = showString "Verbose"
  showsPrec _ (C_Main x1) = (showString "(Main") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_Exports = showString "Exports"
  showsPrec _ (C_InitFuncs x1) = (showString "(InitFuncs") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Required x1) = (showString "(Required") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Import x1) = (showString "(Import") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Option where
  readsPrec d s = (readParen False (\r -> [ (C_Verbose,r0) | (_,r0) <- readQualified "CompactFlatCurry" "Verbose" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_Main x1,r1) | (_,r0) <- readQualified "CompactFlatCurry" "Main" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_Exports,r0) | (_,r0) <- readQualified "CompactFlatCurry" "Exports" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_InitFuncs x1,r1) | (_,r0) <- readQualified "CompactFlatCurry" "InitFuncs" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Required x1,r1) | (_,r0) <- readQualified "CompactFlatCurry" "Required" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Import x1,r1) | (_,r0) <- readQualified "CompactFlatCurry" "Import" r, (x1,r1) <- readsPrec 11 r0]) s)))))


instance NonDet C_Option where
  choiceCons = Choice_C_Option
  choicesCons = Choices_C_Option
  failCons = Fail_C_Option
  guardCons = Guard_C_Option
  try (Choice_C_Option cd i x y) = tryChoice cd i x y
  try (Choices_C_Option cd i xs) = tryChoices cd i xs
  try (Fail_C_Option cd info) = Fail cd info
  try (Guard_C_Option cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Option cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Option cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Option cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Option cd i _) = error ("CompactFlatCurry.Option.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Option cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Option cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Option where
  generate s = Choices_C_Option defCover (freeID [0,1,0,1,1,1] s) [C_Verbose,(C_Main (generate (leftSupply s))),C_Exports,(C_InitFuncs (generate (leftSupply s))),(C_Required (generate (leftSupply s))),(C_Import (generate (leftSupply s)))]


instance NormalForm C_Option where
  ($!!) cont C_Verbose cs = cont C_Verbose cs
  ($!!) cont (C_Main x1) cs = ((\y1 cs -> cont (C_Main y1) cs) $!! x1) cs
  ($!!) cont C_Exports cs = cont C_Exports cs
  ($!!) cont (C_InitFuncs x1) cs = ((\y1 cs -> cont (C_InitFuncs y1) cs) $!! x1) cs
  ($!!) cont (C_Required x1) cs = ((\y1 cs -> cont (C_Required y1) cs) $!! x1) cs
  ($!!) cont (C_Import x1) cs = ((\y1 cs -> cont (C_Import y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_Option cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Option cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Option cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Option cd info) _ = failCons cd info
  ($##) cont C_Verbose cs = cont C_Verbose cs
  ($##) cont (C_Main x1) cs = ((\y1 cs -> cont (C_Main y1) cs) $## x1) cs
  ($##) cont C_Exports cs = cont C_Exports cs
  ($##) cont (C_InitFuncs x1) cs = ((\y1 cs -> cont (C_InitFuncs y1) cs) $## x1) cs
  ($##) cont (C_Required x1) cs = ((\y1 cs -> cont (C_Required y1) cs) $## x1) cs
  ($##) cont (C_Import x1) cs = ((\y1 cs -> cont (C_Import y1) cs) $## x1) cs
  ($##) cont (Choice_C_Option cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Option cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Option cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Option cd info) _ = failCons cd info
  searchNF _ cont C_Verbose = cont C_Verbose
  searchNF search cont (C_Main x1) = search (\y1 -> cont (C_Main y1)) x1
  searchNF _ cont C_Exports = cont C_Exports
  searchNF search cont (C_InitFuncs x1) = search (\y1 -> cont (C_InitFuncs y1)) x1
  searchNF search cont (C_Required x1) = search (\y1 -> cont (C_Required y1)) x1
  searchNF search cont (C_Import x1) = search (\y1 -> cont (C_Import y1)) x1
  searchNF _ _ x = error ("CompactFlatCurry.Option.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Option where
  (=.=) C_Verbose C_Verbose cs = C_Success
  (=.=) (C_Main x1) (C_Main y1) cs = (x1 =:= y1) cs
  (=.=) C_Exports C_Exports cs = C_Success
  (=.=) (C_InitFuncs x1) (C_InitFuncs y1) cs = (x1 =:= y1) cs
  (=.=) (C_Required x1) (C_Required y1) cs = (x1 =:= y1) cs
  (=.=) (C_Import x1) (C_Import y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Verbose C_Verbose cs = C_Success
  (=.<=) (C_Main x1) (C_Main y1) cs = (x1 =:<= y1) cs
  (=.<=) C_Exports C_Exports cs = C_Success
  (=.<=) (C_InitFuncs x1) (C_InitFuncs y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Required x1) (C_Required y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Import x1) (C_Import y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Verbose = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_Main x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i C_Exports = ((i :=: (ChooseN 2 0)):(concat []))
  bind i (C_InitFuncs x2) = ((i :=: (ChooseN 3 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Required x2) = ((i :=: (ChooseN 4 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Import x2) = ((i :=: (ChooseN 5 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Option cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Option cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Option cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Option cd i _) = error ("CompactFlatCurry.Option.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Option cd info) = [(Unsolvable info)]
  bind i (Guard_C_Option cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_Verbose = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_Main x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i C_Exports = [(i :=: (ChooseN 2 0))]
  lazyBind i (C_InitFuncs x2) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Required x2) = [(i :=: (ChooseN 4 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Import x2) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Option cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Option cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Option cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Option cd i _) = error ("CompactFlatCurry.Option.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Option cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Option cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Option where
  (=?=) (Choice_C_Option cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Option cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Option cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Option cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Option cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Option cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Option cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Option cd info) _ = failCons cd info
  (=?=) C_Verbose C_Verbose cs = Curry_Prelude.C_True
  (=?=) (C_Main x1) (C_Main y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) C_Exports C_Exports cs = Curry_Prelude.C_True
  (=?=) (C_InitFuncs x1) (C_InitFuncs y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Required x1) (C_Required y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Import x1) (C_Import y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Option cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Option cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Option cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Option cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Option cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Option cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Option cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Option cd info) _ = failCons cd info
  (<?=) C_Verbose C_Verbose cs = Curry_Prelude.C_True
  (<?=) C_Verbose (C_Main _) _ = Curry_Prelude.C_True
  (<?=) C_Verbose C_Exports _ = Curry_Prelude.C_True
  (<?=) C_Verbose (C_InitFuncs _) _ = Curry_Prelude.C_True
  (<?=) C_Verbose (C_Required _) _ = Curry_Prelude.C_True
  (<?=) C_Verbose (C_Import _) _ = Curry_Prelude.C_True
  (<?=) (C_Main x1) (C_Main y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Main _) C_Exports _ = Curry_Prelude.C_True
  (<?=) (C_Main _) (C_InitFuncs _) _ = Curry_Prelude.C_True
  (<?=) (C_Main _) (C_Required _) _ = Curry_Prelude.C_True
  (<?=) (C_Main _) (C_Import _) _ = Curry_Prelude.C_True
  (<?=) C_Exports C_Exports cs = Curry_Prelude.C_True
  (<?=) C_Exports (C_InitFuncs _) _ = Curry_Prelude.C_True
  (<?=) C_Exports (C_Required _) _ = Curry_Prelude.C_True
  (<?=) C_Exports (C_Import _) _ = Curry_Prelude.C_True
  (<?=) (C_InitFuncs x1) (C_InitFuncs y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_InitFuncs _) (C_Required _) _ = Curry_Prelude.C_True
  (<?=) (C_InitFuncs _) (C_Import _) _ = Curry_Prelude.C_True
  (<?=) (C_Required x1) (C_Required y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_Required _) (C_Import _) _ = Curry_Prelude.C_True
  (<?=) (C_Import x1) (C_Import y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Option where
  cover C_Verbose = C_Verbose
  cover (C_Main x1) = C_Main (cover x1)
  cover C_Exports = C_Exports
  cover (C_InitFuncs x1) = C_InitFuncs (cover x1)
  cover (C_Required x1) = C_Required (cover x1)
  cover (C_Import x1) = C_Import (cover x1)
  cover (Choice_C_Option cd i x y) = Choice_C_Option (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Option cd i xs) = Choices_C_Option (incCover cd) i (map cover xs)
  cover (Fail_C_Option cd info) = Fail_C_Option (incCover cd) info
  cover (Guard_C_Option cd c e) = Guard_C_Option (incCover cd) c (cover e)


data C_RequiredSpec
     = C_AlwaysReq (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_Requires (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | Choice_C_RequiredSpec Cover ID C_RequiredSpec C_RequiredSpec
     | Choices_C_RequiredSpec Cover ID ([C_RequiredSpec])
     | Fail_C_RequiredSpec Cover FailInfo
     | Guard_C_RequiredSpec Cover Constraints C_RequiredSpec

instance Show C_RequiredSpec where
  showsPrec d (Choice_C_RequiredSpec cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_RequiredSpec cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_RequiredSpec cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_RequiredSpec cd info) = showChar '!'
  showsPrec _ (C_AlwaysReq x1) = (showString "(AlwaysReq") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Requires x1 x2) = (showString "(Requires") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_RequiredSpec where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_AlwaysReq x1,r1) | (_,r0) <- readQualified "CompactFlatCurry" "AlwaysReq" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Requires x1 x2,r2) | (_,r0) <- readQualified "CompactFlatCurry" "Requires" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_RequiredSpec where
  choiceCons = Choice_C_RequiredSpec
  choicesCons = Choices_C_RequiredSpec
  failCons = Fail_C_RequiredSpec
  guardCons = Guard_C_RequiredSpec
  try (Choice_C_RequiredSpec cd i x y) = tryChoice cd i x y
  try (Choices_C_RequiredSpec cd i xs) = tryChoices cd i xs
  try (Fail_C_RequiredSpec cd info) = Fail cd info
  try (Guard_C_RequiredSpec cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_RequiredSpec cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_RequiredSpec cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_RequiredSpec cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_RequiredSpec cd i _) = error ("CompactFlatCurry.RequiredSpec.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_RequiredSpec cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_RequiredSpec cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_RequiredSpec where
  generate s = Choices_C_RequiredSpec defCover (freeID [1,2] s) [(C_AlwaysReq (generate (leftSupply s))),(C_Requires (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm C_RequiredSpec where
  ($!!) cont (C_AlwaysReq x1) cs = ((\y1 cs -> cont (C_AlwaysReq y1) cs) $!! x1) cs
  ($!!) cont (C_Requires x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Requires y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_RequiredSpec cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_RequiredSpec cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_RequiredSpec cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_RequiredSpec cd info) _ = failCons cd info
  ($##) cont (C_AlwaysReq x1) cs = ((\y1 cs -> cont (C_AlwaysReq y1) cs) $## x1) cs
  ($##) cont (C_Requires x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_Requires y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_RequiredSpec cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_RequiredSpec cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_RequiredSpec cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_RequiredSpec cd info) _ = failCons cd info
  searchNF search cont (C_AlwaysReq x1) = search (\y1 -> cont (C_AlwaysReq y1)) x1
  searchNF search cont (C_Requires x1 x2) = search (\y1 -> search (\y2 -> cont (C_Requires y1 y2)) x2) x1
  searchNF _ _ x = error ("CompactFlatCurry.RequiredSpec.searchNF: no constructor: " ++ (show x))


instance Unifiable C_RequiredSpec where
  (=.=) (C_AlwaysReq x1) (C_AlwaysReq y1) cs = (x1 =:= y1) cs
  (=.=) (C_Requires x1 x2) (C_Requires y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_AlwaysReq x1) (C_AlwaysReq y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_Requires x1 x2) (C_Requires y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_AlwaysReq x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_Requires x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_RequiredSpec cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_RequiredSpec cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_RequiredSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_RequiredSpec cd i _) = error ("CompactFlatCurry.RequiredSpec.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_RequiredSpec cd info) = [(Unsolvable info)]
  bind i (Guard_C_RequiredSpec cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_AlwaysReq x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_Requires x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_RequiredSpec cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_RequiredSpec cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_RequiredSpec cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_RequiredSpec cd i _) = error ("CompactFlatCurry.RequiredSpec.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_RequiredSpec cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_RequiredSpec cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_RequiredSpec where
  (=?=) (Choice_C_RequiredSpec cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_RequiredSpec cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_RequiredSpec cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_RequiredSpec cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_RequiredSpec cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_RequiredSpec cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_RequiredSpec cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_RequiredSpec cd info) _ = failCons cd info
  (=?=) (C_AlwaysReq x1) (C_AlwaysReq y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_Requires x1 x2) (C_Requires y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_RequiredSpec cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_RequiredSpec cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_RequiredSpec cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_RequiredSpec cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_RequiredSpec cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_RequiredSpec cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_RequiredSpec cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_RequiredSpec cd info) _ = failCons cd info
  (<?=) (C_AlwaysReq x1) (C_AlwaysReq y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_AlwaysReq _) (C_Requires _ _) _ = Curry_Prelude.C_True
  (<?=) (C_Requires x1 x2) (C_Requires y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_RequiredSpec where
  cover (C_AlwaysReq x1) = C_AlwaysReq (cover x1)
  cover (C_Requires x1 x2) = C_Requires (cover x1) (cover x2)
  cover (Choice_C_RequiredSpec cd i x y) = Choice_C_RequiredSpec (incCover cd) i (cover x) (cover y)
  cover (Choices_C_RequiredSpec cd i xs) = Choices_C_RequiredSpec (incCover cd) i (map cover xs)
  cover (Fail_C_RequiredSpec cd info) = Fail_C_RequiredSpec (incCover cd) info
  cover (Guard_C_RequiredSpec cd c e) = Guard_C_RequiredSpec (incCover cd) c (cover e)


d_C_isMainOption :: C_Option -> ConstStore -> Curry_Prelude.C_Bool
d_C_isMainOption x1 x3500 = case x1 of
     (C_Main x2) -> Curry_Prelude.C_True
     C_Verbose -> Curry_Prelude.C_False
     C_Exports -> Curry_Prelude.C_False
     (C_InitFuncs x3) -> Curry_Prelude.C_False
     (C_Required x4) -> Curry_Prelude.C_False
     (C_Import x5) -> Curry_Prelude.C_False
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isMainOption x1002 x3500) (d_C_isMainOption x1003 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isMainOption z x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isMainOption x1002) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getMainFuncFromOptions :: Curry_Prelude.OP_List C_Option -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getMainFuncFromOptions x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_139 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getMainFuncFromOptions x1002 x3500) (d_C_getMainFuncFromOptions x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getMainFuncFromOptions z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getMainFuncFromOptions x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getRequiredFromOptions :: Curry_Prelude.OP_List C_Option -> ConstStore -> Curry_Prelude.OP_List C_RequiredSpec
d_C_getRequiredFromOptions x1 x3500 = Curry_Prelude.d_C_concat (Curry_Prelude.d_C_foldr (acceptCs id d_OP_getRequiredFromOptions_dot___hash_lambda5) Curry_Prelude.OP_List x1 x3500) x3500

d_OP_getRequiredFromOptions_dot___hash_lambda5 :: C_Option -> Curry_Prelude.OP_List (Curry_Prelude.OP_List C_RequiredSpec) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List C_RequiredSpec)
d_OP_getRequiredFromOptions_dot___hash_lambda5 x1 x2 x3500 = case x1 of
     (C_Required x3) -> Curry_Prelude.OP_Cons x3 x2
     C_Verbose -> x2
     (C_Main x4) -> x2
     C_Exports -> x2
     (C_InitFuncs x5) -> x2
     (C_Import x6) -> x2
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRequiredFromOptions_dot___hash_lambda5 x1002 x2 x3500) (d_OP_getRequiredFromOptions_dot___hash_lambda5 x1003 x2 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRequiredFromOptions_dot___hash_lambda5 z x2 x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRequiredFromOptions_dot___hash_lambda5 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_addImport2Options :: Curry_Prelude.OP_List C_Option -> ConstStore -> Curry_Prelude.OP_List C_Option
d_C_addImport2Options x1 x3500 = Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_map (acceptCs id C_Import) (Curry_List.d_C_nub (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_addImport2Options_dot_alwaysReqMod_dot_20 x3500) (d_C_getRequiredFromOptions x1 x3500) x3500) x3500) x3500) x3500

d_OP_addImport2Options_dot_alwaysReqMod_dot_20 :: C_RequiredSpec -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_addImport2Options_dot_alwaysReqMod_dot_20 x1 x3500 = case x1 of
     (C_AlwaysReq x2) -> d_OP__case_138 x2 x3500
     (C_Requires x5 x6) -> Curry_Prelude.OP_List
     (Choice_C_RequiredSpec x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addImport2Options_dot_alwaysReqMod_dot_20 x1002 x3500) (d_OP_addImport2Options_dot_alwaysReqMod_dot_20 x1003 x3500)
     (Choices_C_RequiredSpec x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addImport2Options_dot_alwaysReqMod_dot_20 z x3500) x1002
     (Guard_C_RequiredSpec x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addImport2Options_dot_alwaysReqMod_dot_20 x1002) $! (addCs x1001 x3500))
     (Fail_C_RequiredSpec x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_requires :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_RequiredSpec
d_C_requires x1 x2 x3500 = C_Requires x1 x2

d_C_alwaysRequired :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_RequiredSpec
d_C_alwaysRequired x1 x3500 = C_AlwaysReq x1

d_C_defaultRequired :: ConstStore -> Curry_Prelude.OP_List C_RequiredSpec
d_C_defaultRequired x3500 = Curry_Prelude.OP_Cons (d_C_alwaysRequired (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) (Curry_Prelude.OP_Cons (d_C_alwaysRequired (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))) x3500) (Curry_Prelude.OP_Cons (d_C_alwaysRequired (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) x3500) (Curry_Prelude.OP_Cons (d_C_alwaysRequired (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) Curry_Prelude.OP_List)))))))))))))))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (Curry_Prelude.OP_Cons (d_C_requires (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'K'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))))) x3500) Curry_Prelude.OP_List)))))))))))))))

d_C_prelude :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_C_getRequiredInModule :: Curry_Prelude.OP_List C_RequiredSpec -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getRequiredInModule x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_getRequiredInModule_dot_getImpReq_dot_37 x2) x3500) x1 x3500

d_OP_getRequiredInModule_dot_getImpReq_dot_37 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_RequiredSpec -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getRequiredInModule_dot_getImpReq_dot_37 x1 x2 x3500 = case x2 of
     (C_AlwaysReq x3) -> d_OP__case_137 x1 x3 x3500
     (C_Requires x6 x7) -> Curry_Prelude.OP_List
     (Choice_C_RequiredSpec x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRequiredInModule_dot_getImpReq_dot_37 x1 x1002 x3500) (d_OP_getRequiredInModule_dot_getImpReq_dot_37 x1 x1003 x3500)
     (Choices_C_RequiredSpec x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRequiredInModule_dot_getImpReq_dot_37 x1 z x3500) x1002
     (Guard_C_RequiredSpec x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRequiredInModule_dot_getImpReq_dot_37 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_RequiredSpec x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getImplicitlyRequired :: Curry_Prelude.OP_List C_RequiredSpec -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getImplicitlyRequired x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x2) x3500) x1 x3500

d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> C_RequiredSpec -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x1 x2 x3500 = case x2 of
     (C_AlwaysReq x3) -> Curry_Prelude.OP_List
     (C_Requires x4 x5) -> d_OP__case_135 x1 x4 x5 (Curry_Prelude.d_OP_eq_eq x4 x1 x3500) x3500
     (Choice_C_RequiredSpec x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x1 x1002 x3500) (d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x1 x1003 x3500)
     (Choices_C_RequiredSpec x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x1 z x3500) x1002
     (Guard_C_RequiredSpec x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getImplicitlyRequired_dot_getImpReq_dot_45 x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_RequiredSpec x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_defaultRequiredTypes :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_defaultRequiredTypes x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (d_C_prelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List)))))

d_C_generateCompactFlatCurryFile :: Curry_Prelude.OP_List C_Option -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_generateCompactFlatCurryFile x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_computeCompactFlatCurry x1 x2 x3500) (d_OP_generateCompactFlatCurryFile_dot___hash_lambda7 x3) x3500

d_OP_generateCompactFlatCurryFile_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_generateCompactFlatCurryFile_dot___hash_lambda7 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_FlatCurry.d_C_writeFCY x1 x2 x3500) (Curry_Prelude.d_C_done x3500) x3500

d_C_computeCompactFlatCurry :: Curry_Prelude.OP_List C_Option -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_computeCompactFlatCurry x1 x2 x3500 = let
     x3 = d_C_addImport2Options x1 x3500
      in (d_OP__case_134 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem C_Exports x3500) x3 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_isMainOption x3500) x3 x3500) x3500) x3500)

d_OP_computeCompactFlatCurry_dot___hash_lambda8 :: Curry_Prelude.OP_List C_Option -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_computeCompactFlatCurry_dot___hash_lambda8 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_makeCompactFlatCurry x2 x1 x3500) d_OP_computeCompactFlatCurry_dot___hash_lambda8_dot___hash_lambda9 x3500

d_OP_computeCompactFlatCurry_dot___hash_lambda8_dot___hash_lambda9 :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_computeCompactFlatCurry_dot___hash_lambda8_dot___hash_lambda9 x1 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_length (d_C_moduleFuns x1 x3500) x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_return x1 x3500) x3500

d_C_makeCompactFlatCurry :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_Option -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_makeCompactFlatCurry x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_requiredInCompactProg x1 x2 x3500) (d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2) x3500

d_OP_makeCompactFlatCurry_dot___hash_lambda10 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_Option -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple3 x4 x5 x6) -> let
          x7 = d_C_extendFuncTable (Curry_TableRBT.d_C_emptyTableRBT (acceptCs id d_C_leqQName) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_moduleFuns x3500) x6 x3500) x3500
          x8 = d_C_getRequiredFromOptions x2 x3500
          x9 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_getRequiredInModule x8) x3500) (Curry_Prelude.d_C_map d_C_moduleName x6 x3500) x3500
          x10 = Curry_Prelude.d_OP_plus_plus x4 x9 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getCalledFuncs x8 x5 x6 x7 (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_leqQName) x3500) x10 x3500) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_leqQName) x3500) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_leqQName) x3500) x10 x3500) (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1) x3500)
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1002 x3500) (d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_makeCompactFlatCurry_dot___hash_lambda10 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_Option -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple3 x4 x5 x6) -> let
          x2031 = x3000
           in (seq x2031 (let
               x2005 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2005 (seq x2032 (let
                    x2010 = leftSupply x2032
                    x2030 = rightSupply x2032
                     in (seq x2010 (seq x2030 (let
                         x7 = let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (nd_C_extendFuncTable (Curry_TableRBT.nd_C_emptyTableRBT (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2000 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_C_moduleFuns) x2001 x3500) x6 x2002 x3500)))) x2004 x3500))))))
                         x8 = d_C_getRequiredFromOptions x2 x3500
                         x9 = let
                              x2009 = leftSupply x2010
                              x2011 = rightSupply x2010
                               in (seq x2009 (seq x2011 (let
                                   x2007 = leftSupply x2011
                                   x2008 = rightSupply x2011
                                    in (seq x2007 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id (d_C_getRequiredInModule x8)) x2007 x3500) (Curry_Prelude.nd_C_map (wrapDX id d_C_moduleName) x6 x2008 x3500) x2009 x3500))))))
                         x10 = Curry_Prelude.d_OP_plus_plus x4 x9 x3500
                          in (let
                              x2029 = leftSupply x2030
                              x2026 = rightSupply x2030
                               in (seq x2029 (seq x2026 (Curry_Prelude.nd_OP_gt_gt_eq (let
                                   x2027 = leftSupply x2026
                                   x2028 = rightSupply x2026
                                    in (seq x2027 (seq x2028 (let
                                        x2025 = leftSupply x2027
                                        x2017 = rightSupply x2027
                                         in (seq x2025 (seq x2017 (let
                                             x2021 = leftSupply x2028
                                             x2024 = rightSupply x2028
                                              in (seq x2021 (seq x2024 (nd_C_getCalledFuncs x8 x5 x6 x7 (let
                                                  x2016 = leftSupply x2017
                                                  x2018 = rightSupply x2017
                                                   in (seq x2016 (seq x2018 (let
                                                       x2012 = leftSupply x2018
                                                       x2015 = rightSupply x2018
                                                        in (seq x2012 (seq x2015 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2012 x3500) (let
                                                            x2014 = leftSupply x2015
                                                            x2013 = rightSupply x2015
                                                             in (seq x2014 (seq x2013 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2013 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2014 x3500)))) x10 x2016 x3500))))))) (let
                                                  x2020 = leftSupply x2021
                                                  x2019 = rightSupply x2021
                                                   in (seq x2020 (seq x2019 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2019 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2020 x3500)))) (let
                                                  x2023 = leftSupply x2024
                                                  x2022 = rightSupply x2024
                                                   in (seq x2023 (seq x2022 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2022 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2023 x3500)))) x10 x2025 x3500)))))))))) (wrapNX id (nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1)) x2029 x3500))))))))))))
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1002 x3000 x3500) (nd_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_makeCompactFlatCurry_dot___hash_lambda10 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_foldr (acceptCs id Curry_Prelude.d_OP_plus) (Curry_Prelude.C_Int 0#) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_length d_C_moduleFuns x3500) x3 x3500) x3500) x3500) x3500) x3500) (let
          x7 = Curry_Prelude.d_C_map d_C_functionName x4 x3500
           in (Curry_Prelude.d_C_return (Curry_FlatCurry.C_Prog (d_C_moduleName x1 x3500) Curry_Prelude.OP_List (let
               x8 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_moduleTypes x3500) x3 x3500
               x9 = d_C_extendTConsWithConsType x5 x6 x8 x3500
               x10 = d_C_requiredDatatypes x9 x8 x3500
                in (Curry_Prelude.d_C_filter (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x10) x8 x3500)) x4 (Curry_Prelude.d_C_filter (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x7) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_moduleOps x3500) x3 x3500) x3500)) x3500)) x3500
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1002 x3500) (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> let
          x2022 = x3000
           in (seq x2022 (let
               x2004 = leftSupply x2022
               x2021 = rightSupply x2022
                in (seq x2004 (seq x2021 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus)) (Curry_Prelude.C_Int 0#) (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_map (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_length) (wrapDX id d_C_moduleFuns) x2000 x3500) x3 x2001 x3500)))) x2003 x3500)))) x3500) x3500) x3500) (let
                    x2005 = leftSupply x2021
                    x2020 = rightSupply x2021
                     in (seq x2005 (seq x2020 (let
                         x7 = Curry_Prelude.nd_C_map (wrapDX id d_C_functionName) x4 x2005 x3500
                          in (Curry_Prelude.d_C_return (let
                              x2012 = leftSupply x2020
                              x2019 = rightSupply x2020
                               in (seq x2012 (seq x2019 (Curry_FlatCurry.C_Prog (d_C_moduleName x1 x3500) Curry_Prelude.OP_List (let
                                   x2013 = leftSupply x2012
                                   x2014 = rightSupply x2012
                                    in (seq x2013 (seq x2014 (let
                                        x2008 = leftSupply x2013
                                        x2009 = rightSupply x2013
                                         in (seq x2008 (seq x2009 (let
                                             x2010 = leftSupply x2014
                                             x2011 = rightSupply x2014
                                              in (seq x2010 (seq x2011 (let
                                                  x8 = let
                                                       x2007 = leftSupply x2008
                                                       x2006 = rightSupply x2008
                                                        in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_C_moduleTypes) x2006 x3500) x3 x2007 x3500)))
                                                  x9 = nd_C_extendTConsWithConsType x5 x6 x8 x2009 x3500
                                                  x10 = nd_C_requiredDatatypes x9 x8 x2010 x3500
                                                   in (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x10)) x8 x2011 x3500))))))))))) x4 (let
                                   x2018 = leftSupply x2019
                                   x2017 = rightSupply x2019
                                    in (seq x2018 (seq x2017 (Curry_Prelude.nd_C_filter (wrapDX id (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x7)) (let
                                        x2016 = leftSupply x2017
                                        x2015 = rightSupply x2017
                                         in (seq x2016 (seq x2015 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_C_moduleOps) x2015 x3500) x3 x2016 x3500)))) x2018 x3500)))))))) x3500))))) x3500)))))
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1002 x3000 x3500) (nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT (d_C_tconsName x2 x3500) x3500) x1 x3500

nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT (d_C_tconsName x2 x3500) x2000 x3500) x1 x2001 x3500)))))

d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_OpDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_Op x3 x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x3 x3500) x1 x3500
     (Curry_FlatCurry.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x1 x1002 x3500) (d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompactFlatCurry_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda13 x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_requiredDatatypes :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_requiredDatatypes x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_newTypeConsOfTDecl x1) x3500) x2 x3500
      in (d_OP__case_133 x1 x2 x3 (Curry_Prelude.d_C_null x3 x3500) x3500)

nd_C_requiredDatatypes :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_requiredDatatypes x1 x2 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x3 = let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_C_newTypeConsOfTDecl x1)) x2000 x3500) x2 x2001 x3500)))
                in (nd_OP__case_133 x1 x2 x3 (Curry_Prelude.d_C_null x3 x3500) x2003 x3500))))))

d_C_newTypeConsOfTDecl :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_newTypeConsOfTDecl x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_TypeSyn x3 x4 x5 x6) -> d_OP__case_132 x1 x3 x6 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x3 x3500) x1 x3500) x3500
     (Curry_FlatCurry.C_Type x7 x8 x9 x10) -> d_OP__case_131 x1 x7 x10 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x7 x3500) x1 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_newTypeConsOfTDecl x1 x1002 x3500) (d_C_newTypeConsOfTDecl x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_newTypeConsOfTDecl x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_newTypeConsOfTDecl x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_newTypeConsOfTDecl :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_newTypeConsOfTDecl x1 x2 x3000 x3500 = case x2 of
     (Curry_FlatCurry.C_TypeSyn x3 x4 x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_132 x1 x3 x6 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x3 x2000 x3500) x1 x2001 x3500)))) x2003 x3500)))))
     (Curry_FlatCurry.C_Type x7 x8 x9 x10) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_131 x1 x7 x10 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x7 x2000 x3500) x1 x2001 x3500)))) x2003 x3500)))))
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_newTypeConsOfTDecl x1 x1002 x3000 x3500) (nd_C_newTypeConsOfTDecl x1 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_newTypeConsOfTDecl x1 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_newTypeConsOfTDecl x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_newTypeConsOfTDecl_dot___hash_lambda14 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_OP_newTypeConsOfTDecl_dot___hash_lambda14 x1 x2 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3500) x1 x3500) x3500

nd_OP_newTypeConsOfTDecl_dot___hash_lambda14 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_newTypeConsOfTDecl_dot___hash_lambda14 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3500) x1 x2001 x3500)))) x3500))

d_OP_newTypeConsOfTDecl_dot___hash_lambda15 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_OP_newTypeConsOfTDecl_dot___hash_lambda15 x1 x2 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3500) x1 x3500) x3500

nd_OP_newTypeConsOfTDecl_dot___hash_lambda15 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_newTypeConsOfTDecl_dot___hash_lambda15 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3500) x1 x2001 x3500)))) x3500))

d_OP_newTypeConsOfTDecl_dot___hash_lambda16 :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_newTypeConsOfTDecl_dot___hash_lambda16 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_allTypesOfTExpr x3500) x5 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_newTypeConsOfTDecl_dot___hash_lambda16 x1002 x3500) (d_OP_newTypeConsOfTDecl_dot___hash_lambda16 x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_newTypeConsOfTDecl_dot___hash_lambda16 z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_newTypeConsOfTDecl_dot___hash_lambda16 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_extendTConsWithConsType :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_extendTConsWithConsType x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_130 x1 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_extendTConsWithConsType x1 x2 x1002 x3500) (d_C_extendTConsWithConsType x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_extendTConsWithConsType x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_extendTConsWithConsType x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_extendTConsWithConsType :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_extendTConsWithConsType x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_130 x1 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_extendTConsWithConsType x1 x2 x1002 x3000 x3500) (nd_C_extendTConsWithConsType x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_extendTConsWithConsType x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_extendTConsWithConsType x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_extendTConsWithConsType_dot___hash_lambda17 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_extendTConsWithConsType_dot___hash_lambda17 x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT (d_C_consName x2 x3500) x3500) x1 x3500

nd_OP_extendTConsWithConsType_dot___hash_lambda17 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_ConsDecl -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_extendTConsWithConsType_dot___hash_lambda17 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT (d_C_consName x2 x3500) x2000 x3500) x1 x2001 x3500)))))

d_C_extendFuncTable :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl)
d_C_extendFuncTable x1 x2 x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_extendFuncTable_dot___hash_lambda18) x1 x2 x3500

nd_C_extendFuncTable :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl)
nd_C_extendFuncTable x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_extendFuncTable_dot___hash_lambda18)) x1 x2 x2000 x3500))

d_OP_extendFuncTable_dot___hash_lambda18 :: Curry_FlatCurry.C_FuncDecl -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl)
d_OP_extendFuncTable_dot___hash_lambda18 x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_TableRBT.d_C_updateRBT (d_C_functionName x1 x3500) x1 x3500) x2 x3500

nd_OP_extendFuncTable_dot___hash_lambda18 :: Curry_FlatCurry.C_FuncDecl -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl)
nd_OP_extendFuncTable_dot___hash_lambda18 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_TableRBT.nd_C_updateRBT (d_C_functionName x1 x3500) x1 x2000 x3500) x2 x2001 x3500)))))

d_C_requiredInCompactProg :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_Option -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_C_requiredInCompactProg x1 x2 x3500 = let
     x3 = Curry_List.d_C_nub (Curry_Prelude.d_C_foldr (acceptCs id d_OP_requiredInCompactProg_dot___hash_lambda21) Curry_Prelude.OP_List x2 x3500) x3500
     x4 = d_C_moduleName x1 x3500
     x5 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_requiredInCompactProg_dot___hash_lambda25) Curry_Prelude.OP_List x2 x3500
     x6 = d_C_exportedFuncNames (d_C_moduleFuns x1 x3500) x3500
     x7 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x4 x3500) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (Curry_Sort.d_C_leqString x3500) x3500) x3500
      in (d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x5 x3500) x3500) x3500)

nd_C_requiredInCompactProg :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_Option -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
nd_C_requiredInCompactProg x1 x2 x3000 x3500 = let
     x2014 = x3000
      in (seq x2014 (let
          x2015 = leftSupply x2014
          x2016 = rightSupply x2014
           in (seq x2015 (seq x2016 (let
               x2000 = leftSupply x2015
               x2001 = rightSupply x2015
                in (seq x2000 (seq x2001 (let
                    x2011 = leftSupply x2016
                    x2013 = rightSupply x2016
                     in (seq x2011 (seq x2013 (let
                         x3 = Curry_List.d_C_nub (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_requiredInCompactProg_dot___hash_lambda21)) Curry_Prelude.OP_List x2 x2000 x3500) x3500
                         x4 = d_C_moduleName x1 x3500
                         x5 = Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_requiredInCompactProg_dot___hash_lambda25)) Curry_Prelude.OP_List x2 x2001 x3500
                         x6 = d_C_exportedFuncNames (d_C_moduleFuns x1 x3500) x3500
                         x7 = let
                              x2010 = leftSupply x2011
                              x2012 = rightSupply x2011
                               in (seq x2010 (seq x2012 (let
                                   x2004 = leftSupply x2012
                                   x2008 = rightSupply x2012
                                    in (seq x2004 (seq x2008 (Curry_Prelude.nd_C_apply (let
                                        x2003 = leftSupply x2004
                                        x2002 = rightSupply x2004
                                         in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2002 x3500) x4 x2003 x3500)))) (let
                                        x2007 = leftSupply x2008
                                        x2009 = rightSupply x2008
                                         in (seq x2007 (seq x2009 (let
                                             x2005 = leftSupply x2009
                                             x2006 = rightSupply x2009
                                              in (seq x2005 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2005 x3500) (Curry_Sort.nd_C_leqString x2006 x3500) x2007 x3500))))))) x2010 x3500))))))
                          in (nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x5 x3500) x3500) x2013 x3500))))))))))))

d_OP_requiredInCompactProg_dot___hash_lambda21 :: C_Option -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_requiredInCompactProg_dot___hash_lambda21 x1 x2 x3500 = case x1 of
     (C_Import x3) -> Curry_Prelude.OP_Cons x3 x2
     C_Verbose -> x2
     (C_Main x4) -> x2
     C_Exports -> x2
     (C_InitFuncs x5) -> x2
     (C_Required x6) -> x2
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_requiredInCompactProg_dot___hash_lambda21 x1002 x2 x3500) (d_OP_requiredInCompactProg_dot___hash_lambda21 x1003 x2 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_requiredInCompactProg_dot___hash_lambda21 z x2 x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_requiredInCompactProg_dot___hash_lambda21 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_requiredInCompactProg_dot___hash_lambda25 :: C_Option -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_requiredInCompactProg_dot___hash_lambda25 x1 x2 x3500 = case x1 of
     (C_InitFuncs x3) -> Curry_Prelude.OP_Cons x3 x2
     C_Verbose -> x2
     (C_Main x4) -> x2
     C_Exports -> x2
     (C_Required x5) -> x2
     (C_Import x6) -> x2
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_requiredInCompactProg_dot___hash_lambda25 x1002 x2 x3500) (d_OP_requiredInCompactProg_dot___hash_lambda25 x1003 x2 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_requiredInCompactProg_dot___hash_lambda25 z x2 x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_requiredInCompactProg_dot___hash_lambda25 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x1 x2 x3500 = Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x1 x2 x3500

nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x1 x2 x2001 x3500)))))

d_OP_requiredInCompactProg_dot___hash_lambda27 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_requiredInCompactProg_dot___hash_lambda27 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_C_concat x2 x3500) (d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x4 x1 x3500) (Curry_Prelude.OP_Cons x3 x5)) x3500

nd_OP_requiredInCompactProg_dot___hash_lambda27 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
nd_OP_requiredInCompactProg_dot___hash_lambda27 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_C_concat x2 x3500) (nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x4 x1 x2000 x3500) (Curry_Prelude.OP_Cons x3 x5)) x3500))

d_OP_requiredInCompactProg_dot___hash_lambda28 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_requiredInCompactProg_dot___hash_lambda28 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_List.d_C_nub x2 x3500) (d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x4 x1 x3500) (Curry_Prelude.OP_Cons x3 x5)) x3500

nd_OP_requiredInCompactProg_dot___hash_lambda28 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
nd_OP_requiredInCompactProg_dot___hash_lambda28 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_List.d_C_nub x2 x3500) (nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x4 x1 x2000 x3500) (Curry_Prelude.OP_Cons x3 x5)) x3500))

d_OP_requiredInCompactProg_dot___hash_lambda29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_requiredInCompactProg_dot___hash_lambda29 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x1) Curry_Prelude.OP_List) (d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x5 x2 x3500) (Curry_Prelude.OP_Cons x3 x6)) x3500

nd_OP_requiredInCompactProg_dot___hash_lambda29 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
nd_OP_requiredInCompactProg_dot___hash_lambda29 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x1) Curry_Prelude.OP_List) (nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x5 x2 x2000 x3500) (Curry_Prelude.OP_Cons x3 x6)) x3500))

d_OP_requiredInCompactProg_dot___hash_lambda30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_requiredInCompactProg_dot___hash_lambda30 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_List.d_C_nub (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot d_C_exportedFuncNames d_C_moduleFuns x3500) x3500) x4 x3500) x3500) x3500) (d_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x3 (Curry_Prelude.d_C_map d_C_moduleName x4 x3500) x3500) (Curry_Prelude.OP_Cons x2 x4)) x3500

nd_OP_requiredInCompactProg_dot___hash_lambda30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
nd_OP_requiredInCompactProg_dot___hash_lambda30 x1 x2 x3 x4 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (Curry_Prelude.d_C_return (let
          x2004 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2004 (seq x2007 (Curry_Prelude.OP_Tuple3 (Curry_List.d_C_nub (Curry_Prelude.d_OP_plus_plus x1 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_concatMap (Curry_Prelude.nd_OP_dot (wrapDX id d_C_exportedFuncNames) (wrapDX id d_C_moduleFuns) x2000 x3500) x2001 x3500)))) x4 x2003 x3500)))) x3500) x3500) (let
               x2006 = leftSupply x2007
               x2005 = rightSupply x2007
                in (seq x2006 (seq x2005 (nd_OP_requiredInCompactProg_dot_add2mainmodset_dot_117 x3 (Curry_Prelude.nd_C_map (wrapDX id d_C_moduleName) x4 x2005 x3500) x2006 x3500)))) (Curry_Prelude.OP_Cons x2 x4))))) x3500))

d_C_exportedFuncNames :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_exportedFuncNames x1 x3500 = Curry_Prelude.d_C_map d_OP_exportedFuncNames_dot___hash_lambda31 (Curry_Prelude.d_C_filter d_OP_exportedFuncNames_dot___hash_lambda32 x1 x3500) x3500

d_OP_exportedFuncNames_dot___hash_lambda31 :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_exportedFuncNames_dot___hash_lambda31 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> x2
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_exportedFuncNames_dot___hash_lambda31 x1002 x3500) (d_OP_exportedFuncNames_dot___hash_lambda31 x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_exportedFuncNames_dot___hash_lambda31 z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_exportedFuncNames_dot___hash_lambda31 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_exportedFuncNames_dot___hash_lambda32 :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_OP_exportedFuncNames_dot___hash_lambda32 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_exportedFuncNames_dot___hash_lambda32 x1002 x3500) (d_OP_exportedFuncNames_dot___hash_lambda32 x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_exportedFuncNames_dot___hash_lambda32 z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_exportedFuncNames_dot___hash_lambda32 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getCalledFuncs :: Curry_Prelude.OP_List C_RequiredSpec -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple4 x3 Curry_Prelude.OP_List x6 x7) x3500
     (Curry_Prelude.OP_Cons x9 x10) -> d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x9 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_getCalledFuncs :: Curry_Prelude.OP_List C_RequiredSpec -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple4 x3 Curry_Prelude.OP_List x6 x7) x3500
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x9 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getCalledFuncs_dot___hash_lambda33 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_RequiredSpec -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getCalledFuncs_dot___hash_lambda33 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x3500 = let
     x12 = d_C_getRequiredInModule x10 x8 x3500
      in (d_C_getCalledFuncs x10 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x8 x3500) x6 x3500) (Curry_Prelude.OP_Cons x11 x9) (d_C_extendFuncTable x3 (d_C_moduleFuns x11 x3500) x3500) (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x5 x12 x3500) x4 x7 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x1) (Curry_Prelude.d_OP_plus_plus x2 x12 x3500)) x3500)

nd_OP_getCalledFuncs_dot___hash_lambda33 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_FlatCurry.C_FuncDecl) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List C_RequiredSpec -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_OP_getCalledFuncs_dot___hash_lambda33 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x3000 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x12 = d_C_getRequiredInModule x10 x8 x3500
           in (let
               x2011 = leftSupply x2010
               x2012 = rightSupply x2010
                in (seq x2011 (seq x2012 (let
                    x2009 = leftSupply x2011
                    x2004 = rightSupply x2011
                     in (seq x2009 (seq x2004 (let
                         x2005 = leftSupply x2012
                         x2008 = rightSupply x2012
                          in (seq x2005 (seq x2008 (nd_C_getCalledFuncs x10 (let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x8 x2001 x3500)))) x6 x2003 x3500)))) (Curry_Prelude.OP_Cons x11 x9) (nd_C_extendFuncTable x3 (d_C_moduleFuns x11 x3500) x2005 x3500) (let
                              x2007 = leftSupply x2008
                              x2006 = rightSupply x2008
                               in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2006 x3500) x5 x12 x2007 x3500)))) x4 x7 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x1) (Curry_Prelude.d_OP_plus_plus x2 x12 x3500)) x2009 x3500))))))))))))

d_OP_getCalledFuncs_dot___hash_lambda34 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getCalledFuncs_dot___hash_lambda34 x1 x2 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3500) x1 x3500) x3500

nd_OP_getCalledFuncs_dot___hash_lambda34 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_getCalledFuncs_dot___hash_lambda34 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3500) x1 x2001 x3500)))) x3500))

d_OP_getCalledFuncs_dot___hash_lambda35 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getCalledFuncs_dot___hash_lambda35 x1 x2 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3500) x1 x3500) x3500

nd_OP_getCalledFuncs_dot___hash_lambda35 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_getCalledFuncs_dot___hash_lambda35 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3500) x1 x2001 x3500)))) x3500))

d_OP_getCalledFuncs_dot___hash_lambda36 :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getCalledFuncs_dot___hash_lambda36 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple4 x3 (Curry_Prelude.OP_Cons x1 x4) x5 x6) x3500
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getCalledFuncs_dot___hash_lambda36 x1 x1002 x3500) (d_OP_getCalledFuncs_dot___hash_lambda36 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getCalledFuncs_dot___hash_lambda36 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getCalledFuncs_dot___hash_lambda36 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_getCalledFuncs_dot___hash_lambda36 :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_OP_getCalledFuncs_dot___hash_lambda36 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple4 x3 (Curry_Prelude.OP_Cons x1 x4) x5 x6) x3500
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_getCalledFuncs_dot___hash_lambda36 x1 x1002 x3000 x3500) (nd_OP_getCalledFuncs_dot___hash_lambda36 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_getCalledFuncs_dot___hash_lambda36 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_getCalledFuncs_dot___hash_lambda36 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allFuncCalls :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allFuncCalls x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_119 x6 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allFuncCalls x1002 x3500) (d_C_allFuncCalls x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allFuncCalls z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allFuncCalls x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allFuncCallsOfExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allFuncCallsOfExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> let
          x7 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_allFuncCallsOfExpr x3500) x6 x3500
           in (d_OP__case_118 x5 x7 x4 x3500)
     (Curry_FlatCurry.C_Free x10 x11) -> d_C_allFuncCallsOfExpr x11 x3500
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_OP_dot d_C_allFuncCallsOfExpr Curry_Prelude.d_C_snd x3500) x3500) x12 x3500) (d_C_allFuncCallsOfExpr x13 x3500) x3500
     (Curry_FlatCurry.C_Or x14 x15) -> Curry_Prelude.d_OP_plus_plus (d_C_allFuncCallsOfExpr x14 x3500) (d_C_allFuncCallsOfExpr x15 x3500) x3500
     (Curry_FlatCurry.C_Case x16 x17 x18) -> Curry_Prelude.d_OP_plus_plus (d_C_allFuncCallsOfExpr x17 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_allFuncCallsOfBranchExpr x3500) x18 x3500) x3500
     (Curry_FlatCurry.C_Typed x19 x20) -> d_C_allFuncCallsOfExpr x19 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allFuncCallsOfExpr x1002 x3500) (d_C_allFuncCallsOfExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allFuncCallsOfExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allFuncCallsOfExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allFuncCallsOfBranchExpr :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allFuncCallsOfBranchExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_allFuncCallsOfExpr x3 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allFuncCallsOfBranchExpr x1002 x3500) (d_C_allFuncCallsOfBranchExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allFuncCallsOfBranchExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allFuncCallsOfBranchExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allConstructorsOfFunc :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allConstructorsOfFunc x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_117 x6 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allConstructorsOfFunc x1002 x3500) (d_C_allConstructorsOfFunc x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allConstructorsOfFunc z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allConstructorsOfFunc x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allConsOfExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allConsOfExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> let
          x7 = Curry_Prelude.d_C_apply (d_C_unionMap d_C_allConsOfExpr x3500) x6 x3500
           in (d_OP__case_116 x5 x7 x4 x3500)
     (Curry_FlatCurry.C_Free x10 x11) -> d_C_allConsOfExpr x11 x3500
     (Curry_FlatCurry.C_Let x12 x13) -> Curry_List.d_C_union (Curry_Prelude.d_C_apply (d_C_unionMap (Curry_Prelude.d_OP_dot d_C_allConsOfExpr Curry_Prelude.d_C_snd x3500) x3500) x12 x3500) (d_C_allConsOfExpr x13 x3500) x3500
     (Curry_FlatCurry.C_Or x14 x15) -> Curry_List.d_C_union (d_C_allConsOfExpr x14 x3500) (d_C_allConsOfExpr x15 x3500) x3500
     (Curry_FlatCurry.C_Case x16 x17 x18) -> Curry_List.d_C_union (d_C_allConsOfExpr x17 x3500) (Curry_Prelude.d_C_apply (d_C_unionMap d_OP_allConsOfExpr_dot_consOfBranch_dot_254 x3500) x18 x3500) x3500
     (Curry_FlatCurry.C_Typed x19 x20) -> d_C_allConsOfExpr x19 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allConsOfExpr x1002 x3500) (d_C_allConsOfExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allConsOfExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allConsOfExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_allConsOfExpr_dot_consOfBranch_dot_254 :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_allConsOfExpr_dot_consOfBranch_dot_254 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_OP__case_115 x3 x2 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_allConsOfExpr_dot_consOfBranch_dot_254 x1002 x3500) (d_OP_allConsOfExpr_dot_consOfBranch_dot_254 x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_allConsOfExpr_dot_consOfBranch_dot_254 z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_allConsOfExpr_dot_consOfBranch_dot_254 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allTypesOfFunc :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allTypesOfFunc x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_C_allTypesOfTExpr x5 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allTypesOfFunc x1002 x3500) (d_C_allTypesOfFunc x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allTypesOfFunc z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allTypesOfFunc x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_allTypesOfTExpr :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_allTypesOfTExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_List.d_C_union (d_C_allTypesOfTExpr x3 x3500) (d_C_allTypesOfTExpr x4 x3500) x3500
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_List.d_C_union (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (d_C_unionMap d_C_allTypesOfTExpr x3500) x6 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_allTypesOfTExpr x1002 x3500) (d_C_allTypesOfTExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_allTypesOfTExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_allTypesOfTExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unionMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.OP_List t1) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_unionMap x1 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map x1) x3500

nd_C_unionMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_List t1) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t1)
nd_C_unionMap x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id Curry_List.d_C_union)) Curry_Prelude.OP_List)) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2000 x3500))

d_C_functionName :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_functionName x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> x2
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_functionName x1002 x3500) (d_C_functionName x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_functionName z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_functionName x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_consName :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_consName x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> x2
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_consName x1002 x3500) (d_C_consName x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_consName z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_consName x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tconsName :: Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_tconsName x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> x2
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> x6
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tconsName x1002 x3500) (d_C_tconsName x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tconsName z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tconsName x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_moduleImports :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_moduleImports x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x3
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleImports x1002 x3500) (d_C_moduleImports x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleImports z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleImports x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_moduleTypes :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_C_moduleTypes x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x4
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleTypes x1002 x3500) (d_C_moduleTypes x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleTypes z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleTypes x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_moduleOps :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl
d_C_moduleOps x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x6
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleOps x1002 x3500) (d_C_moduleOps x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleOps z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleOps x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_moduleName :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_moduleName x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x2
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleName x1002 x3500) (d_C_moduleName x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleName z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleName x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_moduleFuns :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_C_moduleFuns x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x5
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_moduleFuns x1002 x3500) (d_C_moduleFuns x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_moduleFuns z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_moduleFuns x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_leqQName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqQName x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_114 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqQName x1002 x2 x3500) (d_C_leqQName x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqQName z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqQName x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_readCurrentFlatCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readCurrentFlatCurry x1 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))) x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x1 x3500) d_OP_readCurrentFlatCurry_dot___hash_lambda39 x3500) x3500

d_OP_readCurrentFlatCurry_dot___hash_lambda39 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readCurrentFlatCurry_dot___hash_lambda39 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_FlatCurry.d_C_flatCurryFileName x1 x3500) x3500) (d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40 x1) x3500

d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40 x1 x2 x3500 = d_OP__case_113 x1 x2 (Curry_Prelude.d_C_not x2 x3500) x3500

d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime (Curry_FlatCurry.d_C_flatCurryFileName x1 x3500) x3500) (d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41_dot___hash_lambda42 x2 x1) x3500

d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41_dot___hash_lambda42 :: Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41_dot___hash_lambda42 x1 x2 x3 x3500 = d_OP__case_112 x1 x2 x3 (Curry_Prelude.d_OP_gt x1 x3 x3500) x3500

d_C_getSourceModificationTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Time.C_ClockTime
d_C_getSourceModificationTime x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3500) x3500) (d_OP_getSourceModificationTime_dot___hash_lambda43 x1) x3500

d_OP_getSourceModificationTime_dot___hash_lambda43 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Time.C_ClockTime
d_OP_getSourceModificationTime_dot___hash_lambda43 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Directory.d_C_getModificationTime (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3500) x3500
     Curry_Prelude.C_False -> Curry_Directory.d_C_getModificationTime (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getSourceModificationTime_dot___hash_lambda43 x1 x1002 x3500) (d_OP_getSourceModificationTime_dot___hash_lambda43 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getSourceModificationTime_dot___hash_lambda43 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getSourceModificationTime_dot___hash_lambda43 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_findSourceFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_findSourceFileInLoadPath x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile x1 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda44 x1) x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda44 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda44 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath (Curry_FileGoodies.d_C_baseName x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x2 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda44_dot___hash_lambda45 x1) x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda44_dot___hash_lambda45 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda44_dot___hash_lambda45 x1 x2 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500) x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_FileGoodies.d_C_stripSuffix x3500) x3500) x2 x3500

d_C_processPrimitives :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_processPrimitives x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readPrimSpec (d_C_moduleName x2 x3500) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List))))))))) x3500) x3500) (d_OP_processPrimitives_dot___hash_lambda46 x2) x3500

d_OP_processPrimitives_dot___hash_lambda46 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP_processPrimitives_dot___hash_lambda46 x1 x2 x3500 = Curry_Prelude.d_C_return (d_C_mergePrimSpecIntoModule x2 x1 x3500) x3500

d_C_mergePrimSpecIntoModule :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_mergePrimSpecIntoModule x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_Prog x3 x4 x5 x6 x7) -> Curry_FlatCurry.C_Prog x3 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_mergePrimSpecIntoFunc x1) x3500) x6 x3500) x7
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mergePrimSpecIntoModule x1 x1002 x3500) (d_C_mergePrimSpecIntoModule x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mergePrimSpecIntoModule x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mergePrimSpecIntoModule x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mergePrimSpecIntoFunc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_C_mergePrimSpecIntoFunc x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> let
          x8 = Curry_Prelude.d_C_lookup x3 x1 x3500
           in (d_OP__case_111 x3 x4 x5 x6 x7 x8 (Curry_Prelude.d_OP_eq_eq x8 Curry_Prelude.C_Nothing x3500) x3500)
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mergePrimSpecIntoFunc x1 x1002 x3500) (d_C_mergePrimSpecIntoFunc x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mergePrimSpecIntoFunc x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mergePrimSpecIntoFunc x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> d_OP__case_109 x2 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x1002 x3500) (d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> d_OP__case_108 x2 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x1002 x3500) (d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_readPrimSpec :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_readPrimSpec x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x2 x3500) (d_OP_readPrimSpec_dot___hash_lambda47 x1 x2) x3500

d_OP_readPrimSpec_dot___hash_lambda47 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_readPrimSpec_dot___hash_lambda47 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_XML.d_C_readXmlFile x2 x3500) (d_OP_readPrimSpec_dot___hash_lambda47_dot___hash_lambda48 x1) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readPrimSpec_dot___hash_lambda47 x1 x2 x1002 x3500) (d_OP_readPrimSpec_dot___hash_lambda47 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readPrimSpec_dot___hash_lambda47 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readPrimSpec_dot___hash_lambda47 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readPrimSpec_dot___hash_lambda47_dot___hash_lambda48 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_XML.C_XmlExp -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_readPrimSpec_dot___hash_lambda47_dot___hash_lambda48 x1 x2 x3500 = Curry_Prelude.d_C_return (d_C_xml2primtrans x1 x2 x3500) x3500

d_C_xml2primtrans :: Curry_Prelude.Curry t0 => t0 -> Curry_XML.C_XmlExp -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_xml2primtrans x1 x2 x3500 = case x2 of
     (Curry_XML.C_XElem x3 x4 x5) -> d_OP__case_107 x1 x4 x5 x3 x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_xml2primtrans x1 x1002 x3500) (d_C_xml2primtrans x1 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_xml2primtrans x1 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_xml2primtrans x1 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_xml2primtrans_dot_xml2prim_dot_363 :: Curry_Prelude.Curry t247 => t247 -> Curry_XML.C_XmlExp -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 t247 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_xml2primtrans_dot_xml2prim_dot_363 x1 x2 x3500 = case x2 of
     (Curry_XML.C_XElem x3 x4 x5) -> d_OP__case_85 x1 x4 x5 x3 x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_xml2primtrans_dot_xml2prim_dot_363 x1 x1002 x3500) (d_OP_xml2primtrans_dot_xml2prim_dot_363 x1 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_xml2primtrans_dot_xml2prim_dot_363 x1 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_xml2primtrans_dot_xml2prim_dot_363 x1 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_85 x1 x4 x5 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_84 x1 x4 x5 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x1 x4 x5 x1002 x3500) (d_OP__case_85 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_85 x1 x4 x5 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_84 x1 x4 x5 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_85 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_85 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_85 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_85 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_84 x1 x4 x5 x7 x6 x3500 = case x6 of
     (Curry_Prelude.C_Char 'p'#) -> d_OP__case_83 x1 x4 x5 x7 x3500
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_22 x1 x4 x5 x7 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',d_OP__case_83 x1 x4 x5 x7 x3500),('i',d_OP__case_22 x1 x4 x5 x7 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x1 x4 x5 x7 x1002 x3500) (d_OP__case_84 x1 x4 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x1 x4 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x1 x4 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_84 x1 x4 x5 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.C_Char 'p'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_83 x1 x4 x5 x7 x2000 x3500))
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x4 x5 x7 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_83 x1 x4 x5 x7 x2000 x3500))),('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x4 x5 x7 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_84 x1 x4 x5 x7 x1002 x3000 x3500) (nd_OP__case_84 x1 x4 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_84 x1 x4 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_84 x1 x4 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x4 x5 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x70 x71) -> d_OP__case_21 x1 x4 x5 x71 x70 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x4 x5 x1002 x3500) (d_OP__case_22 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x4 x5 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x70 x71) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x1 x4 x5 x71 x70 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_22 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x4 x5 x71 x70 x3500 = case x70 of
     (Curry_Prelude.C_Char 'g'#) -> d_OP__case_20 x1 x4 x5 x71 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('g',d_OP__case_20 x1 x4 x5 x71 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x4 x5 x71 x1002 x3500) (d_OP__case_21 x1 x4 x5 x71 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x4 x5 x71 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x4 x5 x71 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x4 x5 x71 x70 x3000 x3500 = case x70 of
     (Curry_Prelude.C_Char 'g'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x1 x4 x5 x71 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('g',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x1 x4 x5 x71 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x4 x5 x71 x1002 x3000 x3500) (nd_OP__case_21 x1 x4 x5 x71 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x4 x5 x71 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x4 x5 x71 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x4 x5 x71 x3500 = case x71 of
     (Curry_Prelude.OP_Cons x72 x73) -> d_OP__case_19 x1 x4 x5 x73 x72 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x4 x5 x1002 x3500) (d_OP__case_20 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x4 x5 x71 x3000 x3500 = case x71 of
     (Curry_Prelude.OP_Cons x72 x73) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x1 x4 x5 x73 x72 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_20 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x4 x5 x73 x72 x3500 = case x72 of
     (Curry_Prelude.C_Char 'n'#) -> d_OP__case_18 x1 x4 x5 x73 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',d_OP__case_18 x1 x4 x5 x73 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x4 x5 x73 x1002 x3500) (d_OP__case_19 x1 x4 x5 x73 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x4 x5 x73 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x4 x5 x73 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x4 x5 x73 x72 x3000 x3500 = case x72 of
     (Curry_Prelude.C_Char 'n'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x4 x5 x73 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x4 x5 x73 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x4 x5 x73 x1002 x3000 x3500) (nd_OP__case_19 x1 x4 x5 x73 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x4 x5 x73 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x4 x5 x73 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x4 x5 x73 x3500 = case x73 of
     (Curry_Prelude.OP_Cons x74 x75) -> d_OP__case_17 x1 x4 x5 x75 x74 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x4 x5 x1002 x3500) (d_OP__case_18 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x4 x5 x73 x3000 x3500 = case x73 of
     (Curry_Prelude.OP_Cons x74 x75) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x1 x4 x5 x75 x74 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_18 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x4 x5 x75 x74 x3500 = case x74 of
     (Curry_Prelude.C_Char 'o'#) -> d_OP__case_16 x1 x4 x5 x75 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('o',d_OP__case_16 x1 x4 x5 x75 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x4 x5 x75 x1002 x3500) (d_OP__case_17 x1 x4 x5 x75 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x4 x5 x75 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x4 x5 x75 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x4 x5 x75 x74 x3000 x3500 = case x74 of
     (Curry_Prelude.C_Char 'o'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 x4 x5 x75 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('o',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 x4 x5 x75 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x4 x5 x75 x1002 x3000 x3500) (nd_OP__case_17 x1 x4 x5 x75 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x4 x5 x75 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x4 x5 x75 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x4 x5 x75 x3500 = case x75 of
     (Curry_Prelude.OP_Cons x76 x77) -> d_OP__case_15 x1 x4 x5 x77 x76 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x4 x5 x1002 x3500) (d_OP__case_16 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x4 x5 x75 x3000 x3500 = case x75 of
     (Curry_Prelude.OP_Cons x76 x77) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x1 x4 x5 x77 x76 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_16 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x4 x5 x77 x76 x3500 = case x76 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_14 x1 x4 x5 x77 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_14 x1 x4 x5 x77 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x4 x5 x77 x1002 x3500) (d_OP__case_15 x1 x4 x5 x77 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x4 x5 x77 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x4 x5 x77 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x4 x5 x77 x76 x3000 x3500 = case x76 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x4 x5 x77 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x4 x5 x77 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x4 x5 x77 x1002 x3000 x3500) (nd_OP__case_15 x1 x4 x5 x77 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x4 x5 x77 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x4 x5 x77 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x4 x5 x77 x3500 = case x77 of
     (Curry_Prelude.OP_Cons x78 x79) -> d_OP__case_13 x1 x4 x5 x79 x78 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x4 x5 x1002 x3500) (d_OP__case_14 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x4 x5 x77 x3000 x3500 = case x77 of
     (Curry_Prelude.OP_Cons x78 x79) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x1 x4 x5 x79 x78 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_14 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x4 x5 x79 x78 x3500 = case x78 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_12 x1 x4 x5 x79 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_12 x1 x4 x5 x79 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x4 x5 x79 x1002 x3500) (d_OP__case_13 x1 x4 x5 x79 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x4 x5 x79 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x4 x5 x79 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x4 x5 x79 x78 x3000 x3500 = case x78 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x4 x5 x79 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x4 x5 x79 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x4 x5 x79 x1002 x3000 x3500) (nd_OP__case_13 x1 x4 x5 x79 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x4 x5 x79 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x4 x5 x79 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x4 x5 x79 x3500 = case x79 of
     Curry_Prelude.OP_List -> d_OP__case_11 x1 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x4 x5 x1002 x3500) (d_OP__case_12 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x4 x5 x79 x3000 x3500 = case x79 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x1 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_12 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x80 x81) -> d_OP__case_10 x1 x5 x80 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x5 x1002 x3500) (d_OP__case_11 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x80 x81) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x5 x80 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x5 x1002 x3000 x3500) (nd_OP__case_11 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x5 x80 x3500 = case x80 of
     (Curry_Prelude.OP_Tuple2 x82 x83) -> d_OP__case_9 x1 x5 x83 x82 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x5 x1002 x3500) (d_OP__case_10 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x5 x80 x3000 x3500 = case x80 of
     (Curry_Prelude.OP_Tuple2 x82 x83) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x5 x83 x82 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x5 x1002 x3000 x3500) (nd_OP__case_10 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x5 x83 x82 x3500 = case x82 of
     (Curry_Prelude.OP_Cons x84 x85) -> d_OP__case_8 x1 x5 x83 x85 x84 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x5 x83 x1002 x3500) (d_OP__case_9 x1 x5 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x5 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x5 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x5 x83 x82 x3000 x3500 = case x82 of
     (Curry_Prelude.OP_Cons x84 x85) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x5 x83 x85 x84 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x5 x83 x1002 x3000 x3500) (nd_OP__case_9 x1 x5 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x5 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x5 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x5 x83 x85 x84 x3500 = case x84 of
     (Curry_Prelude.C_Char 'n'#) -> d_OP__case_7 x1 x5 x83 x85 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',d_OP__case_7 x1 x5 x83 x85 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x5 x83 x85 x1002 x3500) (d_OP__case_8 x1 x5 x83 x85 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x5 x83 x85 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x5 x83 x85 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x5 x83 x85 x84 x3000 x3500 = case x84 of
     (Curry_Prelude.C_Char 'n'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x5 x83 x85 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x5 x83 x85 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x5 x83 x85 x1002 x3000 x3500) (nd_OP__case_8 x1 x5 x83 x85 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x5 x83 x85 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x5 x83 x85 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x5 x83 x85 x3500 = case x85 of
     (Curry_Prelude.OP_Cons x86 x87) -> d_OP__case_6 x1 x5 x83 x87 x86 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x5 x83 x1002 x3500) (d_OP__case_7 x1 x5 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x5 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x5 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x5 x83 x85 x3000 x3500 = case x85 of
     (Curry_Prelude.OP_Cons x86 x87) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x5 x83 x87 x86 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x5 x83 x1002 x3000 x3500) (nd_OP__case_7 x1 x5 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x5 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x5 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x5 x83 x87 x86 x3500 = case x86 of
     (Curry_Prelude.C_Char 'a'#) -> d_OP__case_5 x1 x5 x83 x87 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',d_OP__case_5 x1 x5 x83 x87 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x5 x83 x87 x1002 x3500) (d_OP__case_6 x1 x5 x83 x87 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x5 x83 x87 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x5 x83 x87 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x5 x83 x87 x86 x3000 x3500 = case x86 of
     (Curry_Prelude.C_Char 'a'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x5 x83 x87 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x5 x83 x87 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x5 x83 x87 x1002 x3000 x3500) (nd_OP__case_6 x1 x5 x83 x87 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x5 x83 x87 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x5 x83 x87 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x5 x83 x87 x3500 = case x87 of
     (Curry_Prelude.OP_Cons x88 x89) -> d_OP__case_4 x1 x5 x83 x89 x88 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x5 x83 x1002 x3500) (d_OP__case_5 x1 x5 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x5 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x5 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x5 x83 x87 x3000 x3500 = case x87 of
     (Curry_Prelude.OP_Cons x88 x89) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x5 x83 x89 x88 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x5 x83 x1002 x3000 x3500) (nd_OP__case_5 x1 x5 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x5 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x5 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x5 x83 x89 x88 x3500 = case x88 of
     (Curry_Prelude.C_Char 'm'#) -> d_OP__case_3 x1 x5 x83 x89 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',d_OP__case_3 x1 x5 x83 x89 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x5 x83 x89 x1002 x3500) (d_OP__case_4 x1 x5 x83 x89 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x5 x83 x89 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x5 x83 x89 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x5 x83 x89 x88 x3000 x3500 = case x88 of
     (Curry_Prelude.C_Char 'm'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x5 x83 x89 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x5 x83 x89 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x5 x83 x89 x1002 x3000 x3500) (nd_OP__case_4 x1 x5 x83 x89 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x5 x83 x89 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x5 x83 x89 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x5 x83 x89 x3500 = case x89 of
     (Curry_Prelude.OP_Cons x90 x91) -> d_OP__case_2 x1 x5 x83 x91 x90 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x5 x83 x1002 x3500) (d_OP__case_3 x1 x5 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x5 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x5 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x5 x83 x89 x3000 x3500 = case x89 of
     (Curry_Prelude.OP_Cons x90 x91) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x5 x83 x91 x90 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x5 x83 x1002 x3000 x3500) (nd_OP__case_3 x1 x5 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x5 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x5 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x5 x83 x91 x90 x3500 = case x90 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_1 x1 x5 x83 x91 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_1 x1 x5 x83 x91 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x5 x83 x91 x1002 x3500) (d_OP__case_2 x1 x5 x83 x91 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x5 x83 x91 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x5 x83 x91 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x5 x83 x91 x90 x3000 x3500 = case x90 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x5 x83 x91 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x5 x83 x91 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x5 x83 x91 x1002 x3000 x3500) (nd_OP__case_2 x1 x5 x83 x91 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x5 x83 x91 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x5 x83 x91 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x5 x83 x91 x3500 = case x91 of
     Curry_Prelude.OP_List -> d_OP__case_0 x1 x83 x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x5 x83 x1002 x3500) (d_OP__case_1 x1 x5 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x5 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x5 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x5 x83 x91 x3000 x3500 = case x91 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x83 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x5 x83 x1002 x3000 x3500) (nd_OP__case_1 x1 x5 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x5 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x5 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x83 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x1 x83) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x83 x1002 x3500) (d_OP__case_0 x1 x83 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x83 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x83 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x83 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x1 x83) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x83 x1002 x3000 x3500) (nd_OP__case_0 x1 x83 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x83 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x83 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_83 x1 x4 x5 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_82 x1 x4 x5 x9 x8 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x1 x4 x5 x1002 x3500) (d_OP__case_83 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_83 x1 x4 x5 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_82 x1 x4 x5 x9 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_83 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_83 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_83 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_83 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_82 x1 x4 x5 x9 x8 x3500 = case x8 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_81 x1 x4 x5 x9 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_81 x1 x4 x5 x9 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x1 x4 x5 x9 x1002 x3500) (d_OP__case_82 x1 x4 x5 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x1 x4 x5 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x1 x4 x5 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_82 x1 x4 x5 x9 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_81 x1 x4 x5 x9 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_81 x1 x4 x5 x9 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_82 x1 x4 x5 x9 x1002 x3000 x3500) (nd_OP__case_82 x1 x4 x5 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_82 x1 x4 x5 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_82 x1 x4 x5 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_81 x1 x4 x5 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_80 x1 x4 x5 x11 x10 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x1 x4 x5 x1002 x3500) (d_OP__case_81 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_81 x1 x4 x5 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_80 x1 x4 x5 x11 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_81 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_81 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_81 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_81 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_80 x1 x4 x5 x11 x10 x3500 = case x10 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_79 x1 x4 x5 x11 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_79 x1 x4 x5 x11 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x1 x4 x5 x11 x1002 x3500) (d_OP__case_80 x1 x4 x5 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x1 x4 x5 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x1 x4 x5 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_80 x1 x4 x5 x11 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_79 x1 x4 x5 x11 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_79 x1 x4 x5 x11 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_80 x1 x4 x5 x11 x1002 x3000 x3500) (nd_OP__case_80 x1 x4 x5 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_80 x1 x4 x5 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_80 x1 x4 x5 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_79 x1 x4 x5 x11 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_78 x1 x4 x5 x13 x12 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x1 x4 x5 x1002 x3500) (d_OP__case_79 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_79 x1 x4 x5 x11 x3000 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_78 x1 x4 x5 x13 x12 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_79 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_79 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_79 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_79 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_78 x1 x4 x5 x13 x12 x3500 = case x12 of
     (Curry_Prelude.C_Char 'm'#) -> d_OP__case_77 x1 x4 x5 x13 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',d_OP__case_77 x1 x4 x5 x13 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x1 x4 x5 x13 x1002 x3500) (d_OP__case_78 x1 x4 x5 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x1 x4 x5 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x1 x4 x5 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_78 x1 x4 x5 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.C_Char 'm'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_77 x1 x4 x5 x13 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_77 x1 x4 x5 x13 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_78 x1 x4 x5 x13 x1002 x3000 x3500) (nd_OP__case_78 x1 x4 x5 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_78 x1 x4 x5 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_78 x1 x4 x5 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_77 x1 x4 x5 x13 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x14 x15) -> d_OP__case_76 x1 x4 x5 x15 x14 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x1 x4 x5 x1002 x3500) (d_OP__case_77 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_77 x1 x4 x5 x13 x3000 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_76 x1 x4 x5 x15 x14 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_77 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_77 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_77 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_77 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_76 x1 x4 x5 x15 x14 x3500 = case x14 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_75 x1 x4 x5 x15 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_75 x1 x4 x5 x15 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x1 x4 x5 x15 x1002 x3500) (d_OP__case_76 x1 x4 x5 x15 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x1 x4 x5 x15 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x1 x4 x5 x15 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_76 x1 x4 x5 x15 x14 x3000 x3500 = case x14 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_75 x1 x4 x5 x15 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_75 x1 x4 x5 x15 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_76 x1 x4 x5 x15 x1002 x3000 x3500) (nd_OP__case_76 x1 x4 x5 x15 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_76 x1 x4 x5 x15 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_76 x1 x4 x5 x15 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_75 x1 x4 x5 x15 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x16 x17) -> d_OP__case_74 x1 x4 x5 x17 x16 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x1 x4 x5 x1002 x3500) (d_OP__case_75 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_75 x1 x4 x5 x15 x3000 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_74 x1 x4 x5 x17 x16 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_75 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_75 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_75 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_75 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_74 x1 x4 x5 x17 x16 x3500 = case x16 of
     (Curry_Prelude.C_Char 't'#) -> d_OP__case_73 x1 x4 x5 x17 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',d_OP__case_73 x1 x4 x5 x17 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x1 x4 x5 x17 x1002 x3500) (d_OP__case_74 x1 x4 x5 x17 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x1 x4 x5 x17 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x1 x4 x5 x17 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_74 x1 x4 x5 x17 x16 x3000 x3500 = case x16 of
     (Curry_Prelude.C_Char 't'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_73 x1 x4 x5 x17 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_73 x1 x4 x5 x17 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_74 x1 x4 x5 x17 x1002 x3000 x3500) (nd_OP__case_74 x1 x4 x5 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_74 x1 x4 x5 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_74 x1 x4 x5 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_73 x1 x4 x5 x17 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x18 x19) -> d_OP__case_72 x1 x4 x5 x19 x18 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x1 x4 x5 x1002 x3500) (d_OP__case_73 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_73 x1 x4 x5 x17 x3000 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_72 x1 x4 x5 x19 x18 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_73 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_73 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_73 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_73 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_72 x1 x4 x5 x19 x18 x3500 = case x18 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_71 x1 x4 x5 x19 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_71 x1 x4 x5 x19 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x1 x4 x5 x19 x1002 x3500) (d_OP__case_72 x1 x4 x5 x19 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x1 x4 x5 x19 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x1 x4 x5 x19 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_72 x1 x4 x5 x19 x18 x3000 x3500 = case x18 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_71 x1 x4 x5 x19 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_71 x1 x4 x5 x19 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_72 x1 x4 x5 x19 x1002 x3000 x3500) (nd_OP__case_72 x1 x4 x5 x19 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_72 x1 x4 x5 x19 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_72 x1 x4 x5 x19 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_71 x1 x4 x5 x19 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x20 x21) -> d_OP__case_70 x1 x4 x5 x21 x20 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x1 x4 x5 x1002 x3500) (d_OP__case_71 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_71 x1 x4 x5 x19 x3000 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_70 x1 x4 x5 x21 x20 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_71 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_71 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_71 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_71 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_70 x1 x4 x5 x21 x20 x3500 = case x20 of
     (Curry_Prelude.C_Char 'v'#) -> d_OP__case_69 x1 x4 x5 x21 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('v',d_OP__case_69 x1 x4 x5 x21 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x1 x4 x5 x21 x1002 x3500) (d_OP__case_70 x1 x4 x5 x21 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x1 x4 x5 x21 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x1 x4 x5 x21 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_70 x1 x4 x5 x21 x20 x3000 x3500 = case x20 of
     (Curry_Prelude.C_Char 'v'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_69 x1 x4 x5 x21 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('v',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_69 x1 x4 x5 x21 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_70 x1 x4 x5 x21 x1002 x3000 x3500) (nd_OP__case_70 x1 x4 x5 x21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_70 x1 x4 x5 x21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_70 x1 x4 x5 x21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_69 x1 x4 x5 x21 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x22 x23) -> d_OP__case_68 x1 x4 x5 x23 x22 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x1 x4 x5 x1002 x3500) (d_OP__case_69 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_69 x1 x4 x5 x21 x3000 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_68 x1 x4 x5 x23 x22 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_69 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_69 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_69 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_69 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_68 x1 x4 x5 x23 x22 x3500 = case x22 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_67 x1 x4 x5 x23 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_67 x1 x4 x5 x23 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x1 x4 x5 x23 x1002 x3500) (d_OP__case_68 x1 x4 x5 x23 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x1 x4 x5 x23 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x1 x4 x5 x23 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_68 x1 x4 x5 x23 x22 x3000 x3500 = case x22 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_67 x1 x4 x5 x23 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_67 x1 x4 x5 x23 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_68 x1 x4 x5 x23 x1002 x3000 x3500) (nd_OP__case_68 x1 x4 x5 x23 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_68 x1 x4 x5 x23 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_68 x1 x4 x5 x23 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_67 x1 x4 x5 x23 x3500 = case x23 of
     Curry_Prelude.OP_List -> d_OP__case_66 x1 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x1 x4 x5 x1002 x3500) (d_OP__case_67 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_67 x1 x4 x5 x23 x3000 x3500 = case x23 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_66 x1 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_67 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_67 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_67 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_67 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_66 x1 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x24 x25) -> d_OP__case_65 x1 x5 x24 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x1 x5 x1002 x3500) (d_OP__case_66 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_66 x1 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_65 x1 x5 x24 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_66 x1 x5 x1002 x3000 x3500) (nd_OP__case_66 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_66 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_66 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_65 x1 x5 x24 x3500 = case x24 of
     (Curry_Prelude.OP_Tuple2 x26 x27) -> d_OP__case_64 x1 x5 x27 x26 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x1 x5 x1002 x3500) (d_OP__case_65 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_65 x1 x5 x24 x3000 x3500 = case x24 of
     (Curry_Prelude.OP_Tuple2 x26 x27) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_64 x1 x5 x27 x26 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_65 x1 x5 x1002 x3000 x3500) (nd_OP__case_65 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_65 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_65 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_64 x1 x5 x27 x26 x3500 = case x26 of
     (Curry_Prelude.OP_Cons x28 x29) -> d_OP__case_63 x1 x5 x27 x29 x28 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x1 x5 x27 x1002 x3500) (d_OP__case_64 x1 x5 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x1 x5 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x1 x5 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_64 x1 x5 x27 x26 x3000 x3500 = case x26 of
     (Curry_Prelude.OP_Cons x28 x29) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_63 x1 x5 x27 x29 x28 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_64 x1 x5 x27 x1002 x3000 x3500) (nd_OP__case_64 x1 x5 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_64 x1 x5 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_64 x1 x5 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_63 x1 x5 x27 x29 x28 x3500 = case x28 of
     (Curry_Prelude.C_Char 'n'#) -> d_OP__case_62 x1 x5 x27 x29 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',d_OP__case_62 x1 x5 x27 x29 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x1 x5 x27 x29 x1002 x3500) (d_OP__case_63 x1 x5 x27 x29 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x1 x5 x27 x29 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x1 x5 x27 x29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_63 x1 x5 x27 x29 x28 x3000 x3500 = case x28 of
     (Curry_Prelude.C_Char 'n'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x1 x5 x27 x29 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x1 x5 x27 x29 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_63 x1 x5 x27 x29 x1002 x3000 x3500) (nd_OP__case_63 x1 x5 x27 x29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_63 x1 x5 x27 x29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_63 x1 x5 x27 x29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_62 x1 x5 x27 x29 x3500 = case x29 of
     (Curry_Prelude.OP_Cons x30 x31) -> d_OP__case_61 x1 x5 x27 x31 x30 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x1 x5 x27 x1002 x3500) (d_OP__case_62 x1 x5 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x1 x5 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x1 x5 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_62 x1 x5 x27 x29 x3000 x3500 = case x29 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_61 x1 x5 x27 x31 x30 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_62 x1 x5 x27 x1002 x3000 x3500) (nd_OP__case_62 x1 x5 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_62 x1 x5 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_62 x1 x5 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_61 x1 x5 x27 x31 x30 x3500 = case x30 of
     (Curry_Prelude.C_Char 'a'#) -> d_OP__case_60 x1 x5 x27 x31 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',d_OP__case_60 x1 x5 x27 x31 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x1 x5 x27 x31 x1002 x3500) (d_OP__case_61 x1 x5 x27 x31 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x1 x5 x27 x31 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x1 x5 x27 x31 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_61 x1 x5 x27 x31 x30 x3000 x3500 = case x30 of
     (Curry_Prelude.C_Char 'a'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x1 x5 x27 x31 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x1 x5 x27 x31 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_61 x1 x5 x27 x31 x1002 x3000 x3500) (nd_OP__case_61 x1 x5 x27 x31 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_61 x1 x5 x27 x31 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_61 x1 x5 x27 x31 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_60 x1 x5 x27 x31 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x32 x33) -> d_OP__case_59 x1 x5 x27 x33 x32 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x1 x5 x27 x1002 x3500) (d_OP__case_60 x1 x5 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x1 x5 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x1 x5 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_60 x1 x5 x27 x31 x3000 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x32 x33) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_59 x1 x5 x27 x33 x32 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_60 x1 x5 x27 x1002 x3000 x3500) (nd_OP__case_60 x1 x5 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_60 x1 x5 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_60 x1 x5 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_59 x1 x5 x27 x33 x32 x3500 = case x32 of
     (Curry_Prelude.C_Char 'm'#) -> d_OP__case_58 x1 x5 x27 x33 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',d_OP__case_58 x1 x5 x27 x33 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x1 x5 x27 x33 x1002 x3500) (d_OP__case_59 x1 x5 x27 x33 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x1 x5 x27 x33 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x1 x5 x27 x33 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_59 x1 x5 x27 x33 x32 x3000 x3500 = case x32 of
     (Curry_Prelude.C_Char 'm'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_58 x1 x5 x27 x33 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_58 x1 x5 x27 x33 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_59 x1 x5 x27 x33 x1002 x3000 x3500) (nd_OP__case_59 x1 x5 x27 x33 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_59 x1 x5 x27 x33 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_59 x1 x5 x27 x33 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_58 x1 x5 x27 x33 x3500 = case x33 of
     (Curry_Prelude.OP_Cons x34 x35) -> d_OP__case_57 x1 x5 x27 x35 x34 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x1 x5 x27 x1002 x3500) (d_OP__case_58 x1 x5 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x1 x5 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x1 x5 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_58 x1 x5 x27 x33 x3000 x3500 = case x33 of
     (Curry_Prelude.OP_Cons x34 x35) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_57 x1 x5 x27 x35 x34 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x1 x5 x27 x1002 x3000 x3500) (nd_OP__case_58 x1 x5 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 x1 x5 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x1 x5 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_57 x1 x5 x27 x35 x34 x3500 = case x34 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_56 x1 x5 x27 x35 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_56 x1 x5 x27 x35 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x1 x5 x27 x35 x1002 x3500) (d_OP__case_57 x1 x5 x27 x35 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x1 x5 x27 x35 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x1 x5 x27 x35 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_57 x1 x5 x27 x35 x34 x3000 x3500 = case x34 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_56 x1 x5 x27 x35 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_56 x1 x5 x27 x35 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x1 x5 x27 x35 x1002 x3000 x3500) (nd_OP__case_57 x1 x5 x27 x35 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x1 x5 x27 x35 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x1 x5 x27 x35 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_56 x1 x5 x27 x35 x3500 = case x35 of
     Curry_Prelude.OP_List -> d_OP__case_55 x1 x27 x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x1 x5 x27 x1002 x3500) (d_OP__case_56 x1 x5 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x1 x5 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x1 x5 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_56 x1 x5 x27 x35 x3000 x3500 = case x35 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_55 x1 x27 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x1 x5 x27 x1002 x3000 x3500) (nd_OP__case_56 x1 x5 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x1 x5 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x1 x5 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_55 x1 x27 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x36 x37) -> d_OP__case_54 x1 x27 x37 x36 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x1 x27 x1002 x3500) (d_OP__case_55 x1 x27 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x1 x27 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x1 x27 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_55 x1 x27 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x36 x37) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_54 x1 x27 x37 x36 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_55 x1 x27 x1002 x3000 x3500) (nd_OP__case_55 x1 x27 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_55 x1 x27 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_55 x1 x27 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_54 x1 x27 x37 x36 x3500 = case x36 of
     (Curry_XML.C_XElem x38 x39 x40) -> d_OP__case_53 x1 x27 x37 x39 x40 x38 x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x1 x27 x37 x1002 x3500) (d_OP__case_54 x1 x27 x37 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x1 x27 x37 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x1 x27 x37 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_54 x1 x27 x37 x36 x3000 x3500 = case x36 of
     (Curry_XML.C_XElem x38 x39 x40) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_53 x1 x27 x37 x39 x40 x38 x2000 x3500))
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_54 x1 x27 x37 x1002 x3000 x3500) (nd_OP__case_54 x1 x27 x37 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_54 x1 x27 x37 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_54 x1 x27 x37 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_53 x1 x27 x37 x39 x40 x38 x3500 = case x38 of
     (Curry_Prelude.OP_Cons x41 x42) -> d_OP__case_52 x1 x27 x37 x39 x40 x42 x41 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_53 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_53 x1 x27 x37 x39 x40 x38 x3000 x3500 = case x38 of
     (Curry_Prelude.OP_Cons x41 x42) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_52 x1 x27 x37 x39 x40 x42 x41 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_53 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_53 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_53 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_53 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_52 x1 x27 x37 x39 x40 x42 x41 x3500 = case x41 of
     (Curry_Prelude.C_Char 'l'#) -> d_OP__case_51 x1 x27 x37 x39 x40 x42 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('l',d_OP__case_51 x1 x27 x37 x39 x40 x42 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x1 x27 x37 x39 x40 x42 x1002 x3500) (d_OP__case_52 x1 x27 x37 x39 x40 x42 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x1 x27 x37 x39 x40 x42 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x1 x27 x37 x39 x40 x42 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_52 x1 x27 x37 x39 x40 x42 x41 x3000 x3500 = case x41 of
     (Curry_Prelude.C_Char 'l'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_51 x1 x27 x37 x39 x40 x42 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('l',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_51 x1 x27 x37 x39 x40 x42 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_52 x1 x27 x37 x39 x40 x42 x1002 x3000 x3500) (nd_OP__case_52 x1 x27 x37 x39 x40 x42 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_52 x1 x27 x37 x39 x40 x42 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_52 x1 x27 x37 x39 x40 x42 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_51 x1 x27 x37 x39 x40 x42 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x43 x44) -> d_OP__case_50 x1 x27 x37 x39 x40 x44 x43 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_51 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_51 x1 x27 x37 x39 x40 x42 x3000 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x43 x44) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_50 x1 x27 x37 x39 x40 x44 x43 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_51 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_51 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_51 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_51 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_50 x1 x27 x37 x39 x40 x44 x43 x3500 = case x43 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_49 x1 x27 x37 x39 x40 x44 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_49 x1 x27 x37 x39 x40 x44 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x1 x27 x37 x39 x40 x44 x1002 x3500) (d_OP__case_50 x1 x27 x37 x39 x40 x44 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x1 x27 x37 x39 x40 x44 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x1 x27 x37 x39 x40 x44 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_50 x1 x27 x37 x39 x40 x44 x43 x3000 x3500 = case x43 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_49 x1 x27 x37 x39 x40 x44 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_49 x1 x27 x37 x39 x40 x44 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_50 x1 x27 x37 x39 x40 x44 x1002 x3000 x3500) (nd_OP__case_50 x1 x27 x37 x39 x40 x44 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_50 x1 x27 x37 x39 x40 x44 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_50 x1 x27 x37 x39 x40 x44 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_49 x1 x27 x37 x39 x40 x44 x3500 = case x44 of
     (Curry_Prelude.OP_Cons x45 x46) -> d_OP__case_48 x1 x27 x37 x39 x40 x46 x45 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_49 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_49 x1 x27 x37 x39 x40 x44 x3000 x3500 = case x44 of
     (Curry_Prelude.OP_Cons x45 x46) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_48 x1 x27 x37 x39 x40 x46 x45 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_49 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_49 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_49 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_49 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_48 x1 x27 x37 x39 x40 x46 x45 x3500 = case x45 of
     (Curry_Prelude.C_Char 'b'#) -> d_OP__case_47 x1 x27 x37 x39 x40 x46 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('b',d_OP__case_47 x1 x27 x37 x39 x40 x46 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x1 x27 x37 x39 x40 x46 x1002 x3500) (d_OP__case_48 x1 x27 x37 x39 x40 x46 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x1 x27 x37 x39 x40 x46 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x1 x27 x37 x39 x40 x46 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_48 x1 x27 x37 x39 x40 x46 x45 x3000 x3500 = case x45 of
     (Curry_Prelude.C_Char 'b'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x1 x27 x37 x39 x40 x46 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('b',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_47 x1 x27 x37 x39 x40 x46 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_48 x1 x27 x37 x39 x40 x46 x1002 x3000 x3500) (nd_OP__case_48 x1 x27 x37 x39 x40 x46 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_48 x1 x27 x37 x39 x40 x46 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_48 x1 x27 x37 x39 x40 x46 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_47 x1 x27 x37 x39 x40 x46 x3500 = case x46 of
     (Curry_Prelude.OP_Cons x47 x48) -> d_OP__case_46 x1 x27 x37 x39 x40 x48 x47 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_47 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_47 x1 x27 x37 x39 x40 x46 x3000 x3500 = case x46 of
     (Curry_Prelude.OP_Cons x47 x48) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_46 x1 x27 x37 x39 x40 x48 x47 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_47 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_47 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_47 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_47 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_46 x1 x27 x37 x39 x40 x48 x47 x3500 = case x47 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_45 x1 x27 x37 x39 x40 x48 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_45 x1 x27 x37 x39 x40 x48 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x1 x27 x37 x39 x40 x48 x1002 x3500) (d_OP__case_46 x1 x27 x37 x39 x40 x48 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x1 x27 x37 x39 x40 x48 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x1 x27 x37 x39 x40 x48 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_46 x1 x27 x37 x39 x40 x48 x47 x3000 x3500 = case x47 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_45 x1 x27 x37 x39 x40 x48 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_45 x1 x27 x37 x39 x40 x48 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_46 x1 x27 x37 x39 x40 x48 x1002 x3000 x3500) (nd_OP__case_46 x1 x27 x37 x39 x40 x48 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_46 x1 x27 x37 x39 x40 x48 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_46 x1 x27 x37 x39 x40 x48 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_45 x1 x27 x37 x39 x40 x48 x3500 = case x48 of
     (Curry_Prelude.OP_Cons x49 x50) -> d_OP__case_44 x1 x27 x37 x39 x40 x50 x49 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_45 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_45 x1 x27 x37 x39 x40 x48 x3000 x3500 = case x48 of
     (Curry_Prelude.OP_Cons x49 x50) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_44 x1 x27 x37 x39 x40 x50 x49 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_45 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_45 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_45 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_45 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_44 x1 x27 x37 x39 x40 x50 x49 x3500 = case x49 of
     (Curry_Prelude.C_Char 'a'#) -> d_OP__case_43 x1 x27 x37 x39 x40 x50 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',d_OP__case_43 x1 x27 x37 x39 x40 x50 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x1 x27 x37 x39 x40 x50 x1002 x3500) (d_OP__case_44 x1 x27 x37 x39 x40 x50 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x1 x27 x37 x39 x40 x50 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x1 x27 x37 x39 x40 x50 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_44 x1 x27 x37 x39 x40 x50 x49 x3000 x3500 = case x49 of
     (Curry_Prelude.C_Char 'a'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_43 x1 x27 x37 x39 x40 x50 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('a',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_43 x1 x27 x37 x39 x40 x50 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_44 x1 x27 x37 x39 x40 x50 x1002 x3000 x3500) (nd_OP__case_44 x1 x27 x37 x39 x40 x50 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_44 x1 x27 x37 x39 x40 x50 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_44 x1 x27 x37 x39 x40 x50 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_43 x1 x27 x37 x39 x40 x50 x3500 = case x50 of
     (Curry_Prelude.OP_Cons x51 x52) -> d_OP__case_42 x1 x27 x37 x39 x40 x52 x51 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_43 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_43 x1 x27 x37 x39 x40 x50 x3000 x3500 = case x50 of
     (Curry_Prelude.OP_Cons x51 x52) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_42 x1 x27 x37 x39 x40 x52 x51 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_43 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_43 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_43 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_43 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_42 x1 x27 x37 x39 x40 x52 x51 x3500 = case x51 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_41 x1 x27 x37 x39 x40 x52 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_41 x1 x27 x37 x39 x40 x52 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x27 x37 x39 x40 x52 x1002 x3500) (d_OP__case_42 x1 x27 x37 x39 x40 x52 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 x27 x37 x39 x40 x52 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x27 x37 x39 x40 x52 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_42 x1 x27 x37 x39 x40 x52 x51 x3000 x3500 = case x51 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_41 x1 x27 x37 x39 x40 x52 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_41 x1 x27 x37 x39 x40 x52 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_42 x1 x27 x37 x39 x40 x52 x1002 x3000 x3500) (nd_OP__case_42 x1 x27 x37 x39 x40 x52 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_42 x1 x27 x37 x39 x40 x52 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_42 x1 x27 x37 x39 x40 x52 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_41 x1 x27 x37 x39 x40 x52 x3500 = case x52 of
     (Curry_Prelude.OP_Cons x53 x54) -> d_OP__case_40 x1 x27 x37 x39 x40 x54 x53 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_41 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_41 x1 x27 x37 x39 x40 x52 x3000 x3500 = case x52 of
     (Curry_Prelude.OP_Cons x53 x54) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_40 x1 x27 x37 x39 x40 x54 x53 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_41 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_41 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_41 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_41 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_40 x1 x27 x37 x39 x40 x54 x53 x3500 = case x53 of
     (Curry_Prelude.C_Char 'y'#) -> d_OP__case_39 x1 x27 x37 x39 x40 x54 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('y',d_OP__case_39 x1 x27 x37 x39 x40 x54 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x1 x27 x37 x39 x40 x54 x1002 x3500) (d_OP__case_40 x1 x27 x37 x39 x40 x54 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x1 x27 x37 x39 x40 x54 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x1 x27 x37 x39 x40 x54 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_40 x1 x27 x37 x39 x40 x54 x53 x3000 x3500 = case x53 of
     (Curry_Prelude.C_Char 'y'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x1 x27 x37 x39 x40 x54 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('y',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x1 x27 x37 x39 x40 x54 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_40 x1 x27 x37 x39 x40 x54 x1002 x3000 x3500) (nd_OP__case_40 x1 x27 x37 x39 x40 x54 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_40 x1 x27 x37 x39 x40 x54 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_40 x1 x27 x37 x39 x40 x54 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_39 x1 x27 x37 x39 x40 x54 x3500 = case x54 of
     Curry_Prelude.OP_List -> d_OP__case_38 x1 x27 x37 x40 x39 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x1 x27 x37 x39 x40 x1002 x3500) (d_OP__case_39 x1 x27 x37 x39 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x1 x27 x37 x39 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x1 x27 x37 x39 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_39 x1 x27 x37 x39 x40 x54 x3000 x3500 = case x54 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_38 x1 x27 x37 x40 x39 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x1 x27 x37 x39 x40 x1002 x3000 x3500) (nd_OP__case_39 x1 x27 x37 x39 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x1 x27 x37 x39 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x1 x27 x37 x39 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_38 x1 x27 x37 x40 x39 x3500 = case x39 of
     Curry_Prelude.OP_List -> d_OP__case_37 x1 x27 x40 x37 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x1 x27 x37 x40 x1002 x3500) (d_OP__case_38 x1 x27 x37 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x1 x27 x37 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x1 x27 x37 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_38 x1 x27 x37 x40 x39 x3000 x3500 = case x39 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_37 x1 x27 x40 x37 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_38 x1 x27 x37 x40 x1002 x3000 x3500) (nd_OP__case_38 x1 x27 x37 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_38 x1 x27 x37 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_38 x1 x27 x37 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_37 x1 x27 x40 x37 x3500 = case x37 of
     (Curry_Prelude.OP_Cons x55 x56) -> d_OP__case_36 x1 x27 x40 x56 x55 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x1 x27 x40 x1002 x3500) (d_OP__case_37 x1 x27 x40 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x1 x27 x40 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x1 x27 x40 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_37 x1 x27 x40 x37 x3000 x3500 = case x37 of
     (Curry_Prelude.OP_Cons x55 x56) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x1 x27 x40 x56 x55 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_37 x1 x27 x40 x1002 x3000 x3500) (nd_OP__case_37 x1 x27 x40 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_37 x1 x27 x40 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_37 x1 x27 x40 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_36 x1 x27 x40 x56 x55 x3500 = case x55 of
     (Curry_XML.C_XElem x57 x58 x59) -> d_OP__case_35 x1 x27 x40 x56 x58 x59 x57 x3500
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x1 x27 x40 x56 x1002 x3500) (d_OP__case_36 x1 x27 x40 x56 x1003 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x1 x27 x40 x56 z x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x1 x27 x40 x56 x1002) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_36 x1 x27 x40 x56 x55 x3000 x3500 = case x55 of
     (Curry_XML.C_XElem x57 x58 x59) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x1 x27 x40 x56 x58 x59 x57 x2000 x3500))
     (Curry_XML.Choice_C_XmlExp x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x1 x27 x40 x56 x1002 x3000 x3500) (nd_OP__case_36 x1 x27 x40 x56 x1003 x3000 x3500)
     (Curry_XML.Choices_C_XmlExp x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x1 x27 x40 x56 z x3000 x3500) x1002
     (Curry_XML.Guard_C_XmlExp x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x1 x27 x40 x56 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_XML.Fail_C_XmlExp x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_35 x1 x27 x40 x56 x58 x59 x57 x3500 = case x57 of
     (Curry_Prelude.OP_Cons x60 x61) -> d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x60 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_35 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x27 x40 x56 x58 x59 x57 x3000 x3500 = case x57 of
     (Curry_Prelude.OP_Cons x60 x61) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x60 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_35 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x60 x3500 = case x60 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1002 x3500) (d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x60 x3000 x3500 = case x60 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1002 x3000 x3500) (nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x1 x27 x40 x56 x58 x59 x61 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x3500 = case x61 of
     (Curry_Prelude.OP_Cons x62 x63) -> d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x62 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_33 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x1 x27 x40 x56 x58 x59 x61 x3000 x3500 = case x61 of
     (Curry_Prelude.OP_Cons x62 x63) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x62 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_33 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x62 x3500 = case x62 of
     (Curry_Prelude.C_Char 'n'#) -> d_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',d_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1002 x3500) (d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x62 x3000 x3500 = case x62 of
     (Curry_Prelude.C_Char 'n'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('n',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1002 x3000 x3500) (nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x1 x27 x40 x56 x58 x59 x63 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x3500 = case x63 of
     (Curry_Prelude.OP_Cons x64 x65) -> d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x64 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_31 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x27 x40 x56 x58 x59 x63 x3000 x3500 = case x63 of
     (Curry_Prelude.OP_Cons x64 x65) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x64 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_31 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x64 x3500 = case x64 of
     (Curry_Prelude.C_Char 't'#) -> d_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',d_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1002 x3500) (d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x64 x3000 x3500 = case x64 of
     (Curry_Prelude.C_Char 't'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1002 x3000 x3500) (nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x1 x27 x40 x56 x58 x59 x65 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x3500 = case x65 of
     (Curry_Prelude.OP_Cons x66 x67) -> d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x66 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_29 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x1 x27 x40 x56 x58 x59 x65 x3000 x3500 = case x65 of
     (Curry_Prelude.OP_Cons x66 x67) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x66 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_29 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x66 x3500 = case x66 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1002 x3500) (d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x66 x3000 x3500 = case x66 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1002 x3000 x3500) (nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x27 x40 x56 x58 x59 x67 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x3500 = case x67 of
     (Curry_Prelude.OP_Cons x68 x69) -> d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x68 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_27 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x27 x40 x56 x58 x59 x67 x3000 x3500 = case x67 of
     (Curry_Prelude.OP_Cons x68 x69) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x68 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_27 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x68 x3500 = case x68 of
     (Curry_Prelude.C_Char 'y'#) -> d_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('y',d_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1002 x3500) (d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x68 x3000 x3500 = case x68 of
     (Curry_Prelude.C_Char 'y'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('y',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1002 x3000 x3500) (nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x27 x40 x56 x58 x59 x69 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x3500 = case x69 of
     Curry_Prelude.OP_List -> d_OP__case_24 x1 x27 x40 x56 x59 x58 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x27 x40 x56 x58 x59 x1002 x3500) (d_OP__case_25 x1 x27 x40 x56 x58 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x27 x40 x56 x58 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x27 x40 x56 x58 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x27 x40 x56 x58 x59 x69 x3000 x3500 = case x69 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x27 x40 x56 x59 x58 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x27 x40 x56 x58 x59 x1002 x3000 x3500) (nd_OP__case_25 x1 x27 x40 x56 x58 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x27 x40 x56 x58 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x27 x40 x56 x58 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x27 x40 x56 x59 x58 x3500 = case x58 of
     Curry_Prelude.OP_List -> d_OP__case_23 x1 x27 x40 x59 x56 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x27 x40 x56 x59 x1002 x3500) (d_OP__case_24 x1 x27 x40 x56 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x27 x40 x56 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x27 x40 x56 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x27 x40 x56 x59 x58 x3000 x3500 = case x58 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x1 x27 x40 x59 x56 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x27 x40 x56 x59 x1002 x3000 x3500) (nd_OP__case_24 x1 x27 x40 x56 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x27 x40 x56 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x27 x40 x56 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x27 x40 x59 x56 x3500 = case x56 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x1 x27) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_XML.d_C_textOfXml x3500) x40 x3500) (Curry_Prelude.d_C_apply (Curry_XML.d_C_textOfXml x3500) x59 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x27 x40 x59 x1002 x3500) (d_OP__case_23 x1 x27 x40 x59 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x27 x40 x59 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x27 x40 x59 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x27 x40 x59 x56 x3000 x3500 = case x56 of
     Curry_Prelude.OP_List -> let
          x2006 = x3000
           in (seq x2006 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x1 x27) (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (Curry_Prelude.OP_Tuple2 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_XML.nd_C_textOfXml x2000 x3500) x40 x2001 x3500)))) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_XML.nd_C_textOfXml x2003 x3500) x59 x2004 x3500))))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x27 x40 x59 x1002 x3000 x3500) (nd_OP__case_23 x1 x27 x40 x59 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x27 x40 x59 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x27 x40 x59 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_107 x1 x4 x5 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_106 x1 x4 x5 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x1 x4 x5 x1002 x3500) (d_OP__case_107 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_107 x1 x4 x5 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_106 x1 x4 x5 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_107 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_107 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_107 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_107 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_106 x1 x4 x5 x7 x6 x3500 = case x6 of
     (Curry_Prelude.C_Char 'p'#) -> d_OP__case_105 x1 x4 x5 x7 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',d_OP__case_105 x1 x4 x5 x7 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x1 x4 x5 x7 x1002 x3500) (d_OP__case_106 x1 x4 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x1 x4 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x1 x4 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_106 x1 x4 x5 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.C_Char 'p'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_105 x1 x4 x5 x7 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('p',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_105 x1 x4 x5 x7 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_106 x1 x4 x5 x7 x1002 x3000 x3500) (nd_OP__case_106 x1 x4 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_106 x1 x4 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_106 x1 x4 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_105 x1 x4 x5 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_104 x1 x4 x5 x9 x8 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x1 x4 x5 x1002 x3500) (d_OP__case_105 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_105 x1 x4 x5 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_104 x1 x4 x5 x9 x8 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_105 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_105 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_105 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_105 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_104 x1 x4 x5 x9 x8 x3500 = case x8 of
     (Curry_Prelude.C_Char 'r'#) -> d_OP__case_103 x1 x4 x5 x9 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',d_OP__case_103 x1 x4 x5 x9 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x1 x4 x5 x9 x1002 x3500) (d_OP__case_104 x1 x4 x5 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 x1 x4 x5 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x1 x4 x5 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_104 x1 x4 x5 x9 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.C_Char 'r'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_103 x1 x4 x5 x9 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('r',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_103 x1 x4 x5 x9 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_104 x1 x4 x5 x9 x1002 x3000 x3500) (nd_OP__case_104 x1 x4 x5 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_104 x1 x4 x5 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_104 x1 x4 x5 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_103 x1 x4 x5 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_102 x1 x4 x5 x11 x10 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x1 x4 x5 x1002 x3500) (d_OP__case_103 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_103 x1 x4 x5 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_102 x1 x4 x5 x11 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_103 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_103 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_103 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_103 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_102 x1 x4 x5 x11 x10 x3500 = case x10 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_101 x1 x4 x5 x11 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_101 x1 x4 x5 x11 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x1 x4 x5 x11 x1002 x3500) (d_OP__case_102 x1 x4 x5 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 x1 x4 x5 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x1 x4 x5 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_102 x1 x4 x5 x11 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_101 x1 x4 x5 x11 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_101 x1 x4 x5 x11 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_102 x1 x4 x5 x11 x1002 x3000 x3500) (nd_OP__case_102 x1 x4 x5 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_102 x1 x4 x5 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_102 x1 x4 x5 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_101 x1 x4 x5 x11 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x12 x13) -> d_OP__case_100 x1 x4 x5 x13 x12 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x1 x4 x5 x1002 x3500) (d_OP__case_101 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_101 x1 x4 x5 x11 x3000 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_100 x1 x4 x5 x13 x12 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_101 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_101 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_101 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_101 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_100 x1 x4 x5 x13 x12 x3500 = case x12 of
     (Curry_Prelude.C_Char 'm'#) -> d_OP__case_99 x1 x4 x5 x13 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',d_OP__case_99 x1 x4 x5 x13 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x1 x4 x5 x13 x1002 x3500) (d_OP__case_100 x1 x4 x5 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x1 x4 x5 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x1 x4 x5 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_100 x1 x4 x5 x13 x12 x3000 x3500 = case x12 of
     (Curry_Prelude.C_Char 'm'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_99 x1 x4 x5 x13 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('m',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_99 x1 x4 x5 x13 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_100 x1 x4 x5 x13 x1002 x3000 x3500) (nd_OP__case_100 x1 x4 x5 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_100 x1 x4 x5 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_100 x1 x4 x5 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_99 x1 x4 x5 x13 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x14 x15) -> d_OP__case_98 x1 x4 x5 x15 x14 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x1 x4 x5 x1002 x3500) (d_OP__case_99 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_99 x1 x4 x5 x13 x3000 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_98 x1 x4 x5 x15 x14 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_99 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_99 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_99 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_99 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_98 x1 x4 x5 x15 x14 x3500 = case x14 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_97 x1 x4 x5 x15 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_97 x1 x4 x5 x15 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x1 x4 x5 x15 x1002 x3500) (d_OP__case_98 x1 x4 x5 x15 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x1 x4 x5 x15 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x1 x4 x5 x15 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_98 x1 x4 x5 x15 x14 x3000 x3500 = case x14 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_97 x1 x4 x5 x15 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_97 x1 x4 x5 x15 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_98 x1 x4 x5 x15 x1002 x3000 x3500) (nd_OP__case_98 x1 x4 x5 x15 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_98 x1 x4 x5 x15 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_98 x1 x4 x5 x15 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_97 x1 x4 x5 x15 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x16 x17) -> d_OP__case_96 x1 x4 x5 x17 x16 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x1 x4 x5 x1002 x3500) (d_OP__case_97 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_97 x1 x4 x5 x15 x3000 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_96 x1 x4 x5 x17 x16 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_97 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_97 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_97 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_97 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_96 x1 x4 x5 x17 x16 x3500 = case x16 of
     (Curry_Prelude.C_Char 't'#) -> d_OP__case_95 x1 x4 x5 x17 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',d_OP__case_95 x1 x4 x5 x17 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x1 x4 x5 x17 x1002 x3500) (d_OP__case_96 x1 x4 x5 x17 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x1 x4 x5 x17 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x1 x4 x5 x17 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_96 x1 x4 x5 x17 x16 x3000 x3500 = case x16 of
     (Curry_Prelude.C_Char 't'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_95 x1 x4 x5 x17 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('t',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_95 x1 x4 x5 x17 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_96 x1 x4 x5 x17 x1002 x3000 x3500) (nd_OP__case_96 x1 x4 x5 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_96 x1 x4 x5 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_96 x1 x4 x5 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_95 x1 x4 x5 x17 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x18 x19) -> d_OP__case_94 x1 x4 x5 x19 x18 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x1 x4 x5 x1002 x3500) (d_OP__case_95 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_95 x1 x4 x5 x17 x3000 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_94 x1 x4 x5 x19 x18 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_95 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_95 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_95 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_95 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_94 x1 x4 x5 x19 x18 x3500 = case x18 of
     (Curry_Prelude.C_Char 'i'#) -> d_OP__case_93 x1 x4 x5 x19 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',d_OP__case_93 x1 x4 x5 x19 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x1 x4 x5 x19 x1002 x3500) (d_OP__case_94 x1 x4 x5 x19 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x1 x4 x5 x19 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x1 x4 x5 x19 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_94 x1 x4 x5 x19 x18 x3000 x3500 = case x18 of
     (Curry_Prelude.C_Char 'i'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_93 x1 x4 x5 x19 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('i',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_93 x1 x4 x5 x19 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_94 x1 x4 x5 x19 x1002 x3000 x3500) (nd_OP__case_94 x1 x4 x5 x19 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_94 x1 x4 x5 x19 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_94 x1 x4 x5 x19 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_93 x1 x4 x5 x19 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x20 x21) -> d_OP__case_92 x1 x4 x5 x21 x20 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x1 x4 x5 x1002 x3500) (d_OP__case_93 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_93 x1 x4 x5 x19 x3000 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_92 x1 x4 x5 x21 x20 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_93 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_93 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_93 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_93 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_92 x1 x4 x5 x21 x20 x3500 = case x20 of
     (Curry_Prelude.C_Char 'v'#) -> d_OP__case_91 x1 x4 x5 x21 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('v',d_OP__case_91 x1 x4 x5 x21 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x1 x4 x5 x21 x1002 x3500) (d_OP__case_92 x1 x4 x5 x21 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x1 x4 x5 x21 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x1 x4 x5 x21 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_92 x1 x4 x5 x21 x20 x3000 x3500 = case x20 of
     (Curry_Prelude.C_Char 'v'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_91 x1 x4 x5 x21 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('v',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_91 x1 x4 x5 x21 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_92 x1 x4 x5 x21 x1002 x3000 x3500) (nd_OP__case_92 x1 x4 x5 x21 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_92 x1 x4 x5 x21 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_92 x1 x4 x5 x21 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_91 x1 x4 x5 x21 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x22 x23) -> d_OP__case_90 x1 x4 x5 x23 x22 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x1 x4 x5 x1002 x3500) (d_OP__case_91 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_91 x1 x4 x5 x21 x3000 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_90 x1 x4 x5 x23 x22 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_91 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_91 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_91 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_91 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_90 x1 x4 x5 x23 x22 x3500 = case x22 of
     (Curry_Prelude.C_Char 'e'#) -> d_OP__case_89 x1 x4 x5 x23 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',d_OP__case_89 x1 x4 x5 x23 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x1 x4 x5 x23 x1002 x3500) (d_OP__case_90 x1 x4 x5 x23 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x1 x4 x5 x23 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x1 x4 x5 x23 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_90 x1 x4 x5 x23 x22 x3000 x3500 = case x22 of
     (Curry_Prelude.C_Char 'e'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_89 x1 x4 x5 x23 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('e',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_89 x1 x4 x5 x23 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_90 x1 x4 x5 x23 x1002 x3000 x3500) (nd_OP__case_90 x1 x4 x5 x23 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_90 x1 x4 x5 x23 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_90 x1 x4 x5 x23 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_89 x1 x4 x5 x23 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x24 x25) -> d_OP__case_88 x1 x4 x5 x25 x24 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x1 x4 x5 x1002 x3500) (d_OP__case_89 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_89 x1 x4 x5 x23 x3000 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_88 x1 x4 x5 x25 x24 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_89 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_89 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_89 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_89 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_88 x1 x4 x5 x25 x24 x3500 = case x24 of
     (Curry_Prelude.C_Char 's'#) -> d_OP__case_87 x1 x4 x5 x25 x3500
     (Curry_Prelude.CurryChar x5000) -> matchChar [('s',d_OP__case_87 x1 x4 x5 x25 x3500)] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x1 x4 x5 x25 x1002 x3500) (d_OP__case_88 x1 x4 x5 x25 x1003 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x1 x4 x5 x25 z x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x1 x4 x5 x25 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_88 x1 x4 x5 x25 x24 x3000 x3500 = case x24 of
     (Curry_Prelude.C_Char 's'#) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_87 x1 x4 x5 x25 x2000 x3500))
     (Curry_Prelude.CurryChar x5000) -> matchChar [('s',let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_87 x1 x4 x5 x25 x2000 x3500)))] x5000 x3500
     (Curry_Prelude.Choice_C_Char x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_88 x1 x4 x5 x25 x1002 x3000 x3500) (nd_OP__case_88 x1 x4 x5 x25 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Char x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_88 x1 x4 x5 x25 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Char x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_88 x1 x4 x5 x25 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Char x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_87 x1 x4 x5 x25 x3500 = case x25 of
     Curry_Prelude.OP_List -> d_OP__case_86 x1 x5 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x1 x4 x5 x1002 x3500) (d_OP__case_87 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_87 x1 x4 x5 x25 x3000 x3500 = case x25 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_86 x1 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_87 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_87 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_87 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_87 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_86 x1 x5 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_map (d_OP_xml2primtrans_dot_xml2prim_dot_363 x1) x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x1 x5 x1002 x3500) (d_OP__case_86 x1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_86 x1 x5 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_map (wrapDX id (d_OP_xml2primtrans_dot_xml2prim_dot_363 x1)) x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_86 x1 x5 x1002 x3000 x3500) (nd_OP__case_86 x1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_86 x1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_86 x1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_108 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x1002 x3500) (d_OP__case_108 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_108 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_108 x1002 x3000 x3500) (nd_OP__case_108 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_108 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_108 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_109 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x1002 x3500) (d_OP__case_109 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_109 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_109 x1002 x3000 x3500) (nd_OP__case_109 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_109 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_109 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_111 x3 x4 x5 x6 x7 x8 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x9 = d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x8 x3500
          x10 = d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x8 x3500
           in (d_OP__case_110 x3 x4 x5 x6 x9 x10 (Curry_Prelude.d_C_null x10 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x3 x4 x5 x6 x7 x8 x1002 x3500) (d_OP__case_111 x3 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x3 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x3 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_111 x3 x4 x5 x6 x7 x8 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x9 = d_OP_mergePrimSpecIntoFunc_dot___hash_selFP2_hash_lib x8 x3500
               x10 = d_OP_mergePrimSpecIntoFunc_dot___hash_selFP3_hash_entry x8 x3500
                in (nd_OP__case_110 x3 x4 x5 x6 x9 x10 (Curry_Prelude.d_C_null x10 x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_111 x3 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP__case_111 x3 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_111 x3 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_111 x3 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_110 x3 x4 x5 x6 x9 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x3 x4 x5 x6 (Curry_FlatCurry.C_External (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x10) x3500))) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x3 x4 x5 x6 x9 x10 x1002 x3500) (d_OP__case_110 x3 x4 x5 x6 x9 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x3 x4 x5 x6 x9 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x3 x4 x5 x6 x9 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_110 x3 x4 x5 x6 x9 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x3 x4 x5 x6 (Curry_FlatCurry.C_External (Curry_Prelude.d_OP_plus_plus x9 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x10) x3500))) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_110 x3 x4 x5 x6 x9 x10 x1002 x3000 x3500) (nd_OP__case_110 x3 x4 x5 x6 x9 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_110 x3 x4 x5 x6 x9 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_110 x3 x4 x5 x6 x9 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_112 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x2 x3500) (d_C_processPrimitives x2) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile (Curry_FlatCurry.d_C_flatCurryFileName x2 x3500) x3500) (d_C_processPrimitives x2) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x1 x2 x3 x1002 x3500) (d_OP__case_112 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_112 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x2 x3500) (wrapDX id (d_C_processPrimitives x2)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile (Curry_FlatCurry.d_C_flatCurryFileName x2 x3500) x3500) (wrapDX id (d_C_processPrimitives x2)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_112 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_112 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_112 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_112 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_113 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x1 x3500) (d_C_processPrimitives x1) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getSourceModificationTime x1 x3500) (d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41 x1) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x1 x2 x1002 x3500) (d_OP__case_113 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_113 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x1 x3500) (wrapDX id (d_C_processPrimitives x1)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getSourceModificationTime x1 x3500) (wrapDX id (d_OP_readCurrentFlatCurry_dot___hash_lambda39_dot___hash_lambda40_dot___hash_lambda41 x1)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_113 x1 x2 x1002 x3000 x3500) (nd_OP__case_113 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_113 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_113 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_114 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x7 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_cmpString x3500) x3 x3500) x5 x3500
           in (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.C_LT x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.C_EQ x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_leqString x3500) x4 x3500) x6 x3500) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x3 x4 x1002 x3500) (d_OP__case_114 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_114 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2010 = x3000
           in (seq x2010 (let
               x2004 = leftSupply x2010
               x2009 = rightSupply x2010
                in (seq x2004 (seq x2009 (let
                    x7 = let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Sort.nd_C_cmpString x2000 x3500) x3 x2001 x3500)))) x5 x2003 x3500)))
                     in (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.C_LT x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x7 Curry_Prelude.C_EQ x3500) (let
                         x2008 = leftSupply x2009
                         x2007 = rightSupply x2009
                          in (seq x2008 (seq x2007 (Curry_Prelude.nd_C_apply (let
                              x2006 = leftSupply x2007
                              x2005 = rightSupply x2007
                               in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Sort.nd_C_leqString x2005 x3500) x4 x2006 x3500)))) x6 x2008 x3500)))) x3500) x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_114 x3 x4 x1002 x3000 x3500) (nd_OP__case_114 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_114 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_114 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_115 x3 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_LPattern x4) -> d_C_allConsOfExpr x3 x3500
     (Curry_FlatCurry.C_Pattern x5 x6) -> Curry_List.d_C_union (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (d_C_allConsOfExpr x3 x3500) x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x3 x1002 x3500) (d_OP__case_115 x3 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 x3 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x3 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_115 x3 x2 x3000 x3500 = case x2 of
     (Curry_FlatCurry.C_LPattern x4) -> d_C_allConsOfExpr x3 x3500
     (Curry_FlatCurry.C_Pattern x5 x6) -> Curry_List.d_C_union (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List) (d_C_allConsOfExpr x3 x3500) x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_115 x3 x1002 x3000 x3500) (nd_OP__case_115 x3 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_115 x3 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_115 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_116 x5 x7 x4 x3500 = case x4 of
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.OP_Cons x5 x7
     (Curry_FlatCurry.C_ConsPartCall x8) -> Curry_Prelude.OP_Cons x5 x7
     Curry_FlatCurry.C_FuncCall -> x7
     (Curry_FlatCurry.C_FuncPartCall x9) -> x7
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x5 x7 x1002 x3500) (d_OP__case_116 x5 x7 x1003 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 x5 x7 z x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_116 x5 x7 x4 x3000 x3500 = case x4 of
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.OP_Cons x5 x7
     (Curry_FlatCurry.C_ConsPartCall x8) -> Curry_Prelude.OP_Cons x5 x7
     Curry_FlatCurry.C_FuncCall -> x7
     (Curry_FlatCurry.C_FuncPartCall x9) -> x7
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_116 x5 x7 x1002 x3000 x3500) (nd_OP__case_116 x5 x7 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_116 x5 x7 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_116 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_117 x6 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_allConsOfExpr x9 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x1002 x3500) (d_OP__case_117 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_117 x6 x3000 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_allConsOfExpr x9 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_117 x1002 x3000 x3500) (nd_OP__case_117 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_117 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_117 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_118 x5 x7 x4 x3500 = case x4 of
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.OP_Cons x5 x7
     (Curry_FlatCurry.C_FuncPartCall x8) -> Curry_Prelude.OP_Cons x5 x7
     Curry_FlatCurry.C_ConsCall -> x7
     (Curry_FlatCurry.C_ConsPartCall x9) -> x7
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x5 x7 x1002 x3500) (d_OP__case_118 x5 x7 x1003 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 x5 x7 z x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_118 x5 x7 x4 x3000 x3500 = case x4 of
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.OP_Cons x5 x7
     (Curry_FlatCurry.C_FuncPartCall x8) -> Curry_Prelude.OP_Cons x5 x7
     Curry_FlatCurry.C_ConsCall -> x7
     (Curry_FlatCurry.C_ConsPartCall x9) -> x7
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_118 x5 x7 x1002 x3000 x3500) (nd_OP__case_118 x5 x7 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_118 x5 x7 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_118 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_119 x6 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> Curry_List.d_C_nub (d_C_allFuncCallsOfExpr x9 x3500) x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x1002 x3500) (d_OP__case_119 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_119 x6 x3000 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> Curry_List.d_C_nub (d_C_allFuncCallsOfExpr x9 x3500) x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_119 x1002 x3000 x3500) (nd_OP__case_119 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_119 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_119 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x11 x3500) x2 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1002 x3500) (d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_C_not (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x11 x2000 x3500) x2 x2001 x3500)))) x3500) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1002 x3000 x3500) (nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_123 x1 x2 x3 x4 x5 x6 x7 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_readCurrentFlatCurry x11 x3500) (d_OP_getCalledFuncs_dot___hash_lambda33 x12 x10 x4 x6 x5 x2 x7 x11 x3 x1) x3500
     Curry_Prelude.C_False -> d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_TableRBT.d_C_lookupRBT (Curry_Prelude.OP_Tuple2 x11 x12) x3500) x4 x3500) Curry_Prelude.C_Nothing x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3500) (d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_readCurrentFlatCurry x11 x3500) (wrapNX id (nd_OP_getCalledFuncs_dot___hash_lambda33 x12 x10 x4 x6 x5 x2 x7 x11 x3 x1)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_OP_eq_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_TableRBT.nd_C_lookupRBT (Curry_Prelude.OP_Tuple2 x11 x12) x2000 x3500) x4 x2001 x3500)))) Curry_Prelude.C_Nothing x3500) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000 x3500) (nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 x3500 = case x13 of
     Curry_Prelude.C_True -> d_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x10 x3500
     Curry_Prelude.C_False -> d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3500) (d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13 x3000 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x10 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000 x3500) (nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x20 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x13 = Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_apply (Curry_TableRBT.d_C_lookupRBT (Curry_Prelude.OP_Tuple2 x11 x12) x3500) x4 x3500) x3500
          x14 = d_C_allFuncCalls x13 x3500
          x15 = Curry_Prelude.d_C_filter (d_OP_getCalledFuncs_dot___hash_lambda34 x5) x14 x3500
          x16 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_getImplicitlyRequired x1) x3500) x15 x3500
          x17 = d_C_allConstructorsOfFunc x13 x3500
          x18 = Curry_Prelude.d_C_filter (d_OP_getCalledFuncs_dot___hash_lambda35 x6) x17 x3500
          x19 = d_C_allTypesOfFunc x13 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (d_C_getCalledFuncs x1 x2 x3 x4 (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x5 (Curry_Prelude.d_OP_plus_plus x15 x16 x3500) x3500) (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x6 x17 x3500) (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x7 x19 x3500) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.d_OP_plus_plus x16 x18 x3500) x3500) x3500) x3500) (d_OP_getCalledFuncs_dot___hash_lambda36 x13) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3500) (d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x20 x3000 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x2023 = x3000
           in (seq x2023 (let
               x2024 = leftSupply x2023
               x2025 = rightSupply x2023
                in (seq x2024 (seq x2025 (let
                    x2002 = leftSupply x2024
                    x2003 = rightSupply x2024
                     in (seq x2002 (seq x2003 (let
                         x2006 = leftSupply x2025
                         x2026 = rightSupply x2025
                          in (seq x2006 (seq x2026 (let
                              x2007 = leftSupply x2026
                              x2022 = rightSupply x2026
                               in (seq x2007 (seq x2022 (let
                                   x13 = Curry_Maybe.d_C_fromJust (let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_TableRBT.nd_C_lookupRBT (Curry_Prelude.OP_Tuple2 x11 x12) x2000 x3500) x4 x2001 x3500)))) x3500
                                   x14 = d_C_allFuncCalls x13 x3500
                                   x15 = Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_getCalledFuncs_dot___hash_lambda34 x5)) x14 x2003 x3500
                                   x16 = let
                                        x2005 = leftSupply x2006
                                        x2004 = rightSupply x2006
                                         in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id (d_C_getImplicitlyRequired x1)) x2004 x3500) x15 x2005 x3500)))
                                   x17 = d_C_allConstructorsOfFunc x13 x3500
                                   x18 = Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_getCalledFuncs_dot___hash_lambda35 x6)) x17 x2007 x3500
                                   x19 = d_C_allTypesOfFunc x13 x3500
                                    in (let
                                        x2021 = leftSupply x2022
                                        x2018 = rightSupply x2022
                                         in (seq x2021 (seq x2018 (Curry_Prelude.nd_OP_gt_gt_eq (let
                                             x2019 = leftSupply x2018
                                             x2020 = rightSupply x2018
                                              in (seq x2019 (seq x2020 (let
                                                  x2017 = leftSupply x2019
                                                  x2010 = rightSupply x2019
                                                   in (seq x2017 (seq x2010 (let
                                                       x2013 = leftSupply x2020
                                                       x2016 = rightSupply x2020
                                                        in (seq x2013 (seq x2016 (nd_C_getCalledFuncs x1 x2 x3 x4 (let
                                                            x2009 = leftSupply x2010
                                                            x2008 = rightSupply x2010
                                                             in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2008 x3500) x5 (Curry_Prelude.d_OP_plus_plus x15 x16 x3500) x2009 x3500)))) (let
                                                            x2012 = leftSupply x2013
                                                            x2011 = rightSupply x2013
                                                             in (seq x2012 (seq x2011 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2011 x3500) x6 x17 x2012 x3500)))) (let
                                                            x2015 = leftSupply x2016
                                                            x2014 = rightSupply x2016
                                                             in (seq x2015 (seq x2014 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2014 x3500) x7 x19 x2015 x3500)))) (Curry_Prelude.d_OP_plus_plus x10 (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.d_OP_plus_plus x16 x18 x3500) x3500) x3500) x2017 x3500)))))))))) (wrapNX id (nd_OP_getCalledFuncs_dot___hash_lambda36 x13)) x2021 x3500))))))))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000 x3500) (nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readCurrentFlatCurry x3500) x3 x3500) (d_OP_requiredInCompactProg_dot___hash_lambda27 x3 x5 x1 x7) x3500
     Curry_Prelude.C_False -> d_OP__case_127 x1 x2 x3 x4 x6 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem C_Exports x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapDX id d_C_readCurrentFlatCurry) x2000 x3500) x3 x2001 x3500)))) (wrapNX id (nd_OP_requiredInCompactProg_dot___hash_lambda27 x3 x5 x1 x7)) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_127 x1 x2 x3 x4 x6 x7 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem C_Exports x2000 x3500) x2 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_128 x1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_127 x1 x2 x3 x4 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readCurrentFlatCurry x3500) x3 x3500) (d_OP_requiredInCompactProg_dot___hash_lambda28 x3 x6 x1 x7) x3500
     Curry_Prelude.C_False -> d_OP__case_126 x1 x2 x3 x4 x6 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_isMainOption x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x1 x2 x3 x4 x6 x7 x1002 x3500) (d_OP__case_127 x1 x2 x3 x4 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x1 x2 x3 x4 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x1 x2 x3 x4 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_127 x1 x2 x3 x4 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapDX id d_C_readCurrentFlatCurry) x2000 x3500) x3 x2001 x3500)))) (wrapNX id (nd_OP_requiredInCompactProg_dot___hash_lambda28 x3 x6 x1 x7)) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_126 x1 x2 x3 x4 x6 x7 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_any (wrapDX id d_C_isMainOption) x2000 x3500) x2 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_127 x1 x2 x3 x4 x6 x7 x1002 x3000 x3500) (nd_OP__case_127 x1 x2 x3 x4 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_127 x1 x2 x3 x4 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_127 x1 x2 x3 x4 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_126 x1 x2 x3 x4 x6 x7 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x8 = d_C_getMainFuncFromOptions x2 x3500
           in (d_OP__case_125 x1 x3 x4 x7 x8 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.OP_Tuple2 x4 x8) x3500) (Curry_Prelude.d_C_map d_C_functionName (d_C_moduleFuns x1 x3500) x3500) x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_124 x1 x3 x6 x7 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x1 x2 x3 x4 x6 x7 x1002 x3500) (d_OP__case_126 x1 x2 x3 x4 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x1 x2 x3 x4 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x1 x2 x3 x4 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_126 x1 x2 x3 x4 x6 x7 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2006 = x3000
           in (seq x2006 (let
               x8 = d_C_getMainFuncFromOptions x2 x3500
                in (let
                    x2005 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2005 (seq x2003 (nd_OP__case_125 x1 x3 x4 x7 x8 (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem (Curry_Prelude.OP_Tuple2 x4 x8) x2000 x3500) (Curry_Prelude.nd_C_map (wrapDX id d_C_functionName) (d_C_moduleFuns x1 x3500) x2001 x3500) x2002 x3500))))))) x2005 x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_124 x1 x3 x6 x7 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_126 x1 x2 x3 x4 x6 x7 x1002 x3000 x3500) (nd_OP__case_126 x1 x2 x3 x4 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_126 x1 x2 x3 x4 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_126 x1 x2 x3 x4 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_124 x1 x3 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readCurrentFlatCurry x3500) (Curry_List.d_C_nub (Curry_Prelude.d_OP_plus_plus x3 (d_C_moduleImports x1 x3500) x3500) x3500) x3500) (d_OP_requiredInCompactProg_dot___hash_lambda30 x6 x1 x7) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x1 x3 x6 x7 x1002 x3500) (d_OP__case_124 x1 x3 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x1 x3 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x1 x3 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_124 x1 x3 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapDX id d_C_readCurrentFlatCurry) x2000 x3500) (Curry_List.d_C_nub (Curry_Prelude.d_OP_plus_plus x3 (d_C_moduleImports x1 x3500) x3500) x3500) x2001 x3500)))) (wrapNX id (nd_OP_requiredInCompactProg_dot___hash_lambda30 x6 x1 x7)) x2003 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_124 x1 x3 x6 x7 x1002 x3000 x3500) (nd_OP__case_124 x1 x3 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_124 x1 x3 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_124 x1 x3 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_125 x1 x3 x4 x7 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO d_C_readCurrentFlatCurry x3500) x3 x3500) (d_OP_requiredInCompactProg_dot___hash_lambda29 x8 x3 x1 x4 x7) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x1 x3 x4 x7 x8 x1002 x3500) (d_OP__case_125 x1 x3 x4 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x1 x3 x4 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x1 x3 x4 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_125 x1 x3 x4 x7 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapDX id d_C_readCurrentFlatCurry) x2000 x3500) x3 x2001 x3500)))) (wrapNX id (nd_OP_requiredInCompactProg_dot___hash_lambda29 x8 x3 x1 x4 x7)) x2003 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_error) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_125 x1 x3 x4 x7 x8 x1002 x3000 x3500) (nd_OP__case_125 x1 x3 x4 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_125 x1 x3 x4 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_125 x1 x3 x4 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_130 x1 x2 x5 x4 x3500 = case x4 of
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> d_C_extendTConsWithConsType x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x6 x3500) x2 x3500) x5 x3500
     (Curry_FlatCurry.C_Type x10 x11 x12 x13) -> d_OP__case_129 x1 x2 x5 x10 x13 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x10 x3500) (d_C_defaultRequiredTypes x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_OP_extendTConsWithConsType_dot___hash_lambda17 x1) x3500) x13 x3500) x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x1 x2 x5 x1002 x3500) (d_OP__case_130 x1 x2 x5 x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x1 x2 x5 z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_130 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_C_extendTConsWithConsType x1 (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x6 x2001 x3500)))) x2 x2003 x3500)))) x5 x2005 x3500)))))
     (Curry_FlatCurry.C_Type x10 x11 x12 x13) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (nd_OP__case_129 x1 x2 x5 x10 x13 (let
                    x2002 = leftSupply x2006
                    x2005 = rightSupply x2006
                     in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_bar_bar (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem x10 x2000 x3500) (d_C_defaultRequiredTypes x3500) x2001 x3500)))) (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_any (wrapNX id (nd_OP_extendTConsWithConsType_dot___hash_lambda17 x1)) x2003 x3500) x13 x2004 x3500)))) x3500)))) x2007 x3500)))))
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_130 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_130 x1 x2 x5 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_130 x1 x2 x5 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_130 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_129 x1 x2 x5 x10 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_extendTConsWithConsType x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x10 x3500) x2 x3500) x5 x3500
     Curry_Prelude.C_False -> d_C_extendTConsWithConsType x1 x2 x5 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x1 x2 x5 x10 x13 x1002 x3500) (d_OP__case_129 x1 x2 x5 x10 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x1 x2 x5 x10 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x1 x2 x5 x10 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_129 x1 x2 x5 x10 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (nd_C_extendTConsWithConsType x1 (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x10 x2001 x3500)))) x2 x2003 x3500)))) x5 x2005 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_extendTConsWithConsType x1 x2 x5 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_129 x1 x2 x5 x10 x13 x1002 x3000 x3500) (nd_OP__case_129 x1 x2 x5 x10 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_129 x1 x2 x5 x10 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_129 x1 x2 x5 x10 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_131 x1 x7 x10 x11 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_filter (d_OP_newTypeConsOfTDecl_dot___hash_lambda15 x1) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_newTypeConsOfTDecl_dot___hash_lambda16 x3500) x10 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_131 x1 x7 x10 x1002 x3500) (d_OP__case_131 x1 x7 x10 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_131 x1 x7 x10 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_131 x1 x7 x10 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_131 x1 x7 x10 x11 x3000 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_newTypeConsOfTDecl_dot___hash_lambda15 x1)) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_newTypeConsOfTDecl_dot___hash_lambda16) x2000 x3500) x10 x2001 x3500)))) x2003 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_131 x1 x7 x10 x1002 x3000 x3500) (nd_OP__case_131 x1 x7 x10 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_131 x1 x7 x10 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_131 x1 x7 x10 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_132 x1 x3 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_filter (d_OP_newTypeConsOfTDecl_dot___hash_lambda14 x1) (d_C_allTypesOfTExpr x6 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_132 x1 x3 x6 x1002 x3500) (d_OP__case_132 x1 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_132 x1 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_132 x1 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_132 x1 x3 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_newTypeConsOfTDecl_dot___hash_lambda14 x1)) (d_C_allTypesOfTExpr x6 x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_132 x1 x3 x6 x1002 x3000 x3500) (nd_OP__case_132 x1 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_132 x1 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_132 x1 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_133 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_C_requiredDatatypes (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3500) x1 x3 x3500) x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_133 x1 x2 x3 x1002 x3500) (d_OP__case_133 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_133 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_133 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_133 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_requiredDatatypes (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x1 x3 x2001 x3500)))) x2 x2003 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_133 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_133 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_133 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_133 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_134 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_readCurrentFlatCurry x2 x3500) (d_OP_computeCompactFlatCurry_dot___hash_lambda8 x3) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_134 x2 x3 x1002 x3500) (d_OP__case_134 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_134 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_134 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_134 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_readCurrentFlatCurry x2 x3500) (wrapDX id (d_OP_computeCompactFlatCurry_dot___hash_lambda8 x3)) x2000 x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_134 x2 x3 x1002 x3000 x3500) (nd_OP__case_134 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_134 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_134 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_135 x1 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_135 x1 x4 x5 x1002 x3500) (d_OP__case_135 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_135 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_135 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_135 x1 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_135 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_135 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_135 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_135 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_137 x1 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_136 x1 x4 x5 (Curry_Prelude.d_OP_eq_eq x4 x1 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x1 x1002 x3500) (d_OP__case_137 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_137 x1 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_136 x1 x4 x5 (Curry_Prelude.d_OP_eq_eq x4 x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_137 x1 x1002 x3000 x3500) (nd_OP__case_137 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_137 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_137 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_136 x1 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_136 x1 x4 x5 x1002 x3500) (d_OP__case_136 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_136 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_136 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_136 x1 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_136 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_136 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_136 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_136 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_138 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_138 x1002 x3500) (d_OP__case_138 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_138 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_138 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_138 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_138 x1002 x3000 x3500) (nd_OP__case_138 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_138 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_138 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_139 x3 x2 x3500 = case x2 of
     (C_Main x4) -> x4
     C_Verbose -> d_C_getMainFuncFromOptions x3 x3500
     C_Exports -> d_C_getMainFuncFromOptions x3 x3500
     (C_InitFuncs x5) -> d_C_getMainFuncFromOptions x3 x3500
     (C_Required x6) -> d_C_getMainFuncFromOptions x3 x3500
     (C_Import x7) -> d_C_getMainFuncFromOptions x3 x3500
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x3 x1002 x3500) (d_OP__case_139 x3 x1003 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x3 z x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x3 x1002) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_139 x3 x2 x3000 x3500 = case x2 of
     (C_Main x4) -> x4
     C_Verbose -> d_C_getMainFuncFromOptions x3 x3500
     C_Exports -> d_C_getMainFuncFromOptions x3 x3500
     (C_InitFuncs x5) -> d_C_getMainFuncFromOptions x3 x3500
     (C_Required x6) -> d_C_getMainFuncFromOptions x3 x3500
     (C_Import x7) -> d_C_getMainFuncFromOptions x3 x3500
     (Choice_C_Option x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_139 x3 x1002 x3000 x3500) (nd_OP__case_139 x3 x1003 x3000 x3500)
     (Choices_C_Option x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_139 x3 z x3000 x3500) x1002
     (Guard_C_Option x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_139 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Option x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
