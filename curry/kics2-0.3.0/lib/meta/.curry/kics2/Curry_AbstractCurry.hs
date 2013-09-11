{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractCurry (C_CurryProg (..), C_CVisibility (..), C_CTypeDecl (..), C_CConsDecl (..), C_CTypeExpr (..), C_COpDecl (..), C_CFixity (..), C_CFuncDecl (..), C_CRules (..), C_CEvalAnnot (..), C_CRule (..), C_CLocalDecl (..), C_CExpr (..), C_CStatement (..), C_CPattern (..), C_CBranchExpr (..), C_CLiteral (..), C_QName, C_CTVarIName, C_CField, C_CLabel, C_CVarIName, d_C_readCurry, d_C_readUntypedCurry, d_C_readCurryWithParseOptions, d_C_readUntypedCurryWithParseOptions, d_C_abstractCurryFileName, d_C_untypedAbstractCurryFileName, d_C_readAbstractCurryFile, d_C_tryReadACYFile, d_C_writeAbstractCurryFile) where

import Basics
import qualified Curry_Char
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
type C_QName = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_CTVarIName = Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_CField t0 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0

type C_CLabel = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_CVarIName = Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)

data C_CurryProg
     = C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_CTypeDecl) (Curry_Prelude.OP_List C_CFuncDecl) (Curry_Prelude.OP_List C_COpDecl)
     | Choice_C_CurryProg Cover ID C_CurryProg C_CurryProg
     | Choices_C_CurryProg Cover ID ([C_CurryProg])
     | Fail_C_CurryProg Cover FailInfo
     | Guard_C_CurryProg Cover Constraints C_CurryProg

instance Show C_CurryProg where
  showsPrec d (Choice_C_CurryProg cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CurryProg cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CurryProg cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CurryProg cd info) = showChar '!'
  showsPrec _ (C_CurryProg x1 x2 x3 x4 x5) = (showString "(CurryProg") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read C_CurryProg where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CurryProg x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AbstractCurry" "CurryProg" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


instance NonDet C_CurryProg where
  choiceCons = Choice_C_CurryProg
  choicesCons = Choices_C_CurryProg
  failCons = Fail_C_CurryProg
  guardCons = Guard_C_CurryProg
  try (Choice_C_CurryProg cd i x y) = tryChoice cd i x y
  try (Choices_C_CurryProg cd i xs) = tryChoices cd i xs
  try (Fail_C_CurryProg cd info) = Fail cd info
  try (Guard_C_CurryProg cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CurryProg cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CurryProg cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CurryProg cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CurryProg cd i _) = error ("AbstractCurry.CurryProg.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CurryProg cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CurryProg cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CurryProg where
  generate s c = Choices_C_CurryProg c (freeID [5] s) [(C_CurryProg (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_CurryProg where
  ($!!) cont (C_CurryProg x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CurryProg y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CurryProg cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CurryProg cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CurryProg cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CurryProg cd info) _ _ = failCons cd info
  ($##) cont (C_CurryProg x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CurryProg y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CurryProg cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CurryProg cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CurryProg cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CurryProg cd info) _ _ = failCons cd info
  searchNF search cont (C_CurryProg x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_CurryProg y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CurryProg.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CurryProg where
  (=.=) (C_CurryProg x1 x2 x3 x4 x5) (C_CurryProg y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CurryProg x1 x2 x3 x4 x5) (C_CurryProg y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CurryProg x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_CurryProg cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CurryProg cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CurryProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CurryProg cd i _) = error ("AbstractCurry.CurryProg.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CurryProg cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CurryProg cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CurryProg x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_CurryProg cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CurryProg cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CurryProg cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CurryProg cd i _) = error ("AbstractCurry.CurryProg.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CurryProg cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CurryProg cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CurryProg where
  (=?=) (Choice_C_CurryProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CurryProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CurryProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CurryProg cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CurryProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CurryProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CurryProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CurryProg cd info) _ _ = failCons cd info
  (=?=) (C_CurryProg x1 x2 x3 x4 x5) (C_CurryProg y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_CurryProg cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CurryProg cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CurryProg cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CurryProg cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CurryProg cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CurryProg cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CurryProg cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CurryProg cd info) _ _ = failCons cd info
  (<?=) (C_CurryProg x1 x2 x3 x4 x5) (C_CurryProg y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_CVisibility
     = C_Public
     | C_Private
     | Choice_C_CVisibility Cover ID C_CVisibility C_CVisibility
     | Choices_C_CVisibility Cover ID ([C_CVisibility])
     | Fail_C_CVisibility Cover FailInfo
     | Guard_C_CVisibility Cover Constraints C_CVisibility

instance Show C_CVisibility where
  showsPrec d (Choice_C_CVisibility cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CVisibility cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CVisibility cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CVisibility cd info) = showChar '!'
  showsPrec _ C_Public = showString "Public"
  showsPrec _ C_Private = showString "Private"


instance Read C_CVisibility where
  readsPrec _ s = (readParen False (\r -> [ (C_Public,r0) | (_,r0) <- readQualified "AbstractCurry" "Public" r]) s) ++ (readParen False (\r -> [ (C_Private,r0) | (_,r0) <- readQualified "AbstractCurry" "Private" r]) s)


instance NonDet C_CVisibility where
  choiceCons = Choice_C_CVisibility
  choicesCons = Choices_C_CVisibility
  failCons = Fail_C_CVisibility
  guardCons = Guard_C_CVisibility
  try (Choice_C_CVisibility cd i x y) = tryChoice cd i x y
  try (Choices_C_CVisibility cd i xs) = tryChoices cd i xs
  try (Fail_C_CVisibility cd info) = Fail cd info
  try (Guard_C_CVisibility cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CVisibility cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CVisibility cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CVisibility cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CVisibility cd i _) = error ("AbstractCurry.CVisibility.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CVisibility cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CVisibility cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CVisibility where
  generate s c = Choices_C_CVisibility c (freeID [0,0] s) [C_Public,C_Private]


instance NormalForm C_CVisibility where
  ($!!) cont C_Public d cs = cont C_Public d cs
  ($!!) cont C_Private d cs = cont C_Private d cs
  ($!!) cont (Choice_C_CVisibility cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CVisibility cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CVisibility cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CVisibility cd info) _ _ = failCons cd info
  ($##) cont C_Public d cs = cont C_Public d cs
  ($##) cont C_Private d cs = cont C_Private d cs
  ($##) cont (Choice_C_CVisibility cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CVisibility cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CVisibility cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CVisibility cd info) _ _ = failCons cd info
  searchNF _ cont C_Public = cont C_Public
  searchNF _ cont C_Private = cont C_Private
  searchNF _ _ x = error ("AbstractCurry.CVisibility.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CVisibility where
  (=.=) C_Public C_Public d cs = C_Success
  (=.=) C_Private C_Private d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Public C_Public d cs = C_Success
  (=.<=) C_Private C_Private d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Public = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Private = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_CVisibility cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CVisibility cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CVisibility cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CVisibility cd i _) = error ("AbstractCurry.CVisibility.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CVisibility cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CVisibility cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Public = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Private = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_CVisibility cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CVisibility cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CVisibility cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CVisibility cd i _) = error ("AbstractCurry.CVisibility.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CVisibility cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CVisibility cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CVisibility where
  (=?=) (Choice_C_CVisibility cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CVisibility cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CVisibility cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CVisibility cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CVisibility cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CVisibility cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CVisibility cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CVisibility cd info) _ _ = failCons cd info
  (=?=) C_Public C_Public d cs = Curry_Prelude.C_True
  (=?=) C_Private C_Private d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CVisibility cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CVisibility cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CVisibility cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CVisibility cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CVisibility cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CVisibility cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CVisibility cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CVisibility cd info) _ _ = failCons cd info
  (<?=) C_Public C_Public d cs = Curry_Prelude.C_True
  (<?=) C_Public C_Private _ _ = Curry_Prelude.C_True
  (<?=) C_Private C_Private d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CTypeDecl
     = C_CType (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CVisibility (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List C_CConsDecl)
     | C_CTypeSyn (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CVisibility (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) C_CTypeExpr
     | Choice_C_CTypeDecl Cover ID C_CTypeDecl C_CTypeDecl
     | Choices_C_CTypeDecl Cover ID ([C_CTypeDecl])
     | Fail_C_CTypeDecl Cover FailInfo
     | Guard_C_CTypeDecl Cover Constraints C_CTypeDecl

instance Show C_CTypeDecl where
  showsPrec d (Choice_C_CTypeDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CTypeDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CTypeDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CTypeDecl cd info) = showChar '!'
  showsPrec _ (C_CType x1 x2 x3 x4) = (showString "(CType") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_CTypeSyn x1 x2 x3 x4) = (showString "(CTypeSyn") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_CTypeDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CType x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractCurry" "CType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ (readParen (d > 10) (\r -> [ (C_CTypeSyn x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractCurry" "CTypeSyn" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s)


instance NonDet C_CTypeDecl where
  choiceCons = Choice_C_CTypeDecl
  choicesCons = Choices_C_CTypeDecl
  failCons = Fail_C_CTypeDecl
  guardCons = Guard_C_CTypeDecl
  try (Choice_C_CTypeDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_CTypeDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_CTypeDecl cd info) = Fail cd info
  try (Guard_C_CTypeDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CTypeDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CTypeDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CTypeDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CTypeDecl cd i _) = error ("AbstractCurry.CTypeDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CTypeDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CTypeDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CTypeDecl where
  generate s c = Choices_C_CTypeDecl c (freeID [4,4] s) [(C_CType (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_CTypeSyn (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_CTypeDecl where
  ($!!) cont (C_CType x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CType y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CTypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CTypeSyn y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CTypeDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CTypeDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CTypeDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CTypeDecl cd info) _ _ = failCons cd info
  ($##) cont (C_CType x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CType y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CTypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CTypeSyn y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CTypeDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CTypeDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CTypeDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CTypeDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_CType x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_CType y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_CTypeSyn x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_CTypeSyn y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CTypeDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CTypeDecl where
  (=.=) (C_CType x1 x2 x3 x4) (C_CType y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_CTypeSyn x1 x2 x3 x4) (C_CTypeSyn y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CType x1 x2 x3 x4) (C_CType y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_CTypeSyn x1 x2 x3 x4) (C_CTypeSyn y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CType x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_CTypeSyn x3 x4 x5 x6) = ((i :=: (ChooseN 1 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_CTypeDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CTypeDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CTypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CTypeDecl cd i _) = error ("AbstractCurry.CTypeDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CTypeDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CTypeDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CType x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_CTypeSyn x3 x4 x5 x6) = [(i :=: (ChooseN 1 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_CTypeDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CTypeDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CTypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CTypeDecl cd i _) = error ("AbstractCurry.CTypeDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CTypeDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CTypeDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CTypeDecl where
  (=?=) (Choice_C_CTypeDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CTypeDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CTypeDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CTypeDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CTypeDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CTypeDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CTypeDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CTypeDecl cd info) _ _ = failCons cd info
  (=?=) (C_CType x1 x2 x3 x4) (C_CType y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_CTypeSyn x1 x2 x3 x4) (C_CTypeSyn y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CTypeDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CTypeDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CTypeDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CTypeDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CTypeDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CTypeDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CTypeDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CTypeDecl cd info) _ _ = failCons cd info
  (<?=) (C_CType x1 x2 x3 x4) (C_CType y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_CType _ _ _ _) (C_CTypeSyn _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CTypeSyn x1 x2 x3 x4) (C_CTypeSyn y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CConsDecl
     = C_CCons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_CVisibility (Curry_Prelude.OP_List C_CTypeExpr)
     | Choice_C_CConsDecl Cover ID C_CConsDecl C_CConsDecl
     | Choices_C_CConsDecl Cover ID ([C_CConsDecl])
     | Fail_C_CConsDecl Cover FailInfo
     | Guard_C_CConsDecl Cover Constraints C_CConsDecl

instance Show C_CConsDecl where
  showsPrec d (Choice_C_CConsDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CConsDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CConsDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CConsDecl cd info) = showChar '!'
  showsPrec _ (C_CCons x1 x2 x3 x4) = (showString "(CCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_CConsDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CCons x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractCurry" "CCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet C_CConsDecl where
  choiceCons = Choice_C_CConsDecl
  choicesCons = Choices_C_CConsDecl
  failCons = Fail_C_CConsDecl
  guardCons = Guard_C_CConsDecl
  try (Choice_C_CConsDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_CConsDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_CConsDecl cd info) = Fail cd info
  try (Guard_C_CConsDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CConsDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CConsDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CConsDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CConsDecl cd i _) = error ("AbstractCurry.CConsDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CConsDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CConsDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CConsDecl where
  generate s c = Choices_C_CConsDecl c (freeID [4] s) [(C_CCons (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_CConsDecl where
  ($!!) cont (C_CCons x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CCons y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CConsDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CConsDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CConsDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CConsDecl cd info) _ _ = failCons cd info
  ($##) cont (C_CCons x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CCons y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CConsDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CConsDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CConsDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CConsDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_CCons x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_CCons y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CConsDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CConsDecl where
  (=.=) (C_CCons x1 x2 x3 x4) (C_CCons y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CCons x1 x2 x3 x4) (C_CCons y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CCons x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_CConsDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CConsDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CConsDecl cd i _) = error ("AbstractCurry.CConsDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CConsDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CConsDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CCons x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_CConsDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CConsDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CConsDecl cd i _) = error ("AbstractCurry.CConsDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CConsDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CConsDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CConsDecl where
  (=?=) (Choice_C_CConsDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CConsDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CConsDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CConsDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CConsDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CConsDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CConsDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CConsDecl cd info) _ _ = failCons cd info
  (=?=) (C_CCons x1 x2 x3 x4) (C_CCons y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_CConsDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CConsDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CConsDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CConsDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CConsDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CConsDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CConsDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CConsDecl cd info) _ _ = failCons cd info
  (<?=) (C_CCons x1 x2 x3 x4) (C_CCons y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_CTypeExpr
     = C_CTVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_CFuncType C_CTypeExpr C_CTypeExpr
     | C_CTCons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_CTypeExpr)
     | C_CRecordType (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_CTypeExpr)) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | Choice_C_CTypeExpr Cover ID C_CTypeExpr C_CTypeExpr
     | Choices_C_CTypeExpr Cover ID ([C_CTypeExpr])
     | Fail_C_CTypeExpr Cover FailInfo
     | Guard_C_CTypeExpr Cover Constraints C_CTypeExpr

instance Show C_CTypeExpr where
  showsPrec d (Choice_C_CTypeExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CTypeExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CTypeExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CTypeExpr cd info) = showChar '!'
  showsPrec _ (C_CTVar x1) = (showString "(CTVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CFuncType x1 x2) = (showString "(CFuncType") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CTCons x1 x2) = (showString "(CTCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CRecordType x1 x2) = (showString "(CRecordType") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_CTypeExpr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CTVar x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CTVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CFuncType x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CFuncType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CTCons x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CTCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CRecordType x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CRecordType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))


instance NonDet C_CTypeExpr where
  choiceCons = Choice_C_CTypeExpr
  choicesCons = Choices_C_CTypeExpr
  failCons = Fail_C_CTypeExpr
  guardCons = Guard_C_CTypeExpr
  try (Choice_C_CTypeExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_CTypeExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_CTypeExpr cd info) = Fail cd info
  try (Guard_C_CTypeExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CTypeExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CTypeExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CTypeExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CTypeExpr cd i _) = error ("AbstractCurry.CTypeExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CTypeExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CTypeExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CTypeExpr where
  generate s c = Choices_C_CTypeExpr c (freeID [1,2,2,2] s) [(C_CTVar (generate (leftSupply s) c)),(C_CFuncType (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CTCons (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CRecordType (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_CTypeExpr where
  ($!!) cont (C_CTVar x1) d cs = (((\y1 d cs -> cont (C_CTVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CFuncType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CFuncType y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CTCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CTCons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CRecordType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecordType y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CTypeExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CTypeExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CTypeExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CTypeExpr cd info) _ _ = failCons cd info
  ($##) cont (C_CTVar x1) d cs = (((\y1 d cs -> cont (C_CTVar y1) d cs) $## x1) d) cs
  ($##) cont (C_CFuncType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CFuncType y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CTCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CTCons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CRecordType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecordType y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CTypeExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CTypeExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CTypeExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CTypeExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_CTVar x1) = search (\y1 -> cont (C_CTVar y1)) x1
  searchNF search cont (C_CFuncType x1 x2) = search (\y1 -> search (\y2 -> cont (C_CFuncType y1 y2)) x2) x1
  searchNF search cont (C_CTCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_CTCons y1 y2)) x2) x1
  searchNF search cont (C_CRecordType x1 x2) = search (\y1 -> search (\y2 -> cont (C_CRecordType y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CTypeExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CTypeExpr where
  (=.=) (C_CTVar x1) (C_CTVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CFuncType x1 x2) (C_CFuncType y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CTCons x1 x2) (C_CTCons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CRecordType x1 x2) (C_CRecordType y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CTVar x1) (C_CTVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CFuncType x1 x2) (C_CFuncType y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CTCons x1 x2) (C_CTCons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CRecordType x1 x2) (C_CRecordType y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CTVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CFuncType x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CTCons x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CRecordType x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_CTypeExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CTypeExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CTypeExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CTypeExpr cd i _) = error ("AbstractCurry.CTypeExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CTypeExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CTypeExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CTVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CFuncType x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CTCons x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CRecordType x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_CTypeExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CTypeExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CTypeExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CTypeExpr cd i _) = error ("AbstractCurry.CTypeExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CTypeExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CTypeExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CTypeExpr where
  (=?=) (Choice_C_CTypeExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CTypeExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CTypeExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CTypeExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CTypeExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CTypeExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CTypeExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CTypeExpr cd info) _ _ = failCons cd info
  (=?=) (C_CTVar x1) (C_CTVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CFuncType x1 x2) (C_CFuncType y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CTCons x1 x2) (C_CTCons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CRecordType x1 x2) (C_CRecordType y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CTypeExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CTypeExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CTypeExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CTypeExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CTypeExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CTypeExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CTypeExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CTypeExpr cd info) _ _ = failCons cd info
  (<?=) (C_CTVar x1) (C_CTVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CTVar _) (C_CFuncType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CTVar _) (C_CTCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CTVar _) (C_CRecordType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CFuncType x1 x2) (C_CFuncType y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CFuncType _ _) (C_CTCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CFuncType _ _) (C_CRecordType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CTCons x1 x2) (C_CTCons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CTCons _ _) (C_CRecordType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CRecordType x1 x2) (C_CRecordType y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_COpDecl
     = C_COp (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CFixity Curry_Prelude.C_Int
     | Choice_C_COpDecl Cover ID C_COpDecl C_COpDecl
     | Choices_C_COpDecl Cover ID ([C_COpDecl])
     | Fail_C_COpDecl Cover FailInfo
     | Guard_C_COpDecl Cover Constraints C_COpDecl

instance Show C_COpDecl where
  showsPrec d (Choice_C_COpDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_COpDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_COpDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_COpDecl cd info) = showChar '!'
  showsPrec _ (C_COp x1 x2 x3) = (showString "(COp") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_COpDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_COp x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractCurry" "COp" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_COpDecl where
  choiceCons = Choice_C_COpDecl
  choicesCons = Choices_C_COpDecl
  failCons = Fail_C_COpDecl
  guardCons = Guard_C_COpDecl
  try (Choice_C_COpDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_COpDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_COpDecl cd info) = Fail cd info
  try (Guard_C_COpDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_COpDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_COpDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_COpDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_COpDecl cd i _) = error ("AbstractCurry.COpDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_COpDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_COpDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_COpDecl where
  generate s c = Choices_C_COpDecl c (freeID [3] s) [(C_COp (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_COpDecl where
  ($!!) cont (C_COp x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_COp y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_COpDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_COpDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_COpDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_COpDecl cd info) _ _ = failCons cd info
  ($##) cont (C_COp x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_COp y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_COpDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_COpDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_COpDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_COpDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_COp x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_COp y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.COpDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_COpDecl where
  (=.=) (C_COp x1 x2 x3) (C_COp y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_COp x1 x2 x3) (C_COp y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_COp x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_COpDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_COpDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_COpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_COpDecl cd i _) = error ("AbstractCurry.COpDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_COpDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_COpDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_COp x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_COpDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_COpDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_COpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_COpDecl cd i _) = error ("AbstractCurry.COpDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_COpDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_COpDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_COpDecl where
  (=?=) (Choice_C_COpDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_COpDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_COpDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_COpDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_COpDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_COpDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_COpDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_COpDecl cd info) _ _ = failCons cd info
  (=?=) (C_COp x1 x2 x3) (C_COp y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_COpDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_COpDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_COpDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_COpDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_COpDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_COpDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_COpDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_COpDecl cd info) _ _ = failCons cd info
  (<?=) (C_COp x1 x2 x3) (C_COp y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_CFixity
     = C_CInfixOp
     | C_CInfixlOp
     | C_CInfixrOp
     | Choice_C_CFixity Cover ID C_CFixity C_CFixity
     | Choices_C_CFixity Cover ID ([C_CFixity])
     | Fail_C_CFixity Cover FailInfo
     | Guard_C_CFixity Cover Constraints C_CFixity

instance Show C_CFixity where
  showsPrec d (Choice_C_CFixity cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CFixity cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CFixity cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CFixity cd info) = showChar '!'
  showsPrec _ C_CInfixOp = showString "CInfixOp"
  showsPrec _ C_CInfixlOp = showString "CInfixlOp"
  showsPrec _ C_CInfixrOp = showString "CInfixrOp"


instance Read C_CFixity where
  readsPrec _ s = (readParen False (\r -> [ (C_CInfixOp,r0) | (_,r0) <- readQualified "AbstractCurry" "CInfixOp" r]) s) ++ ((readParen False (\r -> [ (C_CInfixlOp,r0) | (_,r0) <- readQualified "AbstractCurry" "CInfixlOp" r]) s) ++ (readParen False (\r -> [ (C_CInfixrOp,r0) | (_,r0) <- readQualified "AbstractCurry" "CInfixrOp" r]) s))


instance NonDet C_CFixity where
  choiceCons = Choice_C_CFixity
  choicesCons = Choices_C_CFixity
  failCons = Fail_C_CFixity
  guardCons = Guard_C_CFixity
  try (Choice_C_CFixity cd i x y) = tryChoice cd i x y
  try (Choices_C_CFixity cd i xs) = tryChoices cd i xs
  try (Fail_C_CFixity cd info) = Fail cd info
  try (Guard_C_CFixity cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CFixity cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CFixity cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CFixity cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CFixity cd i _) = error ("AbstractCurry.CFixity.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CFixity cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CFixity cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CFixity where
  generate s c = Choices_C_CFixity c (freeID [0,0,0] s) [C_CInfixOp,C_CInfixlOp,C_CInfixrOp]


instance NormalForm C_CFixity where
  ($!!) cont C_CInfixOp d cs = cont C_CInfixOp d cs
  ($!!) cont C_CInfixlOp d cs = cont C_CInfixlOp d cs
  ($!!) cont C_CInfixrOp d cs = cont C_CInfixrOp d cs
  ($!!) cont (Choice_C_CFixity cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CFixity cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CFixity cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CFixity cd info) _ _ = failCons cd info
  ($##) cont C_CInfixOp d cs = cont C_CInfixOp d cs
  ($##) cont C_CInfixlOp d cs = cont C_CInfixlOp d cs
  ($##) cont C_CInfixrOp d cs = cont C_CInfixrOp d cs
  ($##) cont (Choice_C_CFixity cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CFixity cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CFixity cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CFixity cd info) _ _ = failCons cd info
  searchNF _ cont C_CInfixOp = cont C_CInfixOp
  searchNF _ cont C_CInfixlOp = cont C_CInfixlOp
  searchNF _ cont C_CInfixrOp = cont C_CInfixrOp
  searchNF _ _ x = error ("AbstractCurry.CFixity.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CFixity where
  (=.=) C_CInfixOp C_CInfixOp d cs = C_Success
  (=.=) C_CInfixlOp C_CInfixlOp d cs = C_Success
  (=.=) C_CInfixrOp C_CInfixrOp d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_CInfixOp C_CInfixOp d cs = C_Success
  (=.<=) C_CInfixlOp C_CInfixlOp d cs = C_Success
  (=.<=) C_CInfixrOp C_CInfixrOp d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_CInfixOp = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_CInfixlOp = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_CInfixrOp = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_CFixity cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CFixity cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CFixity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CFixity cd i _) = error ("AbstractCurry.CFixity.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CFixity cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CFixity cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_CInfixOp = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_CInfixlOp = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_CInfixrOp = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_CFixity cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CFixity cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CFixity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CFixity cd i _) = error ("AbstractCurry.CFixity.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CFixity cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CFixity cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CFixity where
  (=?=) (Choice_C_CFixity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CFixity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CFixity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CFixity cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CFixity cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CFixity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CFixity cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CFixity cd info) _ _ = failCons cd info
  (=?=) C_CInfixOp C_CInfixOp d cs = Curry_Prelude.C_True
  (=?=) C_CInfixlOp C_CInfixlOp d cs = Curry_Prelude.C_True
  (=?=) C_CInfixrOp C_CInfixrOp d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CFixity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CFixity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CFixity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CFixity cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CFixity cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CFixity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CFixity cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CFixity cd info) _ _ = failCons cd info
  (<?=) C_CInfixOp C_CInfixOp d cs = Curry_Prelude.C_True
  (<?=) C_CInfixOp C_CInfixlOp _ _ = Curry_Prelude.C_True
  (<?=) C_CInfixOp C_CInfixrOp _ _ = Curry_Prelude.C_True
  (<?=) C_CInfixlOp C_CInfixlOp d cs = Curry_Prelude.C_True
  (<?=) C_CInfixlOp C_CInfixrOp _ _ = Curry_Prelude.C_True
  (<?=) C_CInfixrOp C_CInfixrOp d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CFuncDecl
     = C_CFunc (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_CVisibility C_CTypeExpr C_CRules
     | C_CmtFunc (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_CVisibility C_CTypeExpr C_CRules
     | Choice_C_CFuncDecl Cover ID C_CFuncDecl C_CFuncDecl
     | Choices_C_CFuncDecl Cover ID ([C_CFuncDecl])
     | Fail_C_CFuncDecl Cover FailInfo
     | Guard_C_CFuncDecl Cover Constraints C_CFuncDecl

instance Show C_CFuncDecl where
  showsPrec d (Choice_C_CFuncDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CFuncDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CFuncDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CFuncDecl cd info) = showChar '!'
  showsPrec _ (C_CFunc x1 x2 x3 x4 x5) = (showString "(CFunc") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))
  showsPrec _ (C_CmtFunc x1 x2 x3 x4 x5 x6) = (showString "(CmtFunc") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . (showChar ')')))))))))))))


instance Read C_CFuncDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CFunc x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AbstractCurry" "CFunc" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s) ++ (readParen (d > 10) (\r -> [ (C_CmtFunc x1 x2 x3 x4 x5 x6,r6) | (_,r0) <- readQualified "AbstractCurry" "CmtFunc" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5]) s)


instance NonDet C_CFuncDecl where
  choiceCons = Choice_C_CFuncDecl
  choicesCons = Choices_C_CFuncDecl
  failCons = Fail_C_CFuncDecl
  guardCons = Guard_C_CFuncDecl
  try (Choice_C_CFuncDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_CFuncDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_CFuncDecl cd info) = Fail cd info
  try (Guard_C_CFuncDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CFuncDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CFuncDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CFuncDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CFuncDecl cd i _) = error ("AbstractCurry.CFuncDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CFuncDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CFuncDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CFuncDecl where
  generate s c = Choices_C_CFuncDecl c (freeID [5,6] s) [(C_CFunc (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_CmtFunc (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_CFuncDecl where
  ($!!) cont (C_CFunc x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CFunc y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CmtFunc x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CFuncDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CFuncDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CFuncDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CFuncDecl cd info) _ _ = failCons cd info
  ($##) cont (C_CFunc x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CFunc y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CmtFunc x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CFuncDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CFuncDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CFuncDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CFuncDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_CFunc x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_CFunc y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF search cont (C_CmtFunc x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (C_CmtFunc y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CFuncDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CFuncDecl where
  (=.=) (C_CFunc x1 x2 x3 x4 x5) (C_CFunc y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_CmtFunc x1 x2 x3 x4 x5 x6) (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CFunc x1 x2 x3 x4 x5) (C_CFunc y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_CmtFunc x1 x2 x3 x4 x5 x6) (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CFunc x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind cd i (C_CmtFunc x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 1 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_C_CFuncDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CFuncDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CFuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CFuncDecl cd i _) = error ("AbstractCurry.CFuncDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CFuncDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CFuncDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CFunc x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind cd i (C_CmtFunc x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 1 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_C_CFuncDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CFuncDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CFuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CFuncDecl cd i _) = error ("AbstractCurry.CFuncDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CFuncDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CFuncDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CFuncDecl where
  (=?=) (Choice_C_CFuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CFuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CFuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CFuncDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CFuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CFuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CFuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CFuncDecl cd info) _ _ = failCons cd info
  (=?=) (C_CFunc x1 x2 x3 x4 x5) (C_CFunc y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) (C_CmtFunc x1 x2 x3 x4 x5 x6) (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.=?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CFuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CFuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CFuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CFuncDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CFuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CFuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CFuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CFuncDecl cd info) _ _ = failCons cd info
  (<?=) (C_CFunc x1 x2 x3 x4 x5) (C_CFunc y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_CFunc _ _ _ _ _) (C_CmtFunc _ _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CmtFunc x1 x2 x3 x4 x5 x6) (C_CmtFunc y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.<?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CRules
     = C_CRules C_CEvalAnnot (Curry_Prelude.OP_List C_CRule)
     | C_CExternal (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_CRules Cover ID C_CRules C_CRules
     | Choices_C_CRules Cover ID ([C_CRules])
     | Fail_C_CRules Cover FailInfo
     | Guard_C_CRules Cover Constraints C_CRules

instance Show C_CRules where
  showsPrec d (Choice_C_CRules cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CRules cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CRules cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CRules cd info) = showChar '!'
  showsPrec _ (C_CRules x1 x2) = (showString "(CRules") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CExternal x1) = (showString "(CExternal") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_CRules where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CRules x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CRules" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CExternal x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CExternal" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_CRules where
  choiceCons = Choice_C_CRules
  choicesCons = Choices_C_CRules
  failCons = Fail_C_CRules
  guardCons = Guard_C_CRules
  try (Choice_C_CRules cd i x y) = tryChoice cd i x y
  try (Choices_C_CRules cd i xs) = tryChoices cd i xs
  try (Fail_C_CRules cd info) = Fail cd info
  try (Guard_C_CRules cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CRules cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CRules cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CRules cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CRules cd i _) = error ("AbstractCurry.CRules.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CRules cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CRules cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CRules where
  generate s c = Choices_C_CRules c (freeID [2,1] s) [(C_CRules (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CExternal (generate (leftSupply s) c))]


instance NormalForm C_CRules where
  ($!!) cont (C_CRules x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRules y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CExternal x1) d cs = (((\y1 d cs -> cont (C_CExternal y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CRules cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CRules cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CRules cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CRules cd info) _ _ = failCons cd info
  ($##) cont (C_CRules x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRules y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CExternal x1) d cs = (((\y1 d cs -> cont (C_CExternal y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_CRules cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CRules cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CRules cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CRules cd info) _ _ = failCons cd info
  searchNF search cont (C_CRules x1 x2) = search (\y1 -> search (\y2 -> cont (C_CRules y1 y2)) x2) x1
  searchNF search cont (C_CExternal x1) = search (\y1 -> cont (C_CExternal y1)) x1
  searchNF _ _ x = error ("AbstractCurry.CRules.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CRules where
  (=.=) (C_CRules x1 x2) (C_CRules y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CExternal x1) (C_CExternal y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CRules x1 x2) (C_CRules y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CExternal x1) (C_CExternal y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CRules x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CExternal x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_CRules cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CRules cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CRules cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CRules cd i _) = error ("AbstractCurry.CRules.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CRules cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CRules cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CRules x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CExternal x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_CRules cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CRules cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CRules cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CRules cd i _) = error ("AbstractCurry.CRules.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CRules cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CRules cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CRules where
  (=?=) (Choice_C_CRules cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CRules cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CRules cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CRules cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CRules cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CRules cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CRules cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CRules cd info) _ _ = failCons cd info
  (=?=) (C_CRules x1 x2) (C_CRules y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CExternal x1) (C_CExternal y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CRules cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CRules cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CRules cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CRules cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CRules cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CRules cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CRules cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CRules cd info) _ _ = failCons cd info
  (<?=) (C_CRules x1 x2) (C_CRules y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CRules _ _) (C_CExternal _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CExternal x1) (C_CExternal y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CEvalAnnot
     = C_CFlex
     | C_CRigid
     | C_CChoice
     | Choice_C_CEvalAnnot Cover ID C_CEvalAnnot C_CEvalAnnot
     | Choices_C_CEvalAnnot Cover ID ([C_CEvalAnnot])
     | Fail_C_CEvalAnnot Cover FailInfo
     | Guard_C_CEvalAnnot Cover Constraints C_CEvalAnnot

instance Show C_CEvalAnnot where
  showsPrec d (Choice_C_CEvalAnnot cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CEvalAnnot cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CEvalAnnot cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CEvalAnnot cd info) = showChar '!'
  showsPrec _ C_CFlex = showString "CFlex"
  showsPrec _ C_CRigid = showString "CRigid"
  showsPrec _ C_CChoice = showString "CChoice"


instance Read C_CEvalAnnot where
  readsPrec _ s = (readParen False (\r -> [ (C_CFlex,r0) | (_,r0) <- readQualified "AbstractCurry" "CFlex" r]) s) ++ ((readParen False (\r -> [ (C_CRigid,r0) | (_,r0) <- readQualified "AbstractCurry" "CRigid" r]) s) ++ (readParen False (\r -> [ (C_CChoice,r0) | (_,r0) <- readQualified "AbstractCurry" "CChoice" r]) s))


instance NonDet C_CEvalAnnot where
  choiceCons = Choice_C_CEvalAnnot
  choicesCons = Choices_C_CEvalAnnot
  failCons = Fail_C_CEvalAnnot
  guardCons = Guard_C_CEvalAnnot
  try (Choice_C_CEvalAnnot cd i x y) = tryChoice cd i x y
  try (Choices_C_CEvalAnnot cd i xs) = tryChoices cd i xs
  try (Fail_C_CEvalAnnot cd info) = Fail cd info
  try (Guard_C_CEvalAnnot cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CEvalAnnot cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CEvalAnnot cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CEvalAnnot cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CEvalAnnot cd i _) = error ("AbstractCurry.CEvalAnnot.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CEvalAnnot cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CEvalAnnot cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CEvalAnnot where
  generate s c = Choices_C_CEvalAnnot c (freeID [0,0,0] s) [C_CFlex,C_CRigid,C_CChoice]


instance NormalForm C_CEvalAnnot where
  ($!!) cont C_CFlex d cs = cont C_CFlex d cs
  ($!!) cont C_CRigid d cs = cont C_CRigid d cs
  ($!!) cont C_CChoice d cs = cont C_CChoice d cs
  ($!!) cont (Choice_C_CEvalAnnot cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CEvalAnnot cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CEvalAnnot cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CEvalAnnot cd info) _ _ = failCons cd info
  ($##) cont C_CFlex d cs = cont C_CFlex d cs
  ($##) cont C_CRigid d cs = cont C_CRigid d cs
  ($##) cont C_CChoice d cs = cont C_CChoice d cs
  ($##) cont (Choice_C_CEvalAnnot cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CEvalAnnot cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CEvalAnnot cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CEvalAnnot cd info) _ _ = failCons cd info
  searchNF _ cont C_CFlex = cont C_CFlex
  searchNF _ cont C_CRigid = cont C_CRigid
  searchNF _ cont C_CChoice = cont C_CChoice
  searchNF _ _ x = error ("AbstractCurry.CEvalAnnot.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CEvalAnnot where
  (=.=) C_CFlex C_CFlex d cs = C_Success
  (=.=) C_CRigid C_CRigid d cs = C_Success
  (=.=) C_CChoice C_CChoice d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_CFlex C_CFlex d cs = C_Success
  (=.<=) C_CRigid C_CRigid d cs = C_Success
  (=.<=) C_CChoice C_CChoice d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_CFlex = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_CRigid = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_CChoice = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_CEvalAnnot cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CEvalAnnot cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CEvalAnnot cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CEvalAnnot cd i _) = error ("AbstractCurry.CEvalAnnot.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CEvalAnnot cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CEvalAnnot cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_CFlex = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_CRigid = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_CChoice = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_CEvalAnnot cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CEvalAnnot cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CEvalAnnot cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CEvalAnnot cd i _) = error ("AbstractCurry.CEvalAnnot.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CEvalAnnot cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CEvalAnnot cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CEvalAnnot where
  (=?=) (Choice_C_CEvalAnnot cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CEvalAnnot cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CEvalAnnot cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CEvalAnnot cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CEvalAnnot cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CEvalAnnot cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CEvalAnnot cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CEvalAnnot cd info) _ _ = failCons cd info
  (=?=) C_CFlex C_CFlex d cs = Curry_Prelude.C_True
  (=?=) C_CRigid C_CRigid d cs = Curry_Prelude.C_True
  (=?=) C_CChoice C_CChoice d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CEvalAnnot cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CEvalAnnot cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CEvalAnnot cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CEvalAnnot cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CEvalAnnot cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CEvalAnnot cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CEvalAnnot cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CEvalAnnot cd info) _ _ = failCons cd info
  (<?=) C_CFlex C_CFlex d cs = Curry_Prelude.C_True
  (<?=) C_CFlex C_CRigid _ _ = Curry_Prelude.C_True
  (<?=) C_CFlex C_CChoice _ _ = Curry_Prelude.C_True
  (<?=) C_CRigid C_CRigid d cs = Curry_Prelude.C_True
  (<?=) C_CRigid C_CChoice _ _ = Curry_Prelude.C_True
  (<?=) C_CChoice C_CChoice d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CRule
     = C_CRule (Curry_Prelude.OP_List C_CPattern) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_CExpr C_CExpr)) (Curry_Prelude.OP_List C_CLocalDecl)
     | Choice_C_CRule Cover ID C_CRule C_CRule
     | Choices_C_CRule Cover ID ([C_CRule])
     | Fail_C_CRule Cover FailInfo
     | Guard_C_CRule Cover Constraints C_CRule

instance Show C_CRule where
  showsPrec d (Choice_C_CRule cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CRule cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CRule cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CRule cd info) = showChar '!'
  showsPrec _ (C_CRule x1 x2 x3) = (showString "(CRule") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_CRule where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CRule x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractCurry" "CRule" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_CRule where
  choiceCons = Choice_C_CRule
  choicesCons = Choices_C_CRule
  failCons = Fail_C_CRule
  guardCons = Guard_C_CRule
  try (Choice_C_CRule cd i x y) = tryChoice cd i x y
  try (Choices_C_CRule cd i xs) = tryChoices cd i xs
  try (Fail_C_CRule cd info) = Fail cd info
  try (Guard_C_CRule cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CRule cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CRule cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CRule cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CRule cd i _) = error ("AbstractCurry.CRule.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CRule cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CRule cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CRule where
  generate s c = Choices_C_CRule c (freeID [3] s) [(C_CRule (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_CRule where
  ($!!) cont (C_CRule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CRule y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CRule cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CRule cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CRule cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CRule cd info) _ _ = failCons cd info
  ($##) cont (C_CRule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CRule y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CRule cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CRule cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CRule cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CRule cd info) _ _ = failCons cd info
  searchNF search cont (C_CRule x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_CRule y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CRule.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CRule where
  (=.=) (C_CRule x1 x2 x3) (C_CRule y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CRule x1 x2 x3) (C_CRule y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CRule x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_CRule cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CRule cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CRule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CRule cd i _) = error ("AbstractCurry.CRule.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CRule cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CRule cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CRule x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_CRule cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CRule cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CRule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CRule cd i _) = error ("AbstractCurry.CRule.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CRule cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CRule cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CRule where
  (=?=) (Choice_C_CRule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CRule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CRule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CRule cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CRule cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CRule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CRule cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CRule cd info) _ _ = failCons cd info
  (=?=) (C_CRule x1 x2 x3) (C_CRule y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_CRule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CRule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CRule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CRule cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CRule cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CRule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CRule cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CRule cd info) _ _ = failCons cd info
  (<?=) (C_CRule x1 x2 x3) (C_CRule y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_CLocalDecl
     = C_CLocalFunc C_CFuncDecl
     | C_CLocalPat C_CPattern C_CExpr (Curry_Prelude.OP_List C_CLocalDecl)
     | C_CLocalVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | Choice_C_CLocalDecl Cover ID C_CLocalDecl C_CLocalDecl
     | Choices_C_CLocalDecl Cover ID ([C_CLocalDecl])
     | Fail_C_CLocalDecl Cover FailInfo
     | Guard_C_CLocalDecl Cover Constraints C_CLocalDecl

instance Show C_CLocalDecl where
  showsPrec d (Choice_C_CLocalDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CLocalDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CLocalDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CLocalDecl cd info) = showChar '!'
  showsPrec _ (C_CLocalFunc x1) = (showString "(CLocalFunc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CLocalPat x1 x2 x3) = (showString "(CLocalPat") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_CLocalVar x1) = (showString "(CLocalVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_CLocalDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CLocalFunc x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CLocalFunc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CLocalPat x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractCurry" "CLocalPat" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_CLocalVar x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CLocalVar" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_CLocalDecl where
  choiceCons = Choice_C_CLocalDecl
  choicesCons = Choices_C_CLocalDecl
  failCons = Fail_C_CLocalDecl
  guardCons = Guard_C_CLocalDecl
  try (Choice_C_CLocalDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_CLocalDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_CLocalDecl cd info) = Fail cd info
  try (Guard_C_CLocalDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CLocalDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CLocalDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CLocalDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CLocalDecl cd i _) = error ("AbstractCurry.CLocalDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CLocalDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CLocalDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CLocalDecl where
  generate s c = Choices_C_CLocalDecl c (freeID [1,3,1] s) [(C_CLocalFunc (generate (leftSupply s) c)),(C_CLocalPat (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_CLocalVar (generate (leftSupply s) c))]


instance NormalForm C_CLocalDecl where
  ($!!) cont (C_CLocalFunc x1) d cs = (((\y1 d cs -> cont (C_CLocalFunc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CLocalPat x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CLocalPat y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CLocalVar x1) d cs = (((\y1 d cs -> cont (C_CLocalVar y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CLocalDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CLocalDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CLocalDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CLocalDecl cd info) _ _ = failCons cd info
  ($##) cont (C_CLocalFunc x1) d cs = (((\y1 d cs -> cont (C_CLocalFunc y1) d cs) $## x1) d) cs
  ($##) cont (C_CLocalPat x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_CLocalPat y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CLocalVar x1) d cs = (((\y1 d cs -> cont (C_CLocalVar y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_CLocalDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CLocalDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CLocalDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CLocalDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_CLocalFunc x1) = search (\y1 -> cont (C_CLocalFunc y1)) x1
  searchNF search cont (C_CLocalPat x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_CLocalPat y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_CLocalVar x1) = search (\y1 -> cont (C_CLocalVar y1)) x1
  searchNF _ _ x = error ("AbstractCurry.CLocalDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CLocalDecl where
  (=.=) (C_CLocalFunc x1) (C_CLocalFunc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CLocalPat x1 x2 x3) (C_CLocalPat y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_CLocalVar x1) (C_CLocalVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CLocalFunc x1) (C_CLocalFunc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CLocalPat x1 x2 x3) (C_CLocalPat y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_CLocalVar x1) (C_CLocalVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CLocalFunc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CLocalPat x3 x4 x5) = ((i :=: (ChooseN 1 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_CLocalVar x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_CLocalDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CLocalDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CLocalDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CLocalDecl cd i _) = error ("AbstractCurry.CLocalDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CLocalDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CLocalDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CLocalFunc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CLocalPat x3 x4 x5) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_CLocalVar x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_CLocalDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CLocalDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CLocalDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CLocalDecl cd i _) = error ("AbstractCurry.CLocalDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CLocalDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CLocalDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CLocalDecl where
  (=?=) (Choice_C_CLocalDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CLocalDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CLocalDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CLocalDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CLocalDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CLocalDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CLocalDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CLocalDecl cd info) _ _ = failCons cd info
  (=?=) (C_CLocalFunc x1) (C_CLocalFunc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CLocalPat x1 x2 x3) (C_CLocalPat y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_CLocalVar x1) (C_CLocalVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CLocalDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CLocalDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CLocalDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CLocalDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CLocalDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CLocalDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CLocalDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CLocalDecl cd info) _ _ = failCons cd info
  (<?=) (C_CLocalFunc x1) (C_CLocalFunc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CLocalFunc _) (C_CLocalPat _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLocalFunc _) (C_CLocalVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLocalPat x1 x2 x3) (C_CLocalPat y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_CLocalPat _ _ _) (C_CLocalVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLocalVar x1) (C_CLocalVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CExpr
     = C_CVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_CLit C_CLiteral
     | C_CSymbol (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_CApply C_CExpr C_CExpr
     | C_CLambda (Curry_Prelude.OP_List C_CPattern) C_CExpr
     | C_CLetDecl (Curry_Prelude.OP_List C_CLocalDecl) C_CExpr
     | C_CDoExpr (Curry_Prelude.OP_List C_CStatement)
     | C_CListComp C_CExpr (Curry_Prelude.OP_List C_CStatement)
     | C_CCase C_CExpr (Curry_Prelude.OP_List C_CBranchExpr)
     | C_CRecConstr (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_CExpr))
     | C_CRecSelect C_CExpr (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_CRecUpdate (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_CExpr)) C_CExpr
     | Choice_C_CExpr Cover ID C_CExpr C_CExpr
     | Choices_C_CExpr Cover ID ([C_CExpr])
     | Fail_C_CExpr Cover FailInfo
     | Guard_C_CExpr Cover Constraints C_CExpr

instance Show C_CExpr where
  showsPrec d (Choice_C_CExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CExpr cd info) = showChar '!'
  showsPrec _ (C_CVar x1) = (showString "(CVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CLit x1) = (showString "(CLit") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CSymbol x1) = (showString "(CSymbol") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CApply x1 x2) = (showString "(CApply") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CLambda x1 x2) = (showString "(CLambda") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CLetDecl x1 x2) = (showString "(CLetDecl") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CDoExpr x1) = (showString "(CDoExpr") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CListComp x1 x2) = (showString "(CListComp") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CCase x1 x2) = (showString "(CCase") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CRecConstr x1) = (showString "(CRecConstr") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CRecSelect x1 x2) = (showString "(CRecSelect") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CRecUpdate x1 x2) = (showString "(CRecUpdate") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_CExpr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CVar x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CLit x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CLit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CSymbol x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CSymbol" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CApply x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CApply" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CLambda x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CLambda" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CLetDecl x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CLetDecl" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CDoExpr x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CDoExpr" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CListComp x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CListComp" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CCase x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CCase" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CRecConstr x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CRecConstr" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CRecSelect x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CRecSelect" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CRecUpdate x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CRecUpdate" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))))))))))


instance NonDet C_CExpr where
  choiceCons = Choice_C_CExpr
  choicesCons = Choices_C_CExpr
  failCons = Fail_C_CExpr
  guardCons = Guard_C_CExpr
  try (Choice_C_CExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_CExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_CExpr cd info) = Fail cd info
  try (Guard_C_CExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CExpr cd i _) = error ("AbstractCurry.CExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CExpr where
  generate s c = Choices_C_CExpr c (freeID [1,1,1,2,2,2,1,2,2,1,2,2] s) [(C_CVar (generate (leftSupply s) c)),(C_CLit (generate (leftSupply s) c)),(C_CSymbol (generate (leftSupply s) c)),(C_CApply (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CLambda (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CLetDecl (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CDoExpr (generate (leftSupply s) c)),(C_CListComp (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CCase (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CRecConstr (generate (leftSupply s) c)),(C_CRecSelect (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CRecUpdate (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_CExpr where
  ($!!) cont (C_CVar x1) d cs = (((\y1 d cs -> cont (C_CVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CLit x1) d cs = (((\y1 d cs -> cont (C_CLit y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CSymbol x1) d cs = (((\y1 d cs -> cont (C_CSymbol y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CApply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CApply y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CLambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CLambda y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CLetDecl x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CLetDecl y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CDoExpr x1) d cs = (((\y1 d cs -> cont (C_CDoExpr y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CListComp x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CListComp y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CCase y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CRecConstr x1) d cs = (((\y1 d cs -> cont (C_CRecConstr y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CRecSelect x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecSelect y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CRecUpdate x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecUpdate y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CExpr cd info) _ _ = failCons cd info
  ($##) cont (C_CVar x1) d cs = (((\y1 d cs -> cont (C_CVar y1) d cs) $## x1) d) cs
  ($##) cont (C_CLit x1) d cs = (((\y1 d cs -> cont (C_CLit y1) d cs) $## x1) d) cs
  ($##) cont (C_CSymbol x1) d cs = (((\y1 d cs -> cont (C_CSymbol y1) d cs) $## x1) d) cs
  ($##) cont (C_CApply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CApply y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CLambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CLambda y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CLetDecl x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CLetDecl y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CDoExpr x1) d cs = (((\y1 d cs -> cont (C_CDoExpr y1) d cs) $## x1) d) cs
  ($##) cont (C_CListComp x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CListComp y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CCase x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CCase y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CRecConstr x1) d cs = (((\y1 d cs -> cont (C_CRecConstr y1) d cs) $## x1) d) cs
  ($##) cont (C_CRecSelect x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecSelect y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CRecUpdate x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CRecUpdate y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_CVar x1) = search (\y1 -> cont (C_CVar y1)) x1
  searchNF search cont (C_CLit x1) = search (\y1 -> cont (C_CLit y1)) x1
  searchNF search cont (C_CSymbol x1) = search (\y1 -> cont (C_CSymbol y1)) x1
  searchNF search cont (C_CApply x1 x2) = search (\y1 -> search (\y2 -> cont (C_CApply y1 y2)) x2) x1
  searchNF search cont (C_CLambda x1 x2) = search (\y1 -> search (\y2 -> cont (C_CLambda y1 y2)) x2) x1
  searchNF search cont (C_CLetDecl x1 x2) = search (\y1 -> search (\y2 -> cont (C_CLetDecl y1 y2)) x2) x1
  searchNF search cont (C_CDoExpr x1) = search (\y1 -> cont (C_CDoExpr y1)) x1
  searchNF search cont (C_CListComp x1 x2) = search (\y1 -> search (\y2 -> cont (C_CListComp y1 y2)) x2) x1
  searchNF search cont (C_CCase x1 x2) = search (\y1 -> search (\y2 -> cont (C_CCase y1 y2)) x2) x1
  searchNF search cont (C_CRecConstr x1) = search (\y1 -> cont (C_CRecConstr y1)) x1
  searchNF search cont (C_CRecSelect x1 x2) = search (\y1 -> search (\y2 -> cont (C_CRecSelect y1 y2)) x2) x1
  searchNF search cont (C_CRecUpdate x1 x2) = search (\y1 -> search (\y2 -> cont (C_CRecUpdate y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CExpr where
  (=.=) (C_CVar x1) (C_CVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CLit x1) (C_CLit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CSymbol x1) (C_CSymbol y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CApply x1 x2) (C_CApply y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CLambda x1 x2) (C_CLambda y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CLetDecl x1 x2) (C_CLetDecl y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CDoExpr x1) (C_CDoExpr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CListComp x1 x2) (C_CListComp y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CCase x1 x2) (C_CCase y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CRecConstr x1) (C_CRecConstr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CRecSelect x1 x2) (C_CRecSelect y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CRecUpdate x1 x2) (C_CRecUpdate y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CVar x1) (C_CVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CLit x1) (C_CLit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CSymbol x1) (C_CSymbol y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CApply x1 x2) (C_CApply y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CLambda x1 x2) (C_CLambda y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CLetDecl x1 x2) (C_CLetDecl y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CDoExpr x1) (C_CDoExpr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CListComp x1 x2) (C_CListComp y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CCase x1 x2) (C_CCase y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CRecConstr x1) (C_CRecConstr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CRecSelect x1 x2) (C_CRecSelect y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CRecUpdate x1 x2) (C_CRecUpdate y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CLit x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CSymbol x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CApply x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CLambda x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CLetDecl x3 x4) = ((i :=: (ChooseN 5 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CDoExpr x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CListComp x3 x4) = ((i :=: (ChooseN 7 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CCase x3 x4) = ((i :=: (ChooseN 8 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CRecConstr x3) = ((i :=: (ChooseN 9 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CRecSelect x3 x4) = ((i :=: (ChooseN 10 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CRecUpdate x3 x4) = ((i :=: (ChooseN 11 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_CExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CExpr cd i _) = error ("AbstractCurry.CExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CLit x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CSymbol x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CApply x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CLambda x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CLetDecl x3 x4) = [(i :=: (ChooseN 5 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CDoExpr x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CListComp x3 x4) = [(i :=: (ChooseN 7 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CCase x3 x4) = [(i :=: (ChooseN 8 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CRecConstr x3) = [(i :=: (ChooseN 9 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CRecSelect x3 x4) = [(i :=: (ChooseN 10 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CRecUpdate x3 x4) = [(i :=: (ChooseN 11 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_CExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CExpr cd i _) = error ("AbstractCurry.CExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CExpr where
  (=?=) (Choice_C_CExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CExpr cd info) _ _ = failCons cd info
  (=?=) (C_CVar x1) (C_CVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CLit x1) (C_CLit y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CSymbol x1) (C_CSymbol y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CApply x1 x2) (C_CApply y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CLambda x1 x2) (C_CLambda y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CLetDecl x1 x2) (C_CLetDecl y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CDoExpr x1) (C_CDoExpr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CListComp x1 x2) (C_CListComp y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CCase x1 x2) (C_CCase y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CRecConstr x1) (C_CRecConstr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CRecSelect x1 x2) (C_CRecSelect y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CRecUpdate x1 x2) (C_CRecUpdate y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CExpr cd info) _ _ = failCons cd info
  (<?=) (C_CVar x1) (C_CVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CVar _) (C_CLit _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CSymbol _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CLetDecl _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CVar _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit x1) (C_CLit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CLit _) (C_CSymbol _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CLetDecl _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLit _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol x1) (C_CSymbol y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CSymbol _) (C_CApply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CLetDecl _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSymbol _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply x1 x2) (C_CApply y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CApply _ _) (C_CLambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CLetDecl _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CApply _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda x1 x2) (C_CLambda y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CLambda _ _) (C_CLetDecl _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLambda _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl x1 x2) (C_CLetDecl y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CLetDecl _ _) (C_CDoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl _ _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl _ _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl _ _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl _ _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CLetDecl _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CDoExpr x1) (C_CDoExpr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CDoExpr _) (C_CListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CDoExpr _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CDoExpr _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CDoExpr _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CDoExpr _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CListComp x1 x2) (C_CListComp y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CListComp _ _) (C_CCase _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CListComp _ _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CListComp _ _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CListComp _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CCase x1 x2) (C_CCase y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CCase _ _) (C_CRecConstr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CCase _ _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CCase _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CRecConstr x1) (C_CRecConstr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CRecConstr _) (C_CRecSelect _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CRecConstr _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CRecSelect x1 x2) (C_CRecSelect y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CRecSelect _ _) (C_CRecUpdate _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CRecUpdate x1 x2) (C_CRecUpdate y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CStatement
     = C_CSExpr C_CExpr
     | C_CSPat C_CPattern C_CExpr
     | C_CSLet (Curry_Prelude.OP_List C_CLocalDecl)
     | Choice_C_CStatement Cover ID C_CStatement C_CStatement
     | Choices_C_CStatement Cover ID ([C_CStatement])
     | Fail_C_CStatement Cover FailInfo
     | Guard_C_CStatement Cover Constraints C_CStatement

instance Show C_CStatement where
  showsPrec d (Choice_C_CStatement cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CStatement cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CStatement cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CStatement cd info) = showChar '!'
  showsPrec _ (C_CSExpr x1) = (showString "(CSExpr") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CSPat x1 x2) = (showString "(CSPat") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CSLet x1) = (showString "(CSLet") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_CStatement where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CSExpr x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CSExpr" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CSPat x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CSPat" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CSLet x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CSLet" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_CStatement where
  choiceCons = Choice_C_CStatement
  choicesCons = Choices_C_CStatement
  failCons = Fail_C_CStatement
  guardCons = Guard_C_CStatement
  try (Choice_C_CStatement cd i x y) = tryChoice cd i x y
  try (Choices_C_CStatement cd i xs) = tryChoices cd i xs
  try (Fail_C_CStatement cd info) = Fail cd info
  try (Guard_C_CStatement cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CStatement cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CStatement cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CStatement cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CStatement cd i _) = error ("AbstractCurry.CStatement.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CStatement cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CStatement cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CStatement where
  generate s c = Choices_C_CStatement c (freeID [1,2,1] s) [(C_CSExpr (generate (leftSupply s) c)),(C_CSPat (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CSLet (generate (leftSupply s) c))]


instance NormalForm C_CStatement where
  ($!!) cont (C_CSExpr x1) d cs = (((\y1 d cs -> cont (C_CSExpr y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CSPat x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CSPat y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CSLet x1) d cs = (((\y1 d cs -> cont (C_CSLet y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CStatement cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CStatement cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CStatement cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CStatement cd info) _ _ = failCons cd info
  ($##) cont (C_CSExpr x1) d cs = (((\y1 d cs -> cont (C_CSExpr y1) d cs) $## x1) d) cs
  ($##) cont (C_CSPat x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CSPat y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CSLet x1) d cs = (((\y1 d cs -> cont (C_CSLet y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_CStatement cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CStatement cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CStatement cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CStatement cd info) _ _ = failCons cd info
  searchNF search cont (C_CSExpr x1) = search (\y1 -> cont (C_CSExpr y1)) x1
  searchNF search cont (C_CSPat x1 x2) = search (\y1 -> search (\y2 -> cont (C_CSPat y1 y2)) x2) x1
  searchNF search cont (C_CSLet x1) = search (\y1 -> cont (C_CSLet y1)) x1
  searchNF _ _ x = error ("AbstractCurry.CStatement.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CStatement where
  (=.=) (C_CSExpr x1) (C_CSExpr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CSPat x1 x2) (C_CSPat y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CSLet x1) (C_CSLet y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CSExpr x1) (C_CSExpr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CSPat x1 x2) (C_CSPat y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CSLet x1) (C_CSLet y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CSExpr x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CSPat x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CSLet x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_CStatement cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CStatement cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CStatement cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CStatement cd i _) = error ("AbstractCurry.CStatement.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CStatement cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CStatement cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CSExpr x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CSPat x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CSLet x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_CStatement cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CStatement cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CStatement cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CStatement cd i _) = error ("AbstractCurry.CStatement.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CStatement cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CStatement cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CStatement where
  (=?=) (Choice_C_CStatement cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CStatement cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CStatement cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CStatement cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CStatement cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CStatement cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CStatement cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CStatement cd info) _ _ = failCons cd info
  (=?=) (C_CSExpr x1) (C_CSExpr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CSPat x1 x2) (C_CSPat y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CSLet x1) (C_CSLet y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CStatement cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CStatement cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CStatement cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CStatement cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CStatement cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CStatement cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CStatement cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CStatement cd info) _ _ = failCons cd info
  (<?=) (C_CSExpr x1) (C_CSExpr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CSExpr _) (C_CSPat _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSExpr _) (C_CSLet _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSPat x1 x2) (C_CSPat y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CSPat _ _) (C_CSLet _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CSLet x1) (C_CSLet y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CPattern
     = C_CPVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_CPLit C_CLiteral
     | C_CPComb (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_CPattern)
     | C_CPAs (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CPattern
     | C_CPFuncComb (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_CPattern)
     | C_CPLazy C_CPattern
     | C_CPRecord (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) C_CPattern)) (Curry_Prelude.C_Maybe C_CPattern)
     | Choice_C_CPattern Cover ID C_CPattern C_CPattern
     | Choices_C_CPattern Cover ID ([C_CPattern])
     | Fail_C_CPattern Cover FailInfo
     | Guard_C_CPattern Cover Constraints C_CPattern

instance Show C_CPattern where
  showsPrec d (Choice_C_CPattern cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CPattern cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CPattern cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CPattern cd info) = showChar '!'
  showsPrec _ (C_CPVar x1) = (showString "(CPVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CPLit x1) = (showString "(CPLit") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CPComb x1 x2) = (showString "(CPComb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CPAs x1 x2) = (showString "(CPAs") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CPFuncComb x1 x2) = (showString "(CPFuncComb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CPLazy x1) = (showString "(CPLazy") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CPRecord x1 x2) = (showString "(CPRecord") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_CPattern where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CPVar x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CPVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CPLit x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CPLit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CPComb x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CPComb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CPAs x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CPAs" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CPFuncComb x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CPFuncComb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_CPLazy x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CPLazy" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_CPRecord x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CPRecord" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))))))


instance NonDet C_CPattern where
  choiceCons = Choice_C_CPattern
  choicesCons = Choices_C_CPattern
  failCons = Fail_C_CPattern
  guardCons = Guard_C_CPattern
  try (Choice_C_CPattern cd i x y) = tryChoice cd i x y
  try (Choices_C_CPattern cd i xs) = tryChoices cd i xs
  try (Fail_C_CPattern cd info) = Fail cd info
  try (Guard_C_CPattern cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CPattern cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CPattern cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CPattern cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CPattern cd i _) = error ("AbstractCurry.CPattern.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CPattern cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CPattern cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CPattern where
  generate s c = Choices_C_CPattern c (freeID [1,1,2,2,2,1,2] s) [(C_CPVar (generate (leftSupply s) c)),(C_CPLit (generate (leftSupply s) c)),(C_CPComb (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CPAs (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CPFuncComb (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CPLazy (generate (leftSupply s) c)),(C_CPRecord (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_CPattern where
  ($!!) cont (C_CPVar x1) d cs = (((\y1 d cs -> cont (C_CPVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CPLit x1) d cs = (((\y1 d cs -> cont (C_CPLit y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CPComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPComb y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CPAs x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPAs y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CPFuncComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPFuncComb y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CPLazy x1) d cs = (((\y1 d cs -> cont (C_CPLazy y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CPRecord x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPRecord y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CPattern cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CPattern cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CPattern cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CPattern cd info) _ _ = failCons cd info
  ($##) cont (C_CPVar x1) d cs = (((\y1 d cs -> cont (C_CPVar y1) d cs) $## x1) d) cs
  ($##) cont (C_CPLit x1) d cs = (((\y1 d cs -> cont (C_CPLit y1) d cs) $## x1) d) cs
  ($##) cont (C_CPComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPComb y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CPAs x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPAs y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CPFuncComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPFuncComb y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CPLazy x1) d cs = (((\y1 d cs -> cont (C_CPLazy y1) d cs) $## x1) d) cs
  ($##) cont (C_CPRecord x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CPRecord y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CPattern cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CPattern cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CPattern cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CPattern cd info) _ _ = failCons cd info
  searchNF search cont (C_CPVar x1) = search (\y1 -> cont (C_CPVar y1)) x1
  searchNF search cont (C_CPLit x1) = search (\y1 -> cont (C_CPLit y1)) x1
  searchNF search cont (C_CPComb x1 x2) = search (\y1 -> search (\y2 -> cont (C_CPComb y1 y2)) x2) x1
  searchNF search cont (C_CPAs x1 x2) = search (\y1 -> search (\y2 -> cont (C_CPAs y1 y2)) x2) x1
  searchNF search cont (C_CPFuncComb x1 x2) = search (\y1 -> search (\y2 -> cont (C_CPFuncComb y1 y2)) x2) x1
  searchNF search cont (C_CPLazy x1) = search (\y1 -> cont (C_CPLazy y1)) x1
  searchNF search cont (C_CPRecord x1 x2) = search (\y1 -> search (\y2 -> cont (C_CPRecord y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CPattern.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CPattern where
  (=.=) (C_CPVar x1) (C_CPVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CPLit x1) (C_CPLit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CPComb x1 x2) (C_CPComb y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CPAs x1 x2) (C_CPAs y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CPFuncComb x1 x2) (C_CPFuncComb y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CPLazy x1) (C_CPLazy y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CPRecord x1 x2) (C_CPRecord y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CPVar x1) (C_CPVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CPLit x1) (C_CPLit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CPComb x1 x2) (C_CPComb y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CPAs x1 x2) (C_CPAs y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CPFuncComb x1 x2) (C_CPFuncComb y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CPLazy x1) (C_CPLazy y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CPRecord x1 x2) (C_CPRecord y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CPVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CPLit x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CPComb x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CPAs x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CPFuncComb x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CPLazy x3) = ((i :=: (ChooseN 5 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CPRecord x3 x4) = ((i :=: (ChooseN 6 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_CPattern cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CPattern cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CPattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CPattern cd i _) = error ("AbstractCurry.CPattern.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CPattern cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CPattern cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CPVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CPLit x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CPComb x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CPAs x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CPFuncComb x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CPLazy x3) = [(i :=: (ChooseN 5 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CPRecord x3 x4) = [(i :=: (ChooseN 6 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_CPattern cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CPattern cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CPattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CPattern cd i _) = error ("AbstractCurry.CPattern.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CPattern cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CPattern cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CPattern where
  (=?=) (Choice_C_CPattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CPattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CPattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CPattern cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CPattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CPattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CPattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CPattern cd info) _ _ = failCons cd info
  (=?=) (C_CPVar x1) (C_CPVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CPLit x1) (C_CPLit y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CPComb x1 x2) (C_CPComb y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CPAs x1 x2) (C_CPAs y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CPFuncComb x1 x2) (C_CPFuncComb y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CPLazy x1) (C_CPLazy y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CPRecord x1 x2) (C_CPRecord y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CPattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CPattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CPattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CPattern cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CPattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CPattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CPattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CPattern cd info) _ _ = failCons cd info
  (<?=) (C_CPVar x1) (C_CPVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CPVar _) (C_CPLit _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPVar _) (C_CPComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPVar _) (C_CPAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPVar _) (C_CPFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPVar _) (C_CPLazy _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPVar _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLit x1) (C_CPLit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CPLit _) (C_CPComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLit _) (C_CPAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLit _) (C_CPFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLit _) (C_CPLazy _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLit _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPComb x1 x2) (C_CPComb y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CPComb _ _) (C_CPAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPComb _ _) (C_CPFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPComb _ _) (C_CPLazy _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPComb _ _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPAs x1 x2) (C_CPAs y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CPAs _ _) (C_CPFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPAs _ _) (C_CPLazy _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPAs _ _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPFuncComb x1 x2) (C_CPFuncComb y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CPFuncComb _ _) (C_CPLazy _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPFuncComb _ _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPLazy x1) (C_CPLazy y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CPLazy _) (C_CPRecord _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CPRecord x1 x2) (C_CPRecord y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CBranchExpr
     = C_CBranch C_CPattern C_CExpr
     | C_CGuardedBranch C_CPattern (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_CExpr C_CExpr))
     | Choice_C_CBranchExpr Cover ID C_CBranchExpr C_CBranchExpr
     | Choices_C_CBranchExpr Cover ID ([C_CBranchExpr])
     | Fail_C_CBranchExpr Cover FailInfo
     | Guard_C_CBranchExpr Cover Constraints C_CBranchExpr

instance Show C_CBranchExpr where
  showsPrec d (Choice_C_CBranchExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CBranchExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CBranchExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CBranchExpr cd info) = showChar '!'
  showsPrec _ (C_CBranch x1 x2) = (showString "(CBranch") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_CGuardedBranch x1 x2) = (showString "(CGuardedBranch") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_CBranchExpr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CBranch x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CBranch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_CGuardedBranch x1 x2,r2) | (_,r0) <- readQualified "AbstractCurry" "CGuardedBranch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet C_CBranchExpr where
  choiceCons = Choice_C_CBranchExpr
  choicesCons = Choices_C_CBranchExpr
  failCons = Fail_C_CBranchExpr
  guardCons = Guard_C_CBranchExpr
  try (Choice_C_CBranchExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_CBranchExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_CBranchExpr cd info) = Fail cd info
  try (Guard_C_CBranchExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CBranchExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CBranchExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CBranchExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CBranchExpr cd i _) = error ("AbstractCurry.CBranchExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CBranchExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CBranchExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CBranchExpr where
  generate s c = Choices_C_CBranchExpr c (freeID [2,2] s) [(C_CBranch (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_CGuardedBranch (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_CBranchExpr where
  ($!!) cont (C_CBranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CBranch y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CGuardedBranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CGuardedBranch y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CBranchExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CBranchExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CBranchExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CBranchExpr cd info) _ _ = failCons cd info
  ($##) cont (C_CBranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CBranch y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CGuardedBranch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CGuardedBranch y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CBranchExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CBranchExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CBranchExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CBranchExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_CBranch x1 x2) = search (\y1 -> search (\y2 -> cont (C_CBranch y1 y2)) x2) x1
  searchNF search cont (C_CGuardedBranch x1 x2) = search (\y1 -> search (\y2 -> cont (C_CGuardedBranch y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractCurry.CBranchExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CBranchExpr where
  (=.=) (C_CBranch x1 x2) (C_CBranch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_CGuardedBranch x1 x2) (C_CGuardedBranch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CBranch x1 x2) (C_CBranch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_CGuardedBranch x1 x2) (C_CGuardedBranch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CBranch x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_CGuardedBranch x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_CBranchExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CBranchExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CBranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CBranchExpr cd i _) = error ("AbstractCurry.CBranchExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CBranchExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CBranchExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CBranch x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_CGuardedBranch x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_CBranchExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CBranchExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CBranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CBranchExpr cd i _) = error ("AbstractCurry.CBranchExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CBranchExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CBranchExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CBranchExpr where
  (=?=) (Choice_C_CBranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CBranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CBranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CBranchExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CBranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CBranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CBranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CBranchExpr cd info) _ _ = failCons cd info
  (=?=) (C_CBranch x1 x2) (C_CBranch y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_CGuardedBranch x1 x2) (C_CGuardedBranch y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CBranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CBranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CBranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CBranchExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CBranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CBranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CBranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CBranchExpr cd info) _ _ = failCons cd info
  (<?=) (C_CBranch x1 x2) (C_CBranch y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_CBranch _ _) (C_CGuardedBranch _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CGuardedBranch x1 x2) (C_CGuardedBranch y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CLiteral
     = C_CIntc Curry_Prelude.C_Int
     | C_CFloatc Curry_Prelude.C_Float
     | C_CCharc Curry_Prelude.C_Char
     | Choice_C_CLiteral Cover ID C_CLiteral C_CLiteral
     | Choices_C_CLiteral Cover ID ([C_CLiteral])
     | Fail_C_CLiteral Cover FailInfo
     | Guard_C_CLiteral Cover Constraints C_CLiteral

instance Show C_CLiteral where
  showsPrec d (Choice_C_CLiteral cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CLiteral cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CLiteral cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CLiteral cd info) = showChar '!'
  showsPrec _ (C_CIntc x1) = (showString "(CIntc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CFloatc x1) = (showString "(CFloatc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CCharc x1) = (showString "(CCharc") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_CLiteral where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CIntc x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CIntc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_CFloatc x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CFloatc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_CCharc x1,r1) | (_,r0) <- readQualified "AbstractCurry" "CCharc" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_CLiteral where
  choiceCons = Choice_C_CLiteral
  choicesCons = Choices_C_CLiteral
  failCons = Fail_C_CLiteral
  guardCons = Guard_C_CLiteral
  try (Choice_C_CLiteral cd i x y) = tryChoice cd i x y
  try (Choices_C_CLiteral cd i xs) = tryChoices cd i xs
  try (Fail_C_CLiteral cd info) = Fail cd info
  try (Guard_C_CLiteral cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CLiteral cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CLiteral cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CLiteral cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CLiteral cd i _) = error ("AbstractCurry.CLiteral.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CLiteral cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CLiteral cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CLiteral where
  generate s c = Choices_C_CLiteral c (freeID [1,1,1] s) [(C_CIntc (generate (leftSupply s) c)),(C_CFloatc (generate (leftSupply s) c)),(C_CCharc (generate (leftSupply s) c))]


instance NormalForm C_CLiteral where
  ($!!) cont (C_CIntc x1) d cs = (((\y1 d cs -> cont (C_CIntc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CFloatc x1) d cs = (((\y1 d cs -> cont (C_CFloatc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CCharc x1) d cs = (((\y1 d cs -> cont (C_CCharc y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CLiteral cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CLiteral cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CLiteral cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CLiteral cd info) _ _ = failCons cd info
  ($##) cont (C_CIntc x1) d cs = (((\y1 d cs -> cont (C_CIntc y1) d cs) $## x1) d) cs
  ($##) cont (C_CFloatc x1) d cs = (((\y1 d cs -> cont (C_CFloatc y1) d cs) $## x1) d) cs
  ($##) cont (C_CCharc x1) d cs = (((\y1 d cs -> cont (C_CCharc y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_CLiteral cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CLiteral cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CLiteral cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CLiteral cd info) _ _ = failCons cd info
  searchNF search cont (C_CIntc x1) = search (\y1 -> cont (C_CIntc y1)) x1
  searchNF search cont (C_CFloatc x1) = search (\y1 -> cont (C_CFloatc y1)) x1
  searchNF search cont (C_CCharc x1) = search (\y1 -> cont (C_CCharc y1)) x1
  searchNF _ _ x = error ("AbstractCurry.CLiteral.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CLiteral where
  (=.=) (C_CIntc x1) (C_CIntc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CFloatc x1) (C_CFloatc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CCharc x1) (C_CCharc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CIntc x1) (C_CIntc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CFloatc x1) (C_CFloatc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CCharc x1) (C_CCharc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CIntc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CFloatc x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CCharc x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_CLiteral cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CLiteral cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CLiteral cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CLiteral cd i _) = error ("AbstractCurry.CLiteral.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CLiteral cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CLiteral cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CIntc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CFloatc x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CCharc x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_CLiteral cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CLiteral cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CLiteral cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CLiteral cd i _) = error ("AbstractCurry.CLiteral.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CLiteral cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CLiteral cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CLiteral where
  (=?=) (Choice_C_CLiteral cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CLiteral cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CLiteral cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CLiteral cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CLiteral cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CLiteral cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CLiteral cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CLiteral cd info) _ _ = failCons cd info
  (=?=) (C_CIntc x1) (C_CIntc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CFloatc x1) (C_CFloatc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CCharc x1) (C_CCharc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CLiteral cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CLiteral cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CLiteral cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CLiteral cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CLiteral cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CLiteral cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CLiteral cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CLiteral cd info) _ _ = failCons cd info
  (<?=) (C_CIntc x1) (C_CIntc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CIntc _) (C_CFloatc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CIntc _) (C_CCharc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CFloatc x1) (C_CFloatc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CFloatc _) (C_CCharc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CCharc x1) (C_CCharc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_readCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_C_readCurry x1 x3250 x3500 = d_C_readCurryWithParseOptions x1 (Curry_Distribution.d_C_setQuiet Curry_Prelude.C_True (Curry_Distribution.d_C_defaultParams x3250 x3500) x3250 x3500) x3250 x3500

d_C_readUntypedCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_C_readUntypedCurry x1 x3250 x3500 = d_C_readUntypedCurryWithParseOptions x1 (Curry_Distribution.d_C_setQuiet Curry_Prelude.C_True (Curry_Distribution.d_C_defaultParams x3250 x3500) x3250 x3500) x3250 x3500

d_C_readCurryWithParseOptions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_C_readCurryWithParseOptions x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (d_OP_readCurryWithParseOptions_dot___hash_lambda1 x2 x1) x3250 x3500

d_OP_readCurryWithParseOptions_dot___hash_lambda1 :: Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readCurryWithParseOptions_dot___hash_lambda1 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) (d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 x3 x1 x2) x3250 x3500

d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.C_Bool -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_5 x4 x1 x3 x2 (Curry_Prelude.d_OP_bar_bar x1 x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x3250 x3500) x3250 x3500

d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x3250 x3500 = d_C_readAbstractCurryFile x1 x3250 x3500

d_C_readUntypedCurryWithParseOptions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_C_readUntypedCurryWithParseOptions x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4 x2 x1) x3250 x3500

d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4 :: Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) (d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5 x3 x1 x2) x3250 x3500

d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.C_Bool -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_4 x4 x1 x3 x2 (Curry_Prelude.d_OP_bar_bar x1 x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x3250 x3500) x3250 x3500

d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_OP_readUntypedCurryWithParseOptions_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x3250 x3500 = d_C_readAbstractCurryFile x1 x3250 x3500

d_C_abstractCurryFileName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_abstractCurryFileName x1 x3250 x3500 = Curry_Distribution.d_C_inCurrySubdir (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500

d_C_untypedAbstractCurryFileName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_untypedAbstractCurryFileName x1 x3250 x3500 = Curry_Distribution.d_C_inCurrySubdir (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500

d_C_readAbstractCurryFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CurryProg
d_C_readAbstractCurryFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_readAbstractCurryFile_dot___hash_lambda8 x1) x3250 x3500

d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20_dot___hash_lambda7 x3250 x3500

d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20_dot___hash_lambda7 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20_dot___hash_lambda7 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_ReadShowTerm.d_C_readUnqualifiedTerm (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) x1 x3250 x3500) x3250 x3500

d_OP_readAbstractCurryFile_dot___hash_lambda8 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readAbstractCurryFile_dot___hash_lambda8 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20 x1 x3250 x3500
     Curry_Prelude.C_False -> let
          x3 = Curry_Distribution.d_C_inCurrySubdir x1 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x3 x3250 x3500) (d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x3) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readAbstractCurryFile_dot___hash_lambda8 x1 x1002 x3250 x3500) (d_OP_readAbstractCurryFile_dot___hash_lambda8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readAbstractCurryFile_dot___hash_lambda8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readAbstractCurryFile_dot___hash_lambda8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_readAbstractCurryFile_dot_readExistingACY_dot_20 x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1002 x3250 x3500) (d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readAbstractCurryFile_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_tryReadACYFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_C_tryReadACYFile x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_tryReadACYFile_dot___hash_lambda12 x2 x1) x3250 x3500)

d_OP_tryReadACYFile_dot_tryRead_dot_29 :: Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP_tryReadACYFile_dot_tryRead_dot_29 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x2 x3250 x3500) (d_OP_tryReadACYFile_dot_tryRead_dot_29_dot___hash_lambda10 x1) x3250 x3500

d_OP_tryReadACYFile_dot_tryRead_dot_29_dot___hash_lambda10 :: Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP_tryReadACYFile_dot_tryRead_dot_29_dot___hash_lambda10 x1 x2 x3250 x3500 = d_OP__case_3 x2 x1 (Curry_ReadShowTerm.d_C_readsUnqualifiedTerm (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) x2 x3250 x3500) x3250 x3500

d_OP_tryReadACYFile_dot___hash_lambda12 :: Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP_tryReadACYFile_dot___hash_lambda12 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_tryReadACYFile_dot_tryRead_dot_29 x1 x2 x3250 x3500
     Curry_Prelude.C_False -> let
          x4 = Curry_Distribution.d_C_inCurrySubdir x2 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x4 x3250 x3500) (d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x4) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryReadACYFile_dot___hash_lambda12 x1 x2 x1002 x3250 x3500) (d_OP_tryReadACYFile_dot___hash_lambda12 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryReadACYFile_dot___hash_lambda12 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryReadACYFile_dot___hash_lambda12 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 :: Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_tryReadACYFile_dot_tryRead_dot_29 x1 x2 x3250 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x1002 x3250 x3500) (d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryReadACYFile_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_writeAbstractCurryFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_CurryProg -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeAbstractCurryFile x1 x2 x3250 x3500 = Curry_Prelude.d_C_writeFile x1 (Curry_ReadShowTerm.d_C_showTerm x2 x3250 x3500) x3250 x3500

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP__case_3 x2 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1 x1002 x3250 x3500) (d_OP__case_3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_Tuple2 C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP__case_2 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_1 x1 x6 x5 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x1 x1002 x3250 x3500) (d_OP__case_2 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_CurryProg -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP__case_1 x1 x6 x5 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_OP__case_0 x6 x1 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x6 x5 x1002 x3250 x3500) (d_OP__case_1 x1 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg) -> C_CurryProg -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe C_CurryProg)
d_OP__case_0 x6 x1 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.C_Just x5) x3250 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x1 x5 x1002 x3250 x3500) (d_OP__case_0 x6 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_4 x4 x1 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Distribution.d_C_callFrontendWithParams Curry_Distribution.C_UACY x2 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x4 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_4 x4 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x4 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x4 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_5 x4 x1 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Distribution.d_C_callFrontendWithParams Curry_Distribution.C_ACY x2 x3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x4 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_5 x4 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x4 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x4 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
