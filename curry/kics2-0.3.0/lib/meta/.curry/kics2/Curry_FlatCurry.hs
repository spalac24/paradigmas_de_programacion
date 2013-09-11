{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlatCurry (C_Prog (..), C_Visibility (..), C_TypeDecl (..), C_ConsDecl (..), C_TypeExpr (..), C_OpDecl (..), C_Fixity (..), C_FuncDecl (..), C_Rule (..), C_CaseType (..), C_CombType (..), C_Expr (..), C_BranchExpr (..), C_Pattern (..), C_Literal (..), C_QName, C_TVarIndex, C_VarIndex, d_C_readFlatCurry, d_C_readFlatCurryWithParseOptions, d_C_flatCurryFileName, d_C_flatCurryIntName, d_C_readFlatCurryFile, d_C_readFlatCurryInt, d_C_writeFCY, d_C_showQNameInModule) where

import Basics
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
type C_QName = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_TVarIndex = Curry_Prelude.C_Int

type C_VarIndex = Curry_Prelude.C_Int

data C_Prog
     = C_Prog (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_TypeDecl) (Curry_Prelude.OP_List C_FuncDecl) (Curry_Prelude.OP_List C_OpDecl)
     | Choice_C_Prog Cover ID C_Prog C_Prog
     | Choices_C_Prog Cover ID ([C_Prog])
     | Fail_C_Prog Cover FailInfo
     | Guard_C_Prog Cover Constraints C_Prog

instance Show C_Prog where
  showsPrec d (Choice_C_Prog cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Prog cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Prog cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Prog cd info) = showChar '!'
  showsPrec _ (C_Prog x1 x2 x3 x4 x5) = (showString "(Prog") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read C_Prog where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Prog x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "FlatCurry" "Prog" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


instance NonDet C_Prog where
  choiceCons = Choice_C_Prog
  choicesCons = Choices_C_Prog
  failCons = Fail_C_Prog
  guardCons = Guard_C_Prog
  try (Choice_C_Prog cd i x y) = tryChoice cd i x y
  try (Choices_C_Prog cd i xs) = tryChoices cd i xs
  try (Fail_C_Prog cd info) = Fail cd info
  try (Guard_C_Prog cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Prog cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Prog cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Prog cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Prog cd i _) = error ("FlatCurry.Prog.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Prog cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Prog cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Prog where
  generate s c = Choices_C_Prog c (freeID [5] s) [(C_Prog (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_Prog where
  ($!!) cont (C_Prog x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_Prog y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Prog cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Prog cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Prog cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Prog cd info) _ _ = failCons cd info
  ($##) cont (C_Prog x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_Prog y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Prog cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Prog cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Prog cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Prog cd info) _ _ = failCons cd info
  searchNF search cont (C_Prog x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_Prog y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("FlatCurry.Prog.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Prog where
  (=.=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Prog x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_Prog cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Prog cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Prog cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Prog cd i _) = error ("FlatCurry.Prog.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Prog cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Prog cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Prog x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_Prog cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Prog cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Prog cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Prog cd i _) = error ("FlatCurry.Prog.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Prog cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Prog cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Prog where
  (=?=) (Choice_C_Prog cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Prog cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Prog cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Prog cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Prog cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Prog cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Prog cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Prog cd info) _ _ = failCons cd info
  (=?=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_Prog cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Prog cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Prog cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Prog cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Prog cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Prog cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Prog cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Prog cd info) _ _ = failCons cd info
  (<?=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_Visibility
     = C_Public
     | C_Private
     | Choice_C_Visibility Cover ID C_Visibility C_Visibility
     | Choices_C_Visibility Cover ID ([C_Visibility])
     | Fail_C_Visibility Cover FailInfo
     | Guard_C_Visibility Cover Constraints C_Visibility

instance Show C_Visibility where
  showsPrec d (Choice_C_Visibility cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Visibility cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Visibility cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Visibility cd info) = showChar '!'
  showsPrec _ C_Public = showString "Public"
  showsPrec _ C_Private = showString "Private"


instance Read C_Visibility where
  readsPrec _ s = (readParen False (\r -> [ (C_Public,r0) | (_,r0) <- readQualified "FlatCurry" "Public" r]) s) ++ (readParen False (\r -> [ (C_Private,r0) | (_,r0) <- readQualified "FlatCurry" "Private" r]) s)


instance NonDet C_Visibility where
  choiceCons = Choice_C_Visibility
  choicesCons = Choices_C_Visibility
  failCons = Fail_C_Visibility
  guardCons = Guard_C_Visibility
  try (Choice_C_Visibility cd i x y) = tryChoice cd i x y
  try (Choices_C_Visibility cd i xs) = tryChoices cd i xs
  try (Fail_C_Visibility cd info) = Fail cd info
  try (Guard_C_Visibility cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Visibility cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Visibility cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Visibility cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Visibility cd i _) = error ("FlatCurry.Visibility.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Visibility cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Visibility cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Visibility where
  generate s c = Choices_C_Visibility c (freeID [0,0] s) [C_Public,C_Private]


instance NormalForm C_Visibility where
  ($!!) cont C_Public d cs = cont C_Public d cs
  ($!!) cont C_Private d cs = cont C_Private d cs
  ($!!) cont (Choice_C_Visibility cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Visibility cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Visibility cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Visibility cd info) _ _ = failCons cd info
  ($##) cont C_Public d cs = cont C_Public d cs
  ($##) cont C_Private d cs = cont C_Private d cs
  ($##) cont (Choice_C_Visibility cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Visibility cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Visibility cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Visibility cd info) _ _ = failCons cd info
  searchNF _ cont C_Public = cont C_Public
  searchNF _ cont C_Private = cont C_Private
  searchNF _ _ x = error ("FlatCurry.Visibility.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Visibility where
  (=.=) C_Public C_Public d cs = C_Success
  (=.=) C_Private C_Private d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Public C_Public d cs = C_Success
  (=.<=) C_Private C_Private d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Public = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Private = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_Visibility cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Visibility cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Visibility cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Visibility cd i _) = error ("FlatCurry.Visibility.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Visibility cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Visibility cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Public = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Private = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_Visibility cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Visibility cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Visibility cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Visibility cd i _) = error ("FlatCurry.Visibility.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Visibility cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Visibility cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Visibility where
  (=?=) (Choice_C_Visibility cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Visibility cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Visibility cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Visibility cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Visibility cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Visibility cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Visibility cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Visibility cd info) _ _ = failCons cd info
  (=?=) C_Public C_Public d cs = Curry_Prelude.C_True
  (=?=) C_Private C_Private d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Visibility cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Visibility cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Visibility cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Visibility cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Visibility cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Visibility cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Visibility cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Visibility cd info) _ _ = failCons cd info
  (<?=) C_Public C_Public d cs = Curry_Prelude.C_True
  (<?=) C_Public C_Private _ _ = Curry_Prelude.C_True
  (<?=) C_Private C_Private d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_TypeDecl
     = C_Type (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Visibility (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List C_ConsDecl)
     | C_TypeSyn (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Visibility (Curry_Prelude.OP_List Curry_Prelude.C_Int) C_TypeExpr
     | Choice_C_TypeDecl Cover ID C_TypeDecl C_TypeDecl
     | Choices_C_TypeDecl Cover ID ([C_TypeDecl])
     | Fail_C_TypeDecl Cover FailInfo
     | Guard_C_TypeDecl Cover Constraints C_TypeDecl

instance Show C_TypeDecl where
  showsPrec d (Choice_C_TypeDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TypeDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TypeDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TypeDecl cd info) = showChar '!'
  showsPrec _ (C_Type x1 x2 x3 x4) = (showString "(Type") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_TypeSyn x1 x2 x3 x4) = (showString "(TypeSyn") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_TypeDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Type x1 x2 x3 x4,r4) | (_,r0) <- readQualified "FlatCurry" "Type" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ (readParen (d > 10) (\r -> [ (C_TypeSyn x1 x2 x3 x4,r4) | (_,r0) <- readQualified "FlatCurry" "TypeSyn" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s)


instance NonDet C_TypeDecl where
  choiceCons = Choice_C_TypeDecl
  choicesCons = Choices_C_TypeDecl
  failCons = Fail_C_TypeDecl
  guardCons = Guard_C_TypeDecl
  try (Choice_C_TypeDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_TypeDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_TypeDecl cd info) = Fail cd info
  try (Guard_C_TypeDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TypeDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TypeDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TypeDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TypeDecl cd i _) = error ("FlatCurry.TypeDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TypeDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TypeDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TypeDecl where
  generate s c = Choices_C_TypeDecl c (freeID [4,4] s) [(C_Type (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_TypeSyn (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_TypeDecl where
  ($!!) cont (C_Type x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Type y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_TypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_TypeSyn y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TypeDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TypeDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Type x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Type y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_TypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_TypeSyn y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TypeDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TypeDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Type x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Type y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_TypeSyn x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_TypeSyn y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("FlatCurry.TypeDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TypeDecl where
  (=.=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Type x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_TypeSyn x3 x4 x5 x6) = ((i :=: (ChooseN 1 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_TypeDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TypeDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TypeDecl cd i _) = error ("FlatCurry.TypeDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Type x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_TypeSyn x3 x4 x5 x6) = [(i :=: (ChooseN 1 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_TypeDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeDecl cd i _) = error ("FlatCurry.TypeDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TypeDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TypeDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TypeDecl where
  (=?=) (Choice_C_TypeDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TypeDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TypeDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TypeDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TypeDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TypeDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  (=?=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_TypeDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TypeDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TypeDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TypeDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TypeDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TypeDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  (<?=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_Type _ _ _ _) (C_TypeSyn _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_ConsDecl
     = C_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_Visibility (Curry_Prelude.OP_List C_TypeExpr)
     | Choice_C_ConsDecl Cover ID C_ConsDecl C_ConsDecl
     | Choices_C_ConsDecl Cover ID ([C_ConsDecl])
     | Fail_C_ConsDecl Cover FailInfo
     | Guard_C_ConsDecl Cover Constraints C_ConsDecl

instance Show C_ConsDecl where
  showsPrec d (Choice_C_ConsDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ConsDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ConsDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ConsDecl cd info) = showChar '!'
  showsPrec _ (C_Cons x1 x2 x3 x4) = (showString "(Cons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_ConsDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Cons x1 x2 x3 x4,r4) | (_,r0) <- readQualified "FlatCurry" "Cons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet C_ConsDecl where
  choiceCons = Choice_C_ConsDecl
  choicesCons = Choices_C_ConsDecl
  failCons = Fail_C_ConsDecl
  guardCons = Guard_C_ConsDecl
  try (Choice_C_ConsDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_ConsDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_ConsDecl cd info) = Fail cd info
  try (Guard_C_ConsDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ConsDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ConsDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ConsDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ConsDecl cd i _) = error ("FlatCurry.ConsDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ConsDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ConsDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ConsDecl where
  generate s c = Choices_C_ConsDecl c (freeID [4] s) [(C_Cons (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_ConsDecl where
  ($!!) cont (C_Cons x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Cons y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ConsDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ConsDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ConsDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ConsDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Cons x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Cons y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_ConsDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ConsDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ConsDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ConsDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Cons x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Cons y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("FlatCurry.ConsDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ConsDecl where
  (=.=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Cons x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_ConsDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ConsDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ConsDecl cd i _) = error ("FlatCurry.ConsDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ConsDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ConsDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Cons x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_ConsDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ConsDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ConsDecl cd i _) = error ("FlatCurry.ConsDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ConsDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ConsDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ConsDecl where
  (=?=) (Choice_C_ConsDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ConsDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ConsDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ConsDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ConsDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ConsDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ConsDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ConsDecl cd info) _ _ = failCons cd info
  (=?=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_ConsDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ConsDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ConsDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ConsDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ConsDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ConsDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ConsDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ConsDecl cd info) _ _ = failCons cd info
  (<?=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_TypeExpr
     = C_TVar Curry_Prelude.C_Int
     | C_FuncType C_TypeExpr C_TypeExpr
     | C_TCons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_TypeExpr)
     | Choice_C_TypeExpr Cover ID C_TypeExpr C_TypeExpr
     | Choices_C_TypeExpr Cover ID ([C_TypeExpr])
     | Fail_C_TypeExpr Cover FailInfo
     | Guard_C_TypeExpr Cover Constraints C_TypeExpr

instance Show C_TypeExpr where
  showsPrec d (Choice_C_TypeExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TypeExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TypeExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TypeExpr cd info) = showChar '!'
  showsPrec _ (C_TVar x1) = (showString "(TVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_FuncType x1 x2) = (showString "(FuncType") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_TCons x1 x2) = (showString "(TCons") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_TypeExpr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_TVar x1,r1) | (_,r0) <- readQualified "FlatCurry" "TVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FuncType x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "FuncType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_TCons x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "TCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


instance NonDet C_TypeExpr where
  choiceCons = Choice_C_TypeExpr
  choicesCons = Choices_C_TypeExpr
  failCons = Fail_C_TypeExpr
  guardCons = Guard_C_TypeExpr
  try (Choice_C_TypeExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_TypeExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_TypeExpr cd info) = Fail cd info
  try (Guard_C_TypeExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TypeExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TypeExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TypeExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TypeExpr cd i _) = error ("FlatCurry.TypeExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TypeExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TypeExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TypeExpr where
  generate s c = Choices_C_TypeExpr c (freeID [1,2,2] s) [(C_TVar (generate (leftSupply s) c)),(C_FuncType (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_TCons (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_TypeExpr where
  ($!!) cont (C_TVar x1) d cs = (((\y1 d cs -> cont (C_TVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_FuncType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_FuncType y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_TCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TCons y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TypeExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TypeExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TypeExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TypeExpr cd info) _ _ = failCons cd info
  ($##) cont (C_TVar x1) d cs = (((\y1 d cs -> cont (C_TVar y1) d cs) $## x1) d) cs
  ($##) cont (C_FuncType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_FuncType y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_TCons x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_TCons y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TypeExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TypeExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TypeExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TypeExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_TVar x1) = search (\y1 -> cont (C_TVar y1)) x1
  searchNF search cont (C_FuncType x1 x2) = search (\y1 -> search (\y2 -> cont (C_FuncType y1 y2)) x2) x1
  searchNF search cont (C_TCons x1 x2) = search (\y1 -> search (\y2 -> cont (C_TCons y1 y2)) x2) x1
  searchNF _ _ x = error ("FlatCurry.TypeExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TypeExpr where
  (=.=) (C_TVar x1) (C_TVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_FuncType x1 x2) (C_FuncType y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_TCons x1 x2) (C_TCons y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_TVar x1) (C_TVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_FuncType x1 x2) (C_FuncType y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_TCons x1 x2) (C_TCons y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_TVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_FuncType x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_TCons x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_TypeExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TypeExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TypeExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TypeExpr cd i _) = error ("FlatCurry.TypeExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FuncType x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_TCons x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_TypeExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeExpr cd i _) = error ("FlatCurry.TypeExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TypeExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TypeExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TypeExpr where
  (=?=) (Choice_C_TypeExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TypeExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TypeExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TypeExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TypeExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TypeExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TypeExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TypeExpr cd info) _ _ = failCons cd info
  (=?=) (C_TVar x1) (C_TVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_FuncType x1 x2) (C_FuncType y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_TCons x1 x2) (C_TCons y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_TypeExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TypeExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TypeExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TypeExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TypeExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TypeExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TypeExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TypeExpr cd info) _ _ = failCons cd info
  (<?=) (C_TVar x1) (C_TVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_TVar _) (C_FuncType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TVar _) (C_TCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FuncType x1 x2) (C_FuncType y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_FuncType _ _) (C_TCons _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TCons x1 x2) (C_TCons y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_OpDecl
     = C_Op (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Fixity Curry_Prelude.C_Int
     | Choice_C_OpDecl Cover ID C_OpDecl C_OpDecl
     | Choices_C_OpDecl Cover ID ([C_OpDecl])
     | Fail_C_OpDecl Cover FailInfo
     | Guard_C_OpDecl Cover Constraints C_OpDecl

instance Show C_OpDecl where
  showsPrec d (Choice_C_OpDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_OpDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_OpDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_OpDecl cd info) = showChar '!'
  showsPrec _ (C_Op x1 x2 x3) = (showString "(Op") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_OpDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Op x1 x2 x3,r3) | (_,r0) <- readQualified "FlatCurry" "Op" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_OpDecl where
  choiceCons = Choice_C_OpDecl
  choicesCons = Choices_C_OpDecl
  failCons = Fail_C_OpDecl
  guardCons = Guard_C_OpDecl
  try (Choice_C_OpDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_OpDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_OpDecl cd info) = Fail cd info
  try (Guard_C_OpDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_OpDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_OpDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_OpDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_OpDecl cd i _) = error ("FlatCurry.OpDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_OpDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_OpDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_OpDecl where
  generate s c = Choices_C_OpDecl c (freeID [3] s) [(C_Op (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_OpDecl where
  ($!!) cont (C_Op x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Op y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_OpDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_OpDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_OpDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_OpDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Op x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Op y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_OpDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_OpDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_OpDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_OpDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Op x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Op y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("FlatCurry.OpDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_OpDecl where
  (=.=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Op x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_OpDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_OpDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_OpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_OpDecl cd i _) = error ("FlatCurry.OpDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_OpDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_OpDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Op x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_OpDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_OpDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_OpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_OpDecl cd i _) = error ("FlatCurry.OpDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_OpDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_OpDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_OpDecl where
  (=?=) (Choice_C_OpDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_OpDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_OpDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_OpDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_OpDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_OpDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_OpDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_OpDecl cd info) _ _ = failCons cd info
  (=?=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_OpDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_OpDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_OpDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_OpDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_OpDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_OpDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_OpDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_OpDecl cd info) _ _ = failCons cd info
  (<?=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_Fixity
     = C_InfixOp
     | C_InfixlOp
     | C_InfixrOp
     | Choice_C_Fixity Cover ID C_Fixity C_Fixity
     | Choices_C_Fixity Cover ID ([C_Fixity])
     | Fail_C_Fixity Cover FailInfo
     | Guard_C_Fixity Cover Constraints C_Fixity

instance Show C_Fixity where
  showsPrec d (Choice_C_Fixity cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Fixity cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Fixity cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Fixity cd info) = showChar '!'
  showsPrec _ C_InfixOp = showString "InfixOp"
  showsPrec _ C_InfixlOp = showString "InfixlOp"
  showsPrec _ C_InfixrOp = showString "InfixrOp"


instance Read C_Fixity where
  readsPrec _ s = (readParen False (\r -> [ (C_InfixOp,r0) | (_,r0) <- readQualified "FlatCurry" "InfixOp" r]) s) ++ ((readParen False (\r -> [ (C_InfixlOp,r0) | (_,r0) <- readQualified "FlatCurry" "InfixlOp" r]) s) ++ (readParen False (\r -> [ (C_InfixrOp,r0) | (_,r0) <- readQualified "FlatCurry" "InfixrOp" r]) s))


instance NonDet C_Fixity where
  choiceCons = Choice_C_Fixity
  choicesCons = Choices_C_Fixity
  failCons = Fail_C_Fixity
  guardCons = Guard_C_Fixity
  try (Choice_C_Fixity cd i x y) = tryChoice cd i x y
  try (Choices_C_Fixity cd i xs) = tryChoices cd i xs
  try (Fail_C_Fixity cd info) = Fail cd info
  try (Guard_C_Fixity cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Fixity cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Fixity cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Fixity cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Fixity cd i _) = error ("FlatCurry.Fixity.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Fixity cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Fixity cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Fixity where
  generate s c = Choices_C_Fixity c (freeID [0,0,0] s) [C_InfixOp,C_InfixlOp,C_InfixrOp]


instance NormalForm C_Fixity where
  ($!!) cont C_InfixOp d cs = cont C_InfixOp d cs
  ($!!) cont C_InfixlOp d cs = cont C_InfixlOp d cs
  ($!!) cont C_InfixrOp d cs = cont C_InfixrOp d cs
  ($!!) cont (Choice_C_Fixity cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Fixity cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Fixity cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Fixity cd info) _ _ = failCons cd info
  ($##) cont C_InfixOp d cs = cont C_InfixOp d cs
  ($##) cont C_InfixlOp d cs = cont C_InfixlOp d cs
  ($##) cont C_InfixrOp d cs = cont C_InfixrOp d cs
  ($##) cont (Choice_C_Fixity cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Fixity cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Fixity cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Fixity cd info) _ _ = failCons cd info
  searchNF _ cont C_InfixOp = cont C_InfixOp
  searchNF _ cont C_InfixlOp = cont C_InfixlOp
  searchNF _ cont C_InfixrOp = cont C_InfixrOp
  searchNF _ _ x = error ("FlatCurry.Fixity.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Fixity where
  (=.=) C_InfixOp C_InfixOp d cs = C_Success
  (=.=) C_InfixlOp C_InfixlOp d cs = C_Success
  (=.=) C_InfixrOp C_InfixrOp d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_InfixOp C_InfixOp d cs = C_Success
  (=.<=) C_InfixlOp C_InfixlOp d cs = C_Success
  (=.<=) C_InfixrOp C_InfixrOp d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_InfixOp = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_InfixlOp = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_InfixrOp = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_Fixity cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Fixity cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Fixity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Fixity cd i _) = error ("FlatCurry.Fixity.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Fixity cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Fixity cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_InfixOp = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_InfixlOp = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_InfixrOp = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Fixity cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Fixity cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Fixity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Fixity cd i _) = error ("FlatCurry.Fixity.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Fixity cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Fixity cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Fixity where
  (=?=) (Choice_C_Fixity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Fixity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Fixity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Fixity cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Fixity cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Fixity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Fixity cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Fixity cd info) _ _ = failCons cd info
  (=?=) C_InfixOp C_InfixOp d cs = Curry_Prelude.C_True
  (=?=) C_InfixlOp C_InfixlOp d cs = Curry_Prelude.C_True
  (=?=) C_InfixrOp C_InfixrOp d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Fixity cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Fixity cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Fixity cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Fixity cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Fixity cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Fixity cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Fixity cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Fixity cd info) _ _ = failCons cd info
  (<?=) C_InfixOp C_InfixOp d cs = Curry_Prelude.C_True
  (<?=) C_InfixOp C_InfixlOp _ _ = Curry_Prelude.C_True
  (<?=) C_InfixOp C_InfixrOp _ _ = Curry_Prelude.C_True
  (<?=) C_InfixlOp C_InfixlOp d cs = Curry_Prelude.C_True
  (<?=) C_InfixlOp C_InfixrOp _ _ = Curry_Prelude.C_True
  (<?=) C_InfixrOp C_InfixrOp d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_FuncDecl
     = C_Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_Visibility C_TypeExpr C_Rule
     | Choice_C_FuncDecl Cover ID C_FuncDecl C_FuncDecl
     | Choices_C_FuncDecl Cover ID ([C_FuncDecl])
     | Fail_C_FuncDecl Cover FailInfo
     | Guard_C_FuncDecl Cover Constraints C_FuncDecl

instance Show C_FuncDecl where
  showsPrec d (Choice_C_FuncDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FuncDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FuncDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FuncDecl cd info) = showChar '!'
  showsPrec _ (C_Func x1 x2 x3 x4 x5) = (showString "(Func") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read C_FuncDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Func x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "FlatCurry" "Func" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


instance NonDet C_FuncDecl where
  choiceCons = Choice_C_FuncDecl
  choicesCons = Choices_C_FuncDecl
  failCons = Fail_C_FuncDecl
  guardCons = Guard_C_FuncDecl
  try (Choice_C_FuncDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_FuncDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_FuncDecl cd info) = Fail cd info
  try (Guard_C_FuncDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FuncDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FuncDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FuncDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FuncDecl cd i _) = error ("FlatCurry.FuncDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FuncDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FuncDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FuncDecl where
  generate s c = Choices_C_FuncDecl c (freeID [5] s) [(C_Func (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_FuncDecl where
  ($!!) cont (C_Func x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_Func y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_FuncDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FuncDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Func x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_Func y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_FuncDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FuncDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Func x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_Func y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("FlatCurry.FuncDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FuncDecl where
  (=.=) (C_Func x1 x2 x3 x4 x5) (C_Func y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Func x1 x2 x3 x4 x5) (C_Func y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Func x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_FuncDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FuncDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FuncDecl cd i _) = error ("FlatCurry.FuncDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FuncDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FuncDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Func x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_FuncDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FuncDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FuncDecl cd i _) = error ("FlatCurry.FuncDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_FuncDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_FuncDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_FuncDecl where
  (=?=) (Choice_C_FuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_FuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_FuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_FuncDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_FuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_FuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  (=?=) (C_Func x1 x2 x3 x4 x5) (C_Func y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_FuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FuncDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  (<?=) (C_Func x1 x2 x3 x4 x5) (C_Func y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_Rule
     = C_Rule (Curry_Prelude.OP_List Curry_Prelude.C_Int) C_Expr
     | C_External (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Rule Cover ID C_Rule C_Rule
     | Choices_C_Rule Cover ID ([C_Rule])
     | Fail_C_Rule Cover FailInfo
     | Guard_C_Rule Cover Constraints C_Rule

instance Show C_Rule where
  showsPrec d (Choice_C_Rule cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Rule cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Rule cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Rule cd info) = showChar '!'
  showsPrec _ (C_Rule x1 x2) = (showString "(Rule") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_External x1) = (showString "(External") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Rule where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Rule x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Rule" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_External x1,r1) | (_,r0) <- readQualified "FlatCurry" "External" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Rule where
  choiceCons = Choice_C_Rule
  choicesCons = Choices_C_Rule
  failCons = Fail_C_Rule
  guardCons = Guard_C_Rule
  try (Choice_C_Rule cd i x y) = tryChoice cd i x y
  try (Choices_C_Rule cd i xs) = tryChoices cd i xs
  try (Fail_C_Rule cd info) = Fail cd info
  try (Guard_C_Rule cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Rule cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Rule cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Rule cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Rule cd i _) = error ("FlatCurry.Rule.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Rule cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Rule cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Rule where
  generate s c = Choices_C_Rule c (freeID [2,1] s) [(C_Rule (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_External (generate (leftSupply s) c))]


instance NormalForm C_Rule where
  ($!!) cont (C_Rule x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Rule y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_External x1) d cs = (((\y1 d cs -> cont (C_External y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Rule cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Rule cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Rule cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  ($##) cont (C_Rule x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Rule y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_External x1) d cs = (((\y1 d cs -> cont (C_External y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Rule cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Rule cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Rule cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  searchNF search cont (C_Rule x1 x2) = search (\y1 -> search (\y2 -> cont (C_Rule y1 y2)) x2) x1
  searchNF search cont (C_External x1) = search (\y1 -> cont (C_External y1)) x1
  searchNF _ _ x = error ("FlatCurry.Rule.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Rule where
  (=.=) (C_Rule x1 x2) (C_Rule y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_External x1) (C_External y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Rule x1 x2) (C_Rule y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_External x1) (C_External y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Rule x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_External x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Rule cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Rule cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Rule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Rule cd i _) = error ("FlatCurry.Rule.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Rule cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Rule cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Rule x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_External x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Rule cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Rule cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Rule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Rule cd i _) = error ("FlatCurry.Rule.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Rule cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Rule cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Rule where
  (=?=) (Choice_C_Rule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Rule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Rule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Rule cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Rule cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Rule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Rule cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  (=?=) (C_Rule x1 x2) (C_Rule y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_External x1) (C_External y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Rule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Rule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Rule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Rule cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Rule cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Rule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Rule cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  (<?=) (C_Rule x1 x2) (C_Rule y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Rule _ _) (C_External _) _ _ = Curry_Prelude.C_True
  (<?=) (C_External x1) (C_External y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CaseType
     = C_Rigid
     | C_Flex
     | Choice_C_CaseType Cover ID C_CaseType C_CaseType
     | Choices_C_CaseType Cover ID ([C_CaseType])
     | Fail_C_CaseType Cover FailInfo
     | Guard_C_CaseType Cover Constraints C_CaseType

instance Show C_CaseType where
  showsPrec d (Choice_C_CaseType cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CaseType cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CaseType cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CaseType cd info) = showChar '!'
  showsPrec _ C_Rigid = showString "Rigid"
  showsPrec _ C_Flex = showString "Flex"


instance Read C_CaseType where
  readsPrec _ s = (readParen False (\r -> [ (C_Rigid,r0) | (_,r0) <- readQualified "FlatCurry" "Rigid" r]) s) ++ (readParen False (\r -> [ (C_Flex,r0) | (_,r0) <- readQualified "FlatCurry" "Flex" r]) s)


instance NonDet C_CaseType where
  choiceCons = Choice_C_CaseType
  choicesCons = Choices_C_CaseType
  failCons = Fail_C_CaseType
  guardCons = Guard_C_CaseType
  try (Choice_C_CaseType cd i x y) = tryChoice cd i x y
  try (Choices_C_CaseType cd i xs) = tryChoices cd i xs
  try (Fail_C_CaseType cd info) = Fail cd info
  try (Guard_C_CaseType cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CaseType cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CaseType cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CaseType cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CaseType cd i _) = error ("FlatCurry.CaseType.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CaseType cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CaseType cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CaseType where
  generate s c = Choices_C_CaseType c (freeID [0,0] s) [C_Rigid,C_Flex]


instance NormalForm C_CaseType where
  ($!!) cont C_Rigid d cs = cont C_Rigid d cs
  ($!!) cont C_Flex d cs = cont C_Flex d cs
  ($!!) cont (Choice_C_CaseType cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CaseType cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CaseType cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CaseType cd info) _ _ = failCons cd info
  ($##) cont C_Rigid d cs = cont C_Rigid d cs
  ($##) cont C_Flex d cs = cont C_Flex d cs
  ($##) cont (Choice_C_CaseType cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CaseType cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CaseType cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CaseType cd info) _ _ = failCons cd info
  searchNF _ cont C_Rigid = cont C_Rigid
  searchNF _ cont C_Flex = cont C_Flex
  searchNF _ _ x = error ("FlatCurry.CaseType.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CaseType where
  (=.=) C_Rigid C_Rigid d cs = C_Success
  (=.=) C_Flex C_Flex d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Rigid C_Rigid d cs = C_Success
  (=.<=) C_Flex C_Flex d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Rigid = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Flex = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_CaseType cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CaseType cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CaseType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CaseType cd i _) = error ("FlatCurry.CaseType.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CaseType cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CaseType cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Rigid = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Flex = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_CaseType cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CaseType cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CaseType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CaseType cd i _) = error ("FlatCurry.CaseType.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CaseType cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CaseType cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CaseType where
  (=?=) (Choice_C_CaseType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CaseType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CaseType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CaseType cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CaseType cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CaseType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CaseType cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CaseType cd info) _ _ = failCons cd info
  (=?=) C_Rigid C_Rigid d cs = Curry_Prelude.C_True
  (=?=) C_Flex C_Flex d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CaseType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CaseType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CaseType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CaseType cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CaseType cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CaseType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CaseType cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CaseType cd info) _ _ = failCons cd info
  (<?=) C_Rigid C_Rigid d cs = Curry_Prelude.C_True
  (<?=) C_Rigid C_Flex _ _ = Curry_Prelude.C_True
  (<?=) C_Flex C_Flex d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_CombType
     = C_FuncCall
     | C_ConsCall
     | C_FuncPartCall Curry_Prelude.C_Int
     | C_ConsPartCall Curry_Prelude.C_Int
     | Choice_C_CombType Cover ID C_CombType C_CombType
     | Choices_C_CombType Cover ID ([C_CombType])
     | Fail_C_CombType Cover FailInfo
     | Guard_C_CombType Cover Constraints C_CombType

instance Show C_CombType where
  showsPrec d (Choice_C_CombType cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CombType cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CombType cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CombType cd info) = showChar '!'
  showsPrec _ C_FuncCall = showString "FuncCall"
  showsPrec _ C_ConsCall = showString "ConsCall"
  showsPrec _ (C_FuncPartCall x1) = (showString "(FuncPartCall") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ConsPartCall x1) = (showString "(ConsPartCall") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_CombType where
  readsPrec d s = (readParen False (\r -> [ (C_FuncCall,r0) | (_,r0) <- readQualified "FlatCurry" "FuncCall" r]) s) ++ ((readParen False (\r -> [ (C_ConsCall,r0) | (_,r0) <- readQualified "FlatCurry" "ConsCall" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_FuncPartCall x1,r1) | (_,r0) <- readQualified "FlatCurry" "FuncPartCall" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_ConsPartCall x1,r1) | (_,r0) <- readQualified "FlatCurry" "ConsPartCall" r, (x1,r1) <- readsPrec 11 r0]) s)))


instance NonDet C_CombType where
  choiceCons = Choice_C_CombType
  choicesCons = Choices_C_CombType
  failCons = Fail_C_CombType
  guardCons = Guard_C_CombType
  try (Choice_C_CombType cd i x y) = tryChoice cd i x y
  try (Choices_C_CombType cd i xs) = tryChoices cd i xs
  try (Fail_C_CombType cd info) = Fail cd info
  try (Guard_C_CombType cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CombType cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CombType cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CombType cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CombType cd i _) = error ("FlatCurry.CombType.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CombType cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CombType cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CombType where
  generate s c = Choices_C_CombType c (freeID [0,0,1,1] s) [C_FuncCall,C_ConsCall,(C_FuncPartCall (generate (leftSupply s) c)),(C_ConsPartCall (generate (leftSupply s) c))]


instance NormalForm C_CombType where
  ($!!) cont C_FuncCall d cs = cont C_FuncCall d cs
  ($!!) cont C_ConsCall d cs = cont C_ConsCall d cs
  ($!!) cont (C_FuncPartCall x1) d cs = (((\y1 d cs -> cont (C_FuncPartCall y1) d cs) $!! x1) d) cs
  ($!!) cont (C_ConsPartCall x1) d cs = (((\y1 d cs -> cont (C_ConsPartCall y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CombType cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CombType cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CombType cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CombType cd info) _ _ = failCons cd info
  ($##) cont C_FuncCall d cs = cont C_FuncCall d cs
  ($##) cont C_ConsCall d cs = cont C_ConsCall d cs
  ($##) cont (C_FuncPartCall x1) d cs = (((\y1 d cs -> cont (C_FuncPartCall y1) d cs) $## x1) d) cs
  ($##) cont (C_ConsPartCall x1) d cs = (((\y1 d cs -> cont (C_ConsPartCall y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_CombType cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CombType cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CombType cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CombType cd info) _ _ = failCons cd info
  searchNF _ cont C_FuncCall = cont C_FuncCall
  searchNF _ cont C_ConsCall = cont C_ConsCall
  searchNF search cont (C_FuncPartCall x1) = search (\y1 -> cont (C_FuncPartCall y1)) x1
  searchNF search cont (C_ConsPartCall x1) = search (\y1 -> cont (C_ConsPartCall y1)) x1
  searchNF _ _ x = error ("FlatCurry.CombType.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CombType where
  (=.=) C_FuncCall C_FuncCall d cs = C_Success
  (=.=) C_ConsCall C_ConsCall d cs = C_Success
  (=.=) (C_FuncPartCall x1) (C_FuncPartCall y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_ConsPartCall x1) (C_ConsPartCall y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_FuncCall C_FuncCall d cs = C_Success
  (=.<=) C_ConsCall C_ConsCall d cs = C_Success
  (=.<=) (C_FuncPartCall x1) (C_FuncPartCall y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_ConsPartCall x1) (C_ConsPartCall y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_FuncCall = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_ConsCall = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i (C_FuncPartCall x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_ConsPartCall x3) = ((i :=: (ChooseN 3 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_CombType cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CombType cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CombType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CombType cd i _) = error ("FlatCurry.CombType.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CombType cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CombType cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_FuncCall = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_ConsCall = [(i :=: (ChooseN 1 0))]
  lazyBind cd i (C_FuncPartCall x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_ConsPartCall x3) = [(i :=: (ChooseN 3 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_CombType cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CombType cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CombType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CombType cd i _) = error ("FlatCurry.CombType.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CombType cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CombType cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CombType where
  (=?=) (Choice_C_CombType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CombType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CombType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CombType cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CombType cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CombType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CombType cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CombType cd info) _ _ = failCons cd info
  (=?=) C_FuncCall C_FuncCall d cs = Curry_Prelude.C_True
  (=?=) C_ConsCall C_ConsCall d cs = Curry_Prelude.C_True
  (=?=) (C_FuncPartCall x1) (C_FuncPartCall y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_ConsPartCall x1) (C_ConsPartCall y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CombType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CombType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CombType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CombType cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CombType cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CombType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CombType cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CombType cd info) _ _ = failCons cd info
  (<?=) C_FuncCall C_FuncCall d cs = Curry_Prelude.C_True
  (<?=) C_FuncCall C_ConsCall _ _ = Curry_Prelude.C_True
  (<?=) C_FuncCall (C_FuncPartCall _) _ _ = Curry_Prelude.C_True
  (<?=) C_FuncCall (C_ConsPartCall _) _ _ = Curry_Prelude.C_True
  (<?=) C_ConsCall C_ConsCall d cs = Curry_Prelude.C_True
  (<?=) C_ConsCall (C_FuncPartCall _) _ _ = Curry_Prelude.C_True
  (<?=) C_ConsCall (C_ConsPartCall _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FuncPartCall x1) (C_FuncPartCall y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_FuncPartCall _) (C_ConsPartCall _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ConsPartCall x1) (C_ConsPartCall y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Expr
     = C_Var Curry_Prelude.C_Int
     | C_Lit C_Literal
     | C_Comb C_CombType (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_Expr)
     | C_Let (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int C_Expr)) C_Expr
     | C_Free (Curry_Prelude.OP_List Curry_Prelude.C_Int) C_Expr
     | C_Or C_Expr C_Expr
     | C_Case C_CaseType C_Expr (Curry_Prelude.OP_List C_BranchExpr)
     | C_Typed C_Expr C_TypeExpr
     | Choice_C_Expr Cover ID C_Expr C_Expr
     | Choices_C_Expr Cover ID ([C_Expr])
     | Fail_C_Expr Cover FailInfo
     | Guard_C_Expr Cover Constraints C_Expr

instance Show C_Expr where
  showsPrec d (Choice_C_Expr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Expr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Expr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Expr cd info) = showChar '!'
  showsPrec _ (C_Var x1) = (showString "(Var") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Lit x1) = (showString "(Lit") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Comb x1 x2 x3) = (showString "(Comb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_Let x1 x2) = (showString "(Let") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Free x1 x2) = (showString "(Free") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Or x1 x2) = (showString "(Or") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Case x1 x2 x3) = (showString "(Case") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_Typed x1 x2) = (showString "(Typed") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Expr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Var x1,r1) | (_,r0) <- readQualified "FlatCurry" "Var" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Lit x1,r1) | (_,r0) <- readQualified "FlatCurry" "Lit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Comb x1 x2 x3,r3) | (_,r0) <- readQualified "FlatCurry" "Comb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_Let x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Let" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Free x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Free" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Or x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Or" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Case x1 x2 x3,r3) | (_,r0) <- readQualified "FlatCurry" "Case" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_Typed x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Typed" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))))))


instance NonDet C_Expr where
  choiceCons = Choice_C_Expr
  choicesCons = Choices_C_Expr
  failCons = Fail_C_Expr
  guardCons = Guard_C_Expr
  try (Choice_C_Expr cd i x y) = tryChoice cd i x y
  try (Choices_C_Expr cd i xs) = tryChoices cd i xs
  try (Fail_C_Expr cd info) = Fail cd info
  try (Guard_C_Expr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Expr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Expr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Expr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Expr cd i _) = error ("FlatCurry.Expr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Expr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Expr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Expr where
  generate s c = Choices_C_Expr c (freeID [1,1,3,2,2,2,3,2] s) [(C_Var (generate (leftSupply s) c)),(C_Lit (generate (leftSupply s) c)),(C_Comb (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_Let (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Free (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Or (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Case (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_Typed (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Expr where
  ($!!) cont (C_Var x1) d cs = (((\y1 d cs -> cont (C_Var y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Lit x1) d cs = (((\y1 d cs -> cont (C_Lit y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Comb x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Comb y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Let x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Let y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Free x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Free y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Or x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Or y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Case x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Case y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Typed x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Typed y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Expr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Expr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Expr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  ($##) cont (C_Var x1) d cs = (((\y1 d cs -> cont (C_Var y1) d cs) $## x1) d) cs
  ($##) cont (C_Lit x1) d cs = (((\y1 d cs -> cont (C_Lit y1) d cs) $## x1) d) cs
  ($##) cont (C_Comb x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Comb y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Let x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Let y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Free x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Free y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Or x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Or y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Case x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Case y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Typed x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Typed y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Expr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Expr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Expr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  searchNF search cont (C_Var x1) = search (\y1 -> cont (C_Var y1)) x1
  searchNF search cont (C_Lit x1) = search (\y1 -> cont (C_Lit y1)) x1
  searchNF search cont (C_Comb x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Comb y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_Let x1 x2) = search (\y1 -> search (\y2 -> cont (C_Let y1 y2)) x2) x1
  searchNF search cont (C_Free x1 x2) = search (\y1 -> search (\y2 -> cont (C_Free y1 y2)) x2) x1
  searchNF search cont (C_Or x1 x2) = search (\y1 -> search (\y2 -> cont (C_Or y1 y2)) x2) x1
  searchNF search cont (C_Case x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Case y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_Typed x1 x2) = search (\y1 -> search (\y2 -> cont (C_Typed y1 y2)) x2) x1
  searchNF _ _ x = error ("FlatCurry.Expr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Expr where
  (=.=) (C_Var x1) (C_Var y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Lit x1) (C_Lit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Comb x1 x2 x3) (C_Comb y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_Let x1 x2) (C_Let y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Free x1 x2) (C_Free y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Or x1 x2) (C_Or y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Case x1 x2 x3) (C_Case y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Var x1) (C_Var y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Lit x1) (C_Lit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Comb x1 x2 x3) (C_Comb y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_Let x1 x2) (C_Let y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Free x1 x2) (C_Free y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Or x1 x2) (C_Or y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Case x1 x2 x3) (C_Case y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Var x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Lit x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Comb x3 x4 x5) = ((i :=: (ChooseN 2 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_Let x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Free x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Or x3 x4) = ((i :=: (ChooseN 5 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Case x3 x4 x5) = ((i :=: (ChooseN 6 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_Typed x3 x4) = ((i :=: (ChooseN 7 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Expr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Expr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Expr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Expr cd i _) = error ("FlatCurry.Expr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Expr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Expr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Var x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Lit x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Comb x3 x4 x5) = [(i :=: (ChooseN 2 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_Let x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Free x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Or x3 x4) = [(i :=: (ChooseN 5 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Case x3 x4 x5) = [(i :=: (ChooseN 6 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_Typed x3 x4) = [(i :=: (ChooseN 7 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Expr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Expr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Expr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Expr cd i _) = error ("FlatCurry.Expr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Expr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Expr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Expr where
  (=?=) (Choice_C_Expr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Expr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Expr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Expr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Expr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Expr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Expr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  (=?=) (C_Var x1) (C_Var y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Lit x1) (C_Lit y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Comb x1 x2 x3) (C_Comb y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_Let x1 x2) (C_Let y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Free x1 x2) (C_Free y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Or x1 x2) (C_Or y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Case x1 x2 x3) (C_Case y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Expr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Expr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Expr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Expr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Expr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Expr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Expr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  (<?=) (C_Var x1) (C_Var y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Var _) (C_Lit _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Comb _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Free _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit x1) (C_Lit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Lit _) (C_Comb _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Free _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comb x1 x2 x3) (C_Comb y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_Comb _ _ _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comb _ _ _) (C_Free _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comb _ _ _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comb _ _ _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Comb _ _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let x1 x2) (C_Let y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Let _ _) (C_Free _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Free x1 x2) (C_Free y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Free _ _) (C_Or _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Free _ _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Free _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Or x1 x2) (C_Or y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Or _ _) (C_Case _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Or _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Case x1 x2 x3) (C_Case y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_Case _ _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_BranchExpr
     = C_Branch C_Pattern C_Expr
     | Choice_C_BranchExpr Cover ID C_BranchExpr C_BranchExpr
     | Choices_C_BranchExpr Cover ID ([C_BranchExpr])
     | Fail_C_BranchExpr Cover FailInfo
     | Guard_C_BranchExpr Cover Constraints C_BranchExpr

instance Show C_BranchExpr where
  showsPrec d (Choice_C_BranchExpr cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_BranchExpr cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_BranchExpr cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_BranchExpr cd info) = showChar '!'
  showsPrec _ (C_Branch x1 x2) = (showString "(Branch") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_BranchExpr where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Branch x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Branch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_BranchExpr where
  choiceCons = Choice_C_BranchExpr
  choicesCons = Choices_C_BranchExpr
  failCons = Fail_C_BranchExpr
  guardCons = Guard_C_BranchExpr
  try (Choice_C_BranchExpr cd i x y) = tryChoice cd i x y
  try (Choices_C_BranchExpr cd i xs) = tryChoices cd i xs
  try (Fail_C_BranchExpr cd info) = Fail cd info
  try (Guard_C_BranchExpr cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_BranchExpr cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_BranchExpr cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_BranchExpr cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_BranchExpr cd i _) = error ("FlatCurry.BranchExpr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_BranchExpr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_BranchExpr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_BranchExpr where
  generate s c = Choices_C_BranchExpr c (freeID [2] s) [(C_Branch (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_BranchExpr where
  ($!!) cont (C_Branch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Branch y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_BranchExpr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_BranchExpr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_BranchExpr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_BranchExpr cd info) _ _ = failCons cd info
  ($##) cont (C_Branch x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Branch y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_BranchExpr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_BranchExpr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_BranchExpr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_BranchExpr cd info) _ _ = failCons cd info
  searchNF search cont (C_Branch x1 x2) = search (\y1 -> search (\y2 -> cont (C_Branch y1 y2)) x2) x1
  searchNF _ _ x = error ("FlatCurry.BranchExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_BranchExpr where
  (=.=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Branch x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_BranchExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_BranchExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_BranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_BranchExpr cd i _) = error ("FlatCurry.BranchExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_BranchExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_BranchExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Branch x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_BranchExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_BranchExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_BranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_BranchExpr cd i _) = error ("FlatCurry.BranchExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_BranchExpr cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_BranchExpr cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_BranchExpr where
  (=?=) (Choice_C_BranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_BranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_BranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_BranchExpr cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_BranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_BranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_BranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_BranchExpr cd info) _ _ = failCons cd info
  (=?=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_BranchExpr cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_BranchExpr cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_BranchExpr cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_BranchExpr cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_BranchExpr cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_BranchExpr cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_BranchExpr cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_BranchExpr cd info) _ _ = failCons cd info
  (<?=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


data C_Pattern
     = C_Pattern (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)
     | C_LPattern C_Literal
     | Choice_C_Pattern Cover ID C_Pattern C_Pattern
     | Choices_C_Pattern Cover ID ([C_Pattern])
     | Fail_C_Pattern Cover FailInfo
     | Guard_C_Pattern Cover Constraints C_Pattern

instance Show C_Pattern where
  showsPrec d (Choice_C_Pattern cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Pattern cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Pattern cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Pattern cd info) = showChar '!'
  showsPrec _ (C_Pattern x1 x2) = (showString "(Pattern") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_LPattern x1) = (showString "(LPattern") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Pattern where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Pattern x1 x2,r2) | (_,r0) <- readQualified "FlatCurry" "Pattern" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_LPattern x1,r1) | (_,r0) <- readQualified "FlatCurry" "LPattern" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Pattern where
  choiceCons = Choice_C_Pattern
  choicesCons = Choices_C_Pattern
  failCons = Fail_C_Pattern
  guardCons = Guard_C_Pattern
  try (Choice_C_Pattern cd i x y) = tryChoice cd i x y
  try (Choices_C_Pattern cd i xs) = tryChoices cd i xs
  try (Fail_C_Pattern cd info) = Fail cd info
  try (Guard_C_Pattern cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Pattern cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Pattern cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Pattern cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Pattern cd i _) = error ("FlatCurry.Pattern.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Pattern cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Pattern cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Pattern where
  generate s c = Choices_C_Pattern c (freeID [2,1] s) [(C_Pattern (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_LPattern (generate (leftSupply s) c))]


instance NormalForm C_Pattern where
  ($!!) cont (C_Pattern x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Pattern y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_LPattern x1) d cs = (((\y1 d cs -> cont (C_LPattern y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Pattern cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Pattern cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Pattern cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  ($##) cont (C_Pattern x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Pattern y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_LPattern x1) d cs = (((\y1 d cs -> cont (C_LPattern y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Pattern cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Pattern cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Pattern cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  searchNF search cont (C_Pattern x1 x2) = search (\y1 -> search (\y2 -> cont (C_Pattern y1 y2)) x2) x1
  searchNF search cont (C_LPattern x1) = search (\y1 -> cont (C_LPattern y1)) x1
  searchNF _ _ x = error ("FlatCurry.Pattern.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Pattern where
  (=.=) (C_Pattern x1 x2) (C_Pattern y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_LPattern x1) (C_LPattern y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Pattern x1 x2) (C_Pattern y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_LPattern x1) (C_LPattern y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Pattern x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_LPattern x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Pattern cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Pattern cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Pattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Pattern cd i _) = error ("FlatCurry.Pattern.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Pattern cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Pattern cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Pattern x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_LPattern x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Pattern cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Pattern cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Pattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Pattern cd i _) = error ("FlatCurry.Pattern.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Pattern cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Pattern cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Pattern where
  (=?=) (Choice_C_Pattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Pattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Pattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Pattern cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Pattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Pattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Pattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  (=?=) (C_Pattern x1 x2) (C_Pattern y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_LPattern x1) (C_LPattern y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Pattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Pattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Pattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Pattern cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Pattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Pattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Pattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  (<?=) (C_Pattern x1 x2) (C_Pattern y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Pattern _ _) (C_LPattern _) _ _ = Curry_Prelude.C_True
  (<?=) (C_LPattern x1) (C_LPattern y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Literal
     = C_Intc Curry_Prelude.C_Int
     | C_Floatc Curry_Prelude.C_Float
     | C_Charc Curry_Prelude.C_Char
     | Choice_C_Literal Cover ID C_Literal C_Literal
     | Choices_C_Literal Cover ID ([C_Literal])
     | Fail_C_Literal Cover FailInfo
     | Guard_C_Literal Cover Constraints C_Literal

instance Show C_Literal where
  showsPrec d (Choice_C_Literal cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Literal cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Literal cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Literal cd info) = showChar '!'
  showsPrec _ (C_Intc x1) = (showString "(Intc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Floatc x1) = (showString "(Floatc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Charc x1) = (showString "(Charc") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Literal where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Intc x1,r1) | (_,r0) <- readQualified "FlatCurry" "Intc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Floatc x1,r1) | (_,r0) <- readQualified "FlatCurry" "Floatc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Charc x1,r1) | (_,r0) <- readQualified "FlatCurry" "Charc" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_Literal where
  choiceCons = Choice_C_Literal
  choicesCons = Choices_C_Literal
  failCons = Fail_C_Literal
  guardCons = Guard_C_Literal
  try (Choice_C_Literal cd i x y) = tryChoice cd i x y
  try (Choices_C_Literal cd i xs) = tryChoices cd i xs
  try (Fail_C_Literal cd info) = Fail cd info
  try (Guard_C_Literal cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Literal cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Literal cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Literal cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Literal cd i _) = error ("FlatCurry.Literal.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Literal cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Literal cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Literal where
  generate s c = Choices_C_Literal c (freeID [1,1,1] s) [(C_Intc (generate (leftSupply s) c)),(C_Floatc (generate (leftSupply s) c)),(C_Charc (generate (leftSupply s) c))]


instance NormalForm C_Literal where
  ($!!) cont (C_Intc x1) d cs = (((\y1 d cs -> cont (C_Intc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Floatc x1) d cs = (((\y1 d cs -> cont (C_Floatc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Charc x1) d cs = (((\y1 d cs -> cont (C_Charc y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Literal cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Literal cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Literal cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Literal cd info) _ _ = failCons cd info
  ($##) cont (C_Intc x1) d cs = (((\y1 d cs -> cont (C_Intc y1) d cs) $## x1) d) cs
  ($##) cont (C_Floatc x1) d cs = (((\y1 d cs -> cont (C_Floatc y1) d cs) $## x1) d) cs
  ($##) cont (C_Charc x1) d cs = (((\y1 d cs -> cont (C_Charc y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Literal cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Literal cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Literal cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Literal cd info) _ _ = failCons cd info
  searchNF search cont (C_Intc x1) = search (\y1 -> cont (C_Intc y1)) x1
  searchNF search cont (C_Floatc x1) = search (\y1 -> cont (C_Floatc y1)) x1
  searchNF search cont (C_Charc x1) = search (\y1 -> cont (C_Charc y1)) x1
  searchNF _ _ x = error ("FlatCurry.Literal.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Literal where
  (=.=) (C_Intc x1) (C_Intc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Floatc x1) (C_Floatc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Charc x1) (C_Charc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Intc x1) (C_Intc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Floatc x1) (C_Floatc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Charc x1) (C_Charc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Intc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Floatc x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Charc x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Literal cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Literal cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Literal cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Literal cd i _) = error ("FlatCurry.Literal.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Literal cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Literal cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Intc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Floatc x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Charc x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Literal cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Literal cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Literal cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Literal cd i _) = error ("FlatCurry.Literal.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Literal cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Literal cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Literal where
  (=?=) (Choice_C_Literal cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Literal cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Literal cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Literal cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Literal cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Literal cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Literal cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Literal cd info) _ _ = failCons cd info
  (=?=) (C_Intc x1) (C_Intc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Floatc x1) (C_Floatc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Charc x1) (C_Charc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Literal cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Literal cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Literal cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Literal cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Literal cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Literal cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Literal cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Literal cd info) _ _ = failCons cd info
  (<?=) (C_Intc x1) (C_Intc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Intc _) (C_Floatc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Intc _) (C_Charc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Floatc x1) (C_Floatc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Floatc _) (C_Charc _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Charc x1) (C_Charc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_readFlatCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_C_readFlatCurry x1 x3250 x3500 = d_C_readFlatCurryWithParseOptions x1 (Curry_Distribution.d_C_setQuiet Curry_Prelude.C_True (Curry_Distribution.d_C_defaultParams x3250 x3500) x3250 x3500) x3250 x3500

d_C_readFlatCurryWithParseOptions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_C_readFlatCurryWithParseOptions x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_lookupFileInLoadPath (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1 x2 x1) x3250 x3500

d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1 :: Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_lookupFileInLoadPath (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) (d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 x3 x1 x2) x3250 x3500

d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_2 x4 x1 x3 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x1 Curry_Prelude.C_Nothing x3250 x3500) (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x3250 x3500) x3250 x3500

d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryWithParseOptions_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x3250 x3500 = d_C_readFlatCurryFile x1 x3250 x3500

d_C_flatCurryFileName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_flatCurryFileName x1 x3250 x3500 = Curry_Distribution.d_C_inCurrySubdir (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500

d_C_flatCurryIntName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_flatCurryIntName x1 x3250 x3500 = Curry_Distribution.d_C_inCurrySubdir (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500

d_C_readFlatCurryFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_C_readFlatCurryFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_readFlatCurryFile_dot___hash_lambda5 x1) x3250 x3500

d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3250 x3500) d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13_dot___hash_lambda4 x3250 x3500

d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_C_return (Curry_ReadShowTerm.d_C_readUnqualifiedTerm (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) x1 x3250 x3500) x3250 x3500

d_OP_readFlatCurryFile_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readFlatCurryFile_dot___hash_lambda5 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13 x1 x3250 x3500
     Curry_Prelude.C_False -> let
          x3 = Curry_Distribution.d_C_inCurrySubdir x1 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x3 x3250 x3500) (d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x3) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readFlatCurryFile_dot___hash_lambda5 x1 x1002 x3250 x3500) (d_OP_readFlatCurryFile_dot___hash_lambda5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readFlatCurryFile_dot___hash_lambda5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readFlatCurryFile_dot___hash_lambda5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_readFlatCurryFile_dot_readExistingFCY_dot_13 x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1002 x3250 x3500) (d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readFlatCurryFile_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readFlatCurryInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_C_readFlatCurryInt x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (d_OP_readFlatCurryInt_dot___hash_lambda7 x1) x3250 x3500

d_OP_readFlatCurryInt_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryInt_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) (d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8 x2 x1) x3250 x3500

d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_1 x3 x1 x2 (Curry_Prelude.d_OP_bar_bar x1 x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8_dot___hash_lambda9 x3250 x3500) x3250 x3500

d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO C_Prog
d_OP_readFlatCurryInt_dot___hash_lambda7_dot___hash_lambda8_dot___hash_lambda9 x1 x3250 x3500 = d_C_readFlatCurryFile x1 x3250 x3500

d_C_writeFCY :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeFCY x1 x2 x3250 x3500 = Curry_Prelude.d_C_writeFile x1 (Curry_ReadShowTerm.d_C_showTerm x2 x3250 x3500) x3250 x3500

d_C_showQNameInModule :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showQNameInModule x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 x1 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showQNameInModule x1 x1002 x3250 x3500) (d_C_showQNameInModule x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showQNameInModule x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showQNameInModule x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_0 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_1 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Distribution.d_C_callFrontend Curry_Distribution.C_FINT x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_1 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Distribution.C_FrontendParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_2 x4 x1 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3250 x3500
     Curry_Prelude.C_False -> Curry_Distribution.d_C_callFrontendWithParams Curry_Distribution.C_FCY x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_2 x4 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
