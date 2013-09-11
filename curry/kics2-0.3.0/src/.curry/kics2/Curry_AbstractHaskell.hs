{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AbstractHaskell (C_Prog (..), C_Visibility (..), C_TypeDecl (..), C_Context (..), C_ConsDecl (..), C_TypeExpr (..), C_TypeSig (..), C_OpDecl (..), C_Fixity (..), C_FuncDecl (..), C_Rules (..), C_Rule (..), C_LocalDecl (..), C_Expr (..), C_Statement (..), C_Pattern (..), C_BranchExpr (..), C_Literal (..), C_QName, C_TVarIName, C_VarIName) where

import Basics
import qualified Curry_Prelude
type C_QName = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_TVarIName = Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)

type C_VarIName = Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)

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
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Prog x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AbstractHaskell" "Prog" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


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
  match _ _ _ _ _ _ (Choices_C_Prog cd i _) = error ("AbstractHaskell.Prog.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.Prog.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Prog where
  (=.=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Prog x1 x2 x3 x4 x5) (C_Prog y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Prog x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_Prog cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Prog cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Prog cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Prog cd i _) = error ("AbstractHaskell.Prog.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Prog cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Prog cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Prog x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_Prog cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Prog cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Prog cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Prog cd i _) = error ("AbstractHaskell.Prog.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  readsPrec _ s = (readParen False (\r -> [ (C_Public,r0) | (_,r0) <- readQualified "AbstractHaskell" "Public" r]) s) ++ (readParen False (\r -> [ (C_Private,r0) | (_,r0) <- readQualified "AbstractHaskell" "Private" r]) s)


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
  match _ _ _ _ _ _ (Choices_C_Visibility cd i _) = error ("AbstractHaskell.Visibility.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.Visibility.searchNF: no constructor: " ++ (show x))


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
  bind _ _ (Choices_C_Visibility cd i _) = error ("AbstractHaskell.Visibility.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Visibility cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Visibility cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Public = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Private = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_Visibility cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Visibility cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Visibility cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Visibility cd i _) = error ("AbstractHaskell.Visibility.lazyBind: Choices with ChoiceID: " ++ (show i))
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
     = C_Type (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Visibility (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List C_ConsDecl)
     | C_TypeSyn (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Visibility (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) C_TypeExpr
     | C_Instance (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_TypeExpr (Curry_Prelude.OP_List C_Context) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Rule))
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
  showsPrec _ (C_Instance x1 x2 x3 x4) = (showString "(Instance") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read C_TypeDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Type x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractHaskell" "Type" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ ((readParen (d > 10) (\r -> [ (C_TypeSyn x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractHaskell" "TypeSyn" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ (readParen (d > 10) (\r -> [ (C_Instance x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractHaskell" "Instance" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s))


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
  match _ _ _ _ _ _ (Choices_C_TypeDecl cd i _) = error ("AbstractHaskell.TypeDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TypeDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TypeDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TypeDecl where
  generate s c = Choices_C_TypeDecl c (freeID [4,4,4] s) [(C_Type (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_TypeSyn (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(C_Instance (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_TypeDecl where
  ($!!) cont (C_Type x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Type y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_TypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_TypeSyn y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Instance x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Instance y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TypeDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TypeDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Type x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Type y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_TypeSyn x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_TypeSyn y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Instance x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_Instance y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TypeDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TypeDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TypeDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TypeDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Type x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Type y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_TypeSyn x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_TypeSyn y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_Instance x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_Instance y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.TypeDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TypeDecl where
  (=.=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_Instance x1 x2 x3 x4) (C_Instance y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Type x1 x2 x3 x4) (C_Type y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_Instance x1 x2 x3 x4) (C_Instance y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Type x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_TypeSyn x3 x4 x5 x6) = ((i :=: (ChooseN 1 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_Instance x3 x4 x5 x6) = ((i :=: (ChooseN 2 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_TypeDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TypeDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TypeDecl cd i _) = error ("AbstractHaskell.TypeDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Type x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_TypeSyn x3 x4 x5 x6) = [(i :=: (ChooseN 1 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_Instance x3 x4 x5 x6) = [(i :=: (ChooseN 2 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_TypeDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeDecl cd i _) = error ("AbstractHaskell.TypeDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  (=?=) (C_Instance x1 x2 x3 x4) (C_Instance y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
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
  (<?=) (C_Type _ _ _ _) (C_Instance _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TypeSyn x1 x2 x3 x4) (C_TypeSyn y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_TypeSyn _ _ _ _) (C_Instance _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Instance x1 x2 x3 x4) (C_Instance y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Context
     = C_Context (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
     | Choice_C_Context Cover ID C_Context C_Context
     | Choices_C_Context Cover ID ([C_Context])
     | Fail_C_Context Cover FailInfo
     | Guard_C_Context Cover Constraints C_Context

instance Show C_Context where
  showsPrec d (Choice_C_Context cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Context cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Context cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Context cd info) = showChar '!'
  showsPrec _ (C_Context x1 x2) = (showString "(Context") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Context where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Context x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Context" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_Context where
  choiceCons = Choice_C_Context
  choicesCons = Choices_C_Context
  failCons = Fail_C_Context
  guardCons = Guard_C_Context
  try (Choice_C_Context cd i x y) = tryChoice cd i x y
  try (Choices_C_Context cd i xs) = tryChoices cd i xs
  try (Fail_C_Context cd info) = Fail cd info
  try (Guard_C_Context cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Context cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Context cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Context cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Context cd i _) = error ("AbstractHaskell.Context.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Context cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Context cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Context where
  generate s c = Choices_C_Context c (freeID [2] s) [(C_Context (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Context where
  ($!!) cont (C_Context x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Context y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Context cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Context cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Context cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Context cd info) _ _ = failCons cd info
  ($##) cont (C_Context x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Context y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Context cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Context cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Context cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Context cd info) _ _ = failCons cd info
  searchNF search cont (C_Context x1 x2) = search (\y1 -> search (\y2 -> cont (C_Context y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.Context.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Context where
  (=.=) (C_Context x1 x2) (C_Context y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Context x1 x2) (C_Context y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Context x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Context cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Context cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Context cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Context cd i _) = error ("AbstractHaskell.Context.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Context cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Context cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Context x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Context cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Context cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Context cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Context cd i _) = error ("AbstractHaskell.Context.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Context cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Context cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Context where
  (=?=) (Choice_C_Context cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Context cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Context cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Context cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Context cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Context cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Context cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Context cd info) _ _ = failCons cd info
  (=?=) (C_Context x1 x2) (C_Context y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (<?=) (Choice_C_Context cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Context cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Context cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Context cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Context cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Context cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Context cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Context cd info) _ _ = failCons cd info
  (<?=) (C_Context x1 x2) (C_Context y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs


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
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Cons x1 x2 x3 x4,r4) | (_,r0) <- readQualified "AbstractHaskell" "Cons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


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
  match _ _ _ _ _ _ (Choices_C_ConsDecl cd i _) = error ("AbstractHaskell.ConsDecl.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.ConsDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ConsDecl where
  (=.=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Cons x1 x2 x3 x4) (C_Cons y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Cons x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_ConsDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ConsDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ConsDecl cd i _) = error ("AbstractHaskell.ConsDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ConsDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ConsDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Cons x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_ConsDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ConsDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ConsDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ConsDecl cd i _) = error ("AbstractHaskell.ConsDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
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
     = C_TVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
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
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_TVar x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "TVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_FuncType x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "FuncType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_TCons x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "TCons" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


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
  match _ _ _ _ _ _ (Choices_C_TypeExpr cd i _) = error ("AbstractHaskell.TypeExpr.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.TypeExpr.searchNF: no constructor: " ++ (show x))


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
  bind _ _ (Choices_C_TypeExpr cd i _) = error ("AbstractHaskell.TypeExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_TVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_FuncType x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_TCons x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_TypeExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeExpr cd i _) = error ("AbstractHaskell.TypeExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
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


data C_TypeSig
     = C_Untyped
     | C_FType C_TypeExpr
     | C_CType (Curry_Prelude.OP_List C_Context) C_TypeExpr
     | Choice_C_TypeSig Cover ID C_TypeSig C_TypeSig
     | Choices_C_TypeSig Cover ID ([C_TypeSig])
     | Fail_C_TypeSig Cover FailInfo
     | Guard_C_TypeSig Cover Constraints C_TypeSig

instance Show C_TypeSig where
  showsPrec d (Choice_C_TypeSig cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_TypeSig cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_TypeSig cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_TypeSig cd info) = showChar '!'
  showsPrec _ C_Untyped = showString "Untyped"
  showsPrec _ (C_FType x1) = (showString "(FType") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_CType x1 x2) = (showString "(CType") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_TypeSig where
  readsPrec d s = (readParen False (\r -> [ (C_Untyped,r0) | (_,r0) <- readQualified "AbstractHaskell" "Untyped" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_FType x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "FType" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_CType x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "CType" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))


instance NonDet C_TypeSig where
  choiceCons = Choice_C_TypeSig
  choicesCons = Choices_C_TypeSig
  failCons = Fail_C_TypeSig
  guardCons = Guard_C_TypeSig
  try (Choice_C_TypeSig cd i x y) = tryChoice cd i x y
  try (Choices_C_TypeSig cd i xs) = tryChoices cd i xs
  try (Fail_C_TypeSig cd info) = Fail cd info
  try (Guard_C_TypeSig cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_TypeSig cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_TypeSig cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_TypeSig cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_TypeSig cd i _) = error ("AbstractHaskell.TypeSig.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_TypeSig cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_TypeSig cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_TypeSig where
  generate s c = Choices_C_TypeSig c (freeID [0,1,2] s) [C_Untyped,(C_FType (generate (leftSupply s) c)),(C_CType (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_TypeSig where
  ($!!) cont C_Untyped d cs = cont C_Untyped d cs
  ($!!) cont (C_FType x1) d cs = (((\y1 d cs -> cont (C_FType y1) d cs) $!! x1) d) cs
  ($!!) cont (C_CType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CType y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_TypeSig cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_TypeSig cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_TypeSig cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_TypeSig cd info) _ _ = failCons cd info
  ($##) cont C_Untyped d cs = cont C_Untyped d cs
  ($##) cont (C_FType x1) d cs = (((\y1 d cs -> cont (C_FType y1) d cs) $## x1) d) cs
  ($##) cont (C_CType x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_CType y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_TypeSig cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_TypeSig cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_TypeSig cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_TypeSig cd info) _ _ = failCons cd info
  searchNF _ cont C_Untyped = cont C_Untyped
  searchNF search cont (C_FType x1) = search (\y1 -> cont (C_FType y1)) x1
  searchNF search cont (C_CType x1 x2) = search (\y1 -> search (\y2 -> cont (C_CType y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.TypeSig.searchNF: no constructor: " ++ (show x))


instance Unifiable C_TypeSig where
  (=.=) C_Untyped C_Untyped d cs = C_Success
  (=.=) (C_FType x1) (C_FType y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_CType x1 x2) (C_CType y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Untyped C_Untyped d cs = C_Success
  (=.<=) (C_FType x1) (C_FType y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_CType x1 x2) (C_CType y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Untyped = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_FType x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_CType x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_TypeSig cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_TypeSig cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_TypeSig cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_TypeSig cd i _) = error ("AbstractHaskell.TypeSig.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_TypeSig cd info) = [(Unsolvable info)]
  bind d i (Guard_C_TypeSig cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Untyped = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_FType x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_CType x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_TypeSig cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_TypeSig cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_TypeSig cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_TypeSig cd i _) = error ("AbstractHaskell.TypeSig.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_TypeSig cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_TypeSig cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_TypeSig where
  (=?=) (Choice_C_TypeSig cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_TypeSig cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_TypeSig cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_TypeSig cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_TypeSig cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_TypeSig cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_TypeSig cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_TypeSig cd info) _ _ = failCons cd info
  (=?=) C_Untyped C_Untyped d cs = Curry_Prelude.C_True
  (=?=) (C_FType x1) (C_FType y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_CType x1 x2) (C_CType y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_TypeSig cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_TypeSig cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_TypeSig cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_TypeSig cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_TypeSig cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_TypeSig cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_TypeSig cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_TypeSig cd info) _ _ = failCons cd info
  (<?=) C_Untyped C_Untyped d cs = Curry_Prelude.C_True
  (<?=) C_Untyped (C_FType _) _ _ = Curry_Prelude.C_True
  (<?=) C_Untyped (C_CType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_FType x1) (C_FType y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_FType _) (C_CType _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CType x1 x2) (C_CType y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
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
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Op x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractHaskell" "Op" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


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
  match _ _ _ _ _ _ (Choices_C_OpDecl cd i _) = error ("AbstractHaskell.OpDecl.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.OpDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_OpDecl where
  (=.=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Op x1 x2 x3) (C_Op y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Op x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_OpDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_OpDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_OpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_OpDecl cd i _) = error ("AbstractHaskell.OpDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_OpDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_OpDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Op x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_OpDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_OpDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_OpDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_OpDecl cd i _) = error ("AbstractHaskell.OpDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  readsPrec _ s = (readParen False (\r -> [ (C_InfixOp,r0) | (_,r0) <- readQualified "AbstractHaskell" "InfixOp" r]) s) ++ ((readParen False (\r -> [ (C_InfixlOp,r0) | (_,r0) <- readQualified "AbstractHaskell" "InfixlOp" r]) s) ++ (readParen False (\r -> [ (C_InfixrOp,r0) | (_,r0) <- readQualified "AbstractHaskell" "InfixrOp" r]) s))


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
  match _ _ _ _ _ _ (Choices_C_Fixity cd i _) = error ("AbstractHaskell.Fixity.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.Fixity.searchNF: no constructor: " ++ (show x))


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
  bind _ _ (Choices_C_Fixity cd i _) = error ("AbstractHaskell.Fixity.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Fixity cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Fixity cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_InfixOp = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_InfixlOp = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_InfixrOp = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_Fixity cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Fixity cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Fixity cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Fixity cd i _) = error ("AbstractHaskell.Fixity.lazyBind: Choices with ChoiceID: " ++ (show i))
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
     = C_Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int C_Visibility C_TypeSig C_Rules
     | Choice_C_FuncDecl Cover ID C_FuncDecl C_FuncDecl
     | Choices_C_FuncDecl Cover ID ([C_FuncDecl])
     | Fail_C_FuncDecl Cover FailInfo
     | Guard_C_FuncDecl Cover Constraints C_FuncDecl

instance Show C_FuncDecl where
  showsPrec d (Choice_C_FuncDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FuncDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FuncDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FuncDecl cd info) = showChar '!'
  showsPrec _ (C_Func x1 x2 x3 x4 x5 x6) = (showString "(Func") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . (showChar ')')))))))))))))


instance Read C_FuncDecl where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Func x1 x2 x3 x4 x5 x6,r6) | (_,r0) <- readQualified "AbstractHaskell" "Func" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5]) s


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
  match _ _ _ _ _ _ (Choices_C_FuncDecl cd i _) = error ("AbstractHaskell.FuncDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FuncDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FuncDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FuncDecl where
  generate s c = Choices_C_FuncDecl c (freeID [6] s) [(C_Func (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_FuncDecl where
  ($!!) cont (C_Func x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_Func y1 y2 y3 y4 y5 y6) d cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_FuncDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FuncDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  ($##) cont (C_Func x1 x2 x3 x4 x5 x6) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> cont (C_Func y1 y2 y3 y4 y5 y6) d cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_FuncDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FuncDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_Func x1 x2 x3 x4 x5 x6) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> cont (C_Func y1 y2 y3 y4 y5 y6)) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.FuncDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FuncDecl where
  (=.=) (C_Func x1 x2 x3 x4 x5 x6) (C_Func y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & (((x6 =:= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Func x1 x2 x3 x4 x5 x6) (C_Func y1 y2 y3 y4 y5 y6) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & (((x6 =:<= y6) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Func x3 x4 x5 x6 x7 x8) = ((i :=: (ChooseN 0 6)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (leftID (rightID i))) x6),(bind cd (rightID (leftID (rightID i))) x7),(bind cd (rightID (rightID i)) x8)]))
  bind d i (Choice_C_FuncDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FuncDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FuncDecl cd i _) = error ("AbstractHaskell.FuncDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FuncDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FuncDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Func x3 x4 x5 x6 x7 x8) = [(i :=: (ChooseN 0 6)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x6))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x7))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x8)))]
  lazyBind d i (Choice_C_FuncDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FuncDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FuncDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FuncDecl cd i _) = error ("AbstractHaskell.FuncDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  (=?=) (C_Func x1 x2 x3 x4 x5 x6) (C_Func y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.=?= y6) d) cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_FuncDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FuncDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FuncDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FuncDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FuncDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FuncDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FuncDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FuncDecl cd info) _ _ = failCons cd info
  (<?=) (C_Func x1 x2 x3 x4 x5 x6) (C_Func y1 y2 y3 y4 y5 y6) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (((x6 Curry_Prelude.<?= y6) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


data C_Rules
     = C_Rules (Curry_Prelude.OP_List C_Rule)
     | C_External (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_Rules Cover ID C_Rules C_Rules
     | Choices_C_Rules Cover ID ([C_Rules])
     | Fail_C_Rules Cover FailInfo
     | Guard_C_Rules Cover Constraints C_Rules

instance Show C_Rules where
  showsPrec d (Choice_C_Rules cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Rules cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Rules cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Rules cd info) = showChar '!'
  showsPrec _ (C_Rules x1) = (showString "(Rules") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_External x1) = (showString "(External") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Rules where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Rules x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Rules" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_External x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "External" r, (x1,r1) <- readsPrec 11 r0]) s)


instance NonDet C_Rules where
  choiceCons = Choice_C_Rules
  choicesCons = Choices_C_Rules
  failCons = Fail_C_Rules
  guardCons = Guard_C_Rules
  try (Choice_C_Rules cd i x y) = tryChoice cd i x y
  try (Choices_C_Rules cd i xs) = tryChoices cd i xs
  try (Fail_C_Rules cd info) = Fail cd info
  try (Guard_C_Rules cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Rules cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Rules cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Rules cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Rules cd i _) = error ("AbstractHaskell.Rules.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Rules cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Rules cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Rules where
  generate s c = Choices_C_Rules c (freeID [1,1] s) [(C_Rules (generate (leftSupply s) c)),(C_External (generate (leftSupply s) c))]


instance NormalForm C_Rules where
  ($!!) cont (C_Rules x1) d cs = (((\y1 d cs -> cont (C_Rules y1) d cs) $!! x1) d) cs
  ($!!) cont (C_External x1) d cs = (((\y1 d cs -> cont (C_External y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Rules cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Rules cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Rules cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Rules cd info) _ _ = failCons cd info
  ($##) cont (C_Rules x1) d cs = (((\y1 d cs -> cont (C_Rules y1) d cs) $## x1) d) cs
  ($##) cont (C_External x1) d cs = (((\y1 d cs -> cont (C_External y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Rules cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Rules cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Rules cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Rules cd info) _ _ = failCons cd info
  searchNF search cont (C_Rules x1) = search (\y1 -> cont (C_Rules y1)) x1
  searchNF search cont (C_External x1) = search (\y1 -> cont (C_External y1)) x1
  searchNF _ _ x = error ("AbstractHaskell.Rules.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Rules where
  (=.=) (C_Rules x1) (C_Rules y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_External x1) (C_External y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Rules x1) (C_Rules y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_External x1) (C_External y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Rules x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_External x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Rules cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Rules cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Rules cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Rules cd i _) = error ("AbstractHaskell.Rules.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Rules cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Rules cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Rules x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_External x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Rules cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Rules cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Rules cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Rules cd i _) = error ("AbstractHaskell.Rules.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Rules cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Rules cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Rules where
  (=?=) (Choice_C_Rules cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Rules cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Rules cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Rules cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Rules cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Rules cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Rules cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Rules cd info) _ _ = failCons cd info
  (=?=) (C_Rules x1) (C_Rules y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_External x1) (C_External y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Rules cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Rules cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Rules cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Rules cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Rules cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Rules cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Rules cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Rules cd info) _ _ = failCons cd info
  (<?=) (C_Rules x1) (C_Rules y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Rules _) (C_External _) _ _ = Curry_Prelude.C_True
  (<?=) (C_External x1) (C_External y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Rule
     = C_Rule (Curry_Prelude.OP_List C_Pattern) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_Expr C_Expr)) (Curry_Prelude.OP_List C_LocalDecl)
     | Choice_C_Rule Cover ID C_Rule C_Rule
     | Choices_C_Rule Cover ID ([C_Rule])
     | Fail_C_Rule Cover FailInfo
     | Guard_C_Rule Cover Constraints C_Rule

instance Show C_Rule where
  showsPrec d (Choice_C_Rule cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Rule cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Rule cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Rule cd info) = showChar '!'
  showsPrec _ (C_Rule x1 x2 x3) = (showString "(Rule") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_Rule where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Rule x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractHaskell" "Rule" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


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
  match _ _ _ _ _ _ (Choices_C_Rule cd i _) = error ("AbstractHaskell.Rule.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Rule cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Rule cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Rule where
  generate s c = Choices_C_Rule c (freeID [3] s) [(C_Rule (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_Rule where
  ($!!) cont (C_Rule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Rule y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Rule cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Rule cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Rule cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  ($##) cont (C_Rule x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_Rule y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Rule cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Rule cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Rule cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  searchNF search cont (C_Rule x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_Rule y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.Rule.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Rule where
  (=.=) (C_Rule x1 x2 x3) (C_Rule y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Rule x1 x2 x3) (C_Rule y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Rule x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_Rule cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Rule cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Rule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Rule cd i _) = error ("AbstractHaskell.Rule.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Rule cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Rule cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Rule x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_Rule cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Rule cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Rule cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Rule cd i _) = error ("AbstractHaskell.Rule.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  (=?=) (C_Rule x1 x2 x3) (C_Rule y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_Rule cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Rule cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Rule cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Rule cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Rule cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Rule cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Rule cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Rule cd info) _ _ = failCons cd info
  (<?=) (C_Rule x1 x2 x3) (C_Rule y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


data C_LocalDecl
     = C_LocalFunc C_FuncDecl
     | C_LocalPat C_Pattern C_Expr (Curry_Prelude.OP_List C_LocalDecl)
     | C_LocalVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | Choice_C_LocalDecl Cover ID C_LocalDecl C_LocalDecl
     | Choices_C_LocalDecl Cover ID ([C_LocalDecl])
     | Fail_C_LocalDecl Cover FailInfo
     | Guard_C_LocalDecl Cover Constraints C_LocalDecl

instance Show C_LocalDecl where
  showsPrec d (Choice_C_LocalDecl cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_LocalDecl cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_LocalDecl cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_LocalDecl cd info) = showChar '!'
  showsPrec _ (C_LocalFunc x1) = (showString "(LocalFunc") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_LocalPat x1 x2 x3) = (showString "(LocalPat") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_LocalVar x1) = (showString "(LocalVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_LocalDecl where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_LocalFunc x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "LocalFunc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_LocalPat x1 x2 x3,r3) | (_,r0) <- readQualified "AbstractHaskell" "LocalPat" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ (readParen (d > 10) (\r -> [ (C_LocalVar x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "LocalVar" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_LocalDecl where
  choiceCons = Choice_C_LocalDecl
  choicesCons = Choices_C_LocalDecl
  failCons = Fail_C_LocalDecl
  guardCons = Guard_C_LocalDecl
  try (Choice_C_LocalDecl cd i x y) = tryChoice cd i x y
  try (Choices_C_LocalDecl cd i xs) = tryChoices cd i xs
  try (Fail_C_LocalDecl cd info) = Fail cd info
  try (Guard_C_LocalDecl cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_LocalDecl cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_LocalDecl cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_LocalDecl cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_LocalDecl cd i _) = error ("AbstractHaskell.LocalDecl.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_LocalDecl cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_LocalDecl cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_LocalDecl where
  generate s c = Choices_C_LocalDecl c (freeID [1,3,1] s) [(C_LocalFunc (generate (leftSupply s) c)),(C_LocalPat (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(C_LocalVar (generate (leftSupply s) c))]


instance NormalForm C_LocalDecl where
  ($!!) cont (C_LocalFunc x1) d cs = (((\y1 d cs -> cont (C_LocalFunc y1) d cs) $!! x1) d) cs
  ($!!) cont (C_LocalPat x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_LocalPat y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_LocalVar x1) d cs = (((\y1 d cs -> cont (C_LocalVar y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_LocalDecl cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_LocalDecl cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_LocalDecl cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_LocalDecl cd info) _ _ = failCons cd info
  ($##) cont (C_LocalFunc x1) d cs = (((\y1 d cs -> cont (C_LocalFunc y1) d cs) $## x1) d) cs
  ($##) cont (C_LocalPat x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_LocalPat y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_LocalVar x1) d cs = (((\y1 d cs -> cont (C_LocalVar y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_LocalDecl cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_LocalDecl cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_LocalDecl cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_LocalDecl cd info) _ _ = failCons cd info
  searchNF search cont (C_LocalFunc x1) = search (\y1 -> cont (C_LocalFunc y1)) x1
  searchNF search cont (C_LocalPat x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_LocalPat y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_LocalVar x1) = search (\y1 -> cont (C_LocalVar y1)) x1
  searchNF _ _ x = error ("AbstractHaskell.LocalDecl.searchNF: no constructor: " ++ (show x))


instance Unifiable C_LocalDecl where
  (=.=) (C_LocalFunc x1) (C_LocalFunc y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_LocalPat x1 x2 x3) (C_LocalPat y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_LocalVar x1) (C_LocalVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_LocalFunc x1) (C_LocalFunc y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_LocalPat x1 x2 x3) (C_LocalPat y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_LocalVar x1) (C_LocalVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_LocalFunc x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_LocalPat x3 x4 x5) = ((i :=: (ChooseN 1 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_LocalVar x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_LocalDecl cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_LocalDecl cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_LocalDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_LocalDecl cd i _) = error ("AbstractHaskell.LocalDecl.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_LocalDecl cd info) = [(Unsolvable info)]
  bind d i (Guard_C_LocalDecl cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_LocalFunc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_LocalPat x3 x4 x5) = [(i :=: (ChooseN 1 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_LocalVar x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_LocalDecl cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_LocalDecl cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_LocalDecl cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_LocalDecl cd i _) = error ("AbstractHaskell.LocalDecl.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_LocalDecl cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_LocalDecl cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_LocalDecl where
  (=?=) (Choice_C_LocalDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_LocalDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_LocalDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_LocalDecl cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_LocalDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_LocalDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_LocalDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_LocalDecl cd info) _ _ = failCons cd info
  (=?=) (C_LocalFunc x1) (C_LocalFunc y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_LocalPat x1 x2 x3) (C_LocalPat y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_LocalVar x1) (C_LocalVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_LocalDecl cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_LocalDecl cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_LocalDecl cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_LocalDecl cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_LocalDecl cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_LocalDecl cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_LocalDecl cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_LocalDecl cd info) _ _ = failCons cd info
  (<?=) (C_LocalFunc x1) (C_LocalFunc y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_LocalFunc _) (C_LocalPat _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_LocalFunc _) (C_LocalVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_LocalPat x1 x2 x3) (C_LocalPat y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_LocalPat _ _ _) (C_LocalVar _) _ _ = Curry_Prelude.C_True
  (<?=) (C_LocalVar x1) (C_LocalVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Expr
     = C_Var (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_Lit C_Literal
     | C_Symbol (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_Apply C_Expr C_Expr
     | C_Lambda (Curry_Prelude.OP_List C_Pattern) C_Expr
     | C_Let (Curry_Prelude.OP_List C_LocalDecl) C_Expr
     | C_DoExpr (Curry_Prelude.OP_List C_Statement)
     | C_ListComp C_Expr (Curry_Prelude.OP_List C_Statement)
     | C_Case C_Expr (Curry_Prelude.OP_List C_BranchExpr)
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
  showsPrec _ (C_Symbol x1) = (showString "(Symbol") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_Apply x1 x2) = (showString "(Apply") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Lambda x1 x2) = (showString "(Lambda") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Let x1 x2) = (showString "(Let") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_DoExpr x1) = (showString "(DoExpr") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_ListComp x1 x2) = (showString "(ListComp") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Case x1 x2) = (showString "(Case") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_Typed x1 x2) = (showString "(Typed") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Expr where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Var x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Var" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Lit x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Lit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Symbol x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Symbol" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Apply x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Apply" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Lambda x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Lambda" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Let x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Let" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_DoExpr x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "DoExpr" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_ListComp x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "ListComp" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_Case x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Case" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_Typed x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Typed" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)))))))))


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
  match _ _ _ _ _ _ (Choices_C_Expr cd i _) = error ("AbstractHaskell.Expr.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Expr cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Expr cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Expr where
  generate s c = Choices_C_Expr c (freeID [1,1,1,2,2,2,1,2,2,2] s) [(C_Var (generate (leftSupply s) c)),(C_Lit (generate (leftSupply s) c)),(C_Symbol (generate (leftSupply s) c)),(C_Apply (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Lambda (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Let (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_DoExpr (generate (leftSupply s) c)),(C_ListComp (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Case (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_Typed (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Expr where
  ($!!) cont (C_Var x1) d cs = (((\y1 d cs -> cont (C_Var y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Lit x1) d cs = (((\y1 d cs -> cont (C_Lit y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Symbol x1) d cs = (((\y1 d cs -> cont (C_Symbol y1) d cs) $!! x1) d) cs
  ($!!) cont (C_Apply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Apply y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Lambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Lambda y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Let x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Let y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_DoExpr x1) d cs = (((\y1 d cs -> cont (C_DoExpr y1) d cs) $!! x1) d) cs
  ($!!) cont (C_ListComp x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ListComp y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Case x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Case y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_Typed x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Typed y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Expr cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Expr cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Expr cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  ($##) cont (C_Var x1) d cs = (((\y1 d cs -> cont (C_Var y1) d cs) $## x1) d) cs
  ($##) cont (C_Lit x1) d cs = (((\y1 d cs -> cont (C_Lit y1) d cs) $## x1) d) cs
  ($##) cont (C_Symbol x1) d cs = (((\y1 d cs -> cont (C_Symbol y1) d cs) $## x1) d) cs
  ($##) cont (C_Apply x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Apply y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Lambda x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Lambda y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Let x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Let y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_DoExpr x1) d cs = (((\y1 d cs -> cont (C_DoExpr y1) d cs) $## x1) d) cs
  ($##) cont (C_ListComp x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_ListComp y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Case x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Case y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_Typed x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Typed y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Expr cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Expr cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Expr cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Expr cd info) _ _ = failCons cd info
  searchNF search cont (C_Var x1) = search (\y1 -> cont (C_Var y1)) x1
  searchNF search cont (C_Lit x1) = search (\y1 -> cont (C_Lit y1)) x1
  searchNF search cont (C_Symbol x1) = search (\y1 -> cont (C_Symbol y1)) x1
  searchNF search cont (C_Apply x1 x2) = search (\y1 -> search (\y2 -> cont (C_Apply y1 y2)) x2) x1
  searchNF search cont (C_Lambda x1 x2) = search (\y1 -> search (\y2 -> cont (C_Lambda y1 y2)) x2) x1
  searchNF search cont (C_Let x1 x2) = search (\y1 -> search (\y2 -> cont (C_Let y1 y2)) x2) x1
  searchNF search cont (C_DoExpr x1) = search (\y1 -> cont (C_DoExpr y1)) x1
  searchNF search cont (C_ListComp x1 x2) = search (\y1 -> search (\y2 -> cont (C_ListComp y1 y2)) x2) x1
  searchNF search cont (C_Case x1 x2) = search (\y1 -> search (\y2 -> cont (C_Case y1 y2)) x2) x1
  searchNF search cont (C_Typed x1 x2) = search (\y1 -> search (\y2 -> cont (C_Typed y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.Expr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Expr where
  (=.=) (C_Var x1) (C_Var y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Lit x1) (C_Lit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Symbol x1) (C_Symbol y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_Apply x1 x2) (C_Apply y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Lambda x1 x2) (C_Lambda y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Let x1 x2) (C_Let y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_DoExpr x1) (C_DoExpr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_ListComp x1 x2) (C_ListComp y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Case x1 x2) (C_Case y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Var x1) (C_Var y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Lit x1) (C_Lit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Symbol x1) (C_Symbol y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_Apply x1 x2) (C_Apply y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Lambda x1 x2) (C_Lambda y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Let x1 x2) (C_Let y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_DoExpr x1) (C_DoExpr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_ListComp x1 x2) (C_ListComp y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Case x1 x2) (C_Case y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Var x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Lit x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Symbol x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_Apply x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Lambda x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Let x3 x4) = ((i :=: (ChooseN 5 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_DoExpr x3) = ((i :=: (ChooseN 6 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_ListComp x3 x4) = ((i :=: (ChooseN 7 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Case x3 x4) = ((i :=: (ChooseN 8 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_Typed x3 x4) = ((i :=: (ChooseN 9 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Expr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Expr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Expr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Expr cd i _) = error ("AbstractHaskell.Expr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Expr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Expr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Var x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Lit x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Symbol x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Apply x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Lambda x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Let x3 x4) = [(i :=: (ChooseN 5 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_DoExpr x3) = [(i :=: (ChooseN 6 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_ListComp x3 x4) = [(i :=: (ChooseN 7 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Case x3 x4) = [(i :=: (ChooseN 8 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_Typed x3 x4) = [(i :=: (ChooseN 9 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Expr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Expr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Expr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Expr cd i _) = error ("AbstractHaskell.Expr.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  (=?=) (C_Symbol x1) (C_Symbol y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_Apply x1 x2) (C_Apply y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Lambda x1 x2) (C_Lambda y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Let x1 x2) (C_Let y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_DoExpr x1) (C_DoExpr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_ListComp x1 x2) (C_ListComp y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_Case x1 x2) (C_Case y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
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
  (<?=) (C_Var _) (C_Symbol _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Apply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Lambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Var _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit x1) (C_Lit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Lit _) (C_Symbol _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Apply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Lambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lit _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol x1) (C_Symbol y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_Symbol _) (C_Apply _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_Lambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Symbol _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply x1 x2) (C_Apply y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Apply _ _) (C_Lambda _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply _ _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply _ _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply _ _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply _ _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Apply _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lambda x1 x2) (C_Lambda y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Lambda _ _) (C_Let _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lambda _ _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lambda _ _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lambda _ _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Lambda _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let x1 x2) (C_Let y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Let _ _) (C_DoExpr _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Let _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DoExpr x1) (C_DoExpr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_DoExpr _) (C_ListComp _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DoExpr _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DoExpr _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ListComp x1 x2) (C_ListComp y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_ListComp _ _) (C_Case _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_ListComp _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Case x1 x2) (C_Case y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Case _ _) (C_Typed _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Typed x1 x2) (C_Typed y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Statement
     = C_SExpr C_Expr
     | C_SPat C_Pattern C_Expr
     | C_SLet (Curry_Prelude.OP_List C_LocalDecl)
     | Choice_C_Statement Cover ID C_Statement C_Statement
     | Choices_C_Statement Cover ID ([C_Statement])
     | Fail_C_Statement Cover FailInfo
     | Guard_C_Statement Cover Constraints C_Statement

instance Show C_Statement where
  showsPrec d (Choice_C_Statement cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Statement cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Statement cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Statement cd info) = showChar '!'
  showsPrec _ (C_SExpr x1) = (showString "(SExpr") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_SPat x1 x2) = (showString "(SPat") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_SLet x1) = (showString "(SLet") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Statement where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_SExpr x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "SExpr" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_SPat x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "SPat" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_SLet x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "SLet" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_Statement where
  choiceCons = Choice_C_Statement
  choicesCons = Choices_C_Statement
  failCons = Fail_C_Statement
  guardCons = Guard_C_Statement
  try (Choice_C_Statement cd i x y) = tryChoice cd i x y
  try (Choices_C_Statement cd i xs) = tryChoices cd i xs
  try (Fail_C_Statement cd info) = Fail cd info
  try (Guard_C_Statement cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Statement cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Statement cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Statement cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Statement cd i _) = error ("AbstractHaskell.Statement.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Statement cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Statement cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Statement where
  generate s c = Choices_C_Statement c (freeID [1,2,1] s) [(C_SExpr (generate (leftSupply s) c)),(C_SPat (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_SLet (generate (leftSupply s) c))]


instance NormalForm C_Statement where
  ($!!) cont (C_SExpr x1) d cs = (((\y1 d cs -> cont (C_SExpr y1) d cs) $!! x1) d) cs
  ($!!) cont (C_SPat x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SPat y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_SLet x1) d cs = (((\y1 d cs -> cont (C_SLet y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Statement cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Statement cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Statement cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Statement cd info) _ _ = failCons cd info
  ($##) cont (C_SExpr x1) d cs = (((\y1 d cs -> cont (C_SExpr y1) d cs) $## x1) d) cs
  ($##) cont (C_SPat x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SPat y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_SLet x1) d cs = (((\y1 d cs -> cont (C_SLet y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_Statement cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Statement cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Statement cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Statement cd info) _ _ = failCons cd info
  searchNF search cont (C_SExpr x1) = search (\y1 -> cont (C_SExpr y1)) x1
  searchNF search cont (C_SPat x1 x2) = search (\y1 -> search (\y2 -> cont (C_SPat y1 y2)) x2) x1
  searchNF search cont (C_SLet x1) = search (\y1 -> cont (C_SLet y1)) x1
  searchNF _ _ x = error ("AbstractHaskell.Statement.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Statement where
  (=.=) (C_SExpr x1) (C_SExpr y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_SPat x1 x2) (C_SPat y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_SLet x1) (C_SLet y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_SExpr x1) (C_SExpr y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_SPat x1 x2) (C_SPat y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_SLet x1) (C_SLet y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_SExpr x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_SPat x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_SLet x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_Statement cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Statement cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Statement cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Statement cd i _) = error ("AbstractHaskell.Statement.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Statement cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Statement cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_SExpr x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_SPat x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_SLet x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Statement cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Statement cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Statement cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Statement cd i _) = error ("AbstractHaskell.Statement.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Statement cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Statement cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Statement where
  (=?=) (Choice_C_Statement cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Statement cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Statement cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Statement cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Statement cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Statement cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Statement cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Statement cd info) _ _ = failCons cd info
  (=?=) (C_SExpr x1) (C_SExpr y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_SPat x1 x2) (C_SPat y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_SLet x1) (C_SLet y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Statement cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Statement cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Statement cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Statement cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Statement cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Statement cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Statement cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Statement cd info) _ _ = failCons cd info
  (<?=) (C_SExpr x1) (C_SExpr y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_SExpr _) (C_SPat _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SExpr _) (C_SLet _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SPat x1 x2) (C_SPat y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_SPat _ _) (C_SLet _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SLet x1) (C_SLet y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_Pattern
     = C_PVar (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
     | C_PLit C_Literal
     | C_PComb (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_Pattern)
     | C_PAs (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Pattern
     | C_PFuncComb (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List C_Pattern)
     | Choice_C_Pattern Cover ID C_Pattern C_Pattern
     | Choices_C_Pattern Cover ID ([C_Pattern])
     | Fail_C_Pattern Cover FailInfo
     | Guard_C_Pattern Cover Constraints C_Pattern

instance Show C_Pattern where
  showsPrec d (Choice_C_Pattern cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Pattern cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Pattern cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Pattern cd info) = showChar '!'
  showsPrec _ (C_PVar x1) = (showString "(PVar") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_PLit x1) = (showString "(PLit") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_PComb x1 x2) = (showString "(PComb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_PAs x1 x2) = (showString "(PAs") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_PFuncComb x1 x2) = (showString "(PFuncComb") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Pattern where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_PVar x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "PVar" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_PLit x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "PLit" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_PComb x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "PComb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_PAs x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "PAs" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ (readParen (d > 10) (\r -> [ (C_PFuncComb x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "PFuncComb" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s))))


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
  match _ _ _ _ _ _ (Choices_C_Pattern cd i _) = error ("AbstractHaskell.Pattern.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Pattern cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Pattern cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Pattern where
  generate s c = Choices_C_Pattern c (freeID [1,1,2,2,2] s) [(C_PVar (generate (leftSupply s) c)),(C_PLit (generate (leftSupply s) c)),(C_PComb (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_PAs (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_PFuncComb (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm C_Pattern where
  ($!!) cont (C_PVar x1) d cs = (((\y1 d cs -> cont (C_PVar y1) d cs) $!! x1) d) cs
  ($!!) cont (C_PLit x1) d cs = (((\y1 d cs -> cont (C_PLit y1) d cs) $!! x1) d) cs
  ($!!) cont (C_PComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PComb y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_PAs x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PAs y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_PFuncComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PFuncComb y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Pattern cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Pattern cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Pattern cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  ($##) cont (C_PVar x1) d cs = (((\y1 d cs -> cont (C_PVar y1) d cs) $## x1) d) cs
  ($##) cont (C_PLit x1) d cs = (((\y1 d cs -> cont (C_PLit y1) d cs) $## x1) d) cs
  ($##) cont (C_PComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PComb y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_PAs x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PAs y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_PFuncComb x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_PFuncComb y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Pattern cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Pattern cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Pattern cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  searchNF search cont (C_PVar x1) = search (\y1 -> cont (C_PVar y1)) x1
  searchNF search cont (C_PLit x1) = search (\y1 -> cont (C_PLit y1)) x1
  searchNF search cont (C_PComb x1 x2) = search (\y1 -> search (\y2 -> cont (C_PComb y1 y2)) x2) x1
  searchNF search cont (C_PAs x1 x2) = search (\y1 -> search (\y2 -> cont (C_PAs y1 y2)) x2) x1
  searchNF search cont (C_PFuncComb x1 x2) = search (\y1 -> search (\y2 -> cont (C_PFuncComb y1 y2)) x2) x1
  searchNF _ _ x = error ("AbstractHaskell.Pattern.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Pattern where
  (=.=) (C_PVar x1) (C_PVar y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_PLit x1) (C_PLit y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_PComb x1 x2) (C_PComb y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_PAs x1 x2) (C_PAs y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_PFuncComb x1 x2) (C_PFuncComb y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_PVar x1) (C_PVar y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_PLit x1) (C_PLit y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_PComb x1 x2) (C_PComb y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_PAs x1 x2) (C_PAs y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_PFuncComb x1 x2) (C_PFuncComb y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_PVar x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_PLit x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_PComb x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_PAs x3 x4) = ((i :=: (ChooseN 3 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_PFuncComb x3 x4) = ((i :=: (ChooseN 4 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Pattern cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Pattern cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Pattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Pattern cd i _) = error ("AbstractHaskell.Pattern.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Pattern cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Pattern cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_PVar x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_PLit x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_PComb x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_PAs x3 x4) = [(i :=: (ChooseN 3 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_PFuncComb x3 x4) = [(i :=: (ChooseN 4 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Pattern cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Pattern cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Pattern cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Pattern cd i _) = error ("AbstractHaskell.Pattern.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  (=?=) (C_PVar x1) (C_PVar y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_PLit x1) (C_PLit y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_PComb x1 x2) (C_PComb y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_PAs x1 x2) (C_PAs y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_PFuncComb x1 x2) (C_PFuncComb y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Pattern cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Pattern cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Pattern cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Pattern cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Pattern cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Pattern cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Pattern cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Pattern cd info) _ _ = failCons cd info
  (<?=) (C_PVar x1) (C_PVar y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_PVar _) (C_PLit _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PVar _) (C_PComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PVar _) (C_PAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PVar _) (C_PFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PLit x1) (C_PLit y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_PLit _) (C_PComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PLit _) (C_PAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PLit _) (C_PFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PComb x1 x2) (C_PComb y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_PComb _ _) (C_PAs _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PComb _ _) (C_PFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PAs x1 x2) (C_PAs y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_PAs _ _) (C_PFuncComb _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_PFuncComb x1 x2) (C_PFuncComb y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
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
  readsPrec d s = readParen (d > 10) (\r -> [ (C_Branch x1 x2,r2) | (_,r0) <- readQualified "AbstractHaskell" "Branch" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


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
  match _ _ _ _ _ _ (Choices_C_BranchExpr cd i _) = error ("AbstractHaskell.BranchExpr.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.BranchExpr.searchNF: no constructor: " ++ (show x))


instance Unifiable C_BranchExpr where
  (=.=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Branch x1 x2) (C_Branch y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Branch x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_BranchExpr cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_BranchExpr cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_BranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_BranchExpr cd i _) = error ("AbstractHaskell.BranchExpr.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_BranchExpr cd info) = [(Unsolvable info)]
  bind d i (Guard_C_BranchExpr cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Branch x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_BranchExpr cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_BranchExpr cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_BranchExpr cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_BranchExpr cd i _) = error ("AbstractHaskell.BranchExpr.lazyBind: Choices with ChoiceID: " ++ (show i))
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
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Intc x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Intc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_Floatc x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Floatc" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_Charc x1,r1) | (_,r0) <- readQualified "AbstractHaskell" "Charc" r, (x1,r1) <- readsPrec 11 r0]) s))


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
  match _ _ _ _ _ _ (Choices_C_Literal cd i _) = error ("AbstractHaskell.Literal.match: Choices with ChoiceID " ++ (show i))
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
  searchNF _ _ x = error ("AbstractHaskell.Literal.searchNF: no constructor: " ++ (show x))


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
  bind _ _ (Choices_C_Literal cd i _) = error ("AbstractHaskell.Literal.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Literal cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Literal cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Intc x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Floatc x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_Charc x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_Literal cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Literal cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Literal cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Literal cd i _) = error ("AbstractHaskell.Literal.lazyBind: Choices with ChoiceID: " ++ (show i))
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
