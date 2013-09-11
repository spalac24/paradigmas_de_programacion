{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Deterministic (C_Deterministic (..), d_C_overlapAnalysis, nd_C_overlapAnalysis, d_C_showOverlap, d_C_showDet, d_C_nondetAnalysis, nd_C_nondetAnalysis) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_FlatCurryGoodies
data C_Deterministic
     = C_NDet
     | C_Det
     | Choice_C_Deterministic Cover ID C_Deterministic C_Deterministic
     | Choices_C_Deterministic Cover ID ([C_Deterministic])
     | Fail_C_Deterministic Cover FailInfo
     | Guard_C_Deterministic Cover Constraints C_Deterministic

instance Show C_Deterministic where
  showsPrec d (Choice_C_Deterministic cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Deterministic cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Deterministic cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Deterministic cd info) = showChar '!'
  showsPrec _ C_NDet = showString "NDet"
  showsPrec _ C_Det = showString "Det"


instance Read C_Deterministic where
  readsPrec _ s = (readParen False (\r -> [ (C_NDet,r0) | (_,r0) <- readQualified "Deterministic" "NDet" r]) s) ++ (readParen False (\r -> [ (C_Det,r0) | (_,r0) <- readQualified "Deterministic" "Det" r]) s)


instance NonDet C_Deterministic where
  choiceCons = Choice_C_Deterministic
  choicesCons = Choices_C_Deterministic
  failCons = Fail_C_Deterministic
  guardCons = Guard_C_Deterministic
  try (Choice_C_Deterministic cd i x y) = tryChoice cd i x y
  try (Choices_C_Deterministic cd i xs) = tryChoices cd i xs
  try (Fail_C_Deterministic cd info) = Fail cd info
  try (Guard_C_Deterministic cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Deterministic cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Deterministic cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Deterministic cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Deterministic cd i _) = error ("Deterministic.Deterministic.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Deterministic cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Deterministic cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Deterministic where
  generate s c = Choices_C_Deterministic c (freeID [0,0] s) [C_NDet,C_Det]


instance NormalForm C_Deterministic where
  ($!!) cont C_NDet d cs = cont C_NDet d cs
  ($!!) cont C_Det d cs = cont C_Det d cs
  ($!!) cont (Choice_C_Deterministic cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Deterministic cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Deterministic cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Deterministic cd info) _ _ = failCons cd info
  ($##) cont C_NDet d cs = cont C_NDet d cs
  ($##) cont C_Det d cs = cont C_Det d cs
  ($##) cont (Choice_C_Deterministic cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Deterministic cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Deterministic cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Deterministic cd info) _ _ = failCons cd info
  searchNF _ cont C_NDet = cont C_NDet
  searchNF _ cont C_Det = cont C_Det
  searchNF _ _ x = error ("Deterministic.Deterministic.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Deterministic where
  (=.=) C_NDet C_NDet d cs = C_Success
  (=.=) C_Det C_Det d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_NDet C_NDet d cs = C_Success
  (=.<=) C_Det C_Det d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_NDet = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Det = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_Deterministic cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Deterministic cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Deterministic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Deterministic cd i _) = error ("Deterministic.Deterministic.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Deterministic cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Deterministic cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_NDet = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Det = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_Deterministic cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Deterministic cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Deterministic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Deterministic cd i _) = error ("Deterministic.Deterministic.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Deterministic cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Deterministic cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_Deterministic where
  (=?=) (Choice_C_Deterministic cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Deterministic cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Deterministic cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Deterministic cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Deterministic cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Deterministic cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Deterministic cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Deterministic cd info) _ _ = failCons cd info
  (=?=) C_NDet C_NDet d cs = Curry_Prelude.C_True
  (=?=) C_Det C_Det d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Deterministic cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Deterministic cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Deterministic cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Deterministic cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Deterministic cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Deterministic cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Deterministic cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Deterministic cd info) _ _ = failCons cd info
  (<?=) C_NDet C_NDet d cs = Curry_Prelude.C_True
  (<?=) C_NDet C_Det _ _ = Curry_Prelude.C_True
  (<?=) C_Det C_Det d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_overlapAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_overlapAnalysis x3250 x3500 = Curry_Analysis.d_C_simpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))) d_C_isOverlappingFunction x3250 x3500

nd_C_overlapAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_overlapAnalysis x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_simpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))) (wrapDX id d_C_isOverlappingFunction) x2000 x3250 x3500))

d_C_isOverlappingFunction :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isOverlappingFunction x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_3 x2 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isOverlappingFunction x1002 x3250 x3500) (d_C_isOverlappingFunction x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isOverlappingFunction z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isOverlappingFunction x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_orInExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_orInExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x5 (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_orInExpr x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_orInExpr x8 x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_orInExpr x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3250 x3500) x3250 x3500) (d_C_orInExpr x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_bar_bar (d_C_orInExpr x14 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_orInExpr_dot_orInBranch_dot_36 x3250 x3500) x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_orInExpr x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_orInExpr x1002 x3250 x3500) (d_C_orInExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_orInExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_orInExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_orInExpr_dot_orInBranch_dot_36 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_orInExpr_dot_orInBranch_dot_36 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_orInExpr x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_orInExpr_dot_orInBranch_dot_36 x1002 x3250 x3500) (d_OP_orInExpr_dot_orInBranch_dot_36 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_orInExpr_dot_orInBranch_dot_36 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_orInExpr_dot_orInBranch_dot_36 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showOverlap :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOverlap x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))
     Curry_Prelude.C_False -> d_OP__case_2 x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOverlap x1 x1002 x3250 x3500) (d_C_showOverlap x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOverlap x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOverlap x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showDet :: Curry_Analysis.C_AOutFormat -> C_Deterministic -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showDet x1 x2 x3250 x3500 = case x2 of
     C_NDet -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))))))))))))
     C_Det -> d_OP__case_1 x1 x3250 x3500
     (Choice_C_Deterministic x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showDet x1 x1002 x3250 x3500) (d_C_showDet x1 x1003 x3250 x3500)
     (Choices_C_Deterministic x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showDet x1 z x3250 x3500) x1002
     (Guard_C_Deterministic x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showDet x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Deterministic x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_nondetAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis C_Deterministic
d_C_nondetAnalysis x3250 x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))) C_Det (acceptCs id d_C_nondetFunc) x3250 x3500

nd_C_nondetAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis C_Deterministic
nd_C_nondetAnalysis x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))) C_Det (wrapDX (wrapDX id) (acceptCs id d_C_nondetFunc)) x2000 x3250 x3500))

d_C_nondetFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Deterministic) -> Cover -> ConstStore -> C_Deterministic
d_C_nondetFunc x1 x2 x3250 x3500 = d_OP__case_0 x2 x1 (Curry_Prelude.d_OP_bar_bar (d_C_isNondetDefined x1 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) C_NDet) x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_isNondetDefined :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isNondetDefined x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_notElem x2 x3250 x3500) (Curry_Prelude.d_C_map d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))))))))) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) (d_C_isNondetRule x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isNondetDefined x1002 x3250 x3500) (d_C_isNondetDefined x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isNondetDefined z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isNondetDefined x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isNondetRule :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isNondetRule x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> Curry_Prelude.d_OP_bar_bar (d_C_orInExpr x3 x3250 x3500) (d_C_extraVarInExpr x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_External x4) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isNondetRule x1002 x3250 x3500) (d_C_isNondetRule x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isNondetRule z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isNondetRule x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_extraVarInExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_extraVarInExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3250 x3500) (Curry_Prelude.d_C_map d_C_extraVarInExpr x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (d_C_extraVarInExpr x8 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_extraVarInExpr x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3250 x3500) x3250 x3500) (d_C_extraVarInExpr x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.d_OP_bar_bar (d_C_extraVarInExpr x11 x3250 x3500) (d_C_extraVarInExpr x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_bar_bar (d_C_extraVarInExpr x14 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 x3250 x3500) x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_extraVarInExpr x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_extraVarInExpr x1002 x3250 x3500) (d_C_extraVarInExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_extraVarInExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_extraVarInExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_extraVarInExpr x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 x1002 x3250 x3500) (d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_extraVarInExpr_dot_extraVarInBranch_dot_90 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pre :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0
d_C_pre x1 x3250 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x1

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Deterministic) -> Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Deterministic
d_OP__case_0 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> C_NDet
     Curry_Prelude.C_False -> C_Det
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_2 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_3 x2 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> d_C_orInExpr x8 x3250 x3500
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1002 x3250 x3500) (d_OP__case_3 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
