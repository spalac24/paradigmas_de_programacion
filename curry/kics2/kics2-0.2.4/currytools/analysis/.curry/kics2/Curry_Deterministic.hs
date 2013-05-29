{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Deterministic (C_Deterministic (..), d_C_overlapAnalysis, nd_C_overlapAnalysis, d_C_showOverlap, d_C_ndAnalysis, nd_C_ndAnalysis, d_C_showDet, d_C_setValAnalysis, nd_C_setValAnalysis, d_C_showSetValued) where

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
  generate s = Choices_C_Deterministic defCover (freeID [0,0] s) [C_NDet,C_Det]


instance NormalForm C_Deterministic where
  ($!!) cont C_NDet cs = cont C_NDet cs
  ($!!) cont C_Det cs = cont C_Det cs
  ($!!) cont (Choice_C_Deterministic cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Deterministic cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Deterministic cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Deterministic cd info) _ = failCons cd info
  ($##) cont C_NDet cs = cont C_NDet cs
  ($##) cont C_Det cs = cont C_Det cs
  ($##) cont (Choice_C_Deterministic cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Deterministic cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Deterministic cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Deterministic cd info) _ = failCons cd info
  searchNF _ cont C_NDet = cont C_NDet
  searchNF _ cont C_Det = cont C_Det
  searchNF _ _ x = error ("Deterministic.Deterministic.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Deterministic where
  (=.=) C_NDet C_NDet cs = C_Success
  (=.=) C_Det C_Det cs = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_NDet C_NDet cs = C_Success
  (=.<=) C_Det C_Det cs = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_NDet = ((i :=: (ChooseN 0 0)):(concat []))
  bind i C_Det = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (Choice_C_Deterministic cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Deterministic cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Deterministic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Deterministic cd i _) = error ("Deterministic.Deterministic.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Deterministic cd info) = [(Unsolvable info)]
  bind i (Guard_C_Deterministic cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i C_NDet = [(i :=: (ChooseN 0 0))]
  lazyBind i C_Det = [(i :=: (ChooseN 1 0))]
  lazyBind i (Choice_C_Deterministic cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Deterministic cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Deterministic cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Deterministic cd i _) = error ("Deterministic.Deterministic.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Deterministic cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Deterministic cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Deterministic where
  (=?=) (Choice_C_Deterministic cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Deterministic cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Deterministic cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Deterministic cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Deterministic cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Deterministic cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Deterministic cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Deterministic cd info) _ = failCons cd info
  (=?=) C_NDet C_NDet cs = Curry_Prelude.C_True
  (=?=) C_Det C_Det cs = Curry_Prelude.C_True
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Deterministic cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Deterministic cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Deterministic cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Deterministic cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Deterministic cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Deterministic cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Deterministic cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Deterministic cd info) _ = failCons cd info
  (<?=) C_NDet C_NDet cs = Curry_Prelude.C_True
  (<?=) C_NDet C_Det _ = Curry_Prelude.C_True
  (<?=) C_Det C_Det cs = Curry_Prelude.C_True
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_Deterministic where
  cover C_NDet = C_NDet
  cover C_Det = C_Det
  cover (Choice_C_Deterministic cd i x y) = Choice_C_Deterministic (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Deterministic cd i xs) = Choices_C_Deterministic (incCover cd) i (map cover xs)
  cover (Fail_C_Deterministic cd info) = Fail_C_Deterministic (incCover cd) info
  cover (Guard_C_Deterministic cd c e) = Guard_C_Deterministic (incCover cd) c (cover e)


d_C_overlapAnalysis :: ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_overlapAnalysis x3500 = Curry_Analysis.d_C_simpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))) d_C_isOverlappingFunction x3500

nd_C_overlapAnalysis :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_overlapAnalysis x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_simpleFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))) (wrapDX id d_C_isOverlappingFunction) x2000 x3500))

d_C_isOverlappingFunction :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_C_isOverlappingFunction x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_1 x2 x6 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isOverlappingFunction x1002 x3500) (d_C_isOverlappingFunction x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isOverlappingFunction z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isOverlappingFunction x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_orInExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.C_Bool
d_C_orInExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x5 (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List) x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_orInExpr x3500) x6 x3500) x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_orInExpr x8 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_orInExpr x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3500) x3500) (d_C_orInExpr x10 x3500) x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_bar_bar (d_C_orInExpr x14 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_orInExpr_dot_orInBranch_dot_36 x3500) x15 x3500) x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_orInExpr x1002 x3500) (d_C_orInExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_orInExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_orInExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_orInExpr_dot_orInBranch_dot_36 :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_Prelude.C_Bool
d_OP_orInExpr_dot_orInBranch_dot_36 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_orInExpr x3 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_orInExpr_dot_orInBranch_dot_36 x1002 x3500) (d_OP_orInExpr_dot_orInBranch_dot_36 x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_orInExpr_dot_orInBranch_dot_36 z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_orInExpr_dot_orInBranch_dot_36 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showOverlap :: Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOverlap x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOverlap x1002 x3500) (d_C_showOverlap x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOverlap z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOverlap x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_ndAnalysis :: ConstStore -> Curry_Analysis.C_Analysis C_Deterministic
d_C_ndAnalysis x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))) C_Det (acceptCs id d_C_ndFunc) x3500

nd_C_ndAnalysis :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis C_Deterministic
nd_C_ndAnalysis x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))) C_Det (wrapDX (wrapDX id) (acceptCs id d_C_ndFunc)) x2000 x3500))

d_C_ndFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_Deterministic) -> ConstStore -> C_Deterministic
d_C_ndFunc x1 x2 x3500 = d_OP__case_0 x1 x2 (Curry_Prelude.d_OP_bar_bar (d_C_isOverlappingFunction x1 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) C_NDet) x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x2 x3500) x3500) x3500) x3500

d_C_showDet :: C_Deterministic -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showDet x1 x3500 = case x1 of
     C_NDet -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))))))))))))))
     C_Det -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))
     (Choice_C_Deterministic x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showDet x1002 x3500) (d_C_showDet x1003 x3500)
     (Choices_C_Deterministic x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showDet z x3500) x1002
     (Guard_C_Deterministic x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showDet x1002) $! (addCs x1001 x3500))
     (Fail_C_Deterministic x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_setValAnalysis :: ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_setValAnalysis x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.C_False (acceptCs id d_C_setValFunc) x3500

nd_C_setValAnalysis :: IDSupply -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_setValAnalysis x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) Curry_Prelude.C_False (wrapDX (wrapDX id) (acceptCs id d_C_setValFunc)) x2000 x3500))

d_C_setValFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.C_Bool
d_C_setValFunc x1 x2 x3500 = Curry_Prelude.d_OP_bar_bar (d_C_isSetValuedDefined x1 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any Curry_Prelude.d_C_snd x3500) x2 x3500) x3500

d_C_isSetValuedDefined :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSetValuedDefined x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_notElem x2 x3500) (Curry_Prelude.d_C_map d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))))))))))) Curry_Prelude.OP_List))))) x3500) x3500) (d_C_isSetValuedRule x6 x3500) x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSetValuedDefined x1002 x3500) (d_C_isSetValuedDefined x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSetValuedDefined z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSetValuedDefined x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isSetValuedRule :: Curry_FlatCurry.C_Rule -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSetValuedRule x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> Curry_Prelude.d_OP_bar_bar (d_C_orInExpr x3 x3500) (d_C_extraVarInExpr x3 x3500) x3500
     (Curry_FlatCurry.C_External x4) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSetValuedRule x1002 x3500) (d_C_isSetValuedRule x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSetValuedRule z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSetValuedRule x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_extraVarInExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.C_Bool
d_C_extraVarInExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_or x3500) (Curry_Prelude.d_C_map d_C_extraVarInExpr x6 x3500) x3500
     (Curry_FlatCurry.C_Free x7 x8) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x7 x3500) x3500) (d_C_extraVarInExpr x8 x3500) x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_extraVarInExpr x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3500) x3500) (d_C_extraVarInExpr x10 x3500) x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.d_OP_bar_bar (d_C_extraVarInExpr x11 x3500) (d_C_extraVarInExpr x12 x3500) x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_bar_bar (d_C_extraVarInExpr x14 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 x3500) x15 x3500) x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_extraVarInExpr x1002 x3500) (d_C_extraVarInExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_extraVarInExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_extraVarInExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_Prelude.C_Bool
d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_extraVarInExpr x3 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 x1002 x3500) (d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_extraVarInExpr_dot_extraVarInBranch_dot_85 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_showSetValued :: Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSetValued x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSetValued x1002 x3500) (d_C_showSetValued x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSetValued z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSetValued x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_pre :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0
d_C_pre x1 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x1

d_OP__case_0 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> C_NDet
     Curry_Prelude.C_False -> C_Det
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x1002 x3500) (d_OP__case_0 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> C_NDet
     Curry_Prelude.C_False -> C_Det
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x6 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> d_C_orInExpr x8 x3500
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x1002 x3500) (d_OP__case_1 x2 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x6 x3000 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> d_C_orInExpr x8 x3500
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x1002 x3000 x3500) (nd_OP__case_1 x2 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
