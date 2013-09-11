{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_RightLinearity (d_C_rlinAnalysis, nd_C_rlinAnalysis, d_C_showRightLinear, d_C_hasRightLinearRules, d_C_linearExpr) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
d_C_rlinAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_rlinAnalysis x3250 x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.C_True (acceptCs id d_C_rlFunc) x3250 x3500

nd_C_rlinAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_rlinAnalysis x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.C_True (wrapDX (wrapDX id) (acceptCs id d_C_rlFunc)) x2000 x3250 x3500))

d_C_rlFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_rlFunc x1 x2 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (d_C_hasRightLinearRules x1 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Prelude.d_C_snd x3250 x3500) x2 x3250 x3500) x3250 x3500

d_C_showRightLinear :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showRightLinear x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))))))
     Curry_Prelude.C_False -> d_OP__case_6 x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showRightLinear x1 x1002 x3250 x3500) (d_C_showRightLinear x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showRightLinear x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showRightLinear x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_hasRightLinearRules :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasRightLinearRules x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_C_isRightLinearRule x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasRightLinearRules x1002 x3250 x3500) (d_C_hasRightLinearRules x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasRightLinearRules z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasRightLinearRules x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isRightLinearRule :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isRightLinearRule x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> d_C_linearExpr x3 x3250 x3500
     (Curry_FlatCurry.C_External x4) -> Curry_Prelude.C_True
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isRightLinearRule x1002 x3250 x3500) (d_C_isRightLinearRule x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isRightLinearRule z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isRightLinearRule x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_linearExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_linearExpr x1 x3250 x3500 = Curry_Prelude.d_C_maybe Curry_Prelude.C_False (Curry_Prelude.d_C_const Curry_Prelude.C_True) (d_C_linearVariables x1 x3250 x3500) x3250 x3500

d_C_linearVariables :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_C_linearVariables x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List)
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_5 x6 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x6 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> Curry_Maybe.d_OP_gt_gt_minus (d_C_linearVariables x8 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda2 x7) x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Maybe.d_OP_gt_gt_minus (Curry_Prelude.d_C_apply (Curry_Maybe.d_C_mapMMaybe d_C_linearVariables x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3250 x3500) x3250 x3500) (d_OP_linearVariables_dot___hash_lambda3 x9 x10) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Maybe.d_OP_gt_gt_minus (d_C_linearVariables x11 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda5 x12) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Maybe.d_OP_gt_gt_minus (d_C_linearVariables x14 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda7 x15) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_linearVariables x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_linearVariables x1002 x3250 x3500) (d_C_linearVariables x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_linearVariables z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_linearVariables x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_linearVariables_dot___hash_lambda1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda1 x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_concat x1 x3250 x3500
      in (d_OP__case_3 x2 (Curry_Prelude.d_OP_eq_eq (Curry_List.d_C_nub x2 x3250 x3500) x2 x3250 x3500) x3250 x3500)

d_OP_linearVariables_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.C_Just (Curry_List.d_OP_backslash_backslash x2 x1 x3250 x3500)

d_OP_linearVariables_dot___hash_lambda3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = Curry_Maybe.d_OP_gt_gt_minus (d_C_linearVariables x2 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda3_dot___hash_lambda4 x1 x3) x3250 x3500

d_OP_linearVariables_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons x3 x2) x3250 x3500
      in (d_OP__case_2 x4 x1 (Curry_Prelude.d_OP_eq_eq (Curry_List.d_C_nub x4 x3250 x3500) x4 x3250 x3500) x3250 x3500)

d_OP_linearVariables_dot___hash_lambda5 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda5 x1 x2 x3250 x3500 = Curry_Maybe.d_OP_gt_gt_minus (d_C_linearVariables x1 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda5_dot___hash_lambda6 x2) x3250 x3500

d_OP_linearVariables_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3250 x3500 = Curry_Prelude.C_Just (Curry_List.d_C_union x1 x2 x3250 x3500)

d_OP_linearVariables_dot_patternVars_dot_53 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_linearVariables_dot_patternVars_dot_53 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_OP__case_1 x2 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_linearVariables_dot_patternVars_dot_53 x1002 x3250 x3500) (d_OP_linearVariables_dot_patternVars_dot_53 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_linearVariables_dot_patternVars_dot_53 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_linearVariables_dot_patternVars_dot_53 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_linearVariables_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Maybe.d_OP_gt_gt_minus (Curry_Prelude.d_C_apply (Curry_Maybe.d_C_mapMMaybe d_C_linearVariables x3250 x3500) (Curry_Prelude.d_C_map d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 x1 x3250 x3500) x3250 x3500) (d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9 x1 x2) x3250 x3500

d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 x1002 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9 x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) Curry_Prelude.OP_List (Curry_Prelude.d_C_map d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 (Curry_Prelude.d_C_zip x1 x3 x3250 x3500) x3250 x3500) x3250 x3500) x2 x3250 x3500
      in (d_OP__case_0 x4 (Curry_Prelude.d_OP_eq_eq (Curry_List.d_C_nub x4 x3250 x3500) x4 x3250 x3500) x3250 x3500)

d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 :: Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_List.d_OP_backslash_backslash x3 (d_OP_linearVariables_dot_patternVars_dot_53 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 x1002 x3250 x3500) (d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_linearVariables_dot___hash_lambda7_dot___hash_lambda9_dot___hash_lambda10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP__case_0 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x4
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x4 x1002 x3250 x3500) (d_OP__case_0 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Pattern x4 x5) -> x5
     (Curry_FlatCurry.C_LPattern x6) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP__case_2 x4 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_List.d_OP_backslash_backslash x4 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x1 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x1 x1002 x3250 x3500) (d_OP__case_2 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP__case_3 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x2
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1002 x3250 x3500) (d_OP__case_3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP__case_5 x6 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_linearVariables (Curry_FlatCurry.C_Or (Curry_Prelude.d_C_head x6 x3250 x3500) (Curry_Prelude.d_C_head (Curry_Prelude.d_C_tail x6 x3250 x3500) x3250 x3500)) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x6 x5 x1002 x3250 x3500) (d_OP__case_5 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_OP__case_4 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Maybe.d_OP_gt_gt_minus (Curry_Prelude.d_C_apply (Curry_Maybe.d_C_mapMMaybe d_C_linearVariables x3250 x3500) x6 x3250 x3500) d_OP_linearVariables_dot___hash_lambda1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x6 x1002 x3250 x3500) (d_OP__case_4 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_6 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
