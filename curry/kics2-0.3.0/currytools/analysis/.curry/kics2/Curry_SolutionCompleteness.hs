{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SolutionCompleteness (d_C_solcompAnalysis, nd_C_solcompAnalysis, d_C_showSolComplete) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_List
d_C_solcompAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
d_C_solcompAnalysis x3250 x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.C_True (acceptCs id d_C_scFunc) x3250 x3500

nd_C_solcompAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis Curry_Prelude.C_Bool
nd_C_solcompAnalysis x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.C_True (wrapDX (wrapDX id) (acceptCs id d_C_scFunc)) x2000 x3250 x3500))

d_C_scFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_scFunc x1 x2 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (d_C_isFlexDefined x1 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Prelude.d_C_snd x3250 x3500) x2 x3250 x3500) x3250 x3500

d_C_isFlexDefined :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFlexDefined x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_0 x2 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFlexDefined x1002 x3250 x3500) (d_C_isFlexDefined x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFlexDefined z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFlexDefined x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isFlexExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFlexExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x5 (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x5 (d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all d_C_isFlexExpr x3250 x3500) x6 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_isFlexExpr x8 x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all d_C_isFlexExpr x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3250 x3500) x3250 x3500) (d_C_isFlexExpr x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.d_OP_ampersand_ampersand (d_C_isFlexExpr x11 x3250 x3500) (d_C_isFlexExpr x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x13 Curry_FlatCurry.C_Flex x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all d_C_isFlexExpr x3250 x3500) (Curry_Prelude.OP_Cons x14 (Curry_Prelude.d_C_map d_OP_isFlexExpr_dot___hash_lambda1 x15 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_isFlexExpr x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFlexExpr x1002 x3250 x3500) (d_C_isFlexExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFlexExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFlexExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isFlexExpr_dot___hash_lambda1 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_isFlexExpr_dot___hash_lambda1 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isFlexExpr_dot___hash_lambda1 x1002 x3250 x3500) (d_OP_isFlexExpr_dot___hash_lambda1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isFlexExpr_dot___hash_lambda1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isFlexExpr_dot___hash_lambda1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showSolComplete :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showSolComplete x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showSolComplete x1 x1002 x3250 x3500) (d_C_showSolComplete x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showSolComplete x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showSolComplete x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_pre :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0
d_C_pre x1 x3250 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x1

d_OP__case_0 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_0 x2 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> d_C_isFlexExpr x8 x3250 x3500
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x2 x3250 x3500) (Curry_Prelude.d_C_map d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3250 x3500) (d_OP__case_0 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
