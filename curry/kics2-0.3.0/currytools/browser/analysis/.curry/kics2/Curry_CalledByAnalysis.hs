{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CalledByAnalysis (d_C_calledBy) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_List
d_C_calledBy :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_calledBy x1 x3250 x3500 = Curry_Prelude.d_C_map (d_OP_calledBy_dot_cfFun_dot_2 x1) x1 x3250 x3500

d_OP_calledBy_dot_cfFun_dot_2 :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_calledBy_dot_cfFun_dot_2 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_getCalls x3) x3250 x3500) x1 x3250 x3500)
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_calledBy_dot_cfFun_dot_2 x1 x1002 x3250 x3500) (d_OP_calledBy_dot_cfFun_dot_2 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_calledBy_dot_cfFun_dot_2 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_calledBy_dot_cfFun_dot_2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getCalls :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getCalls x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> d_OP__case_4 x1 x3 x7 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getCalls x1 x1002 x3250 x3500) (d_C_getCalls x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getCalls x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getCalls x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isCalled :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCalled x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Var x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x4) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x5 x6 x7) -> d_OP__case_1 x1 x6 x7 (Curry_Prelude.d_OP_eq_eq x6 x1 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x8 x9) -> d_C_isCalled x1 x9 x3250 x3500
     (Curry_FlatCurry.C_Let x10 x11) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_C_isCalled x1) x3250 x3500) (Curry_Prelude.OP_Cons x11 (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x10 x3250 x3500)) x3250 x3500
     (Curry_FlatCurry.C_Or x12 x13) -> Curry_Prelude.d_OP_bar_bar (d_C_isCalled x1 x12 x3250 x3500) (d_C_isCalled x1 x13 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x14 x15 x16) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_OP_isCalled_dot_isCalledCase_dot_44 x1) x3250 x3500) x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCalled x1 x1002 x3250 x3500) (d_C_isCalled x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCalled x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCalled x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isCalled_dot_isCalledCase_dot_44 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_isCalled_dot_isCalledCase_dot_44 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Branch x3 x4) -> d_C_isCalled x1 x4 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isCalled_dot_isCalledCase_dot_44 x1 x1002 x3250 x3500) (d_OP_isCalled_dot_isCalledCase_dot_44 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isCalled_dot_isCalledCase_dot_44 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isCalled_dot_isCalledCase_dot_44 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_1 x1 x6 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.C_True
     Curry_Prelude.C_False -> d_OP__case_0 x7 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x6 x7 x1002 x3250 x3500) (d_OP__case_1 x1 x6 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x6 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x6 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_0 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_C_isCalled x1) x3250 x3500) x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x7 x1 x1002 x3250 x3500) (d_OP__case_0 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x1 x3 x7 x3250 x3500 = case x7 of
     (Curry_FlatCurry.C_External x8) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x9 x10) -> d_OP__case_3 x10 x1 x3 (d_C_isCalled x1 x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x3 x1002 x3250 x3500) (d_OP__case_4 x1 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x10 x1 x3 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x10 x1 x3 x1002 x3250 x3500) (d_OP__case_3 x10 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x10 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x10 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
