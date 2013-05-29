{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlatCurry2AbstractHaskell (d_C_fcy2abs, d_C_fcy2absTDecl, d_C_fcy2absOp, d_C_fcy2absFix, d_C_fcy2absFDecl, d_C_fcy2absRule, d_C_fcy2absExpr, d_C_applyLambdaHack, d_C_fcy2absBranch, d_C_fcy2absPattern, d_C_fcy2absVis, d_C_fcy2absTVar, d_C_fcy2absCDecl, d_C_fcy2absTExp, d_C_fcy2absVar, d_C_fcy2absLit, d_C_tvarsOf) where

import Basics
import qualified Curry_AbstractHaskell
import qualified Curry_AbstractHaskellGoodies
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Names
import qualified Curry_Prelude
d_C_fcy2abs :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_AbstractHaskell.C_Prog
d_C_fcy2abs x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> Curry_AbstractHaskell.C_Prog x2 x3 (Curry_Prelude.d_C_map d_C_fcy2absTDecl x4 x3500) (Curry_Prelude.d_C_map d_C_fcy2absFDecl x5 x3500) (Curry_Prelude.d_C_map d_C_fcy2absOp x6 x3500)
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2abs x1002 x3500) (d_C_fcy2abs x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2abs z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2abs x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absTDecl :: Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_AbstractHaskell.C_TypeDecl
d_C_fcy2absTDecl x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TypeSyn x2 x3 x4 x5) -> Curry_AbstractHaskell.C_TypeSyn x2 (d_C_fcy2absVis x3 x3500) (Curry_Prelude.d_C_map d_C_fcy2absTVar x4 x3500) (d_C_fcy2absTExp x5 x3500)
     (Curry_FlatCurry.C_Type x6 x7 x8 x9) -> Curry_AbstractHaskell.C_Type x6 (d_C_fcy2absVis x7 x3500) (Curry_Prelude.d_C_map d_C_fcy2absTVar x8 x3500) (Curry_Prelude.d_C_map d_C_fcy2absCDecl x9 x3500)
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absTDecl x1002 x3500) (d_C_fcy2absTDecl x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absTDecl z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absTDecl x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absOp :: Curry_FlatCurry.C_OpDecl -> ConstStore -> Curry_AbstractHaskell.C_OpDecl
d_C_fcy2absOp x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Op x2 x3 x4) -> Curry_AbstractHaskell.C_Op x2 (d_C_fcy2absFix x3 x3500) x4
     (Curry_FlatCurry.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absOp x1002 x3500) (d_C_fcy2absOp x1003 x3500)
     (Curry_FlatCurry.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absOp z x3500) x1002
     (Curry_FlatCurry.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absOp x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absFix :: Curry_FlatCurry.C_Fixity -> ConstStore -> Curry_AbstractHaskell.C_Fixity
d_C_fcy2absFix x1 x3500 = case x1 of
     Curry_FlatCurry.C_InfixOp -> Curry_AbstractHaskell.C_InfixOp
     Curry_FlatCurry.C_InfixlOp -> Curry_AbstractHaskell.C_InfixlOp
     Curry_FlatCurry.C_InfixrOp -> Curry_AbstractHaskell.C_InfixrOp
     (Curry_FlatCurry.Choice_C_Fixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absFix x1002 x3500) (d_C_fcy2absFix x1003 x3500)
     (Curry_FlatCurry.Choices_C_Fixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absFix z x3500) x1002
     (Curry_FlatCurry.Guard_C_Fixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absFix x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Fixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absFDecl :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_AbstractHaskell.C_FuncDecl
d_C_fcy2absFDecl x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> let
          x7 = d_C_tvarsOf x5 x3500
          x8 = d_OP__case_4 x5 x7 (Curry_Prelude.d_C_null x7 x3500) x3500
           in (d_OP__case_5 x2 x3 x4 x5 x6 x8 (Curry_Prelude.d_OP_eq_eq x5 (Curry_FlatCurry.C_TVar (Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 42#) x3500)) x3500) x3500)
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absFDecl x1002 x3500) (d_C_fcy2absFDecl x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absFDecl z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absFDecl x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_fcy2absFDecl_dot___hash_lambda1 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_Context
d_OP_fcy2absFDecl_dot___hash_lambda1 x1 x3500 = Curry_Prelude.OP_Cons (Curry_AbstractHaskell.C_Context (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (d_C_fcy2absTVar x1 x3500) Curry_Prelude.OP_List)) Curry_Prelude.OP_List

d_C_fcy2absRule :: Curry_FlatCurry.C_Rule -> ConstStore -> Curry_AbstractHaskell.C_Rules
d_C_fcy2absRule x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> Curry_AbstractHaskell.C_Rules (Curry_Prelude.OP_Cons (Curry_AbstractHaskell.C_Rule (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (acceptCs id Curry_AbstractHaskell.C_PVar) d_C_fcy2absVar x3500) x2 x3500) (Curry_Prelude.OP_Cons (Curry_AbstractHaskellGoodies.d_C_noGuard (d_C_fcy2absExpr x3 x3500) x3500) Curry_Prelude.OP_List) Curry_Prelude.OP_List) Curry_Prelude.OP_List)
     (Curry_FlatCurry.C_External x4) -> Curry_AbstractHaskell.C_External x4
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absRule x1002 x3500) (d_C_fcy2absRule x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absRule z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absRule x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_fcy2absExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_AbstractHaskell.C_Var (d_C_fcy2absVar x2 x3500)
     (Curry_FlatCurry.C_Lit x3) -> Curry_AbstractHaskell.C_Lit (d_C_fcy2absLit x3 x3500)
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> let
          x7 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head (Curry_Prelude.d_C_snd x5 x3500) x3500) (Curry_Prelude.C_Char '\\'#) x3500
           in (d_OP__case_3 x5 x6 x7 x3500)
     (Curry_FlatCurry.C_Let x8 x9) -> Curry_AbstractHaskell.C_Let (Curry_Prelude.d_C_map d_OP_fcy2absExpr_dot_ldecl_dot_34 x8 x3500) (d_C_fcy2absExpr x9 x3500)
     (Curry_FlatCurry.C_Free x10 x11) -> Curry_AbstractHaskell.C_Let (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (acceptCs id Curry_AbstractHaskell.C_LocalVar) d_C_fcy2absVar x3500) x10 x3500) (d_C_fcy2absExpr x11 x3500)
     (Curry_FlatCurry.C_Or x12 x13) -> Curry_AbstractHaskellGoodies.d_C_applyF (Curry_AbstractHaskellGoodies.d_C_pre (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List) x3500) (Curry_Prelude.d_C_map d_C_fcy2absExpr (Curry_Prelude.OP_Cons x12 (Curry_Prelude.OP_Cons x13 Curry_Prelude.OP_List)) x3500) x3500
     (Curry_FlatCurry.C_Case x14 x15 x16) -> Curry_AbstractHaskell.C_Case (d_C_fcy2absExpr x15 x3500) (Curry_Prelude.d_C_map d_C_fcy2absBranch x16 x3500)
     (Curry_FlatCurry.C_Typed x17 x18) -> Curry_AbstractHaskell.C_Typed (d_C_fcy2absExpr x17 x3500) (d_C_fcy2absTExp x18 x3500)
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absExpr x1002 x3500) (d_C_fcy2absExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_fcy2absExpr_dot_ldecl_dot_34 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_LocalDecl
d_OP_fcy2absExpr_dot_ldecl_dot_34 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_AbstractHaskell.C_LocalPat (Curry_AbstractHaskell.C_PVar (d_C_fcy2absVar x2 x3500)) (d_C_fcy2absExpr x3 x3500) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_fcy2absExpr_dot_ldecl_dot_34 x1002 x3500) (d_OP_fcy2absExpr_dot_ldecl_dot_34 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_fcy2absExpr_dot_ldecl_dot_34 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_fcy2absExpr_dot_ldecl_dot_34 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_applyLambdaHack :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_C_applyLambdaHack x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x5 = Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 1#) x4 x3500)
           in (Curry_AbstractHaskell.C_Lambda (Curry_Prelude.OP_Cons (Curry_AbstractHaskell.C_PVar (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 1003#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List) (Curry_AbstractHaskellGoodies.d_C_applyF x5 (Curry_Prelude.d_C_map d_OP_applyLambdaHack_dot_applyLambda_dot_47 x2 x3500) x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_applyLambdaHack x1002 x2 x3500) (d_C_applyLambdaHack x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_applyLambdaHack z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_applyLambdaHack x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_applyLambdaHack_dot_applyLambda_dot_47 :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_AbstractHaskell.C_Expr
d_OP_applyLambdaHack_dot_applyLambda_dot_47 x1 x3500 = d_OP__case_1 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_FlatCurry.C_Var (Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 42#) x3500)) x3500) x3500

d_C_fcy2absBranch :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_AbstractHaskell.C_BranchExpr
d_C_fcy2absBranch x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> Curry_AbstractHaskell.C_Branch (d_C_fcy2absPattern x2 x3500) (d_C_fcy2absExpr x3 x3500)
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absBranch x1002 x3500) (d_C_fcy2absBranch x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absBranch z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absBranch x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absPattern :: Curry_FlatCurry.C_Pattern -> ConstStore -> Curry_AbstractHaskell.C_Pattern
d_C_fcy2absPattern x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Pattern x2 x3) -> Curry_AbstractHaskell.C_PComb x2 (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (acceptCs id Curry_AbstractHaskell.C_PVar) d_C_fcy2absVar x3500) x3 x3500)
     (Curry_FlatCurry.C_LPattern x4) -> Curry_AbstractHaskell.C_PLit (d_C_fcy2absLit x4 x3500)
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absPattern x1002 x3500) (d_C_fcy2absPattern x1003 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absPattern z x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absPattern x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absVis :: Curry_FlatCurry.C_Visibility -> ConstStore -> Curry_AbstractHaskell.C_Visibility
d_C_fcy2absVis x1 x3500 = case x1 of
     Curry_FlatCurry.C_Public -> Curry_AbstractHaskell.C_Public
     Curry_FlatCurry.C_Private -> Curry_AbstractHaskell.C_Private
     (Curry_FlatCurry.Choice_C_Visibility x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absVis x1002 x3500) (d_C_fcy2absVis x1003 x3500)
     (Curry_FlatCurry.Choices_C_Visibility x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absVis z x3500) x1002
     (Curry_FlatCurry.Guard_C_Visibility x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absVis x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Visibility x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absTVar :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_fcy2absTVar x1 x3500 = Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.d_C_show x1 x3500))

d_C_fcy2absCDecl :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_AbstractHaskell.C_ConsDecl
d_C_fcy2absCDecl x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_AbstractHaskell.C_Cons x2 x3 (d_C_fcy2absVis x4 x3500) (Curry_Prelude.d_C_map d_C_fcy2absTExp x5 x3500)
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absCDecl x1002 x3500) (d_C_fcy2absCDecl x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absCDecl z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absCDecl x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absTExp :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_AbstractHaskell.C_TypeExpr
d_C_fcy2absTExp x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_AbstractHaskell.C_TVar (d_C_fcy2absTVar x2 x3500)
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_AbstractHaskell.C_FuncType (d_C_fcy2absTExp x3 x3500) (d_C_fcy2absTExp x4 x3500)
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_AbstractHaskell.C_TCons x5 (Curry_Prelude.d_C_map d_C_fcy2absTExp x6 x3500)
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absTExp x1002 x3500) (d_C_fcy2absTExp x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absTExp z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absTExp x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fcy2absVar :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_fcy2absVar x1 x3500 = Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.d_C_show x1 x3500))

d_C_fcy2absLit :: Curry_FlatCurry.C_Literal -> ConstStore -> Curry_AbstractHaskell.C_Literal
d_C_fcy2absLit x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Intc x2) -> Curry_AbstractHaskell.C_Intc x2
     (Curry_FlatCurry.C_Floatc x3) -> Curry_AbstractHaskell.C_Floatc x3
     (Curry_FlatCurry.C_Charc x4) -> Curry_AbstractHaskell.C_Charc x4
     (Curry_FlatCurry.Choice_C_Literal x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fcy2absLit x1002 x3500) (d_C_fcy2absLit x1003 x3500)
     (Curry_FlatCurry.Choices_C_Literal x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fcy2absLit z x3500) x1002
     (Curry_FlatCurry.Guard_C_Literal x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fcy2absLit x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Literal x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tvarsOf :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_tvarsOf x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_List.d_C_union (d_C_tvarsOf x3 x3500) (d_C_tvarsOf x4 x3500) x3500
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_Prelude.d_C_foldr (acceptCs id Curry_List.d_C_union) Curry_Prelude.OP_List (Curry_Prelude.d_C_map d_C_tvarsOf x6 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tvarsOf x1002 x3500) (d_C_tvarsOf x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tvarsOf z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tvarsOf x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Var (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 1003#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> d_OP__case_0 x1 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3500) (d_OP__case_1 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Var (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 1003#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x1002 x3000 x3500) (nd_OP__case_1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_fcy2absExpr x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_fcy2absExpr x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_applyLambdaHack x5 x6 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x5 x6 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x5 x6 x1002 x3500) (d_OP__case_3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_applyLambdaHack x5 x6 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x5 x6 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x5 x6 x1002 x3000 x3500) (nd_OP__case_3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_AbstractHaskellGoodies.d_C_applyF x5 (Curry_Prelude.d_C_map d_C_fcy2absExpr x6 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x5 x6 x1002 x3500) (d_OP__case_2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_AbstractHaskellGoodies.d_C_applyF x5 (Curry_Prelude.nd_C_map (wrapDX id d_C_fcy2absExpr) x6 x2000 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x5 x6 x1002 x3000 x3500) (nd_OP__case_2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_FType (d_C_fcy2absTExp x5 x3500)
     Curry_Prelude.C_False -> Curry_AbstractHaskell.C_CType (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_fcy2absFDecl_dot___hash_lambda1 x3500) x7 x3500) (d_C_fcy2absTExp x5 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x5 x7 x1002 x3500) (d_OP__case_4 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_FType (d_C_fcy2absTExp x5 x3500)
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (Curry_AbstractHaskell.C_CType (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_fcy2absFDecl_dot___hash_lambda1) x2000 x3500) x7 x2001 x3500)))) (d_C_fcy2absTExp x5 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x5 x7 x1002 x3000 x3500) (nd_OP__case_4 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3 x4 x5 x6 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x2 x3 (d_C_fcy2absVis x4 x3500) Curry_AbstractHaskell.C_Untyped (d_C_fcy2absRule x6 x3500)
     Curry_Prelude.C_False -> Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x2 x3 (d_C_fcy2absVis x4 x3500) x8 (d_C_fcy2absRule x6 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x3 x4 x5 x6 x8 x1002 x3500) (d_OP__case_5 x2 x3 x4 x5 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x3 x4 x5 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x3 x4 x5 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3 x4 x5 x6 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x2 x3 (d_C_fcy2absVis x4 x3500) Curry_AbstractHaskell.C_Untyped (d_C_fcy2absRule x6 x3500)
     Curry_Prelude.C_False -> Curry_AbstractHaskell.C_Func Curry_Prelude.OP_List x2 x3 (d_C_fcy2absVis x4 x3500) x8 (d_C_fcy2absRule x6 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x3 x4 x5 x6 x8 x1002 x3000 x3500) (nd_OP__case_5 x2 x3 x4 x5 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 x3 x4 x5 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x3 x4 x5 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
