{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_EliminateCond (d_C_eliminateCond) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_State
d_C_eliminateCond :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_eliminateCond x1 x3500 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_updProgFuncs (Curry_Prelude.d_C_concatMap d_C_transFunc x3500) x3500) x1 x3500

d_C_transFunc :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_C_transFunc x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_31 x1 x2 x3 x4 x5 x6 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_transFunc x1002 x3500) (d_C_transFunc x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_transFunc z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_transFunc x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_transFunc_dot___hash_selFP2_hash_newExp :: Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_transFunc_dot___hash_selFP2_hash_newExp x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_30 x2 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_transFunc_dot___hash_selFP2_hash_newExp x1002 x3500) (d_OP_transFunc_dot___hash_selFP2_hash_newExp x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_transFunc_dot___hash_selFP2_hash_newExp z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_transFunc_dot___hash_selFP2_hash_newExp x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_transFunc_dot___hash_selFP3_hash_newFuns :: Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_OP_transFunc_dot___hash_selFP3_hash_newFuns x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_29 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_transFunc_dot___hash_selFP3_hash_newFuns x1002 x3500) (d_OP_transFunc_dot___hash_selFP3_hash_newFuns x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_transFunc_dot___hash_selFP3_hash_newFuns z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_transFunc_dot___hash_selFP3_hash_newFuns x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_transExpr :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_C_transExpr x1 x2 x3500 = Curry_FlatCurryGoodies.d_C_trExpr (Curry_Prelude.d_OP_dot (acceptCs id Curry_State.d_C_returnS) (acceptCs id Curry_FlatCurry.C_Var) x3500) (Curry_Prelude.d_OP_dot (acceptCs id Curry_State.d_C_returnS) (acceptCs id Curry_FlatCurry.C_Lit) x3500) (acceptCs (acceptCs id) (d_OP_transExpr_dot_transComb_dot_17 x1)) (acceptCs id d_OP_transExpr_dot_transLet_dot_17) (acceptCs id d_OP_transExpr_dot_transFree_dot_17) (acceptCs id d_OP_transExpr_dot_transOr_dot_17) (acceptCs (acceptCs id) d_OP_transExpr_dot_transCase_dot_17) (acceptCs id d_OP_transExpr_dot_transBranch_dot_17) (acceptCs id d_OP_transExpr_dot_transTyped_dot_17) x2 x3500

nd_C_transExpr :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_C_transExpr x1 x2 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_FlatCurryGoodies.nd_C_trExpr (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id Curry_State.d_C_returnS)) (wrapDX id (acceptCs id Curry_FlatCurry.C_Var)) x2000 x3500) (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id Curry_State.d_C_returnS)) (wrapDX id (acceptCs id Curry_FlatCurry.C_Lit)) x2001 x3500) (wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) (nd_OP_transExpr_dot_transComb_dot_17 x1))) (wrapDX (wrapNX id) (acceptCs id nd_OP_transExpr_dot_transLet_dot_17)) (wrapDX (wrapNX id) (acceptCs id nd_OP_transExpr_dot_transFree_dot_17)) (wrapDX (wrapNX id) (acceptCs id nd_OP_transExpr_dot_transOr_dot_17)) (wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) nd_OP_transExpr_dot_transCase_dot_17)) (wrapDX (wrapNX id) (acceptCs id nd_OP_transExpr_dot_transBranch_dot_17)) (wrapDX (wrapNX id) (acceptCs id nd_OP_transExpr_dot_transTyped_dot_17)) x2 x2002 x3500))))))))

d_OP_transExpr_dot_transComb_dot_17 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_OP_transExpr_dot_transComb_dot_17 x1 x2 x3 x4 x3500 = d_OP__case_28 x1 x2 x3 x4 (Curry_Prelude.OP_Tuple2 x3 x4) x3500

nd_OP_transExpr_dot_transComb_dot_17 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_OP_transExpr_dot_transComb_dot_17 x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_28 x1 x2 x3 x4 (Curry_Prelude.OP_Tuple2 x3 x4) x2000 x3500))

d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 x1 x2 x3 x3500 = Curry_State.d_C_bindS x1 (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 x2 x3)

nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 x1 x2 x3 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x1 (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 x2 x3)))

d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3500 = d_C_makeAuxFuncCall x1 x2 x3 x3500

nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_makeAuxFuncCall x1 x2 x3 x2000 x3500))

d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 :: Curry_Prelude.Curry t211 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> ConstStore -> t211 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t211
d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x1 x2 x3 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Comb x2 x1 x3)

nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 :: Curry_Prelude.Curry t211 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t211 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t211)
nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x1 x2 x3 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Comb x2 x1 x3))

d_OP_transExpr_dot_transLet_dot_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)) -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0
d_OP_transExpr_dot_transLet_dot_17 x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_unzip x1 x3500
     x4 = d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x3 x3500
     x5 = d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x3 x3500
      in (Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x5 x3500) (d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 x2 x4))

nd_OP_transExpr_dot_transLet_dot_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0))) -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)
nd_OP_transExpr_dot_transLet_dot_17 x1 x2 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2000 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2000 (seq x2006 (let
               x2001 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2001 (seq x2004 (let
                    x3 = Curry_Prelude.d_C_unzip x1 x3500
                    x4 = nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x3 x2000 x3500
                    x5 = nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x3 x2001 x3500
                     in (wrapNX id (Curry_State.nd_C_bindS (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2002 x3500) x5 x2003 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 x2 x4))))))))))))

d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars :: Curry_Prelude.Curry t152 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List (t152 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152)) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1002 x3500) (d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars :: Curry_Prelude.Curry t152 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List (Func t152 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152))) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1002 x3000 x3500) (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP5_hash_vars x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps :: Curry_Prelude.Curry t152 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List (t152 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152)) -> ConstStore -> Curry_Prelude.OP_List (t152 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152)
d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1002 x3500) (d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps :: Curry_Prelude.Curry t152 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List (Func t152 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152))) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Func t152 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152))
nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1 x3000 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1002 x3000 x3500) (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_transExpr_dot_transLet_dot_17_dot___hash_selFP6_hash_exps x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 :: Curry_Prelude.Curry t152 => (t152 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> ConstStore -> t152 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152
d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 x1 x2 x3 x3500 = Curry_State.d_C_bindS x1 (d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 x3 x2)

nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 :: Curry_Prelude.Curry t152 => Func t152 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t152 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t152)
nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5 x1 x2 x3 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x1 (wrapNX id (nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 x3 x2)))

d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.Curry t212 => Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> ConstStore -> t212 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t212
d_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Let (Curry_Prelude.d_C_zip x2 x1 x3500) x3)

nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.Curry t212 => Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t212 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t212)
nd_OP_transExpr_dot_transLet_dot_17_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Let (Curry_Prelude.d_C_zip x2 x1 x3500) x3))

d_OP_transExpr_dot_transFree_dot_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0
d_OP_transExpr_dot_transFree_dot_17 x1 x2 x3500 = Curry_State.d_C_bindS x2 (d_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 x1)

nd_OP_transExpr_dot_transFree_dot_17 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)
nd_OP_transExpr_dot_transFree_dot_17 x1 x2 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x2 (wrapNX id (nd_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 x1)))

d_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 :: Curry_Prelude.Curry t213 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> ConstStore -> t213 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t213
d_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 x1 x2 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Free x1 x2)

nd_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 :: Curry_Prelude.Curry t213 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t213 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t213)
nd_OP_transExpr_dot_transFree_dot_17_dot___hash_lambda7 x1 x2 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Free x1 x2))

d_OP_transExpr_dot_transOr_dot_17 :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0
d_OP_transExpr_dot_transOr_dot_17 x1 x2 x3500 = Curry_State.d_C_bindS x1 (d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 x2)

nd_OP_transExpr_dot_transOr_dot_17 :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)
nd_OP_transExpr_dot_transOr_dot_17 x1 x2 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x1 (wrapNX id (nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 x2)))

d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 :: Curry_Prelude.Curry t176 => (t176 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t176) -> Curry_FlatCurry.C_Expr -> ConstStore -> t176 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t176
d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 x1 x2 x3500 = Curry_State.d_C_bindS x1 (d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 x2)

nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 :: Curry_Prelude.Curry t176 => Func t176 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t176) -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t176 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t176)
nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8 x1 x2 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x1 (wrapNX id (nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 x2)))

d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.Curry t214 => Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> ConstStore -> t214 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t214
d_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Or x1 x2)

nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.Curry t214 => Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t214 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t214)
nd_OP_transExpr_dot_transOr_dot_17_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Or x1 x2))

d_OP_transExpr_dot_transCase_dot_17 :: Curry_Prelude.Curry t0 => Curry_FlatCurry.C_CaseType -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> Curry_Prelude.OP_List (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0
d_OP_transExpr_dot_transCase_dot_17 x1 x2 x3 x3500 = Curry_State.d_C_bindS x2 (d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 x3 x1)

nd_OP_transExpr_dot_transCase_dot_17 :: Curry_Prelude.Curry t0 => Curry_FlatCurry.C_CaseType -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> Curry_Prelude.OP_List (Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t0)) -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)
nd_OP_transExpr_dot_transCase_dot_17 x1 x2 x3 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x2 (wrapNX id (nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 x3 x1)))

d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 :: Curry_Prelude.Curry t192 => Curry_Prelude.OP_List (t192 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t192) -> Curry_FlatCurry.C_CaseType -> Curry_FlatCurry.C_Expr -> ConstStore -> t192 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t192
d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 x1 x2 x3 x3500 = Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x1 x3500) (d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 x2 x3)

nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 :: Curry_Prelude.Curry t192 => Curry_Prelude.OP_List (Func t192 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t192)) -> Curry_FlatCurry.C_CaseType -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t192 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t192)
nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x1 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 x2 x3)))))

d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.Curry t215 => Curry_FlatCurry.C_CaseType -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> ConstStore -> t215 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t215
d_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Case x1 x2 x3)

nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.Curry t215 => Curry_FlatCurry.C_CaseType -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> IDSupply -> ConstStore -> Func t215 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t215)
nd_OP_transExpr_dot_transCase_dot_17_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Case x1 x2 x3))

d_OP_transExpr_dot_transBranch_dot_17 :: Curry_Prelude.Curry t0 => Curry_FlatCurry.C_Pattern -> (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t0
d_OP_transExpr_dot_transBranch_dot_17 x1 x2 x3500 = Curry_State.d_C_bindS x2 (d_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 x1)

nd_OP_transExpr_dot_transBranch_dot_17 :: Curry_Prelude.Curry t0 => Curry_FlatCurry.C_Pattern -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t0)
nd_OP_transExpr_dot_transBranch_dot_17 x1 x2 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x2 (wrapNX id (nd_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 x1)))

d_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 :: Curry_Prelude.Curry t216 => Curry_FlatCurry.C_Pattern -> Curry_FlatCurry.C_Expr -> ConstStore -> t216 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t216
d_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 x1 x2 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Branch x1 x2)

nd_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 :: Curry_Prelude.Curry t216 => Curry_FlatCurry.C_Pattern -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t216 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_BranchExpr t216)
nd_OP_transExpr_dot_transBranch_dot_17_dot___hash_lambda12 x1 x2 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Branch x1 x2))

d_OP_transExpr_dot_transTyped_dot_17 :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> Curry_FlatCurry.C_TypeExpr -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0
d_OP_transExpr_dot_transTyped_dot_17 x1 x2 x3500 = Curry_State.d_C_bindS x1 (d_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 x2)

nd_OP_transExpr_dot_transTyped_dot_17 :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0) -> Curry_FlatCurry.C_TypeExpr -> IDSupply -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t0)
nd_OP_transExpr_dot_transTyped_dot_17 x1 x2 x3000 x3500 = wrapNX id (Curry_State.nd_C_bindS x1 (wrapNX id (nd_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 x2)))

d_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 :: Curry_Prelude.Curry t217 => Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_Expr -> ConstStore -> t217 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t217
d_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 x1 x2 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Typed x2 x1)

nd_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 :: Curry_Prelude.Curry t217 => Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func t217 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t217)
nd_OP_transExpr_dot_transTyped_dot_17_dot___hash_lambda13 x1 x2 x3000 x3500 = wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Typed x2 x1))

d_C_makeAuxFuncCall :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_C_makeAuxFuncCall x1 x2 x3 x3500 = let
     x4 = d_C_unboundVars x3 x3500
     x5 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_length x4 x3500) (Curry_Prelude.C_Int 1#) x3500
      in (Curry_State.d_C_bindS Curry_State.d_C_getS (d_OP_makeAuxFuncCall_dot___hash_lambda14 x4 x1 x3 x5 x2))

nd_C_makeAuxFuncCall :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_C_makeAuxFuncCall x1 x2 x3 x3000 x3500 = let
     x4 = d_C_unboundVars x3 x3500
     x5 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_length x4 x3500) (Curry_Prelude.C_Int 1#) x3500
      in (wrapNX id (Curry_State.nd_C_bindS (wrapDX id Curry_State.d_C_getS) (wrapNX id (nd_OP_makeAuxFuncCall_dot___hash_lambda14 x4 x1 x3 x5 x2))))

d_OP_makeAuxFuncCall_dot_renameVarFun_dot_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_makeAuxFuncCall_dot_renameVarFun_dot_52 x1 x2 x3500 = Curry_Prelude.d_C_maybe x2 (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus) (Curry_Prelude.C_Int 1#)) (Curry_Prelude.d_C_apply (Curry_List.d_C_elemIndex x2 x3500) x1 x3500) x3500

d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) x4 x3500) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x1002 x2 x3500) (d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeAuxFuncCall_dot_newFun_dot_52 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> t0 -> ConstStore -> Curry_FlatCurry.C_FuncDecl
d_OP_makeAuxFuncCall_dot_newFun_dot_52 x1 x2 x3 x4 x5 x3500 = Curry_FlatCurry.C_Func (d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x2 x5 x3500) x4 Curry_FlatCurry.C_Private (Curry_FlatCurry.C_TVar (Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 42#) x3500)) (Curry_FlatCurry.C_Rule (Curry_Prelude.d_C_enumFromTo (Curry_Prelude.C_Int 1#) x4 x3500) (Curry_FlatCurry.C_Case Curry_FlatCurry.C_Flex (Curry_FlatCurry.C_Var x4) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Branch (Curry_FlatCurry.C_Pattern (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_rnmAllVars (d_OP_makeAuxFuncCall_dot_renameVarFun_dot_52 x1) x3500) x3 x3500)) Curry_Prelude.OP_List)))

d_OP_makeAuxFuncCall_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int)
d_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_State.d_C_bindS (Curry_State.d_C_setS (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (d_OP_makeAuxFuncCall_dot_newFun_dot_52 x1 x2 x3 x4 x8 x3500) x7) (Curry_Prelude.d_OP_plus x8 (Curry_Prelude.C_Int 1#) x3500))) (d_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 x1 x8 x2 x5)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002 x3500) (d_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_makeAuxFuncCall_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int) (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) Curry_Prelude.C_Int))
nd_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> wrapNX id (Curry_State.nd_C_bindS (wrapDX id (Curry_State.d_C_setS (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (d_OP_makeAuxFuncCall_dot_newFun_dot_52 x1 x2 x3 x4 x8 x3500) x7) (Curry_Prelude.d_OP_plus x8 (Curry_Prelude.C_Int 1#) x3500)))) (wrapNX id (nd_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 x1 x8 x2 x5)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_makeAuxFuncCall_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.Curry t218 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Unit -> ConstStore -> t218 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t218
d_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3 x4 x5 x3500 = Curry_State.d_C_returnS (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_FuncCall (d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x3 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (acceptCs id Curry_FlatCurry.C_Var) x1 x3500) (Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List) x3500))

nd_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.Curry t218 => Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Unit -> IDSupply -> ConstStore -> Func t218 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr t218)
nd_OP_makeAuxFuncCall_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapDX id (Curry_State.d_C_returnS (Curry_FlatCurry.C_Comb Curry_FlatCurry.C_FuncCall (d_OP_makeAuxFuncCall_dot_mkNewName_dot_52 x3 x2 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_map (wrapDX id (acceptCs id Curry_FlatCurry.C_Var)) x1 x2000 x3500) (Curry_Prelude.OP_Cons x4 Curry_Prelude.OP_List) x3500)))))

d_C_unboundVars :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_unboundVars x1 x3500 = Curry_List.d_C_nub (Curry_FlatCurryGoodies.d_C_trExpr (Curry_Prelude.d_C_flip (acceptCs (acceptCs id) Curry_Prelude.OP_Cons) Curry_Prelude.OP_List) (Curry_Prelude.d_C_const Curry_Prelude.OP_List) (acceptCs id d_OP_unboundVars_dot_comb_dot_70) (acceptCs id d_OP_unboundVars_dot_leT_dot_70) d_OP_unboundVars_dot_freE_dot_70 (acceptCs id Curry_Prelude.d_OP_plus_plus) (acceptCs (acceptCs id) d_OP_unboundVars_dot_casE_dot_70) (acceptCs id d_OP_unboundVars_dot_branch_dot_70) (acceptCs id d_OP_unboundVars_dot_typed_dot_70) x1 x3500) x3500

d_OP_unboundVars_dot_comb_dot_70 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => t0 -> t1 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t2) -> ConstStore -> Curry_Prelude.OP_List t2
d_OP_unboundVars_dot_comb_dot_70 x1 x2 x3500 = Curry_Prelude.d_C_concat

nd_OP_unboundVars_dot_comb_dot_70 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => t0 -> t1 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List t2)) (Curry_Prelude.OP_List t2)
nd_OP_unboundVars_dot_comb_dot_70 x1 x2 x3000 x3500 = wrapDX id Curry_Prelude.d_C_concat

d_OP_unboundVars_dot_leT_dot_70 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t0)) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_unboundVars_dot_leT_dot_70 x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_unzip x1 x3500
     x4 = d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound x3 x3500
     x5 = d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS x3 x3500
      in (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x4) (Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons x2 x5) x3500) x3500)

d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound :: Curry_Prelude.Curry t15 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t15) (Curry_Prelude.OP_List (Curry_Prelude.OP_List t15)) -> ConstStore -> Curry_Prelude.OP_List t15
d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound x1002 x3500) (d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP8_hash_bound x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS :: Curry_Prelude.Curry t15 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t15) (Curry_Prelude.OP_List (Curry_Prelude.OP_List t15)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t15)
d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS x1002 x3500) (d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unboundVars_dot_leT_dot_70_dot___hash_selFP9_hash_varsRHS x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unboundVars_dot_freE_dot_70 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_unboundVars_dot_freE_dot_70 x1 x3500 = Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x1)

nd_OP_unboundVars_dot_freE_dot_70 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_unboundVars_dot_freE_dot_70 x1 x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_filter (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_notElem) x1)))

d_OP_unboundVars_dot_casE_dot_70 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List t1 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t1) -> ConstStore -> Curry_Prelude.OP_List t1
d_OP_unboundVars_dot_casE_dot_70 x1 x2 x3 x3500 = Curry_Prelude.d_C_concat (Curry_Prelude.OP_Cons x2 x3) x3500

d_OP_unboundVars_dot_branch_dot_70 :: Curry_FlatCurry.C_Pattern -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_unboundVars_dot_branch_dot_70 x1 x2 x3500 = case x1 of
     (Curry_FlatCurry.C_Pattern x3 x4) -> Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x4) x2 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_unboundVars_dot_branch_dot_70 x1002 x2 x3500) (d_OP_unboundVars_dot_branch_dot_70 x1003 x2 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_unboundVars_dot_branch_dot_70 z x2 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_unboundVars_dot_branch_dot_70 x1002 x2) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_unboundVars_dot_typed_dot_70 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t0 -> t1 -> ConstStore -> t0
d_OP_unboundVars_dot_typed_dot_70 x1 x2 x3500 = x1

d_OP__case_28 x1 x2 x3 x4 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_27 x1 x2 x3 x4 x6 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x1 x2 x3 x4 x1002 x3500) (d_OP__case_28 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x1 x2 x3 x4 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x1 x2 x3 x4 x6 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_28 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_27 x1 x2 x3 x4 x6 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__case_26 x1 x2 x3 x4 x6 x8 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_27 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x1 x2 x3 x4 x6 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x1 x2 x3 x4 x6 x8 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_27 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_26 x1 x2 x3 x4 x6 x8 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'P'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_26 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x1 x2 x3 x4 x6 x8 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (let
               x11 = x9
                in (nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'P'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_26 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_24 x1 x2 x3 x4 x6 x8 x10 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1002 x3500) (d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x2 x3 x4 x6 x8 x10 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1002 x3000 x3500) (nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x2 x3 x4 x6 x8 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_24 x1 x2 x3 x4 x6 x8 x10 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'r'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_24 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x2 x3 x4 x6 x8 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (let
               x14 = x12
                in (nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'r'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_24 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_22 x1 x2 x3 x4 x6 x8 x13 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1002 x3500) (d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x2 x3 x4 x6 x8 x13 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1002 x3000 x3500) (nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x2 x3 x4 x6 x8 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x4 x6 x8 x13 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x17 = x15
           in (d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'e'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_22 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x4 x6 x8 x13 x3000 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (let
               x17 = x15
                in (nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'e'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x18 x3500 = case x18 of
     Curry_Prelude.C_True -> d_OP__case_20 x1 x2 x3 x4 x6 x8 x16 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1002 x3500) (d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x18 x3000 x3500 = case x18 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_20 x1 x2 x3 x4 x6 x8 x16 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1002 x3000 x3500) (nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x2 x3 x4 x6 x8 x16 x17 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x2 x3 x4 x6 x8 x16 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'l'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_20 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x2 x3 x4 x6 x8 x16 x3000 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x2000 = x3000
           in (seq x2000 (let
               x20 = x18
                in (nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'l'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_20 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x21 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_18 x1 x2 x3 x4 x6 x8 x19 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1002 x3500) (d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x21 x3000 x3500 = case x21 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x2 x3 x4 x6 x8 x19 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1002 x3000 x3500) (nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x2 x3 x4 x6 x8 x19 x20 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x2 x3 x4 x6 x8 x19 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'u'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_18 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x2 x3 x4 x6 x8 x19 x3000 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x2000 = x3000
           in (seq x2000 (let
               x23 = x21
                in (nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'u'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_18 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x24 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_16 x1 x2 x3 x4 x6 x8 x22 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1002 x3500) (d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x24 x3000 x3500 = case x24 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 x2 x3 x4 x6 x8 x22 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1002 x3000 x3500) (nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x2 x3 x4 x6 x8 x22 x23 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x2 x3 x4 x6 x8 x22 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'd'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_16 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x2 x3 x4 x6 x8 x22 x3000 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x2000 = x3000
           in (seq x2000 (let
               x26 = x24
                in (nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'd'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_16 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x27 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_14 x1 x2 x3 x4 x6 x8 x25 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1002 x3500) (d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x27 x3000 x3500 = case x27 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x2 x3 x4 x6 x8 x25 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1002 x3000 x3500) (nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x2 x3 x4 x6 x8 x25 x26 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x2 x3 x4 x6 x8 x25 x3500 = case x25 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char 'e'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_14 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x2 x3 x4 x6 x8 x25 x3000 x3500 = case x25 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x2000 = x3000
           in (seq x2000 (let
               x29 = x27
                in (nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char 'e'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_14 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x30 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_12 x1 x2 x3 x4 x6 x8 x28 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1002 x3500) (d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x30 x3000 x3500 = case x30 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x2 x3 x4 x6 x8 x28 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1002 x3000 x3500) (nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x2 x3 x4 x6 x8 x28 x29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x3 x4 x6 x8 x28 x3500 = case x28 of
     Curry_Prelude.OP_List -> d_OP__case_11 x1 x2 x3 x4 x6 x8 x3500
     (Curry_Prelude.OP_Cons x50 x51) -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x2 x3 x4 x6 x8 x1002 x3500) (d_OP__case_12 x1 x2 x3 x4 x6 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x2 x3 x4 x6 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x2 x3 x4 x6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x3 x4 x6 x8 x28 x3000 x3500 = case x28 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x1 x2 x3 x4 x6 x8 x2000 x3500))
     (Curry_Prelude.OP_Cons x50 x51) -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x2 x3 x4 x6 x8 x1002 x3000 x3500) (nd_OP__case_12 x1 x2 x3 x4 x6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x2 x3 x4 x6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x2 x3 x4 x6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x2 x3 x4 x6 x8 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x32 = x30
           in (d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 (Curry_Prelude.d_OP_eq_eq x32 (Curry_Prelude.C_Char 'c'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_11 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x2 x3 x4 x6 x8 x3000 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x2000 = x3000
           in (seq x2000 (let
               x32 = x30
                in (nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 (Curry_Prelude.d_OP_eq_eq x32 (Curry_Prelude.C_Char 'c'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_11 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x33 x3500 = case x33 of
     Curry_Prelude.C_True -> d_OP__case_9 x1 x2 x3 x4 x6 x31 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1002 x3500) (d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x33 x3000 x3500 = case x33 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x1 x2 x3 x4 x6 x31 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x3 x4 x6 x31 x32 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x3 x4 x6 x31 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x33 x34) -> let
          x35 = x33
           in (d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 (Curry_Prelude.d_OP_eq_eq x35 (Curry_Prelude.C_Char 'o'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_9 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x3 x4 x6 x31 x3000 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x33 x34) -> let
          x2000 = x3000
           in (seq x2000 (let
               x35 = x33
                in (nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 (Curry_Prelude.d_OP_eq_eq x35 (Curry_Prelude.C_Char 'o'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x36 x3500 = case x36 of
     Curry_Prelude.C_True -> d_OP__case_7 x1 x2 x3 x4 x6 x34 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1002 x3500) (d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x36 x3000 x3500 = case x36 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x2 x3 x4 x6 x34 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x3 x4 x6 x34 x35 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x4 x6 x34 x3500 = case x34 of
     (Curry_Prelude.OP_Cons x36 x37) -> let
          x38 = x36
           in (d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 (Curry_Prelude.d_OP_eq_eq x38 (Curry_Prelude.C_Char 'n'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_7 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x4 x6 x34 x3000 x3500 = case x34 of
     (Curry_Prelude.OP_Cons x36 x37) -> let
          x2000 = x3000
           in (seq x2000 (let
               x38 = x36
                in (nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 (Curry_Prelude.d_OP_eq_eq x38 (Curry_Prelude.C_Char 'n'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x39 x3500 = case x39 of
     Curry_Prelude.C_True -> d_OP__case_5 x1 x2 x3 x4 x6 x37 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1002 x3500) (d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x39 x3000 x3500 = case x39 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x2 x3 x4 x6 x37 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x3 x4 x6 x37 x38 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x3 x4 x6 x37 x3500 = case x37 of
     (Curry_Prelude.OP_Cons x39 x40) -> let
          x41 = x39
           in (d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 (Curry_Prelude.d_OP_eq_eq x41 (Curry_Prelude.C_Char 'd'#) x3500) x3500)
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_5 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x3 x4 x6 x37 x3000 x3500 = case x37 of
     (Curry_Prelude.OP_Cons x39 x40) -> let
          x2000 = x3000
           in (seq x2000 (let
               x41 = x39
                in (nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 (Curry_Prelude.d_OP_eq_eq x41 (Curry_Prelude.C_Char 'd'#) x3500) x2000 x3500)))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x42 x3500 = case x42 of
     Curry_Prelude.C_True -> d_OP__case_3 x1 x2 x3 x4 x6 x40 x3500
     Curry_Prelude.C_False -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1002 x3500) (d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x42 x3000 x3500 = case x42 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x2 x3 x4 x6 x40 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x4 x6 x40 x41 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x6 x40 x3500 = case x40 of
     Curry_Prelude.OP_List -> d_OP__case_2 x1 x2 x3 x4 x6 x3500
     (Curry_Prelude.OP_Cons x48 x49) -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x4 x6 x1002 x3500) (d_OP__case_3 x1 x2 x3 x4 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x4 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x4 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x6 x40 x3000 x3500 = case x40 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x2 x3 x4 x6 x2000 x3500))
     (Curry_Prelude.OP_Cons x48 x49) -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x4 x6 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x4 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x4 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x4 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x4 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x42 x43) -> d_OP__case_1 x1 x2 x3 x4 x42 x43 x3500
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x3 x4 x1002 x3500) (d_OP__case_2 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x4 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x42 x43) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x2 x3 x4 x42 x43 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x4 x42 x43 x3500 = case x43 of
     (Curry_Prelude.OP_Cons x44 x45) -> d_OP__case_0 x1 x2 x3 x4 x42 x44 x45 x3500
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x3 x4 x42 x1002 x3500) (d_OP__case_1 x1 x2 x3 x4 x42 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 x3 x4 x42 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x3 x4 x42 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x4 x42 x43 x3000 x3500 = case x43 of
     (Curry_Prelude.OP_Cons x44 x45) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x2 x3 x4 x42 x44 x45 x2000 x3500))
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x3 x4 x42 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x3 x4 x42 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 x3 x4 x42 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x3 x4 x42 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3 x4 x42 x44 x45 x3500 = case x45 of
     Curry_Prelude.OP_List -> Curry_State.d_C_bindS x42 (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 x44 x1)
     (Curry_Prelude.OP_Cons x46 x47) -> Curry_State.d_C_bindS (Curry_Prelude.d_C_apply (Curry_State.d_C_sequenceS x3500) x4 x3500) (d_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x3 x4 x42 x44 x1002 x3500) (d_OP__case_0 x1 x2 x3 x4 x42 x44 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 x3 x4 x42 x44 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x3 x4 x42 x44 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x4 x42 x44 x45 x3000 x3500 = case x45 of
     Curry_Prelude.OP_List -> wrapNX id (Curry_State.nd_C_bindS x42 (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda2 x44 x1)))
     (Curry_Prelude.OP_Cons x46 x47) -> let
          x2002 = x3000
           in (seq x2002 (wrapNX id (Curry_State.nd_C_bindS (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_State.nd_C_sequenceS x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_transExpr_dot_transComb_dot_17_dot___hash_lambda4 x3 x2)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x3 x4 x42 x44 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x3 x4 x42 x44 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 x3 x4 x42 x44 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x3 x4 x42 x44 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x1002 x3500) (d_OP__case_29 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x1002 x3000 x3500) (nd_OP__case_29 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x2 x1002 x3500) (d_OP__case_30 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x2 x1002 x3000 x3500) (nd_OP__case_30 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> let
          x10 = Curry_State.d_C_runState (d_C_transExpr x2 x9 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.C_Int 0#)) x3500
          x11 = d_OP_transFunc_dot___hash_selFP2_hash_newExp x10 x3500
          x12 = d_OP_transFunc_dot___hash_selFP3_hash_newFuns x10 x3500
           in (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x2 x3 x4 x5 (Curry_FlatCurry.C_Rule x8 x11)) x12)
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_31 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     (Curry_FlatCurry.C_External x7) -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x10 = let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_State.nd_C_runState (nd_C_transExpr x2 x9 x2000 x3500) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.C_Int 0#)) x2001 x3500)))
               x11 = d_OP_transFunc_dot___hash_selFP2_hash_newExp x10 x3500
               x12 = d_OP_transFunc_dot___hash_selFP3_hash_newFuns x10 x3500
                in (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Func x2 x3 x4 x5 (Curry_FlatCurry.C_Rule x8 x11)) x12)))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_31 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
