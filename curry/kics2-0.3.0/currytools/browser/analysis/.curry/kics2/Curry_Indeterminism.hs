{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Indeterminism (d_C_analyseIndeterminism, nd_C_analyseIndeterminism, d_C_choiceInExpr) where

import Basics
import qualified Curry_Dependency
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_List
d_C_analyseIndeterminism :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_analyseIndeterminism x3250 x3500 = Curry_Dependency.d_C_analyseWithDependencies d_C_isIndeterministic (Curry_Prelude.d_C_or x3250 x3500)

nd_C_analyseIndeterminism :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool))
nd_C_analyseIndeterminism x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (Curry_Dependency.nd_C_analyseWithDependencies (wrapDX id d_C_isIndeterministic) (Curry_Prelude.nd_C_or x2000 x3250 x3500))))

d_C_isIndeterministic :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isIndeterministic x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_C_isIndetRule x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isIndeterministic x1002 x3250 x3500) (d_C_isIndeterministic x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isIndeterministic z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isIndeterministic x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isIndetRule :: Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isIndetRule x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Rule x2 x3) -> d_C_choiceInExpr x3 x3250 x3500
     (Curry_FlatCurry.C_External x4) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isIndetRule x1002 x3250 x3500) (d_C_isIndetRule x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isIndetRule z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isIndetRule x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_choiceInExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_choiceInExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_C_foldr (acceptCs id Curry_Prelude.d_OP_bar_bar) Curry_Prelude.C_False (Curry_Prelude.d_C_map d_C_choiceInExpr x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_choiceInExpr x8 x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_choiceInExpr x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x9 x3250 x3500) x3250 x3500) (d_C_choiceInExpr x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.d_OP_bar_bar (d_C_choiceInExpr x11 x3250 x3500) (d_C_choiceInExpr x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_OP_bar_bar (d_C_choiceInExpr x14 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_choiceInExpr_dot_choiceInBranch_dot_33 x3250 x3500) x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_choiceInExpr x1002 x3250 x3500) (d_C_choiceInExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_choiceInExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_choiceInExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_choiceInExpr_dot_choiceInBranch_dot_33 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_choiceInExpr_dot_choiceInBranch_dot_33 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_choiceInExpr x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_choiceInExpr_dot_choiceInBranch_dot_33 x1002 x3250 x3500) (d_OP_choiceInExpr_dot_choiceInBranch_dot_33 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_choiceInExpr_dot_choiceInBranch_dot_33 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_choiceInExpr_dot_choiceInBranch_dot_33 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
