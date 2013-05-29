{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlatCurryDependency (d_C_dependsDirectlyOnTypes, d_C_callsDirectly) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
import qualified Curry_SetRBT
d_C_dependsDirectlyOnTypes :: Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_dependsDirectlyOnTypes x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> Curry_List.d_C_nub (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 x3500) x5 x3500) x3500
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> Curry_List.d_C_nub (d_C_tconsOf x9 x3500) x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dependsDirectlyOnTypes x1002 x3500) (d_C_dependsDirectlyOnTypes x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dependsDirectlyOnTypes z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dependsDirectlyOnTypes x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 :: Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_tconsOf x3500) x5 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 x1002 x3500) (d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dependsDirectlyOnTypes_dot___hash_lambda1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tconsOf :: Curry_FlatCurry.C_TypeExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_tconsOf x1 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_tconsOf x3 x3500) (d_C_tconsOf x4 x3500) x3500
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tconsOf x1002 x3500) (d_C_tconsOf x1003 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tconsOf z x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tconsOf x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_callsDirectly :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_callsDirectly x1 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3500) (Curry_Prelude.d_C_snd (d_C_directlyDependent x1 x3500) x3500) x3500

d_C_directlyDependent :: Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_directlyDependent x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_2 x2 x6 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_directlyDependent x1002 x3500) (d_C_directlyDependent x1003 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_directlyDependent z x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_directlyDependent x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_directlyDependent :: Curry_FlatCurry.C_FuncDecl -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_directlyDependent x1 x3000 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x2 x6 x2000 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_directlyDependent x1002 x3000 x3500) (nd_C_directlyDependent x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_directlyDependent z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_directlyDependent x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_funcSetOfExpr :: Curry_FlatCurry.C_Expr -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_funcSetOfExpr x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> d_C_emptySet x3500
     (Curry_FlatCurry.C_Lit x3) -> d_C_emptySet x3500
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_1 x4 x5 x6 (d_C_isConstructorComb x4 x3500) x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_funcSetOfExpr x8 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_SetRBT.d_C_unionRBT (Curry_Prelude.d_C_apply (d_C_unionMap (Curry_Prelude.d_OP_dot d_C_funcSetOfExpr Curry_Prelude.d_C_snd x3500) x3500) x9 x3500) (d_C_funcSetOfExpr x10 x3500) x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_SetRBT.d_C_unionRBT (d_C_funcSetOfExpr x11 x3500) (d_C_funcSetOfExpr x12 x3500) x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_SetRBT.d_C_unionRBT (d_C_funcSetOfExpr x14 x3500) (Curry_Prelude.d_C_apply (d_C_unionMap d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x3500) x15 x3500) x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_funcSetOfExpr x1002 x3500) (d_C_funcSetOfExpr x1003 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_funcSetOfExpr z x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_funcSetOfExpr x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_funcSetOfExpr :: Curry_FlatCurry.C_Expr -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_funcSetOfExpr x1 x3000 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_emptySet x2000 x3500))
     (Curry_FlatCurry.C_Lit x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_emptySet x2000 x3500))
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x4 x5 x6 (d_C_isConstructorComb x4 x3500) x2000 x3500))
     (Curry_FlatCurry.C_Free x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcSetOfExpr x8 x2000 x3500))
     (Curry_FlatCurry.C_Let x9 x10) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2004 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2004 (seq x2005 (Curry_SetRBT.nd_C_unionRBT (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (nd_C_unionMap (Curry_Prelude.nd_OP_dot (wrapNX id nd_C_funcSetOfExpr) (wrapDX id Curry_Prelude.d_C_snd) x2000 x3500) x2001 x3500)))) x9 x2003 x3500)))) (nd_C_funcSetOfExpr x10 x2005 x3500) x2006 x3500))))))))
     (Curry_FlatCurry.C_Or x11 x12) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_SetRBT.nd_C_unionRBT (nd_C_funcSetOfExpr x11 x2000 x3500) (nd_C_funcSetOfExpr x12 x2001 x3500) x2002 x3500))))))))
     (Curry_FlatCurry.C_Case x13 x14 x15) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_SetRBT.nd_C_unionRBT (nd_C_funcSetOfExpr x14 x2000 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54) x2001 x3500) x15 x2002 x3500)))) x2004 x3500))))))))
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_funcSetOfExpr x1002 x3000 x3500) (nd_C_funcSetOfExpr x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_funcSetOfExpr z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_funcSetOfExpr x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 :: Curry_FlatCurry.C_BranchExpr -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_funcSetOfExpr x3 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1002 x3500) (d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1003 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 z x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 :: Curry_FlatCurry.C_BranchExpr -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1 x3000 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcSetOfExpr x3 x2000 x3500))
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1002 x3000 x3500) (nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_54 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isConstructorComb :: Curry_FlatCurry.C_CombType -> ConstStore -> Curry_Prelude.C_Bool
d_C_isConstructorComb x1 x3500 = case x1 of
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_ConsPartCall x2) -> Curry_Prelude.C_True
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncPartCall x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isConstructorComb x1002 x3500) (d_C_isConstructorComb x1003 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isConstructorComb z x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isConstructorComb x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unionMap :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_unionMap x1 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_foldr (acceptCs id Curry_SetRBT.d_C_unionRBT) (d_C_emptySet x3500)) (Curry_Prelude.d_C_map x1) x3500

nd_C_unionMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_unionMap x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id Curry_SetRBT.nd_C_unionRBT)) (nd_C_emptySet x2000 x3500))) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3500)))))

d_C_emptySet :: ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_emptySet x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3500) (acceptCs id d_C_leqQName) x3500

nd_C_emptySet :: IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_emptySet x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2000 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2001 x3500)))))

d_C_leqQName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqQName x1 x2 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqQName x1002 x2 x3500) (d_C_leqQName x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqQName z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqQName x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x3 x4 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_lt_eq (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x4) x3500) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x6) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_OP_lt_eq (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x4) x3500) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x6) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (d_C_unionMap d_C_funcSetOfExpr x3500) x6 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3500) x5 x3500) (Curry_Prelude.d_C_apply (d_C_unionMap d_C_funcSetOfExpr x3500) x6 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x5 x6 x1002 x3500) (d_OP__case_1 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_C_funcSetOfExpr) x2000 x3500) x6 x2001 x3500)))))
     Curry_Prelude.C_False -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2002 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2002 (seq x2005 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3500) x5 x2001 x3500)))) (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_C_funcSetOfExpr) x2003 x3500) x6 x2004 x3500)))) x2006 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_1 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x2 x6 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> Curry_Prelude.OP_Tuple2 x2 (d_C_funcSetOfExpr x8 x3500)
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.OP_Tuple2 x2 (d_C_emptySet x3500)
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x1002 x3500) (d_OP__case_2 x2 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x2 x6 x3000 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x2 (nd_C_funcSetOfExpr x8 x2000 x3500)))
     (Curry_FlatCurry.C_External x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x2 (nd_C_emptySet x2000 x3500)))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x2 x1002 x3000 x3500) (nd_OP__case_2 x2 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x2 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
