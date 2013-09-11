{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Dependency2 (d_C_analyseWithDependencies, nd_C_analyseWithDependencies, d_C_externalDependent, d_C_indirectlyDependent, d_C_callsDirectly, d_C_dependencyGraphs, d_C_localDependencyGraphs, d_C_funcsInExpr, d_C_callsDirectly2) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
import qualified Curry_SetRBT
import qualified Curry_Sort
d_C_analyseWithDependencies :: Curry_Prelude.Curry t0 => (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t0) -> (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)
d_C_analyseWithDependencies x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_C_map (d_OP_analyseWithDependencies_dot___hash_lambda1 x1) x3 x3250 x3500
     x5 = d_C_indirectlyDependent x3 x3250 x3500
      in (Curry_Prelude.d_C_map (d_OP_analyseWithDependencies_dot_anaFun_dot_2 x2 x4) x5 x3250 x3500)

nd_C_analyseWithDependencies :: Curry_Prelude.Curry t0 => Func Curry_FlatCurry.C_FuncDecl t0 -> Func (Curry_Prelude.OP_List t0) t0 -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)
nd_C_analyseWithDependencies x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x4 = Curry_Prelude.nd_C_map (wrapNX id (nd_OP_analyseWithDependencies_dot___hash_lambda1 x1)) x3 x2000 x3250 x3500
               x5 = d_C_indirectlyDependent x3 x3250 x3500
                in (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x2 x4)) x5 x2001 x3250 x3500))))))

d_OP_analyseWithDependencies_dot_funcName_dot_2 :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_analyseWithDependencies_dot_funcName_dot_2 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> x2
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_analyseWithDependencies_dot_funcName_dot_2 x1002 x3250 x3500) (d_OP_analyseWithDependencies_dot_funcName_dot_2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_analyseWithDependencies_dot_funcName_dot_2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_analyseWithDependencies_dot_funcName_dot_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_analyseWithDependencies_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t0) -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
d_OP_analyseWithDependencies_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 (d_OP_analyseWithDependencies_dot_funcName_dot_2 x2 x3250 x3500) (Curry_Prelude.d_C_apply x1 x2 x3250 x3500)

nd_OP_analyseWithDependencies_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => Func Curry_FlatCurry.C_FuncDecl t0 -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
nd_OP_analyseWithDependencies_dot___hash_lambda1 x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 (d_OP_analyseWithDependencies_dot_funcName_dot_2 x2 x3250 x3500) (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500)))

d_OP_analyseWithDependencies_dot_lookupProp_dot_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> t0
d_OP_analyseWithDependencies_dot_lookupProp_dot_2 x1 x2 x3250 x3500 = Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x1 x3250 x3500) x3250 x3500

d_OP_analyseWithDependencies_dot_anaFun_dot_2 :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
d_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_map (d_OP_analyseWithDependencies_dot_lookupProp_dot_2 x2) (Curry_Prelude.OP_Cons x4 x5) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1002 x3250 x3500) (d_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_analyseWithDependencies_dot_anaFun_dot_2 :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List t0) t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0
nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Tuple2 x4 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.nd_C_map (wrapDX id (d_OP_analyseWithDependencies_dot_lookupProp_dot_2 x2)) (Curry_Prelude.OP_Cons x4 x5) x2000 x3250 x3500) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_analyseWithDependencies_dot_anaFun_dot_2 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_externalDependent :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_externalDependent x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_externalDependent_dot_getExternal_dot_17 x3250 x3500) x1 x3250 x3500
      in (Curry_Prelude.d_C_map (d_OP_externalDependent_dot___hash_lambda2 x2) (d_C_indirectlyDependent x1 x3250 x3500) x3250 x3500)

d_OP_externalDependent_dot_getExternal_dot_17 :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_externalDependent_dot_getExternal_dot_17 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_6 x2 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_externalDependent_dot_getExternal_dot_17 x1002 x3250 x3500) (d_OP_externalDependent_dot_getExternal_dot_17 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_externalDependent_dot_getExternal_dot_17 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_externalDependent_dot_getExternal_dot_17 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_externalDependent_dot___hash_lambda2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_externalDependent_dot___hash_lambda2 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x1) x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_externalDependent_dot___hash_lambda2 x1 x1002 x3250 x3500) (d_OP_externalDependent_dot___hash_lambda2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_externalDependent_dot___hash_lambda2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_externalDependent_dot___hash_lambda2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_indirectlyDependent :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_indirectlyDependent x1 x3250 x3500 = Curry_Prelude.d_C_map d_OP_indirectlyDependent_dot___hash_lambda3 (d_C_depsClosure (Curry_Prelude.d_C_map d_C_directlyDependent x1 x3250 x3500) x3250 x3500) x3250 x3500

d_OP_indirectlyDependent_dot___hash_lambda3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_indirectlyDependent_dot___hash_lambda3 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) x3 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_indirectlyDependent_dot___hash_lambda3 x1002 x3250 x3500) (d_OP_indirectlyDependent_dot___hash_lambda3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_indirectlyDependent_dot___hash_lambda3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_indirectlyDependent_dot___hash_lambda3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_indirectlyDependent_dot___hash_lambda3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_indirectlyDependent_dot___hash_lambda3 x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Tuple2 x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) x3 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_indirectlyDependent_dot___hash_lambda3 x1002 x3000 x3250 x3500) (nd_OP_indirectlyDependent_dot___hash_lambda3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_indirectlyDependent_dot___hash_lambda3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_indirectlyDependent_dot___hash_lambda3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_callsDirectly :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_callsDirectly x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Prelude.d_C_snd (d_C_directlyDependent x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_directlyDependent :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_directlyDependent x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_5 x2 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_directlyDependent x1002 x3250 x3500) (d_C_directlyDependent x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_directlyDependent z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_directlyDependent x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_directlyDependent :: Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_directlyDependent x1 x3000 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x2 x6 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_directlyDependent x1002 x3000 x3250 x3500) (nd_C_directlyDependent x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_directlyDependent z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_directlyDependent x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_depsClosure :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_depsClosure x1 x3250 x3500 = Curry_Prelude.d_C_map (d_OP_depsClosure_dot___hash_lambda5 x1) x1 x3250 x3500

nd_C_depsClosure :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_C_depsClosure x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_depsClosure_dot___hash_lambda5 x1)) x1 x2000 x3250 x3500))

d_OP_depsClosure_dot_closure_dot_52 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_depsClosure_dot_closure_dot_52 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = Curry_Prelude.d_C_filter (d_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 x2) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Prelude.d_C_maybe (d_C_emptySet x3250 x3500) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x4 x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
           in (d_OP_depsClosure_dot_closure_dot_52 x1 (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3250 x3500) x2 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x6 x5 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_depsClosure_dot_closure_dot_52 x1 x2 x1002 x3250 x3500) (d_OP_depsClosure_dot_closure_dot_52 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_depsClosure_dot_closure_dot_52 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_depsClosure_dot_closure_dot_52 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_depsClosure_dot_closure_dot_52 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_depsClosure_dot_closure_dot_52 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2014 = x3000
           in (seq x2014 (let
               x2008 = leftSupply x2014
               x2013 = rightSupply x2014
                in (seq x2008 (seq x2013 (let
                    x6 = let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 x2)) (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_maybe (nd_C_emptySet x2001 x3250 x3500) (wrapDX id Curry_Prelude.d_C_id) (Curry_Prelude.d_C_lookup x4 x1 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))
                     in (let
                         x2012 = leftSupply x2013
                         x2011 = rightSupply x2013
                          in (seq x2012 (seq x2011 (nd_OP_depsClosure_dot_closure_dot_52 x1 (let
                              x2010 = leftSupply x2011
                              x2009 = rightSupply x2011
                               in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2009 x3250 x3500) x2 x6 x2010 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus x6 x5 x3250 x3500) x2012 x3250 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_depsClosure_dot_closure_dot_52 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_depsClosure_dot_closure_dot_52 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_depsClosure_dot_closure_dot_52 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_depsClosure_dot_closure_dot_52 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 x1 x2 x3250 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3250 x3500) x1 x3250 x3500) x3250 x3500

nd_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_depsClosure_dot_closure_dot_52_dot___hash_lambda4 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3250 x3500))

d_OP_depsClosure_dot___hash_lambda5 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_depsClosure_dot___hash_lambda5 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (d_OP_depsClosure_dot_closure_dot_52 x1 x4 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) x4 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_depsClosure_dot___hash_lambda5 x1 x1002 x3250 x3500) (d_OP_depsClosure_dot___hash_lambda5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_depsClosure_dot___hash_lambda5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_depsClosure_dot___hash_lambda5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_depsClosure_dot___hash_lambda5 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_depsClosure_dot___hash_lambda5 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.OP_Tuple2 x3 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_depsClosure_dot_closure_dot_52 x1 x4 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x2003 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_depsClosure_dot___hash_lambda5 x1 x1002 x3000 x3250 x3500) (nd_OP_depsClosure_dot___hash_lambda5 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_depsClosure_dot___hash_lambda5 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_depsClosure_dot___hash_lambda5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_dependencyGraphs :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))
d_C_dependencyGraphs x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_map d_C_directlyDependent x1 x3250 x3500
      in (Curry_Prelude.d_C_map (d_OP_dependencyGraphs_dot___hash_lambda6 x2) (d_C_depsClosure x2 x3250 x3500) x3250 x3500)

d_OP_dependencyGraphs_dot___hash_lambda6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
d_OP_dependencyGraphs_dot___hash_lambda6 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_map (d_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 x1) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3250 x3500) x3 x3250 x3500) x4 x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_dependencyGraphs_dot___hash_lambda6 x1 x1002 x3250 x3500) (d_OP_dependencyGraphs_dot___hash_lambda6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_dependencyGraphs_dot___hash_lambda6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_dependencyGraphs_dot___hash_lambda6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_dependencyGraphs_dot___hash_lambda6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_OP_dependencyGraphs_dot___hash_lambda6 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2010 = x3000
           in (seq x2010 (Curry_Prelude.OP_Tuple2 x3 (let
               x2009 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2009 (seq x2007 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 x1)) (let
                    x2006 = leftSupply x2007
                    x2008 = rightSupply x2007
                     in (seq x2006 (seq x2008 (let
                         x2000 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                                   x2002 = leftSupply x2003
                                   x2001 = rightSupply x2003
                                    in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2001 x3250 x3500) x3 x2002 x3250 x3500)))) x4 x2004 x3250 x3500)))) x2006 x3250 x3500))))))) x2009 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_dependencyGraphs_dot___hash_lambda6 x1 x1002 x3000 x3250 x3500) (nd_OP_dependencyGraphs_dot___hash_lambda6 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_dependencyGraphs_dot___hash_lambda6 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_dependencyGraphs_dot___hash_lambda6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x1 x3250 x3500) x3250 x3500) x3250 x3500)

nd_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_dependencyGraphs_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.OP_Tuple2 x2 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x2 x1 x3250 x3500) x3250 x3500) x2001 x3250 x3500))))))

d_C_localDependencyGraphs :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))))
d_C_localDependencyGraphs x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_map d_C_directlyDependent x1 x3250 x3500
      in (Curry_Prelude.d_C_map (d_OP_localDependencyGraphs_dot___hash_lambda8 x2) (d_C_localDepsClosure x2 x3250 x3500) x3250 x3500)

d_OP_localDependencyGraphs_dot___hash_lambda8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
d_OP_localDependencyGraphs_dot___hash_lambda8 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_map (d_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 x1 x3) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3250 x3500) x3 x3250 x3500) x4 x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1002 x3250 x3500) (d_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_localDependencyGraphs_dot___hash_lambda8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_localDependencyGraphs_dot___hash_lambda8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_OP_localDependencyGraphs_dot___hash_lambda8 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2010 = x3000
           in (seq x2010 (Curry_Prelude.OP_Tuple2 x3 (let
               x2009 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2009 (seq x2007 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 x1 x3)) (let
                    x2006 = leftSupply x2007
                    x2008 = rightSupply x2007
                     in (seq x2006 (seq x2008 (let
                         x2000 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                                   x2002 = leftSupply x2003
                                   x2001 = rightSupply x2003
                                    in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2001 x3250 x3500) x3 x2002 x3250 x3500)))) x4 x2004 x3250 x3500)))) x2006 x3250 x3500))))))) x2009 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1002 x3000 x3250 x3500) (nd_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_localDependencyGraphs_dot___hash_lambda8 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_localDependencyGraphs_dot___hash_lambda8 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x3250 x3500 = Curry_Prelude.OP_Tuple2 x3 (d_OP__case_4 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x2 x3250 x3500) (Curry_Prelude.d_C_fst x3 x3250 x3500) x3250 x3500) x3250 x3500)

nd_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_localDependencyGraphs_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 x3 (nd_OP__case_4 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x2 x3250 x3500) (Curry_Prelude.d_C_fst x3 x3250 x3500) x3250 x3500) x2000 x3250 x3500)))

d_C_localDepsClosure :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_localDepsClosure x1 x3250 x3500 = Curry_Prelude.d_C_map (d_OP_localDepsClosure_dot___hash_lambda11 x1) x1 x3250 x3500

nd_C_localDepsClosure :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_C_localDepsClosure x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_localDepsClosure_dot___hash_lambda11 x1)) x1 x2000 x3250 x3500))

d_OP_localDepsClosure_dot_closure_dot_74 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_3 x5 x2 x6 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.d_C_fst x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1002 x3250 x3500) (d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_localDepsClosure_dot_closure_dot_74 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x5 x2 x6 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.d_C_fst x5 x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 x1 x2 x3250 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_elemRBT x2 x3250 x3500) x1 x3250 x3500) x3250 x3500

nd_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 :: Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_elemRBT x2 x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3250 x3500))

d_OP_localDepsClosure_dot___hash_lambda11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_localDepsClosure_dot___hash_lambda11 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (d_OP_localDepsClosure_dot_closure_dot_74 x1 (Curry_Prelude.d_C_fst x3 x3250 x3500) x4 (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) x4 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_localDepsClosure_dot___hash_lambda11 x1 x1002 x3250 x3500) (d_OP_localDepsClosure_dot___hash_lambda11 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_localDepsClosure_dot___hash_lambda11 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_localDepsClosure_dot___hash_lambda11 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_localDepsClosure_dot___hash_lambda11 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_localDepsClosure_dot___hash_lambda11 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.OP_Tuple2 x3 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_localDepsClosure_dot_closure_dot_74 x1 (Curry_Prelude.d_C_fst x3 x3250 x3500) x4 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x2003 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_localDepsClosure_dot___hash_lambda11 x1 x1002 x3000 x3250 x3500) (nd_OP_localDepsClosure_dot___hash_lambda11 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_localDepsClosure_dot___hash_lambda11 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_localDepsClosure_dot___hash_lambda11 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_funcsInExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_funcsInExpr x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (d_C_funcSetOfExpr x1 x3250 x3500) x3250 x3500

d_C_funcSetOfExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_funcSetOfExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> d_C_emptySet x3250 x3500
     (Curry_FlatCurry.C_Lit x3) -> d_C_emptySet x3250 x3500
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> d_OP__case_1 x4 x6 x5 (d_C_isConstructorComb x4 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x7 x8) -> d_C_funcSetOfExpr x8 x3250 x3500
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_SetRBT.d_C_unionRBT (Curry_Prelude.d_C_apply (d_C_unionMap (Curry_Prelude.d_OP_dot d_C_funcSetOfExpr Curry_Prelude.d_C_snd x3250 x3500) x3250 x3500) x9 x3250 x3500) (d_C_funcSetOfExpr x10 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_SetRBT.d_C_unionRBT (d_C_funcSetOfExpr x11 x3250 x3500) (d_C_funcSetOfExpr x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_SetRBT.d_C_unionRBT (d_C_funcSetOfExpr x14 x3250 x3500) (Curry_Prelude.d_C_apply (d_C_unionMap d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x3250 x3500) x15 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_funcSetOfExpr x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_funcSetOfExpr x1002 x3250 x3500) (d_C_funcSetOfExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_funcSetOfExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_funcSetOfExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_funcSetOfExpr :: Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_funcSetOfExpr x1 x3000 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_emptySet x2000 x3250 x3500))
     (Curry_FlatCurry.C_Lit x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_emptySet x2000 x3250 x3500))
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x4 x6 x5 (d_C_isConstructorComb x4 x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.C_Free x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcSetOfExpr x8 x2000 x3250 x3500))
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
                               in (seq x2001 (seq x2000 (nd_C_unionMap (Curry_Prelude.nd_OP_dot (wrapNX id nd_C_funcSetOfExpr) (wrapDX id Curry_Prelude.d_C_snd) x2000 x3250 x3500) x2001 x3250 x3500)))) x9 x2003 x3250 x3500)))) (nd_C_funcSetOfExpr x10 x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_FlatCurry.C_Or x11 x12) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_SetRBT.nd_C_unionRBT (nd_C_funcSetOfExpr x11 x2000 x3250 x3500) (nd_C_funcSetOfExpr x12 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_FlatCurry.C_Case x13 x14 x15) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_SetRBT.nd_C_unionRBT (nd_C_funcSetOfExpr x14 x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103) x2001 x3250 x3500) x15 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     (Curry_FlatCurry.C_Typed x16 x17) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcSetOfExpr x16 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_funcSetOfExpr x1002 x3000 x3250 x3500) (nd_C_funcSetOfExpr x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_funcSetOfExpr z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_funcSetOfExpr x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> d_C_funcSetOfExpr x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1002 x3250 x3500) (d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 :: Curry_FlatCurry.C_BranchExpr -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1 x3000 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcSetOfExpr x3 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1002 x3000 x3250 x3500) (nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_funcSetOfExpr_dot_funcSetOfBranch_dot_103 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isConstructorComb :: Curry_FlatCurry.C_CombType -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isConstructorComb x1 x3250 x3500 = case x1 of
     Curry_FlatCurry.C_ConsCall -> Curry_Prelude.C_True
     (Curry_FlatCurry.C_ConsPartCall x2) -> Curry_Prelude.C_True
     Curry_FlatCurry.C_FuncCall -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_FuncPartCall x3) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_CombType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isConstructorComb x1002 x3250 x3500) (d_C_isConstructorComb x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CombType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isConstructorComb z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CombType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isConstructorComb x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CombType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_unionMap :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_unionMap x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_foldr (acceptCs id Curry_SetRBT.d_C_unionRBT) (d_C_emptySet x3250 x3500)) (Curry_Prelude.d_C_map x1) x3250 x3500

nd_C_unionMap :: Curry_Prelude.Curry t0 => Func t0 (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_unionMap x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id Curry_SetRBT.nd_C_unionRBT)) (nd_C_emptySet x2000 x3250 x3500))) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3250 x3500)))))

d_C_emptySet :: Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_emptySet x3250 x3500 = Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_emptySetRBT x3250 x3500) (acceptCs id d_C_leqQName) x3250 x3500

nd_C_emptySet :: IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_emptySet x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_emptySetRBT x2000 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_leqQName)) x2001 x3250 x3500)))))

d_C_leqQName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqQName x1 x2 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x4 x3 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqQName x1002 x2 x3250 x3500) (d_C_leqQName x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqQName z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqQName x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_callsDirectly2 :: Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_callsDirectly2 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> Curry_List.d_C_nub (d_C_goThroughConsList x5 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x6 x7 x8 x9) -> Curry_List.d_C_nub (d_C_calledTypes x9 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_callsDirectly2 x1002 x3250 x3500) (d_C_callsDirectly2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_callsDirectly2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_callsDirectly2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_goThroughConsList :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_goThroughConsList x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x2 x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_calledTypes x3250 x3500) x4 x3250 x3500) (d_C_goThroughConsList x3 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_goThroughConsList x1002 x3250 x3500) (d_C_goThroughConsList x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_goThroughConsList z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_goThroughConsList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr
d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> x5
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1002 x3250 x3500) (d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_goThroughConsList_dot___hash_selFP2_hash_typeExprs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_calledTypes :: Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_calledTypes x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_TVar x2) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_FuncType x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_C_calledTypes x3 x3250 x3500) (d_C_calledTypes x4 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TCons x5 x6) -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_calledTypes x1002 x3250 x3500) (d_C_calledTypes x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_calledTypes z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_calledTypes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_0 x4 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Sort.d_C_leqString x3250 x3500) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x4) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) x6) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x4 x3 x1002 x3250 x3500) (d_OP__case_0 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_1 x4 x6 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (d_C_unionMap d_C_funcSetOfExpr x3250 x3500) x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_insertRBT x3250 x3500) x5 x3250 x3500) (Curry_Prelude.d_C_apply (d_C_unionMap d_C_funcSetOfExpr x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x6 x5 x1002 x3250 x3500) (d_OP__case_1 x4 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_1 x4 x6 x5 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_C_funcSetOfExpr) x2000 x3250 x3500) x6 x2001 x3250 x3500)))))
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
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_insertRBT x2000 x3250 x3500) x5 x2001 x3250 x3500)))) (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_unionMap (wrapNX id nd_C_funcSetOfExpr) x2003 x3250 x3500) x6 x2004 x3250 x3500)))) x2006 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x4 x6 x5 x1002 x3000 x3250 x3500) (nd_OP__case_1 x4 x6 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x4 x6 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x4 x6 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x5 x2 x6 x3 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x7 = Curry_Prelude.d_C_filter (d_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 x3) (Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Prelude.d_C_maybe (d_C_emptySet x3250 x3500) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x5 x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
           in (d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 (Curry_Prelude.d_C_foldr (Curry_SetRBT.d_C_insertRBT x3250 x3500) x3 x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x7 x6 x3250 x3500) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_2 x6 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x5 x2 x6 x3 x1 x1002 x3250 x3500) (d_OP__case_3 x5 x2 x6 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x5 x2 x6 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x5 x2 x6 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_3 x5 x2 x6 x3 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2014 = x3000
           in (seq x2014 (let
               x2008 = leftSupply x2014
               x2013 = rightSupply x2014
                in (seq x2008 (seq x2013 (let
                    x7 = let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_localDepsClosure_dot_closure_dot_74_dot___hash_lambda10 x3)) (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_maybe (nd_C_emptySet x2001 x3250 x3500) (wrapDX id Curry_Prelude.d_C_id) (Curry_Prelude.d_C_lookup x5 x1 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500))))))) x2007 x3250 x3500)))
                     in (let
                         x2012 = leftSupply x2013
                         x2011 = rightSupply x2013
                          in (seq x2012 (seq x2011 (nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 (let
                              x2010 = leftSupply x2011
                              x2009 = rightSupply x2011
                               in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_foldr (Curry_SetRBT.nd_C_insertRBT x2009 x3250 x3500) x3 x7 x2010 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus x7 x6 x3250 x3500) x2012 x3250 x3500)))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x6 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x5 x2 x6 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_3 x5 x2 x6 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x5 x2 x6 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x5 x2 x6 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x6 x3 x2 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x6 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_2 x6 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x6 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x6 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_2 x6 x3 x2 x1 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_localDepsClosure_dot_closure_dot_74 x1 x2 x3 x6 x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x6 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_2 x6 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x6 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x6 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_SetRBT.d_C_setRBT2list x3250 x3500) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x3 x1 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_4 x3 x2 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_SetRBT.nd_C_setRBT2list x2000 x3250 x3500) (Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x3 x1 x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_4 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_5 x2 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> Curry_Prelude.OP_Tuple2 x2 (d_C_funcSetOfExpr x8 x3250 x3500)
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.OP_Tuple2 x2 (d_C_emptySet x3250 x3500)
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x1002 x3250 x3500) (d_OP__case_5 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP__case_5 x2 x6 x3000 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x2 (nd_C_funcSetOfExpr x8 x2000 x3250 x3500)))
     (Curry_FlatCurry.C_External x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x2 (nd_C_emptySet x2000 x3250 x3500)))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x1002 x3000 x3250 x3500) (nd_OP__case_5 x2 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_6 x2 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Rule x7 x8) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_External x9) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x2 x1002 x3250 x3500) (d_OP__case_6 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
