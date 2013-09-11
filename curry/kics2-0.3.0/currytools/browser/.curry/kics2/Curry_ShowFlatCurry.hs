{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ShowFlatCurry (d_C_showInterface, d_C_showCurryMod, d_C_leqFunc, d_C_showFuncDeclAsCurry, d_C_showFuncDeclAsFlatCurry, d_C_funcModule) where

import Basics
import qualified Curry_Char
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_FlatCurryShow
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_Sort
d_C_showInterface :: Curry_Prelude.C_Bool -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInterface x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Prog x3 x4 x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_showInterfaceImport x3250 x3500) x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_showInterfaceOpDecl x3250 x3500) (Curry_Sort.d_C_mergeSort (acceptCs id d_C_leqOp) x7 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_43 x7 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showInterfaceType (Curry_FlatCurry.d_C_showQNameInModule x3)) x3250 x3500) (Curry_Sort.d_C_mergeSort (acceptCs id d_C_leqType) x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showInterfaceFunc (Curry_FlatCurry.d_C_showQNameInModule x3) x1) x3250 x3500) (Curry_Sort.d_C_mergeSort (acceptCs id d_C_leqFunc) x6 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInterface x1 x1002 x3250 x3500) (d_C_showInterface x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInterface x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInterface x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showInterfaceImport :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInterfaceImport x1 x3250 x3500 = d_OP__case_42 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500) x3250 x3500

d_C_showInterfaceOpDecl :: Curry_FlatCurry.C_OpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInterfaceOpDecl x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Op x2 x3 x4) -> d_OP__case_41 x2 x4 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInterfaceOpDecl x1002 x3250 x3500) (d_C_showInterfaceOpDecl x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInterfaceOpDecl z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInterfaceOpDecl x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showOp :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showOp x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_40 x3 (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showOp x1002 x3250 x3500) (d_C_showOp x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showOp z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showOp x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showInterfaceType :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInterfaceType x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> d_OP__case_39 x6 x1 x4 x5 x3 x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x10 x11 x12 x13) -> d_OP__case_36 x11 x13 x1 x12 x10 x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInterfaceType x1 x1002 x3250 x3500) (d_C_showInterfaceType x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInterfaceType x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInterfaceType x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showInterfaceType :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showInterfaceType x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_39 x6 x1 x4 x5 x3 x2000 x3250 x3500))
     (Curry_FlatCurry.C_TypeSyn x10 x11 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_36 x11 x13 x1 x12 x10 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showInterfaceType x1 x1002 x3000 x3250 x3500) (nd_C_showInterfaceType x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showInterfaceType x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showInterfaceType x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showInterfaceType_dot___hash_lambda1 :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showInterfaceType_dot___hash_lambda1 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showInterfaceType_dot___hash_lambda1 x1002 x3250 x3500) (d_OP_showInterfaceType_dot___hash_lambda1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showInterfaceType_dot___hash_lambda1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showInterfaceType_dot___hash_lambda1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showInterfaceType_dot___hash_lambda2 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showInterfaceType_dot___hash_lambda2 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)

d_OP_showInterfaceType_dot___hash_lambda3 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showInterfaceType_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)

d_C_showExportConsDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showExportConsDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Cons x3 x4 x5 x6) -> d_OP__case_34 x6 x1 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showExportConsDecl x1 x1002 x3250 x3500) (d_C_showExportConsDecl x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showExportConsDecl x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showExportConsDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showExportConsDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_ConsDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showExportConsDecl x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Cons x3 x4 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_34 x6 x1 x3 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showExportConsDecl x1 x1002 x3000 x3250 x3500) (nd_C_showExportConsDecl x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showExportConsDecl x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showExportConsDecl x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showExportConsDecl_dot___hash_lambda4 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showExportConsDecl_dot___hash_lambda4 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_True x2 x3250 x3500) x3250 x3500

nd_OP_showExportConsDecl_dot___hash_lambda4 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showExportConsDecl_dot___hash_lambda4 x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_True x2 x2000 x3250 x3500) x3250 x3500))

d_C_showInterfaceFunc :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInterfaceFunc x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Func x4 x5 x6 x7 x8) -> d_OP__case_33 x6 x2 x7 x1 x4 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showInterfaceFunc x1 x2 x1002 x3250 x3500) (d_C_showInterfaceFunc x1 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showInterfaceFunc x1 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showInterfaceFunc x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showInterfaceFunc :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showInterfaceFunc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Func x4 x5 x6 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_33 x6 x2 x7 x1 x4 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showInterfaceFunc x1 x2 x1002 x3000 x3250 x3500) (nd_C_showInterfaceFunc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showInterfaceFunc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showInterfaceFunc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryMod :: Curry_Prelude.C_Bool -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryMod x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Prog x3 x4 x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showTypeExports x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showFuncExports x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_showInterfaceImport x3250 x3500) x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_showInterfaceOpDecl x3250 x3500) x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_30 x7 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showCurryDataDecl (Curry_FlatCurry.d_C_showQNameInModule x3)) x3250 x3500) x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_C_showCurryFuncDecl (Curry_FlatCurry.d_C_showQNameInModule x3) (Curry_FlatCurry.d_C_showQNameInModule x3) x1) x3250 x3500) x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryMod x1 x1002 x3250 x3500) (d_C_showCurryMod x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryMod x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryMod x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showTypeExports :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showTypeExports x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showTypeExports_dot_exptype_dot_43 x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP_showTypeExports_dot_expc_dot_43 :: Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showTypeExports_dot_expc_dot_43 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Cons x2 x3 x4 x5) -> d_OP__case_29 x4 x2 (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showTypeExports_dot_expc_dot_43 x1002 x3250 x3500) (d_OP_showTypeExports_dot_expc_dot_43 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showTypeExports_dot_expc_dot_43 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showTypeExports_dot_expc_dot_43 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showTypeExports_dot_expcons_dot_43 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showTypeExports_dot_expcons_dot_43 x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showTypeExports_dot_expc_dot_43 x3250 x3500) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_OP_showTypeExports_dot_exptype_dot_43 :: Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showTypeExports_dot_exptype_dot_43 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> d_OP__case_28 x3 x5 x2 (Curry_Prelude.d_OP_eq_eq x3 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x7 x8 x9 x10) -> d_OP__case_26 x8 x7 (Curry_Prelude.d_OP_eq_eq x8 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showTypeExports_dot_exptype_dot_43 x1002 x3250 x3500) (d_OP_showTypeExports_dot_exptype_dot_43 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showTypeExports_dot_exptype_dot_43 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showTypeExports_dot_exptype_dot_43 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showFuncExports :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncExports x1 x3250 x3500 = Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showFuncExports_dot_expfun_dot_60 x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP_showFuncExports_dot_expfun_dot_60 :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showFuncExports_dot_expfun_dot_60 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x2 x3 x4 x5 x6) -> d_OP__case_25 x4 x2 (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showFuncExports_dot_expfun_dot_60 x1002 x3250 x3500) (d_OP_showFuncExports_dot_expfun_dot_60 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showFuncExports_dot_expfun_dot_60 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showFuncExports_dot_expfun_dot_60 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryDataDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryDataDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> let
          x7 = Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_map (d_C_showCurryConsDecl x1) x6 x3250 x3500) x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showCurryDataDecl_dot___hash_lambda5 x3250 x3500) x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_24 x7 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_FlatCurry.C_TypeSyn x8 x9 x10 x11) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x8 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showCurryDataDecl_dot___hash_lambda6 x3250 x3500) x10 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_True x11 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryDataDecl x1 x1002 x3250 x3500) (d_C_showCurryDataDecl x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryDataDecl x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryDataDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryDataDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryDataDecl x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Type x3 x4 x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x7 = Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_showCurryConsDecl x1)) x6 x2000 x3250 x3500) x3250 x3500
                     in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x3 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showCurryDataDecl_dot___hash_lambda5) x2001 x3250 x3500) x5 x2002 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (d_OP__case_24 x7 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500))))))
     (Curry_FlatCurry.C_TypeSyn x8 x9 x10 x11) -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x8 x3250 x3500) (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showCurryDataDecl_dot___hash_lambda6) x2000 x3250 x3500) x10 x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_True x11 x2003 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500))
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryDataDecl x1 x1002 x3000 x3250 x3500) (nd_C_showCurryDataDecl x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryDataDecl x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryDataDecl x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showCurryDataDecl_dot___hash_lambda5 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCurryDataDecl_dot___hash_lambda5 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)

d_OP_showCurryDataDecl_dot___hash_lambda6 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCurryDataDecl_dot___hash_lambda6 x1 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)

d_C_showCurryConsDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryConsDecl x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Cons x3 x4 x5 x6) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x3 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showCurryConsDecl_dot___hash_lambda7 x1) x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryConsDecl x1 x1002 x3250 x3500) (d_C_showCurryConsDecl x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryConsDecl x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryConsDecl x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryConsDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_ConsDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryConsDecl x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Cons x3 x4 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x3 x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_showCurryConsDecl_dot___hash_lambda7 x1)) x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x3250 x3500))
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryConsDecl x1 x1002 x3000 x3250 x3500) (nd_C_showCurryConsDecl x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryConsDecl x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryConsDecl x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showCurryConsDecl_dot___hash_lambda7 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCurryConsDecl_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_True x2 x3250 x3500) x3250 x3500

nd_OP_showCurryConsDecl_dot___hash_lambda7 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showCurryConsDecl_dot___hash_lambda7 x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_True x2 x2000 x3250 x3500) x3250 x3500))

d_C_showCurryFuncDecl :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryFuncDecl x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Func x5 x6 x7 x8 x9) -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_False x8 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_C_showCurryRule x2 x3 x5 x9 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryFuncDecl x1 x2 x3 x1002 x3250 x3500) (d_C_showCurryFuncDecl x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryFuncDecl x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryFuncDecl x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryFuncDecl :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_FuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryFuncDecl x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Func x5 x6 x7 x8 x9) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_False x8 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (nd_C_showCurryRule x2 x3 x5 x9 x2001 x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500))
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryFuncDecl x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_showCurryFuncDecl x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryFuncDecl x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryFuncDecl x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryRule :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryRule x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_External x5) -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))) x3250 x3500
     (Curry_FlatCurry.C_Rule x6 x7) -> d_OP__case_23 x7 x6 x3 x1 x2 x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryRule x1 x2 x3 x1002 x3250 x3500) (d_C_showCurryRule x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryRule x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryRule x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryRule :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryRule x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_External x5) -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))) x3250 x3500
     (Curry_FlatCurry.C_Rule x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x7 x6 x3 x1 x2 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryRule x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_showCurryRule x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryRule x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryRule x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryRuleAsCase :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryRuleAsCase x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Rule x4 x5) -> d_OP__case_22 x2 x4 x5 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x4 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) (Curry_Prelude.d_C_not (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head (Curry_Prelude.d_C_snd x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_External x6) -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryRuleAsCase x1 x2 x1002 x3250 x3500) (d_C_showCurryRuleAsCase x1 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryRuleAsCase x1 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryRuleAsCase x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryRuleAsCase :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryRuleAsCase x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Rule x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x2 x4 x5 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x4 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) (Curry_Prelude.d_C_not (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head (Curry_Prelude.d_C_snd x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_FlatCurry.C_External x6) -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId (Curry_Prelude.d_C_snd x2 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)))))))))) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryRuleAsCase x1 x2 x1002 x3000 x3250 x3500) (nd_C_showCurryRuleAsCase x1 x2 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryRuleAsCase x1 x2 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryRuleAsCase x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryRuleAsPatterns :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryRuleAsPatterns x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Rule x4 x5) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1) x3250 x3500) (d_C_rule2equations (d_C_shallowPattern2Expr x2 x4 x3250 x3500) x5 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showCurryRuleAsPatterns x1 x2 x1002 x3250 x3500) (d_C_showCurryRuleAsPatterns x1 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showCurryRuleAsPatterns x1 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showCurryRuleAsPatterns x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_showCurryRuleAsPatterns :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryRuleAsPatterns x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Rule x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1)) x2000 x3250 x3500) (d_C_rule2equations (d_C_shallowPattern2Expr x2 x4 x3250 x3500) x5 x3250 x3500) x2001 x3250 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500))
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_showCurryRuleAsPatterns x1 x2 x1002 x3000 x3250 x3500) (nd_C_showCurryRuleAsPatterns x1 x2 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_showCurryRuleAsPatterns x1 x2 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_showCurryRuleAsPatterns x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_C_showCurryPatternRule x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1002 x3250 x3500) (d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showCurryPatternRule x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1002 x3000 x3250 x3500) (nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showCurryRuleAsPatterns_dot___hash_lambda8 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_splitFreeVars :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) Curry_FlatCurry.C_Expr
d_C_splitFreeVars x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Free x2 x3) -> Curry_Prelude.OP_Tuple2 x2 x3
     (Curry_FlatCurry.C_Var x4) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Lit x5) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Comb x6 x7 x8) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Let x9 x10) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.C_Typed x16 x17) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x1
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitFreeVars x1002 x3250 x3500) (d_C_splitFreeVars x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitFreeVars z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitFreeVars x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryPatternRule :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryPatternRule x1 x2 x3 x3250 x3500 = let
     x4 = d_C_splitFreeVars x3 x3250 x3500
     x5 = d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x4 x3250 x3500
     x6 = d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x4 x3250 x3500
      in (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_C_showCurryCRHS x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_20 x5 (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.OP_List x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500)

nd_C_showCurryPatternRule :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryPatternRule x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x4 = d_C_splitFreeVars x3 x3250 x3500
          x5 = d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x4 x3250 x3500
          x6 = d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x4 x3250 x3500
           in (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x2 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (nd_C_showCurryCRHS x1 x6 x2001 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_20 x5 (Curry_Prelude.d_OP_eq_eq x5 Curry_Prelude.OP_List x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500))))))

d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x1002 x3250 x3500) (d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showCurryPatternRule_dot___hash_selFP2_hash_vars x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x1002 x3250 x3500) (d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showCurryPatternRule_dot___hash_selFP3_hash_e x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showCurryCRHS :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCurryCRHS x1 x2 x3250 x3500 = d_OP__case_19 x2 x1 (d_C_isGuardedExpr x2 x3250 x3500) x3250 x3500

nd_C_showCurryCRHS :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_showCurryCRHS x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_19 x2 x1 (d_C_isGuardedExpr x2 x3250 x3500) x2000 x3250 x3500))

d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Comb x3 x4 x5) -> d_OP__case_18 x1 x5 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1002 x3250 x3500) (d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Comb x3 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x1 x5 x2000 x3250 x3500))
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1002 x3000 x3250 x3500) (nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_rule2equations :: Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_C_rule2equations x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Case x3 x4 x5) -> d_OP__case_15 x2 x1 x4 x5 x3 x3250 x3500
     (Curry_FlatCurry.C_Or x22 x23) -> Curry_Prelude.d_OP_plus_plus (d_C_rule2equations x1 x22 x3250 x3500) (d_C_rule2equations x1 x23 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Var x24) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Lit x25) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x26 x27 x28) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Let x29 x30) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Free x31 x32) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Typed x33 x34) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_rule2equations x1 x1002 x3250 x3500) (d_C_rule2equations x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_rule2equations x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_rule2equations x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_caseIntoLhs :: Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_C_caseIntoLhs x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_13 x5 x2 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_caseIntoLhs x1 x2 x1002 x3250 x3500) (d_C_caseIntoLhs x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_caseIntoLhs x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_caseIntoLhs x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_shallowPattern2Expr :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_shallowPattern2Expr x1 x2 x3250 x3500 = Curry_FlatCurry.C_Comb Curry_FlatCurry.C_ConsCall x1 (Curry_Prelude.d_C_map d_OP_shallowPattern2Expr_dot___hash_lambda11 x2 x3250 x3500)

d_OP_shallowPattern2Expr_dot___hash_lambda11 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_shallowPattern2Expr_dot___hash_lambda11 x1 x3250 x3500 = Curry_FlatCurry.C_Var x1

d_C_substitute :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_substitute x1 x2 x3 x3250 x3500 = d_C_substituteAll x1 x2 (Curry_Prelude.C_Int 0#) x3 x3250 x3500

d_C_substituteAll :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_C_substituteAll x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Var x5) -> d_OP_substituteAll_dot_replaceVar_dot_151 x3 x1 x2 x5 x3250 x3500
     (Curry_FlatCurry.C_Lit x6) -> Curry_FlatCurry.C_Lit x6
     (Curry_FlatCurry.C_Comb x7 x8 x9) -> Curry_FlatCurry.C_Comb x7 x8 (Curry_Prelude.d_C_map (d_C_substituteAll x1 x2 x3) x9 x3250 x3500)
     (Curry_FlatCurry.C_Let x10 x11) -> Curry_FlatCurry.C_Let (Curry_Prelude.d_C_map (d_OP_substituteAll_dot___hash_lambda12 x3 x2 x1) x10 x3250 x3500) (d_C_substituteAll x1 x2 x3 x11 x3250 x3500)
     (Curry_FlatCurry.C_Free x12 x13) -> Curry_FlatCurry.C_Free (Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus) x3) x12 x3250 x3500) (d_C_substituteAll x1 x2 x3 x13 x3250 x3500)
     (Curry_FlatCurry.C_Or x14 x15) -> Curry_FlatCurry.C_Or (d_C_substituteAll x1 x2 x3 x14 x3250 x3500) (d_C_substituteAll x1 x2 x3 x15 x3250 x3500)
     (Curry_FlatCurry.C_Case x16 x17 x18) -> Curry_FlatCurry.C_Case x16 (d_C_substituteAll x1 x2 x3 x17 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substituteAllCase x1 x2 x3) x18 x3250 x3500)
     (Curry_FlatCurry.C_Typed x19 x20) -> Curry_FlatCurry.C_Typed (d_C_substituteAll x1 x2 x3 x19 x3250 x3500) x20
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substituteAll x1 x2 x3 x1002 x3250 x3500) (d_C_substituteAll x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substituteAll x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substituteAll x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_substituteAll_dot_replaceVar_dot_151 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_substituteAll_dot_replaceVar_dot_151 x1 x2 x3 x4 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_11 x4 x1 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_10 x4 x5 x6 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_substituteAll_dot_replaceVar_dot_151 x1 x1002 x3 x4 x3250 x3500) (d_OP_substituteAll_dot_replaceVar_dot_151 x1 x1003 x3 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_substituteAll_dot_replaceVar_dot_151 x1 z x3 x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_substituteAll_dot_replaceVar_dot_151 x1 x1002 x3 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_substituteAll_dot___hash_lambda12 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr
d_OP_substituteAll_dot___hash_lambda12 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus x5 x1 x3250 x3500) (d_C_substituteAll x3 x2 x1 x6 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_substituteAll_dot___hash_lambda12 x1 x2 x3 x1002 x3250 x3500) (d_OP_substituteAll_dot___hash_lambda12 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_substituteAll_dot___hash_lambda12 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_substituteAll_dot___hash_lambda12 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substituteAllCase :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_BranchExpr
d_C_substituteAllCase x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Branch x5 x6) -> d_OP__case_8 x6 x3 x2 x1 x5 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substituteAllCase x1 x2 x3 x1002 x3250 x3500) (d_C_substituteAllCase x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substituteAllCase x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substituteAllCase x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isGuardedExpr :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isGuardedExpr x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Comb x2 x3 x4) -> Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))) x3250 x3500
     (Curry_FlatCurry.C_Var x5) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Lit x6) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Let x7 x8) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Free x9 x10) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Or x11 x12) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.C_False
     (Curry_FlatCurry.C_Typed x16 x17) -> Curry_Prelude.C_False
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isGuardedExpr x1002 x3250 x3500) (d_C_isGuardedExpr x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isGuardedExpr z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isGuardedExpr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leqOp :: Curry_FlatCurry.C_OpDecl -> Curry_FlatCurry.C_OpDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqOp x1 x2 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Op x3 x4 x5) -> d_OP__case_7 x2 x5 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqOp x1002 x2 x3250 x3500) (d_C_leqOp x1003 x2 x3250 x3500)
     (Curry_FlatCurry.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqOp z x2 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqOp x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leqType :: Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqType x1 x2 x3250 x3500 = Curry_Prelude.d_OP_lt_eq (d_OP_leqType_dot_tname_dot_194 x1 x3250 x3500) (d_OP_leqType_dot_tname_dot_194 x2 x3250 x3500) x3250 x3500

d_OP_leqType_dot_tname_dot_194 :: Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_leqType_dot_tname_dot_194 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Type x2 x3 x4 x5) -> d_OP__case_4 x2 x3250 x3500
     (Curry_FlatCurry.C_TypeSyn x8 x9 x10 x11) -> d_OP__case_3 x8 x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_leqType_dot_tname_dot_194 x1002 x3250 x3500) (d_OP_leqType_dot_tname_dot_194 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_leqType_dot_tname_dot_194 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_leqType_dot_tname_dot_194 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leqFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqFunc x1 x2 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> d_OP__case_2 x2 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqFunc x1002 x2 x3250 x3500) (d_C_leqFunc x1003 x2 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqFunc z x2 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqFunc x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_showFuncDeclAsCurry :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDeclAsCurry x1 x3250 x3500 = d_C_showCurryFuncDecl (Curry_FlatCurry.d_C_showQNameInModule (d_C_funcModule x1 x3250 x3500)) (Curry_FlatCurry.d_C_showQNameInModule (d_C_funcModule x1 x3250 x3500)) Curry_Prelude.C_False x1 x3250 x3500

d_C_showFuncDeclAsFlatCurry :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showFuncDeclAsFlatCurry x1 x3250 x3500 = d_C_showCurryFuncDecl (Curry_FlatCurry.d_C_showQNameInModule (d_C_funcModule x1 x3250 x3500)) (Curry_FlatCurry.d_C_showQNameInModule (d_C_funcModule x1 x3250 x3500)) Curry_Prelude.C_True x1 x3250 x3500

d_C_funcModule :: Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_funcModule x1 x3250 x3500 = Curry_Prelude.d_C_fst (Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_funcName x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP__case_2 :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_2 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_1 x9 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x1002 x3250 x3500) (d_OP__case_2 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_1 x9 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x10 x11 x12 x13 x14) -> d_OP__case_0 x9 x10 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x9 x1002 x3250 x3500) (d_OP__case_1 x9 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x9 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_0 x9 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x15 x16) -> Curry_Prelude.d_OP_lt_eq x9 x16 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x9 x1002 x3250 x3500) (d_OP__case_0 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> x13
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_4 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3250 x3500) (d_OP__case_4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_FlatCurry.C_OpDecl -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_7 x2 x5 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_6 x7 x5 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x5 x1002 x3250 x3500) (d_OP__case_7 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_OpDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_6 x7 x5 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Op x8 x9 x10) -> d_OP__case_5 x7 x10 x5 x8 x3250 x3500
     (Curry_FlatCurry.Choice_C_OpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x7 x5 x1002 x3250 x3500) (d_OP__case_6 x7 x5 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_OpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x7 x5 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_OpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_OpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_5 x7 x10 x5 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_gt x5 x10 x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 x10 x3250 x3500) (Curry_Prelude.d_OP_lt_eq x7 x12 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x7 x10 x5 x1002 x3250 x3500) (d_OP__case_5 x7 x10 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x7 x10 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x7 x10 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_FlatCurry.C_BranchExpr
d_OP__case_8 x6 x3 x2 x1 x5 x3250 x3500 = case x5 of
     (Curry_FlatCurry.C_Pattern x7 x8) -> Curry_FlatCurry.C_Branch (Curry_FlatCurry.C_Pattern x7 (Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus) x3) x8 x3250 x3500)) (d_C_substituteAll x1 x2 x3 x6 x3250 x3500)
     (Curry_FlatCurry.C_LPattern x9) -> Curry_FlatCurry.C_Branch (Curry_FlatCurry.C_LPattern x9) (d_C_substituteAll x1 x2 x3 x6 x3250 x3500)
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x6 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_8 x6 x3 x2 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x6 x3 x2 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x6 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_10 x4 x5 x6 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_9 x4 x5 x8 x6 x1 x7 (Curry_Prelude.d_OP_eq_eq x5 x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x4 x5 x6 x1 x1002 x3250 x3500) (d_OP__case_10 x4 x5 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x4 x5 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x4 x5 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_9 x4 x5 x8 x6 x1 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> x7
     Curry_Prelude.C_False -> d_OP_substituteAll_dot_replaceVar_dot_151 x1 x6 x8 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x4 x5 x8 x6 x1 x7 x1002 x3250 x3500) (d_OP__case_9 x4 x5 x8 x6 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x4 x5 x8 x6 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x4 x5 x8 x6 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP__case_11 x4 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_FlatCurry.C_Var (Curry_Prelude.d_OP_plus x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x1 x1002 x3250 x3500) (d_OP__case_11 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_OP__case_13 x5 x2 x1 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Branch x6 x7) -> d_OP__case_12 x5 x2 x1 x7 x6 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x5 x2 x1 x1002 x3250 x3500) (d_OP__case_13 x5 x2 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x5 x2 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x5 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_OP__case_12 x5 x2 x1 x7 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Pattern x8 x9) -> Curry_Prelude.d_OP_plus_plus (d_C_rule2equations (d_C_substitute (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (d_C_shallowPattern2Expr x8 x9 x3250 x3500) Curry_Prelude.OP_List) x1 x3250 x3500) x7 x3250 x3500) (d_C_caseIntoLhs x1 x2 x5 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_LPattern x10) -> Curry_Prelude.d_OP_plus_plus (d_C_rule2equations (d_C_substitute (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Lit x10) Curry_Prelude.OP_List) x1 x3250 x3500) x7 x3250 x3500) (d_C_caseIntoLhs x1 x2 x5 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x5 x2 x1 x7 x1002 x3250 x3500) (d_OP__case_12 x5 x2 x1 x7 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x5 x2 x1 x7 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x5 x2 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_CaseType -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_OP__case_15 x2 x1 x4 x5 x3 x3250 x3500 = case x3 of
     Curry_FlatCurry.C_Flex -> d_OP__case_14 x2 x1 x5 x4 x3250 x3500
     Curry_FlatCurry.C_Rigid -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_CaseType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x2 x1 x4 x5 x1002 x3250 x3500) (d_OP__case_15 x2 x1 x4 x5 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_CaseType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x2 x1 x4 x5 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_CaseType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x2 x1 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_CaseType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_FlatCurry.C_Expr -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Expr Curry_FlatCurry.C_Expr)
d_OP__case_14 x2 x1 x5 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Var x6) -> d_C_caseIntoLhs x1 x6 x5 x3250 x3500
     (Curry_FlatCurry.C_Lit x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Comb x8 x9 x10) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Let x11 x12) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Free x13 x14) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Or x15 x16) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Case x17 x18 x19) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Typed x20 x21) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) Curry_Prelude.OP_List
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x2 x1 x5 x1002 x3250 x3500) (d_OP__case_14 x2 x1 x5 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x2 x1 x5 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x2 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_18 x1 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_17 x1 x6 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x1002 x3250 x3500) (d_OP__case_18 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_18 x1 x5 x3000 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x1 x6 x7 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x1002 x3000 x3250 x3500) (nd_OP__case_18 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_17 x1 x6 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_16 x8 x1 x6 x9 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x6 x1002 x3250 x3500) (d_OP__case_17 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_17 x1 x6 x7 x3000 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x8 x1 x6 x9 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_17 x1 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_FlatCurry.C_Expr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_16 x8 x1 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 2#) x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 4#) x8 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x8 x1 x6 x1002 x3250 x3500) (d_OP__case_16 x8 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x8 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x8 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_FlatCurry.C_Expr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_16 x8 x1 x6 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.OP_List -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 2#) x6 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 4#) x8 x2001 x3250 x3500) x3250 x3500) x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x8 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_16 x8 x1 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x8 x1 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x8 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_FlatCurry.C_Expr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_19 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (d_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 2#) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x2 x1 x1002 x3250 x3500) (d_OP__case_19 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_FlatCurry.C_Expr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_19 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (nd_OP_showCurryCRHS_dot_showCurryCondRule_dot_119 x1 x2 x2000 x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 2#) x2 x2000 x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_19 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_20 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map Curry_FlatCurryShow.d_C_showCurryVar x5 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x5 x1002 x3250 x3500) (d_OP__case_20 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_22 x2 x4 x5 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryVar (Curry_Prelude.d_C_head x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryVar (Curry_Prelude.d_OP_bang_bang x4 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_21 x5 x1 x4 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x2 x4 x5 x1 x1002 x3250 x3500) (d_OP__case_22 x2 x4 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x2 x4 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x2 x4 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_22 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_22 x2 x4 x5 x1 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryVar (Curry_Prelude.d_C_head x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryVar (Curry_Prelude.d_OP_bang_bang x4 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x5 x2001 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x5 x1 x4 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x2 x4 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_22 x2 x4 x5 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x2 x4 x5 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x2 x4 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_FlatCurry.C_Expr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_21 x5 x1 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map Curry_FlatCurryShow.d_C_showCurryVar x4 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x5 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x5 x1 x4 x2 x1002 x3250 x3500) (d_OP__case_21 x5 x1 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x5 x1 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x5 x1 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_21 :: Curry_FlatCurry.C_Expr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_21 x5 x1 x4 x2 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (let
                    x2001 = leftSupply x2003
                    x2002 = rightSupply x2003
                     in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.nd_C_map (wrapDX id Curry_FlatCurryShow.d_C_showCurryVar) x4 x2001 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryExpr x1 Curry_Prelude.C_False (Curry_Prelude.C_Int 0#) x5 x2002 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x5 x1 x4 x2 x1002 x3000 x3250 x3500) (nd_OP__case_21 x5 x1 x4 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x5 x1 x4 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x5 x1 x4 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_23 x7 x6 x3 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_showCurryRuleAsCase x1 x3 (Curry_FlatCurry.C_Rule x6 x7) x3250 x3500
     Curry_Prelude.C_False -> d_C_showCurryRuleAsPatterns x1 x3 (Curry_FlatCurry.C_Rule x6 x7) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x7 x6 x3 x1 x1002 x3250 x3500) (d_OP__case_23 x7 x6 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x7 x6 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x7 x6 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_23 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_23 x7 x6 x3 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showCurryRuleAsCase x1 x3 (Curry_FlatCurry.C_Rule x6 x7) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_showCurryRuleAsPatterns x1 x3 (Curry_FlatCurry.C_Rule x6 x7) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x7 x6 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_23 x7 x6 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x7 x6 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x7 x6 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_24 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x7 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x7 x1002 x3250 x3500) (d_OP__case_24 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_25 x4 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_snd x2 x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x4 x2 x1002 x3250 x3500) (d_OP__case_25 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_26 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_snd x7 x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x8 x7 x1002 x3250 x3500) (d_OP__case_26 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_28 x3 x5 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_snd x2 x3250 x3500) (let
          x6 = d_OP_showTypeExports_dot_expcons_dot_43 x5 x3250 x3500
           in (d_OP__case_27 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500)) x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x3 x5 x2 x1002 x3250 x3500) (d_OP__case_28 x3 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x3 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x3 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_27 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> x6
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x6 x1002 x3250 x3500) (d_OP__case_27 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_29 x4 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_snd x2 x3250 x3500) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x4 x2 x1002 x3250 x3500) (d_OP__case_29 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_30 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x7 x1002 x3250 x3500) (d_OP__case_30 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_TypeExpr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_33 x6 x2 x7 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> d_OP__case_32 x6 x2 x10 x7 x1 (Curry_Prelude.d_OP_eq_eq x6 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x6 x2 x7 x1 x1002 x3250 x3500) (d_OP__case_33 x6 x2 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x6 x2 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x6 x2 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_33 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Bool -> Curry_FlatCurry.C_TypeExpr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_33 x6 x2 x7 x1 x4 x3000 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_32 x6 x2 x10 x7 x1 (Curry_Prelude.d_OP_eq_eq x6 Curry_FlatCurry.C_Public x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x6 x2 x7 x1 x1002 x3000 x3250 x3500) (nd_OP__case_33 x6 x2 x7 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 x6 x2 x7 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x6 x2 x7 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_TypeExpr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_32 x6 x2 x10 x7 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId x10 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_False x7 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_OP__case_31 x10 x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x6 x2 x10 x7 x1 x1002 x3250 x3500) (d_OP__case_32 x6 x2 x10 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x6 x2 x10 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x6 x2 x10 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_32 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_TypeExpr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_32 x6 x2 x10 x7 x1 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId x10 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_False x7 x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) (d_OP__case_31 x10 x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x6 x2 x10 x7 x1 x1002 x3000 x3250 x3500) (nd_OP__case_32 x6 x2 x10 x7 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x6 x2 x10 x7 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x6 x2 x10 x7 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_31 x10 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryId x10 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x10 x1002 x3250 x3500) (d_OP__case_31 x10 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x10 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x10 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_34 x6 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_showExportConsDecl_dot___hash_lambda4 x1) x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x6 x1 x1002 x3250 x3500) (d_OP__case_34 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_34 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_34 x6 x1 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus x8 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapNX id (nd_OP_showExportConsDecl_dot___hash_lambda4 x1)) x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x6 x1 x1002 x3000 x3250 x3500) (nd_OP__case_34 x6 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x6 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeExpr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_36 x11 x13 x1 x12 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> d_OP__case_35 x11 x13 x1 x12 x15 (Curry_Prelude.d_OP_eq_eq x11 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x11 x13 x1 x12 x1002 x3250 x3500) (d_OP__case_36 x11 x13 x1 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x11 x13 x1 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x11 x13 x1 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_36 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeExpr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_36 x11 x13 x1 x12 x10 x3000 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_35 x11 x13 x1 x12 x15 (Curry_Prelude.d_OP_eq_eq x11 Curry_FlatCurry.C_Public x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_36 x11 x13 x1 x12 x1002 x3000 x3250 x3500) (nd_OP__case_36 x11 x13 x1 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_36 x11 x13 x1 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_36 x11 x13 x1 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeExpr -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_35 x11 x13 x1 x12 x15 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x15 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showInterfaceType_dot___hash_lambda3 x3250 x3500) x12 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.d_C_showCurryType x1 Curry_Prelude.C_True x13 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x11 x13 x1 x12 x15 x1002 x3250 x3500) (d_OP__case_35 x11 x13 x1 x12 x15 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x11 x13 x1 x12 x15 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x11 x13 x1 x12 x15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_35 :: Curry_FlatCurry.C_Visibility -> Curry_FlatCurry.C_TypeExpr -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_35 x11 x13 x1 x12 x15 x16 x3000 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x15 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_plus_plus (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_showInterfaceType_dot___hash_lambda3) x2000 x3250 x3500) x12 x2001 x3250 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FlatCurryShow.nd_C_showCurryType x1 Curry_Prelude.C_True x13 x2003 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x11 x13 x1 x12 x15 x1002 x3000 x3250 x3500) (nd_OP__case_35 x11 x13 x1 x12 x15 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x11 x13 x1 x12 x15 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x11 x13 x1 x12 x15 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_39 x6 x1 x4 x5 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x9 = Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_C_map (d_C_showExportConsDecl x1) (Curry_Prelude.d_C_filter d_OP_showInterfaceType_dot___hash_lambda1 x6 x3250 x3500) x3250 x3500) x3250 x3500
           in (d_OP__case_38 x4 x9 x5 x8 (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x6 x1 x4 x5 x1002 x3250 x3500) (d_OP__case_39 x6 x1 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x6 x1 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x6 x1 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_39 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_OP__case_39 x6 x1 x4 x5 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2002 = x3000
           in (seq x2002 (let
               x9 = Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_showExportConsDecl x1)) (Curry_Prelude.nd_C_filter (wrapDX id d_OP_showInterfaceType_dot___hash_lambda1) x6 x2000 x3250 x3500) x2001 x3250 x3500)))) x3250 x3500
                in (d_OP__case_38 x4 x9 x5 x8 (Curry_Prelude.d_OP_eq_eq x4 Curry_FlatCurry.C_Public x3250 x3500) x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_39 x6 x1 x4 x5 x1002 x3000 x3250 x3500) (nd_OP__case_39 x6 x1 x4 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_39 x6 x1 x4 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_39 x6 x1 x4 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_FlatCurry.C_Visibility -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_38 x4 x9 x5 x8 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x8 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showInterfaceType_dot___hash_lambda2 x3250 x3500) x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_37 x9 (Curry_Prelude.d_C_null x9 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x4 x9 x5 x8 x1002 x3250 x3500) (d_OP__case_38 x4 x9 x5 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x4 x9 x5 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x4 x9 x5 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_37 x9 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x9 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x9 x1002 x3250 x3500) (d_OP__case_37 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_40 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) x3250 x3500)
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x3 x1002 x3250 x3500) (d_OP__case_40 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_FlatCurry.C_Fixity -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_41 x2 x4 x3 x3250 x3500 = case x3 of
     Curry_FlatCurry.C_InfixOp -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showOp x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_FlatCurry.C_InfixlOp -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showOp x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_FlatCurry.C_InfixrOp -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_showOp x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Fixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x2 x4 x1002 x3250 x3500) (d_OP__case_41 x2 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Fixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x2 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Fixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Fixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_42 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x1 x1002 x3250 x3500) (d_OP__case_42 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_43 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x7 x1002 x3250 x3500) (d_OP__case_43 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
