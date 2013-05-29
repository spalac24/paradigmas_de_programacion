{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AddTypes (nd_C_main, nd_C_addTypeSignatures) where

import Basics
import qualified Curry_AbstractCurry
import qualified Curry_AbstractCurryPrinter
import qualified Curry_AllSolutions
import qualified Curry_CurryStringClassifier
import qualified Curry_FileGoodies
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_System
nd_C_main :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_main x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getArgs x3500) (wrapNX id nd_OP_main_dot___hash_lambda1) x2000 x3500))

nd_OP_main_dot___hash_lambda1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_main_dot___hash_lambda1 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_35 x1 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_length x1 x3500) (Curry_Prelude.C_Int 1#) x3500) x2000 x3500))

nd_C_writeWithTypeSignatures :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_writeWithTypeSignatures x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x3500) x3500) x3500) (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_addTypeSignatures x1 x2000 x3500) (wrapDX id (d_OP_writeWithTypeSignatures_dot___hash_lambda2 x1)) x2001 x3500)))) x3500))

d_OP_writeWithTypeSignatures_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_writeWithTypeSignatures_dot___hash_lambda2 x1 x2 x3500 = Curry_Prelude.d_C_writeFile (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) x2 x3500

nd_C_addTypeSignatures :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_addTypeSignatures x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AbstractCurry.d_C_readCurry x1 x3500) (wrapNX id (nd_OP_addTypeSignatures_dot___hash_lambda3 x1)) x2000 x3500))

nd_OP_addTypeSignatures_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_addTypeSignatures_dot___hash_lambda3 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AbstractCurry.d_C_readUntypedCurry x1 x3500) (wrapNX id (nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4 x1 x2)) x2000 x3500))

nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Curry_AbstractCurry.C_CurryProg -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) x3500) (wrapNX id (nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3)) x2000 x3500))

nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AllSolutions.nd_C_getOneSolution (wrapNX id (nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x4 x2 x3)) x2000 x3500) (wrapDX id (d_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 x1)) x2001 x3500)))))

nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_Success
nd_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_OP_eq_colon_eq x4 (Curry_CurryStringClassifier.d_C_unscan (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_addTypes (Curry_CurryStringClassifier.nd_C_scan x1 x2000 x3500) (d_C_getTypes x2 x3 x3500) x2001 x3500)))) x3500) x3500))

d_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_addTypeSignatures_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda7 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x3500) x3500) x3500) x3500) x3500) (Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))) x3500) Curry_Prelude.d_C_return x2 x3500) x3500

d_C_getTypes :: Curry_AbstractCurry.C_CurryProg -> Curry_AbstractCurry.C_CurryProg -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr)
d_C_getTypes x1 x2 x3500 = case x1 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> d_OP__case_34 x6 x2 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getTypes x1002 x2 x3500) (d_C_getTypes x1003 x2 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getTypes z x2 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getTypes x1002 x2) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getTypes_dot_getTypesFuncDecls_dot_26 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr)
d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_33 x2 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_32 x2 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x1002 x2 x3500) (d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getTypes_dot_getTypesFuncDecls_dot_26 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_addTypes :: Curry_Prelude.OP_List Curry_CurryStringClassifier.C_Token -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_CurryStringClassifier.C_Token
nd_C_addTypes x1 x2 x3000 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_27 x2 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_addTypes x1002 x2 x3000 x3500) (nd_C_addTypes x1003 x2 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_addTypes z x2 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_addTypes x1002 x2 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_addTypes_dot___hash_selFP1_hash_lastline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_addTypes_dot___hash_selFP1_hash_lastline x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addTypes_dot___hash_selFP1_hash_lastline x1002 x3500) (d_OP_addTypes_dot___hash_selFP1_hash_lastline x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addTypes_dot___hash_selFP1_hash_lastline z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addTypes_dot___hash_selFP1_hash_lastline x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_addTypes_dot___hash_selFP2_hash_newline :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_addTypes_dot___hash_selFP2_hash_newline x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addTypes_dot___hash_selFP2_hash_newline x1002 x3500) (d_OP_addTypes_dot___hash_selFP2_hash_newline x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addTypes_dot___hash_selFP2_hash_newline z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addTypes_dot___hash_selFP2_hash_newline x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_addTypesCode :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_AbstractCurry.C_CTypeExpr) -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_addTypesCode x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_25 x1 x2 x2000 x3500))
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x1 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_addTypesCode x1 x2 x1002 x3000 x3500) (nd_C_addTypesCode x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_addTypesCode x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_addTypesCode x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_addTypesCode_dot___hash_selFP6_hash_line :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_addTypesCode_dot___hash_selFP6_hash_line x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addTypesCode_dot___hash_selFP6_hash_line x1002 x3500) (d_OP_addTypesCode_dot___hash_selFP6_hash_line x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addTypesCode_dot___hash_selFP6_hash_line z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addTypesCode_dot___hash_selFP6_hash_line x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_addTypesCode_dot___hash_selFP7_hash_remainder :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_addTypesCode_dot___hash_selFP7_hash_remainder x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addTypesCode_dot___hash_selFP7_hash_remainder x1002 x3500) (d_OP_addTypesCode_dot___hash_selFP7_hash_remainder x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addTypesCode_dot___hash_selFP7_hash_remainder z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addTypesCode_dot___hash_selFP7_hash_remainder x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_addTypesCode_dot___hash_selFP5_hash_lhs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_addTypesCode_dot___hash_selFP5_hash_lhs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addTypesCode_dot___hash_selFP5_hash_lhs x1002 x3500) (d_OP_addTypesCode_dot___hash_selFP5_hash_lhs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addTypesCode_dot___hash_selFP5_hash_lhs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addTypesCode_dot___hash_selFP5_hash_lhs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_toTVar :: Curry_Prelude.C_Int -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr
d_C_toTVar x1 x3500 = d_OP__case_17 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 26#) x3500) x3500

d_C_isUntyped :: Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_C_isUntyped x1 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTCons x2 x3) -> d_OP__case_15 x3 x2 x3500
     (Curry_AbstractCurry.C_CTVar x8) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x9 x10) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x11 x12) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isUntyped x1002 x3500) (d_C_isUntyped x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isUntyped z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isUntyped x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_normalize :: Curry_AbstractCurry.C_CTypeExpr -> IDSupply -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr
nd_C_normalize x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x2 = generate x2001
                in (nd_OP___cond_0_normalize x2 (d_C_varNames (Curry_Prelude.C_Int 0#) (d_C_tvars x1 x2 x3500) x3500) x2000 x3500))))))

d_OP___cond_0_normalize x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_normalize x1 x1002 x3500) (d_OP___cond_0_normalize x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_normalize x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_normalize x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_normalize x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_normalize x1 x1002 x3000 x3500) (nd_OP___cond_0_normalize x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_normalize x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_normalize x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tvars :: Curry_AbstractCurry.C_CTypeExpr -> Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_AbstractCurry.C_CTypeExpr)
d_C_tvars x1 x2 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x3) -> d_OP__case_13 x2 x3 x3500
     (Curry_AbstractCurry.C_CTCons x6 x7) -> d_OP__case_12 x6 x7 x2 x3500
     (Curry_AbstractCurry.C_CFuncType x10 x11) -> d_OP__case_11 x10 x11 x2 x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tvars x1002 x2 x3500) (d_C_tvars x1003 x2 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tvars z x2 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tvars x1002 x2) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_varNames :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 Curry_AbstractCurry.C_CTypeExpr) -> ConstStore -> Curry_Prelude.C_Success
d_C_varNames x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_success x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_10 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_varNames x1 x1002 x3500) (d_C_varNames x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_varNames x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_varNames x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_varNames_dot___hash_lambda10 :: Curry_Prelude.Curry t107 => t107 -> Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_OP_varNames_dot___hash_lambda10 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_eq_eq x1 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_varNames_dot___hash_lambda10 x1 x1002 x3500) (d_OP_varNames_dot___hash_lambda10 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_varNames_dot___hash_lambda10 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_varNames_dot___hash_lambda10 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_varNames_dot___hash_selFP9_hash_is :: Curry_Prelude.Curry t107 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)
d_OP_varNames_dot___hash_selFP9_hash_is x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_varNames_dot___hash_selFP9_hash_is x1002 x3500) (d_OP_varNames_dot___hash_selFP9_hash_is x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_varNames_dot___hash_selFP9_hash_is z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_varNames_dot___hash_selFP9_hash_is x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_varNames_dot___hash_selFP10_hash_others :: Curry_Prelude.Curry t107 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t107 Curry_AbstractCurry.C_CTypeExpr)
d_OP_varNames_dot___hash_selFP10_hash_others x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_varNames_dot___hash_selFP10_hash_others x1002 x3500) (d_OP_varNames_dot___hash_selFP10_hash_others x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_varNames_dot___hash_selFP10_hash_others z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_varNames_dot___hash_selFP10_hash_others x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_varNames_dot_giveName_dot_102 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Success
d_OP_varNames_dot_giveName_dot_102 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_success x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_ampersand (Curry_Prelude.d_OP_eq_colon_eq x1 x3 x3500) (d_OP_varNames_dot_giveName_dot_102 x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_varNames_dot_giveName_dot_102 x1 x1002 x3500) (d_OP_varNames_dot_giveName_dot_102 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_varNames_dot_giveName_dot_102 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_varNames_dot_giveName_dot_102 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dualMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> t2) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_List t2
d_C_dualMap x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_7 x3 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_6 x1 x4 x5 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_dualMap x1 x1002 x3 x3500) (d_C_dualMap x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_dualMap x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_dualMap x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_dualMap :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 (Func t1 t2) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t2
nd_C_dualMap x1 x2 x3 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x3 x2000 x3500))
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x4 x5 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_dualMap x1 x1002 x3 x3000 x3500) (nd_C_dualMap x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_dualMap x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_dualMap x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_defines :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_defines x1 x2 x3500 = let
     x3 = d_C_symbols x2 x3500
      in (d_OP__case_5 x1 x2 x3 (Curry_Prelude.d_C_null x3 x3500) x3500)

d_C_delimiters :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_delimiters x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))))))

d_C_infixIDs :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_infixIDs x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))

d_C_symbols :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_symbols x1 x3500 = d_OP_symbols_dot_syms_dot_126 Curry_Prelude.OP_List x1 x3500

d_OP_symbols_dot_maybeSym_dot_126 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP_symbols_dot_maybeSym_dot_126 x1 x3500 = d_OP__case_2 x1 (Curry_Prelude.d_C_null x1 x3500) x3500

d_OP_symbols_dot_syms_dot_126 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_symbols_dot_syms_dot_126 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP_symbols_dot_maybeSym_dot_126 x1 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_1 x1 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x3 x3500) (d_C_delimiters x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_symbols_dot_syms_dot_126 x1 x1002 x3500) (d_OP_symbols_dot_syms_dot_126 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_symbols_dot_syms_dot_126 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_symbols_dot_syms_dot_126 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (d_OP_symbols_dot_maybeSym_dot_126 x1 x3500) (d_OP_symbols_dot_syms_dot_126 Curry_Prelude.OP_List (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_delimiters x3500)) x4 x3500) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_0 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x1002 x3500) (d_OP__case_1 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus (d_OP_symbols_dot_maybeSym_dot_126 x1 x3500) (d_OP_symbols_dot_syms_dot_126 Curry_Prelude.OP_List (Curry_Prelude.nd_C_dropWhile (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (d_C_delimiters x3500))) x4 x2000 x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_1 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP_symbols_dot_syms_dot_126 (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3500) x4 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x3 x4 x1002 x3500) (d_OP__case_0 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP_symbols_dot_syms_dot_126 (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3500) x4 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> d_OP__case_4 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x2 x3500) (Curry_Prelude.C_Char ' '#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x3 x1002 x3500) (d_OP__case_5 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x2 x3500) (Curry_Prelude.C_Char ' '#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> d_OP__case_3 x1 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x1002 x3500) (d_OP__case_4 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) x3 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x3 x1002 x3500) (d_OP__case_3 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_elem x1 x2000 x3500) x3 x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x3 x1002 x3000 x3500) (nd_OP__case_3 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x4 x5 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x6 x3500) (d_C_dualMap x1 x5 x7 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x4 x5 x1002 x3500) (d_OP__case_6 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x4 x5 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.OP_Cons (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x6 x2001 x3500)))) (nd_C_dualMap x1 x5 x7 x2003 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_6 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3500) (d_OP__case_7 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1002 x3000 x3500) (nd_OP__case_7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x4 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x7 = Curry_List.d_C_partition (d_OP_varNames_dot___hash_lambda10 x5) x4 x3500
          x8 = d_OP_varNames_dot___hash_selFP9_hash_is x7 x3500
          x9 = d_OP_varNames_dot___hash_selFP10_hash_others x7 x3500
           in (d_OP__case_9 x1 x6 x8 x9 (Curry_Prelude.d_C_null x8 x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x4 x1002 x3500) (d_OP__case_10 x1 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x4 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x7 = Curry_List.nd_C_partition (wrapDX id (d_OP_varNames_dot___hash_lambda10 x5)) x4 x2000 x3500
                    x8 = d_OP_varNames_dot___hash_selFP9_hash_is x7 x3500
                    x9 = d_OP_varNames_dot___hash_selFP10_hash_others x7 x3500
                     in (nd_OP__case_9 x1 x6 x8 x9 (Curry_Prelude.d_C_null x8 x3500) x2001 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x4 x1002 x3000 x3500) (nd_OP__case_10 x1 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x6 (Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 0#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))) x3500) (d_C_varNames x1 x9 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_8 x1 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x6 x8 x9 x1002 x3500) (d_OP__case_9 x1 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x6 (Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Int 0#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List))) x3500) (d_C_varNames x1 x9 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x6 x8 x9 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_9 x1 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_gt (d_OP_varNames_dot_giveName_dot_102 (d_C_toTVar x1 x3500) (Curry_Prelude.OP_Cons x6 (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x8 x3500)) x3500) (d_C_varNames (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x6 x8 x9 x1002 x3500) (d_OP__case_8 x1 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_ampersand_gt (d_OP_varNames_dot_giveName_dot_102 (d_C_toTVar x1 x3500) (Curry_Prelude.OP_Cons x6 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_snd) x8 x2000 x3500)) x3500) (d_C_varNames (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_8 x1 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x10 x11 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CFuncType x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_tvars x10 x12 x3500) (d_C_tvars x11 x13 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x10 x11 x1002 x3500) (d_OP__case_11 x10 x11 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x10 x11 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x10 x11 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x10 x11 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CFuncType x12 x13) -> Curry_Prelude.d_OP_plus_plus (d_C_tvars x10 x12 x3500) (d_C_tvars x11 x13 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x10 x11 x1002 x3000 x3500) (nd_OP__case_11 x10 x11 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x10 x11 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x10 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x6 x7 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CTCons x8 x9) -> d_OP___cond_0__case_12 x7 x9 (Curry_Prelude.d_OP_eq_colon_eq x6 x8 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x6 x7 x1002 x3500) (d_OP__case_12 x6 x7 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x6 x7 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x6 x7 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CTCons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP___cond_0__case_12 x7 x9 (Curry_Prelude.d_OP_eq_colon_eq x6 x8 x3500) x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x6 x7 x1002 x3000 x3500) (nd_OP__case_12 x6 x7 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x6 x7 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0__case_12 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_concat (d_C_dualMap (acceptCs id d_C_tvars) x1 x2 x3500) x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_12 x1 x2 x1002 x3500) (d_OP___cond_0__case_12 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_12 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_12 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__case_12 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_Success -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_concat (nd_C_dualMap (wrapDX (wrapDX id) (acceptCs id d_C_tvars)) x1 x2 x2000 x3500) x3500))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__case_12 x1 x2 x1002 x3000 x3500) (nd_OP___cond_0__case_12 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__case_12 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__case_12 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x2) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x2 x1002 x3500) (d_OP__case_13 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x2) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x2 x1002 x3000 x3500) (nd_OP__case_13 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_14 x4 x5 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x3 x1002 x3500) (d_OP__case_15 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x4 x5 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x3 x1002 x3000 x3500) (nd_OP__case_15 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x4 x5 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3500
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x4 x5 x1002 x3500) (d_OP__case_14 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x4 x5 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3500
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x4 x5 x1002 x3000 x3500) (nd_OP__case_14 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3500) x3500) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> d_OP__case_16 x1 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x1002 x3500) (d_OP__case_17 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x1 x3500) x3500) Curry_Prelude.OP_List))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x1002 x3000 x3500) (nd_OP__case_17 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 26#) x3500) x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x1002 x3500) (d_OP__case_16 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_AbstractCurry.C_CTVar (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 26#) x3500) x3500) x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x1002 x3000 x3500) (nd_OP__case_16 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_24 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2012 = x3000
           in (seq x2012 (let
               x2013 = leftSupply x2012
               x2014 = rightSupply x2012
                in (seq x2013 (seq x2014 (let
                    x2002 = leftSupply x2013
                    x2005 = rightSupply x2013
                     in (seq x2002 (seq x2005 (let
                         x2010 = leftSupply x2014
                         x2011 = rightSupply x2014
                          in (seq x2010 (seq x2011 (let
                              x8 = let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '\n'#))) x2000 x3500) x1 x2001 x3500)))
                              x9 = d_OP_addTypesCode_dot___hash_selFP6_hash_line x8 x3500
                              x10 = d_OP_addTypesCode_dot___hash_selFP7_hash_remainder x8 x3500
                              x11 = let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '='#))) x2003 x3500) x9 x2004 x3500)))
                              x12 = d_OP_addTypesCode_dot___hash_selFP5_hash_lhs x11 x3500
                              x13 = let
                                   x2009 = leftSupply x2010
                                   x2008 = rightSupply x2010
                                    in (seq x2009 (seq x2008 (nd_OP__case_18 x6 (let
                                        x2007 = leftSupply x2008
                                        x2006 = rightSupply x2008
                                         in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_all (wrapNX id (Curry_Prelude.nd_C_flip (wrapNX id Curry_Prelude.nd_C_elem) (d_C_infixIDs x3500))) x2006 x3500) x6 x2007 x3500)))) x2009 x3500)))
                               in (nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 (Curry_Prelude.d_C_null x1 x3500) x2011 x3500))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_24 x1 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x1 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> x6
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x6 x1002 x3500) (d_OP__case_18 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> x6
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x6 x1002 x3000 x3500) (nd_OP__case_18 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x3500) Curry_Prelude.OP_List x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000 x3500) (nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 x12 x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000 x3500) (nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 x12 x3000 x3500 = case x12 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_head x10 x3500) (nd_C_addTypesCode (Curry_Prelude.d_C_tail x10 x3500) x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x2000 x3500)))
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (let
               x16 = x14
                in (nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char ' '#) x3500) x2000 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 x1002 x3000 x3500) (nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x5 x6 x7 x9 x10 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 x17 x3000 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x9 (nd_C_addTypesCode x10 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 (d_C_defines x6 x12 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 x1002 x3000 x3500) (nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x2 x5 x6 x7 x9 x10 x12 x13 x16 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 x18 x3000 x3500 = case x18 of
     Curry_Prelude.C_True -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x17 = Curry_Prelude.d_OP_plus_plus (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_AbstractCurryPrinter.d_C_showTypeExpr Curry_Prelude.C_False)) (nd_C_normalize x7 x2000 x3500) x2001 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3500
                     in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_plus_plus x13 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x17 x3500) x3500) (Curry_Prelude.d_OP_plus_plus x9 (nd_C_addTypesCode x10 x2 x5 x2003 x3500) x3500) x3500))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_plus_plus x9 (nd_C_addTypesCode x10 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x2000 x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000 x3500) (nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x2 x5 x6 x7 x9 x10 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_25 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x1 x1002 x3500) (d_OP__case_25 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_25 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x1 x1002 x3000 x3500) (nd_OP__case_25 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_27 x2 x4 x3 x3000 x3500 = case x3 of
     (Curry_CurryStringClassifier.C_ModuleHead x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_ModuleHead x5) (nd_C_addTypes x4 x2 x2000 x3500)))
     (Curry_CurryStringClassifier.C_SmallComment x6) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_SmallComment x6) (nd_C_addTypes x4 x2 x2000 x3500)))
     (Curry_CurryStringClassifier.C_BigComment x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_BigComment x7) (nd_C_addTypes x4 x2 x2000 x3500)))
     (Curry_CurryStringClassifier.C_Text x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_Text x8) (nd_C_addTypes x4 x2 x2000 x3500)))
     (Curry_CurryStringClassifier.C_Letter x9) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_Letter x9) (nd_C_addTypes x4 x2 x2000 x3500)))
     (Curry_CurryStringClassifier.C_Code x10) -> let
          x2011 = x3000
           in (seq x2011 (let
               x2002 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2002 (seq x2012 (let
                    x2005 = leftSupply x2012
                    x2010 = rightSupply x2012
                     in (seq x2005 (seq x2010 (let
                         x11 = d_OP_addTypes_dot___hash_selFP1_hash_lastline (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '\n'#))) x2000 x3500) x10 x2001 x3500)))) x3500
                         x12 = d_OP_addTypes_dot___hash_selFP2_hash_newline (let
                              x2004 = leftSupply x2005
                              x2003 = rightSupply x2005
                               in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_break (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.C_Char '\n'#))) x2003 x3500) x10 x2004 x3500)))) x3500
                          in (let
                              x2008 = leftSupply x2010
                              x2009 = rightSupply x2010
                               in (seq x2008 (seq x2009 (let
                                   x13 = generate x2009
                                    in (let
                                        x2006 = leftSupply x2008
                                        x2007 = rightSupply x2008
                                         in (seq x2006 (seq x2007 (let
                                             x14 = Curry_Prelude.d_OP_plus_plus x11 (nd_C_addTypesCode x12 x13 x2 x2006 x3500) x3500
                                             x15 = nd_OP__case_26 x4 x13 (Curry_Prelude.d_C_null x13 x3500) x2007 x3500
                                              in (Curry_Prelude.OP_Cons (Curry_CurryStringClassifier.C_Code x14) x15)))))))))))))))))
     (Curry_CurryStringClassifier.Choice_C_Token x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x2 x4 x1002 x3000 x3500) (nd_OP__case_27 x2 x4 x1003 x3000 x3500)
     (Curry_CurryStringClassifier.Choices_C_Token x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x2 x4 z x3000 x3500) x1002
     (Curry_CurryStringClassifier.Guard_C_Token x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_CurryStringClassifier.Fail_C_Token x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_26 x4 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_addTypes x4 x13 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x4 x13 x1002 x3000 x3500) (nd_OP__case_26 x4 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x4 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x4 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_32 x2 x4 x3 x3500 = case x3 of
     (Curry_AbstractCurry.C_CFunc x5 x6 x7 x8 x9) -> d_OP__case_31 x4 x5 x8 x2 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x2 x4 x1002 x3500) (d_OP__case_32 x2 x4 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x2 x4 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_32 x2 x4 x3 x3000 x3500 = case x3 of
     (Curry_AbstractCurry.C_CFunc x5 x6 x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_31 x4 x5 x8 x2 x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_32 x2 x4 x1002 x3000 x3500) (nd_OP__case_32 x2 x4 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_32 x2 x4 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_32 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_31 x4 x5 x8 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_30 x4 x5 x8 x11 x10 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x4 x5 x8 x1002 x3500) (d_OP__case_31 x4 x5 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x4 x5 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x4 x5 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_31 x4 x5 x8 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_30 x4 x5 x8 x11 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_31 x4 x5 x8 x1002 x3000 x3500) (nd_OP__case_31 x4 x5 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_31 x4 x5 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_31 x4 x5 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_30 x4 x5 x8 x11 x10 x3500 = case x10 of
     (Curry_AbstractCurry.C_CFunc x12 x13 x14 x15 x16) -> d_OP__case_29 x4 x5 x8 x11 x15 (d_C_isUntyped x15 x3500) x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x4 x5 x8 x11 x1002 x3500) (d_OP__case_30 x4 x5 x8 x11 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x4 x5 x8 x11 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x4 x5 x8 x11 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_30 x4 x5 x8 x11 x10 x3000 x3500 = case x10 of
     (Curry_AbstractCurry.C_CFunc x12 x13 x14 x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_29 x4 x5 x8 x11 x15 (d_C_isUntyped x15 x3500) x2000 x3500))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_30 x4 x5 x8 x11 x1002 x3000 x3500) (nd_OP__case_30 x4 x5 x8 x11 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_30 x4 x5 x8 x11 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_30 x4 x5 x8 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_29 x4 x5 x8 x11 x15 x16 x3500 = case x16 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_snd x5 x3500) x8) (d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x4 x11 x3500)
     Curry_Prelude.C_False -> d_OP__case_28 x4 x11 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x4 x5 x8 x11 x15 x1002 x3500) (d_OP__case_29 x4 x5 x8 x11 x15 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x4 x5 x8 x11 x15 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x4 x5 x8 x11 x15 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_29 x4 x5 x8 x11 x15 x16 x3000 x3500 = case x16 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_snd x5 x3500) x8) (d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x4 x11 x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_28 x4 x11 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_29 x4 x5 x8 x11 x15 x1002 x3000 x3500) (nd_OP__case_29 x4 x5 x8 x11 x15 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_29 x4 x5 x8 x11 x15 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_29 x4 x5 x8 x11 x15 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_28 x4 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x4 x11 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x4 x11 x1002 x3500) (d_OP__case_28 x4 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x4 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x4 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_28 x4 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x4 x11 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x4 x11 x1002 x3000 x3500) (nd_OP__case_28 x4 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x4 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x4 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_33 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x1002 x3500) (d_OP__case_33 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_33 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_33 x1002 x3000 x3500) (nd_OP__case_33 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_33 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_33 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_34 x6 x2 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x8 x9 x10 x11 x12) -> d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x6 x11 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x6 x1002 x3500) (d_OP__case_34 x6 x1003 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x6 z x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x6 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_34 x6 x2 x3000 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x8 x9 x10 x11 x12) -> d_OP_getTypes_dot_getTypesFuncDecls_dot_26 x6 x11 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_34 x6 x1002 x3000 x3500) (nd_OP__case_34 x6 x1003 x3000 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_34 x6 z x3000 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_34 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_35 x1 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) x3500
     Curry_Prelude.C_False -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (let
                    x2 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FileGoodies.nd_C_stripSuffix x2000 x3500) (Curry_Prelude.d_C_head x1 x3500) x2001 x3500)))
                     in (let
                         x2003 = leftSupply x2005
                         x2004 = rightSupply x2005
                          in (seq x2003 (seq x2004 (Curry_Prelude.d_OP_gt_gt (nd_C_writeWithTypeSignatures x2 x2003 x3500) (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_putStrLn) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))) x3500) x3500) x3500) x2004 x3500) x3500)))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_35 x1 x1002 x3000 x3500) (nd_OP__case_35 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_35 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_35 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
