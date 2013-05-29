{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_RCFile (d_C_readRC, d_C_setRCProperty, d_C_rcValue) where

import Basics
import qualified Curry_Char
import qualified Curry_Directory
import qualified Curry_FilePath
import qualified Curry_Installation
import qualified Curry_Prelude
import qualified Curry_PropertyFile
import qualified Curry_Sort
import qualified Curry_Utils
d_C_defaultRC :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_defaultRC x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3500) (Curry_Installation.d_C_installDir x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))))) x3500

d_C_rcFileName :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_rcFileName x3500 = Curry_Utils.d_C_liftIO (Curry_Prelude.d_C_flip (Curry_FilePath.d_OP_lt_slash_gt x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '2'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))) (Curry_Directory.d_C_getHomeDirectory x3500) x3500

d_C_readRC :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_readRC x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_rcFileName x3500) d_OP_readRC_dot___hash_lambda1 x3500

d_OP_readRC_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_readRC_dot___hash_lambda1 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3500) (d_OP_readRC_dot___hash_lambda1_dot___hash_lambda2 x1) x3500

d_OP_readRC_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_readRC_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_0 x1 x2 x3500) (Curry_PropertyFile.d_C_readPropertyFile x1 x3500) x3500

d_C_rcKeys :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_rcKeys x3500 = Curry_Prelude.d_OP_dot (Curry_Sort.d_C_mergeSort (acceptCs id Curry_Prelude.d_OP_lt_eq)) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst) x3500

nd_C_rcKeys :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_rcKeys x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Sort.nd_C_mergeSort (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt_eq)))) (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst))) x2000 x3500))

d_C_updateRC :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_updateRC x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_rcFileName x3500) d_OP_updateRC_dot___hash_lambda3 x3500

d_OP_updateRC_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateRC_dot___hash_lambda3 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_PropertyFile.d_C_readPropertyFile x1 x3500) (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4 x1) x3500

d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_PropertyFile.d_C_readPropertyFile (d_C_defaultRC x3500) x3500) (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2) x3500

d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_unless (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (d_C_rcKeys x3500) x2 x3500) (Curry_Prelude.d_C_apply (d_C_rcKeys x3500) x3 x3500) x3500)) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3500) x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Directory.d_C_renameFile x1) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_dot_gt x3500) x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) Curry_Prelude.OP_List))) x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Directory.d_C_copyFile (d_C_defaultRC x3500) x1 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2) x3500) x3 x3500) x3500) x3500) x3500) x3500

d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_done x3500) (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x4 x1 x5) (Curry_Prelude.d_C_lookup x4 x2 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1002 x3500) (d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updateRC_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_unless (Curry_Prelude.d_OP_eq_eq x4 x3 x3500)) (Curry_PropertyFile.d_C_updatePropertyFile x2 x1 x4 x3500) x3500

d_C_setRCProperty :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setRCProperty x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_readRC x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_rcFileName x3500) (d_OP_setRCProperty_dot___hash_lambda8 x1 x2) x3500) x3500

d_OP_setRCProperty_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_setRCProperty_dot___hash_lambda8 x1 x2 x3 x3500 = Curry_PropertyFile.d_C_updatePropertyFile x3 x1 x2 x3500

d_C_rcValue :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_rcValue x1 x2 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_strip x3500) (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_C_maybe Curry_Prelude.OP_List Curry_Prelude.d_C_id) (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_map Curry_Char.d_C_toLower x2 x3500) (Curry_Prelude.d_C_map (Curry_Utils.d_C_mapFst (Curry_Prelude.d_C_map Curry_Char.d_C_toLower)) x1 x3500) x3500) x3500) x3500

d_OP__case_0 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_updateRC x3500
     Curry_Prelude.C_False -> Curry_Directory.d_C_copyFile (d_C_defaultRC x3500) x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> d_C_updateRC x3500
     Curry_Prelude.C_False -> Curry_Directory.d_C_copyFile (d_C_defaultRC x3500) x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
