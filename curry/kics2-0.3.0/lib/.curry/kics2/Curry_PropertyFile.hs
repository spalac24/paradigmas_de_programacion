{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_PropertyFile (d_C_readPropertyFile, d_C_updatePropertyFile) where

import Basics
import qualified Curry_Char
import qualified Curry_Directory
import qualified Curry_IOExts
import qualified Curry_Prelude
d_C_readPropertyFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_readPropertyFile x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_readPropertyFile_dot___hash_lambda2 x1) x3250 x3500

d_OP_readPropertyFile_dot_splitEqs_dot_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_readPropertyFile_dot_splitEqs_dot_2 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readPropertyFile_dot_splitEqs_dot_2 x1002 x3250 x3500) (d_OP_readPropertyFile_dot_splitEqs_dot_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readPropertyFile_dot_splitEqs_dot_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readPropertyFile_dot_splitEqs_dot_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readPropertyFile_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_readPropertyFile_dot___hash_lambda2 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_readCompleteFile x1 x3250 x3500) d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readPropertyFile_dot___hash_lambda2 x1 x1002 x3250 x3500) (d_OP_readPropertyFile_dot___hash_lambda2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readPropertyFile_dot___hash_lambda2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readPropertyFile_dot___hash_lambda2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot d_OP_readPropertyFile_dot_splitEqs_dot_2 (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_filter d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4) Curry_Prelude.d_C_lines x3250 x3500) x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readPropertyFile_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x1 x3250 x3500) x3250 x3500) (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x1 x3250 x3500) x3250 x3500) x3250 x3500

d_C_updatePropertyFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_updatePropertyFile x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_readPropertyFile x1 x3250 x3500) (d_OP_updatePropertyFile_dot___hash_lambda5 x1 x2 x3) x3250 x3500

d_OP_updatePropertyFile_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_updatePropertyFile_dot___hash_lambda5 x1 x2 x3 x4 x3250 x3500 = d_OP__case_2 x4 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_lookup x2 x4 x3250 x3500) Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500

d_C_changePropertyInFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_changePropertyInFile x1 x2 x3 x3250 x3500 = Curry_IOExts.d_C_updateFile (d_OP_changePropertyInFile_dot___hash_lambda6 x2 x3) x1 x3250 x3500

d_OP_changePropertyInFile_dot_changeProp_dot_20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_changePropertyInFile_dot_changeProp_dot_20 x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '='#)) x3250 x3500) x3 x3250 x3500
     x5 = d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 x4 x3250 x3500
     x6 = d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 x4 x3250 x3500
      in (d_OP__case_1 x6 x3 x1 x5 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x3 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_not (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 x1002 x3250 x3500) (d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP2_hash_s1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 x1002 x3250 x3500) (d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_changePropertyInFile_dot_changeProp_dot_20_dot___hash_selFP3_hash_s2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_changePropertyInFile_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_changePropertyInFile_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map (d_OP_changePropertyInFile_dot_changeProp_dot_20 x1 x2)) Curry_Prelude.d_C_lines x3250 x3500) x3250 x3500) x3 x3250 x3500

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x6 x3 x1 x5 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_OP__case_0 x1 x5 x3 x2 (Curry_Prelude.d_OP_eq_eq x5 x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x6 x3 x1 x5 x2 x1002 x3250 x3500) (d_OP__case_1 x6 x3 x1 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x6 x3 x1 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x6 x3 x1 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x1 x5 x3 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x5 x3 x2 x1002 x3250 x3500) (d_OP__case_0 x1 x5 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x5 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x5 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_2 x4 x2 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_appendFile x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_changePropertyInFile x1 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_2 x4 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x2 x3 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_3 x3 x4 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x3 x1002 x3250 x3500) (d_OP__case_4 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x3 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x7) (d_OP_readPropertyFile_dot_splitEqs_dot_2 x3 x3250 x3500)
     Curry_Prelude.OP_List -> d_OP_readPropertyFile_dot_splitEqs_dot_2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x4 x1002 x3250 x3500) (d_OP__case_3 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
