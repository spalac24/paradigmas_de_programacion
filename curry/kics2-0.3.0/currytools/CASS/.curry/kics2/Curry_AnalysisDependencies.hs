{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AnalysisDependencies (d_C_getModulesToAnalyze, nd_C_getModulesToAnalyze, d_C_reduceDependencies) where

import Basics
import qualified Curry_Analysis
import qualified Curry_Configuration
import qualified Curry_CurryFiles
import qualified Curry_Directory
import qualified Curry_List
import qualified Curry_LoadAnalysis
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_Time
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_Distribution
import qualified Curry_GenericProgInfo
d_C_debugMessage :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_debugMessage x1 x2 x3250 x3500 = Curry_Configuration.d_C_debugMessageLevel x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x2 x3250 x3500) x3250 x3500

d_C_getModulesToAnalyze :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_getModulesToAnalyze x1 x2 x3250 x3500 = let
     x3 = Curry_Analysis.d_C_analysisName x1 x3250 x3500
      in (d_OP__case_27 x1 x2 x3 (Curry_Analysis.d_C_isSimpleAnalysis x1 x3250 x3500) x3250 x3500)

nd_C_getModulesToAnalyze :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_C_getModulesToAnalyze x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x3 = Curry_Analysis.nd_C_analysisName x1 x2000 x3250 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (nd_OP__case_27 x1 x2 x3 (Curry_Analysis.nd_C_isSimpleAnalysis x1 x2001 x3250 x3500) x2002 x3250 x3500)))))))))

d_OP_getModulesToAnalyze_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List t0)))
d_OP_getModulesToAnalyze_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (d_OP__case_26 x1 x2 x3250 x3500) x3250 x3500

d_OP_getModulesToAnalyze_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getModulesToAnalyze_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getDependencyList (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) Curry_Prelude.OP_List x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3 x1 x2) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getModulesToAnalyze_dot___hash_lambda2 x1 x2 x1002 x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getModulesToAnalyze_dot___hash_lambda2 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getModulesToAnalyze_dot___hash_lambda2 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_LoadAnalysis.d_C_storeImportModuleList x2 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO Curry_CurryFiles.d_C_getSourceFileTime x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x3 x3250 x3500) x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x3) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO (d_C_getAnaFileTime x1) x3250 x3500) (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x2 x3250 x3500) x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x2 x3) x3250 x3500

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x3250 x3500 = let
     x4 = d_C_findModulesToAnalyze x1 x3 x2 (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x3250 x3500
     x5 = d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo x4 x3250 x3500
     x6 = d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate x4 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Configuration.d_C_getWithPrelude x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x5 x6) x3250 x3500)

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo x1002 x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP2_hash_modulesToDo x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate x1002 x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_selFP3_hash_modulesUpToDate x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getModulesToAnalyze_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = let
     x4 = d_OP__case_25 x3 x2 x1 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.d_C_show x4 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x4 x3250 x3500) x3250 x3500)

d_C_isAnalysisFileNewer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_isAnalysisFileNewer x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnaFileTime x1 x2 x3250 x3500) (d_OP_isAnalysisFileNewer_dot___hash_lambda8 x2) x3250 x3500

d_OP_isAnalysisFileNewer_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisFileNewer_dot___hash_lambda8 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getSourceFileTime x1 x3250 x3500) (d_OP_isAnalysisFileNewer_dot___hash_lambda8_dot___hash_lambda9 x2) x3250 x3500

d_OP_isAnalysisFileNewer_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisFileNewer_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_gt_eq (Curry_Prelude.d_C_snd x1 x3250 x3500) (Curry_Prelude.C_Just (Curry_Prelude.d_C_snd x2 x3250 x3500)) x3250 x3500) x3250 x3500

d_C_isAnalysisValid :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_isAnalysisValid x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_getImportModuleListFile x2 x3250 x3500) (Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10 x1 x2)) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x3 x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11 x1 x3 x2) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getSourceFileTime x3 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return Curry_Prelude.d_C_snd x3250 x3500) x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x4) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3 x4 x3250 x3500 = d_OP__case_7 x4 x3 x1 x2 (Curry_Prelude.d_OP_gt_eq x3 x4 x3250 x3500) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO Curry_CurryFiles.d_C_getSourceFileTime x3250 x3500) x2 x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x2) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO (d_C_getAnaFileTime x1) x3250 x3500) x2 x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14_dot___hash_lambda15 x3) x3250 x3500

d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all (Curry_Prelude.d_C_uncurry (acceptCs id Curry_Prelude.d_OP_gt_eq)) x3250 x3500) (Curry_Prelude.d_C_zip (Curry_Prelude.d_C_map Curry_Prelude.d_C_snd x2 x3250 x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) Curry_Prelude.d_C_snd x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_getDependencyList :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_C_getDependencyList x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return x2 x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x3 x3250 x3500) (d_OP_getDependencyList_dot___hash_lambda16 x3 x2 x4) x3250 x3500) (d_OP_getDependencyList_dot___hash_lambda17 x4) (d_C_lookupAndReorder x3 Curry_Prelude.OP_List x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getDependencyList x1002 x2 x3250 x3500) (d_C_getDependencyList x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getDependencyList z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getDependencyList x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getDependencyList_dot___hash_lambda16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getDependencyList_dot___hash_lambda16 x1 x2 x3 x4 x3250 x3500 = d_C_getDependencyList (d_C_addNewMods x3 x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x4) x2) x3250 x3500

d_OP_getDependencyList_dot___hash_lambda17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP_getDependencyList_dot___hash_lambda17 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_C_getDependencyList (d_C_addNewMods x1 x4 x3250 x3500) x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getDependencyList_dot___hash_lambda17 x1 x1002 x3250 x3500) (d_OP_getDependencyList_dot___hash_lambda17 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getDependencyList_dot___hash_lambda17 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getDependencyList_dot___hash_lambda17 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addNewMods :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_addNewMods x1 x2 x3250 x3500 = Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip Curry_Prelude.d_C_notElem x1) x2 x3250 x3500) x3250 x3500

d_C_lookupAndReorder :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) t1)
d_C_lookupAndReorder x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_6 x1 x5 x2 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupAndReorder x1 x2 x1002 x3250 x3500) (d_C_lookupAndReorder x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupAndReorder x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupAndReorder x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getAnaFileTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime))
d_C_getAnaFileTime x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_getAnalysisPublicFile x2 x1 x3250 x3500) (d_OP_getAnaFileTime_dot___hash_lambda18 x2) x3250 x3500

d_OP_getAnaFileTime_dot___hash_lambda18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime))
d_OP_getAnaFileTime_dot___hash_lambda18 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x2 x3250 x3500) (d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x2 x1) x3250 x3500

d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime))
d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 x2) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.C_Nothing) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x1002 x3250 x3500) (d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime))
d_OP_getAnaFileTime_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.C_Just x2)) x3250 x3500

d_C_findModulesToAnalyze :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_findModulesToAnalyze x1 x2 x3 x4 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_3 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x9 = d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod x7 x3250 x3500
          x10 = d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports x7 x3250 x3500
          x11 = d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo x4 x3250 x3500
          x12 = d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate x4 x3250 x3500
           in (d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 (Curry_Prelude.d_C_lookup x9 x2 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_findModulesToAnalyze x1002 x2 x3 x4 x3250 x3500) (d_C_findModulesToAnalyze x1003 x2 x3 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_findModulesToAnalyze z x2 x3 x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_findModulesToAnalyze x1002 x2 x3 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod x1002 x3250 x3500) (d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_findModulesToAnalyze_dot___hash_selFP8_hash_mod x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports x1002 x3250 x3500) (d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_findModulesToAnalyze_dot___hash_selFP9_hash_imports x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo x1002 x3250 x3500) (d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_findModulesToAnalyze_dot___hash_selFP6_hash_modulesToDo x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate x1002 x3250 x3500) (d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_findModulesToAnalyze_dot___hash_selFP7_hash_modulesUpToDate x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_checkTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_checkTime x1 x2 x3 x4 x5 x6 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_eq (Curry_Prelude.C_Just x2) (Curry_Prelude.d_C_lookup x1 x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_lookup x7 x6 x3250 x3500) Curry_Prelude.C_Nothing x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt_eq (Curry_Prelude.C_Just x2) (Curry_Maybe.d_C_fromMaybe Curry_Prelude.C_Nothing (Curry_Prelude.d_C_lookup x7 x4 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_checkTime x1 x2 x8 x4 x5 x6 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_checkTime x1 x2 x1002 x4 x5 x6 x3250 x3500) (d_C_checkTime x1 x2 x1003 x4 x5 x6 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_checkTime x1 x2 z x4 x5 x6 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_checkTime x1 x2 x1002 x4 x5 x6 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_reduceDependencies :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_reduceDependencies x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_C_map (d_OP_reduceDependencies_dot___hash_lambda22 x3) x1 x3250 x3500
           in (d_C_reduceDependencies x5 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_reduceDependencies x1 x1002 x3250 x3500) (d_C_reduceDependencies x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_reduceDependencies x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_reduceDependencies x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_reduceDependencies_dot___hash_lambda22 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_reduceDependencies_dot___hash_lambda22 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_List.d_C_delete x3250 x3500) x1 x3250 x3500) x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_reduceDependencies_dot___hash_lambda22 x1 x1002 x3250 x3500) (d_OP_reduceDependencies_dot___hash_lambda22 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_reduceDependencies_dot___hash_lambda22 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_reduceDependencies_dot___hash_lambda22 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Maybe (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 x14 x3250 x3500 = case x14 of
     (Curry_Prelude.C_Just x13) -> d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 x13 x3250 x3500
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 x1002 x3250 x3500) (d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x9 x11 x3 x10 x12 x7 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Maybe Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_Nothing -> d_C_findModulesToAnalyze x8 x2 x3 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x7 x11) x12) x3250 x3500
     (Curry_Prelude.C_Just x14) -> d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 (d_C_checkTime x9 x14 x10 x2 x3 x11 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 x1002 x3250 x3500) (d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x11 x3 x2 x10 x9 x12 x7 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_C_findModulesToAnalyze x8 x2 x3 (Curry_Prelude.OP_Tuple2 x11 (Curry_Prelude.OP_Cons x9 x12)) x3250 x3500
     Curry_Prelude.C_False -> d_C_findModulesToAnalyze x8 x2 x3 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x7 x11) x12) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 x1002 x3250 x3500) (d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x11 x3 x2 x10 x14 x9 x12 x7 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x5 x3250 x3500) x6
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) t1)
d_OP__case_6 x1 x5 x2 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_5 x6 x1 x5 x2 x7 (Curry_Prelude.d_OP_eq_eq x1 x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x5 x2 x1002 x3250 x3500) (d_OP__case_6 x1 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> t1 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) t1)
d_OP__case_5 x6 x1 x5 x2 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x2 x3250 x3500) x5 x3250 x3500)) x7)
     Curry_Prelude.C_False -> d_OP__case_4 x5 x2 x7 x6 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x6 x1 x5 x2 x7 x1002 x3250 x3500) (d_OP__case_5 x6 x1 x5 x2 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x6 x1 x5 x2 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x6 x1 x5 x2 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> t1 -> t0 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) t1)
d_OP__case_4 x5 x2 x7 x6 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_lookupAndReorder x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x2) x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x5 x2 x7 x6 x1 x1002 x3250 x3500) (d_OP__case_4 x5 x2 x7 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x5 x2 x7 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x5 x2 x7 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP__case_7 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x2 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return Curry_ReadShowTerm.d_C_readQTerm x3250 x3500) x3250 x3500) (d_OP_isAnalysisValid_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x1) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_7 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_25 x3 x2 x1 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x5 = d_C_reduceDependencies x1 (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
           in (d_OP__case_24 x5 x3250 x3500)
     Curry_Prelude.C_False -> d_C_reduceDependencies x1 x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_25 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_24 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_23 x5 x7 x6 x3250 x3500
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x1002 x3250 x3500) (d_OP__case_24 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_23 x5 x7 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_22 x5 x7 x8 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x5 x7 x1002 x3250 x3500) (d_OP__case_23 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_22 x5 x7 x8 x3250 x3500 = case x8 of
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x12 = x10
           in (d_OP__case_21 x12 x5 x11 x7 (Curry_Prelude.d_OP_eq_eq x12 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x5 x7 x1002 x3250 x3500) (d_OP__case_22 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_21 x12 x5 x11 x7 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP__case_20 x5 x7 x11 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x12 x5 x11 x7 x1002 x3250 x3500) (d_OP__case_21 x12 x5 x11 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x12 x5 x11 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x12 x5 x11 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_20 x5 x7 x11 x3250 x3500 = case x11 of
     (Curry_Prelude.OP_Cons x13 x14) -> let
          x15 = x13
           in (d_OP__case_19 x15 x5 x14 x7 (Curry_Prelude.d_OP_eq_eq x15 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x5 x7 x1002 x3250 x3500) (d_OP__case_20 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_19 x15 x5 x14 x7 x16 x3250 x3500 = case x16 of
     Curry_Prelude.C_True -> d_OP__case_18 x5 x7 x14 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x15 x5 x14 x7 x1002 x3250 x3500) (d_OP__case_19 x15 x5 x14 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x15 x5 x14 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x15 x5 x14 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_18 x5 x7 x14 x3250 x3500 = case x14 of
     (Curry_Prelude.OP_Cons x16 x17) -> let
          x18 = x16
           in (d_OP__case_17 x18 x5 x17 x7 (Curry_Prelude.d_OP_eq_eq x18 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x5 x7 x1002 x3250 x3500) (d_OP__case_18 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_17 x18 x5 x17 x7 x19 x3250 x3500 = case x19 of
     Curry_Prelude.C_True -> d_OP__case_16 x5 x7 x17 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x18 x5 x17 x7 x1002 x3250 x3500) (d_OP__case_17 x18 x5 x17 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x18 x5 x17 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x18 x5 x17 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_16 x5 x7 x17 x3250 x3500 = case x17 of
     (Curry_Prelude.OP_Cons x19 x20) -> let
          x21 = x19
           in (d_OP__case_15 x21 x5 x20 x7 (Curry_Prelude.d_OP_eq_eq x21 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x5 x7 x1002 x3250 x3500) (d_OP__case_16 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_15 x21 x5 x20 x7 x22 x3250 x3500 = case x22 of
     Curry_Prelude.C_True -> d_OP__case_14 x5 x7 x20 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x21 x5 x20 x7 x1002 x3250 x3500) (d_OP__case_15 x21 x5 x20 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x21 x5 x20 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x21 x5 x20 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_14 x5 x7 x20 x3250 x3500 = case x20 of
     (Curry_Prelude.OP_Cons x22 x23) -> let
          x24 = x22
           in (d_OP__case_13 x24 x5 x23 x7 (Curry_Prelude.d_OP_eq_eq x24 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x5 x7 x1002 x3250 x3500) (d_OP__case_14 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_13 x24 x5 x23 x7 x25 x3250 x3500 = case x25 of
     Curry_Prelude.C_True -> d_OP__case_12 x5 x7 x23 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x24 x5 x23 x7 x1002 x3250 x3500) (d_OP__case_13 x24 x5 x23 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x24 x5 x23 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x24 x5 x23 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_12 x5 x7 x23 x3250 x3500 = case x23 of
     (Curry_Prelude.OP_Cons x25 x26) -> let
          x27 = x25
           in (d_OP__case_11 x27 x5 x26 x7 (Curry_Prelude.d_OP_eq_eq x27 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x5 x7 x1002 x3250 x3500) (d_OP__case_12 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_11 x27 x5 x26 x7 x28 x3250 x3500 = case x28 of
     Curry_Prelude.C_True -> d_OP__case_10 x5 x7 x26 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x27 x5 x26 x7 x1002 x3250 x3500) (d_OP__case_11 x27 x5 x26 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x27 x5 x26 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x27 x5 x26 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_10 x5 x7 x26 x3250 x3500 = case x26 of
     (Curry_Prelude.OP_Cons x28 x29) -> let
          x30 = x28
           in (d_OP__case_9 x30 x5 x29 x7 (Curry_Prelude.d_OP_eq_eq x30 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x5 x7 x1002 x3250 x3500) (d_OP__case_10 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_9 x30 x5 x29 x7 x31 x3250 x3500 = case x31 of
     Curry_Prelude.C_True -> d_OP__case_8 x5 x7 x29 x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x30 x5 x29 x7 x1002 x3250 x3500) (d_OP__case_9 x30 x5 x29 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x30 x5 x29 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x30 x5 x29 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP__case_8 x5 x7 x29 x3250 x3500 = case x29 of
     Curry_Prelude.OP_List -> x7
     (Curry_Prelude.OP_Cons x31 x32) -> x5
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x5 x7 x1002 x3250 x3500) (d_OP__case_8 x5 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x5 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x5 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List t0))
d_OP__case_26 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x1 x1002 x3250 x3500) (d_OP__case_26 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
d_OP__case_27 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_isAnalysisFileNewer x3 x2 x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda1 x2) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_isAnalysisValid x3 x2 x3250 x3500) (d_OP_getModulesToAnalyze_dot___hash_lambda2 x3 x2) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x1 x2 x3 x1002 x3250 x3500) (d_OP__case_27 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_27 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_OP__case_27 x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_isAnalysisFileNewer x3 x2 x3250 x3500) (wrapDX id (d_OP_getModulesToAnalyze_dot___hash_lambda1 x2)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_isAnalysisValid x3 x2 x3250 x3500) (wrapDX id (d_OP_getModulesToAnalyze_dot___hash_lambda2 x3 x2)) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_27 x1 x2 x3 x1002 x3000 x3250 x3500) (nd_OP__case_27 x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_27 x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_27 x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
