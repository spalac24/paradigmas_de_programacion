{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_LoadAnalysis (d_C_debugMessage, d_C_getAnalysisBaseFile, d_C_getAnalysisPublicFile, d_C_getAnalysisDirectory, d_C_splitDirectories, d_C_getInterfaceInfos, nd_C_getInterfaceInfos, d_C_loadDefaultAnalysisValues, d_C_loadCompleteAnalysis, nd_C_loadCompleteAnalysis, d_C_loadPublicAnalysis, nd_C_loadPublicAnalysis, d_C_storeImportModuleList, d_C_getImportModuleListFile, d_C_storeAnalysisResult, nd_C_storeAnalysisResult, d_C_createDirectoryR, d_C_createDirectoryRHelp, d_C_deleteAnalysisFiles) where

import Basics
import qualified Curry_Configuration
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_GenericProgInfo
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_System
import qualified Curry_IO
import qualified Curry_FiniteMap
import qualified Curry_FlatCurry
d_C_debugMessage :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_debugMessage x1 x2 x3500 = Curry_Configuration.d_C_debugMessageLevel x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x2 x3500) x3500

d_C_getAnalysisBaseFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getAnalysisBaseFile x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisDirectory x3500) (d_OP_getAnalysisBaseFile_dot___hash_lambda1 x2 x1) x3500

d_OP_getAnalysisBaseFile_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getAnalysisBaseFile_dot___hash_lambda1 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) x3500) (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2 x1 x3 x2) x3500

d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3500 = let
     x5 = Curry_FileGoodies.d_C_splitDirectoryBaseName x4 x3500
     x6 = d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir x5 x3500
      in (d_OP__case_5 x1 x2 x3 x6 (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x3500) x3500)

d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir x1002 x3500) (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_fileDir x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x1 x3500) x3500) x3500) x3500) x3500) x3500

d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x1 x3500) x3500) x3500) x3500) x3500) x3500) x3500) x3500

d_C_getAnalysisPublicFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getAnalysisPublicFile x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisBaseFile x1 x2 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))))) x3500) x3500

d_C_getAnalysisDirectory :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getAnalysisDirectory x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getHomeDirectory x3500) d_OP_getAnalysisDirectory_dot___hash_lambda5 x3500

d_OP_getAnalysisDirectory_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getAnalysisDirectory_dot___hash_lambda5 x1 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List))))))))))))))))) x3500) x3500

d_C_splitDirectories :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_splitDirectories x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_FileGoodies.d_C_separatorChar x3500)) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x1 x3500) x3500
     x3 = d_OP_splitDirectories_dot___hash_selFP4_hash_rbase x2 x3500
     x4 = d_OP_splitDirectories_dot___hash_selFP5_hash_rdir x2 x3500
      in (d_OP__case_3 x3 x4 (Curry_Prelude.d_C_null x4 x3500) x3500)

d_OP_splitDirectories_dot___hash_selFP4_hash_rbase :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitDirectories_dot___hash_selFP4_hash_rbase x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitDirectories_dot___hash_selFP4_hash_rbase x1002 x3500) (d_OP_splitDirectories_dot___hash_selFP4_hash_rbase x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitDirectories_dot___hash_selFP4_hash_rbase z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitDirectories_dot___hash_selFP4_hash_rbase x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitDirectories_dot___hash_selFP5_hash_rdir :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitDirectories_dot___hash_selFP5_hash_rdir x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitDirectories_dot___hash_selFP5_hash_rdir x1002 x3500) (d_OP_splitDirectories_dot___hash_selFP5_hash_rdir x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitDirectories_dot___hash_selFP5_hash_rdir z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitDirectories_dot___hash_selFP5_hash_rdir x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getInterfaceInfos :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
d_C_getInterfaceInfos x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_GenericProgInfo.d_C_emptyProgInfo x3500) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_loadPublicAnalysis x1 x3 x3500) (d_OP_getInterfaceInfos_dot___hash_lambda6 x1 x4) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getInterfaceInfos x1 x1002 x3500) (d_C_getInterfaceInfos x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getInterfaceInfos x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getInterfaceInfos x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_getInterfaceInfos :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
nd_C_getInterfaceInfos x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_return (Curry_GenericProgInfo.nd_C_emptyProgInfo x2000 x3500) x3500))
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_loadPublicAnalysis x1 x3 x2000 x3500) (wrapNX id (nd_OP_getInterfaceInfos_dot___hash_lambda6 x1 x4)) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_getInterfaceInfos x1 x1002 x3000 x3500) (nd_C_getInterfaceInfos x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_getInterfaceInfos x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_getInterfaceInfos x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_getInterfaceInfos_dot___hash_lambda6 :: Curry_Prelude.Curry t91 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GenericProgInfo.C_ProgInfo t91 -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t91)
d_OP_getInterfaceInfos_dot___hash_lambda6 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getInterfaceInfos x1 x2 x3500) (d_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 x3) x3500

nd_OP_getInterfaceInfos_dot___hash_lambda6 :: Curry_Prelude.Curry t91 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_GenericProgInfo.C_ProgInfo t91 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t91)
nd_OP_getInterfaceInfos_dot___hash_lambda6 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getInterfaceInfos x1 x2 x2000 x3500) (wrapNX id (nd_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 x3)) x2001 x3500)))))

d_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t91 => Curry_GenericProgInfo.C_ProgInfo t91 -> Curry_GenericProgInfo.C_ProgInfo t91 -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t91)
d_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_GenericProgInfo.d_C_combineProgInfo x1 x2 x3500) x3500

nd_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t91 => Curry_GenericProgInfo.C_ProgInfo t91 -> Curry_GenericProgInfo.C_ProgInfo t91 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t91)
nd_OP_getInterfaceInfos_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_return (Curry_GenericProgInfo.nd_C_combineProgInfo x1 x2 x2000 x3500) x3500))

d_C_loadDefaultAnalysisValues :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0))
d_C_loadDefaultAnalysisValues x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3500) x3500) (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8 x1 x2) x3500

d_OP_loadDefaultAnalysisValues_dot___hash_lambda8 :: Curry_Prelude.Curry t121 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t121))
d_OP_loadDefaultAnalysisValues_dot___hash_lambda8 x1 x2 x3 x3500 = let
     x4 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3500) x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))) x1 x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x4 x3500) (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x4 x2) x3500)

d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.Curry t121 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t121))
d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))) x1 x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return Curry_ReadShowTerm.d_C_readQTerm x3500) x3500) (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 x2) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1002 x3500) (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 :: Curry_Prelude.Curry t121 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t121) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t121))
d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_map (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1) x2 x3500) x3500

d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.Curry t121 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t121 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t121
d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 x1 x3) x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x1002 x3500) (d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_loadDefaultAnalysisValues_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_loadCompleteAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
d_C_loadCompleteAnalysis x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisBaseFile x2 x1 x3500) Curry_GenericProgInfo.d_C_readAnalysisFiles x3500

nd_C_loadCompleteAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
nd_C_loadCompleteAnalysis x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAnalysisBaseFile x2 x1 x3500) (wrapNX id Curry_GenericProgInfo.nd_C_readAnalysisFiles) x2000 x3500))

d_C_loadPublicAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
d_C_loadPublicAnalysis x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Configuration.d_C_getWithPrelude x3500) (d_OP_loadPublicAnalysis_dot___hash_lambda12 x1 x2) x3500

nd_C_loadPublicAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t0)
nd_C_loadPublicAnalysis x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Configuration.d_C_getWithPrelude x3500) (wrapNX id (nd_OP_loadPublicAnalysis_dot___hash_lambda12 x1 x2)) x2000 x3500))

d_OP_loadPublicAnalysis_dot___hash_lambda12 :: Curry_Prelude.Curry t174 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t174)
d_OP_loadPublicAnalysis_dot___hash_lambda12 x1 x2 x3 x3500 = d_OP__case_2 x1 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3500) x3500

nd_OP_loadPublicAnalysis_dot___hash_lambda12 :: Curry_Prelude.Curry t174 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_GenericProgInfo.C_ProgInfo t174)
nd_OP_loadPublicAnalysis_dot___hash_lambda12 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_2 x1 x2 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)) x3500) (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3500) x3500) x2000 x3500))

d_C_storeImportModuleList :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_storeImportModuleList x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisBaseFile x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))))))))) x3500) (d_OP_storeImportModuleList_dot___hash_lambda13 x2) x3500

d_OP_storeImportModuleList_dot___hash_lambda13 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_storeImportModuleList_dot___hash_lambda13 x1 x2 x3500 = let
     x3 = Curry_FileGoodies.d_C_splitDirectoryBaseName x2 x3500
     x4 = d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir x3 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_createDirectoryR x4 x3500) (Curry_Prelude.d_C_writeFile x2 (Curry_ReadShowTerm.d_C_showQTerm x1 x3500) x3500) x3500)

d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir x1002 x3500) (d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_storeImportModuleList_dot___hash_lambda13_dot___hash_selFP7_hash_dir x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getImportModuleListFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getImportModuleListFile x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisBaseFile x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) Curry_Prelude.OP_List)))))))))) x3500) d_OP_getImportModuleListFile_dot___hash_lambda14 x3500

d_OP_getImportModuleListFile_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getImportModuleListFile_dot___hash_lambda14 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3500) (d_OP_getImportModuleListFile_dot___hash_lambda14_dot___hash_lambda15 x1) x3500

d_OP_getImportModuleListFile_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getImportModuleListFile_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (d_OP__case_1 x1 x2 x3500) x3500

d_C_storeAnalysisResult :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_storeAnalysisResult x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisBaseFile x2 x1 x3500) (d_OP_storeAnalysisResult_dot___hash_lambda16 x3) x3500

nd_C_storeAnalysisResult :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_storeAnalysisResult x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAnalysisBaseFile x2 x1 x3500) (wrapNX id (nd_OP_storeAnalysisResult_dot___hash_lambda16 x3)) x2000 x3500))

d_OP_storeAnalysisResult_dot___hash_lambda16 :: Curry_Prelude.Curry t173 => Curry_GenericProgInfo.C_ProgInfo t173 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_storeAnalysisResult_dot___hash_lambda16 x1 x2 x3500 = let
     x3 = Curry_FileGoodies.d_C_splitDirectoryBaseName x2 x3500
     x4 = d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x3 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_createDirectoryR x4 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_GenericProgInfo.d_C_showProgInfo x1 x3500) x3500) x3500) (Curry_GenericProgInfo.d_C_writeAnalysisFiles x2 x1 x3500) x3500) x3500)

nd_OP_storeAnalysisResult_dot___hash_lambda16 :: Curry_Prelude.Curry t173 => Curry_GenericProgInfo.C_ProgInfo t173 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_storeAnalysisResult_dot___hash_lambda16 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x3 = Curry_FileGoodies.d_C_splitDirectoryBaseName x2 x3500
          x4 = d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x3 x3500
           in (Curry_Prelude.d_OP_gt_gt (d_C_createDirectoryR x4 x3500) (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) (Curry_GenericProgInfo.nd_C_showProgInfo x1 x2000 x3500) x3500) x3500) (Curry_GenericProgInfo.nd_C_writeAnalysisFiles x2 x1 x2001 x3500) x3500)))) x3500)))

d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x1002 x3500) (d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_storeAnalysisResult_dot___hash_lambda16_dot___hash_selFP9_hash_dir x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_createDirectoryR :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDirectoryR x1 x3500 = let
     x2 = d_C_splitDirectories x1 x3500
      in (d_C_createDirectoryRHelp Curry_Prelude.OP_List x2 x3500)

d_C_createDirectoryRHelp :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDirectoryRHelp x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x3 x3500) x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesDirectoryExist x5 x3500) (d_OP_createDirectoryRHelp_dot___hash_lambda17 x5 x4) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_createDirectoryRHelp x1 x1002 x3500) (d_C_createDirectoryRHelp x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_createDirectoryRHelp x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_createDirectoryRHelp x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_createDirectoryRHelp_dot___hash_lambda17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDirectoryRHelp_dot___hash_lambda17 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_0 x1 x3 x3500) (d_C_createDirectoryRHelp x1 x2 x3500) x3500

d_C_deleteAnalysisFiles :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_deleteAnalysisFiles x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisDirectory x3500) (d_OP_deleteAnalysisFiles_dot___hash_lambda18 x1) x3500

d_OP_deleteAnalysisFiles_dot___hash_lambda18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP_deleteAnalysisFiles_dot___hash_lambda18 x1 x2 x3500 = Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))) x3500) x3500) x3500) x3500) x3500

d_OP__case_0 x1 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Directory.d_C_createDirectory x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> Curry_Directory.d_C_createDirectory x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x1
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3500) (d_OP__case_1 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x1
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x1002 x3000 x3500) (nd_OP__case_1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_GenericProgInfo.d_C_emptyProgInfo x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getAnalysisPublicFile x2 x1 x3500) Curry_GenericProgInfo.d_C_readAnalysisPublicFile x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x3 x1002 x3500) (d_OP__case_2 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_return (Curry_GenericProgInfo.nd_C_emptyProgInfo x2000 x3500) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAnalysisPublicFile x2 x1 x3500) (wrapNX id Curry_GenericProgInfo.nd_C_readAnalysisPublicFile) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (d_C_splitDirectories (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_tail x4 x3500) x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x3 x3500) Curry_Prelude.OP_List) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x4 x1002 x3500) (d_OP__case_3 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_plus_plus (d_C_splitDirectories (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) (Curry_Prelude.d_C_tail x4 x3500) x2001 x3500)))) x3500) (Curry_Prelude.OP_Cons (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2003 x3500) x3 x2004 x3500)))) Curry_Prelude.OP_List) x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x3 x4 x1002 x3000 x3500) (nd_OP__case_3 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x3 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getCurrentDirectory x3500) (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3) x3500
     Curry_Prelude.C_False -> d_OP__case_4 x1 x2 x3 x6 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x6 x3500) (Curry_Prelude.C_Char '/'#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x3 x6 x1002 x3500) (d_OP__case_5 x1 x2 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x3 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Directory.d_C_getCurrentDirectory x3500) (wrapDX id (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x2 x3 x6 (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_head x6 x3500) (Curry_Prelude.C_Char '/'#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x3 x6 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getCurrentDirectory x3500) (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x6 x3) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x1 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x6 x1002 x3500) (d_OP__case_4 x1 x2 x3 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Directory.d_C_getCurrentDirectory x3500) (wrapDX id (d_OP_getAnalysisBaseFile_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x6 x3)) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) x1 x3500) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x6 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
