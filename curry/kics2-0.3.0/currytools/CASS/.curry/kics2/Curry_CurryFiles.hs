{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryFiles (d_C_getImports, d_C_findSourceFileInLoadPath, d_C_getSourceFileTime, d_C_readNewestFlatCurry) where

import Basics
import qualified Curry_Configuration
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_Prelude
import qualified Curry_Time
d_C_getImports :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getImports x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Configuration.d_C_debugMessageLevel (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_readNewestFlatCurryInt x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_FlatCurryGoodies.d_C_progImports x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_findSourceFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_findSourceFileInLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile x1 x3250 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda1 x1) x3250 x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_FileGoodies.d_C_getFileInPath (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x2 x3250 x3500

d_C_getSourceFileTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime)
d_C_getSourceFileTime x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x1 x3250 x3500) (d_OP_getSourceFileTime_dot___hash_lambda2 x1) x3250 x3500

d_OP_getSourceFileTime_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime)
d_OP_getSourceFileTime_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x2 x3250 x3500) (d_OP_getSourceFileTime_dot___hash_lambda2_dot___hash_lambda3 x1) x3250 x3500

d_OP_getSourceFileTime_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Time.C_ClockTime)
d_OP_getSourceFileTime_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x1 x2) x3250 x3500

d_C_flatCurryFileNewer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_flatCurryFileNewer x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x1 x3250 x3500) d_OP_flatCurryFileNewer_dot___hash_lambda4 x3250 x3500

d_OP_flatCurryFileNewer_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_flatCurryFileNewer_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5 x1) x3250 x3500

d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3250 x3500 = let
     x3 = Curry_FlatCurry.d_C_flatCurryFileName x1 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x3 x3250 x3500) (d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x3 x1 x2) x3250 x3500)

d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x2 x3) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x1002 x3250 x3500) (d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_flatCurryFileNewer_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_return (d_OP__case_0 x2 x3 x1 (Curry_Prelude.d_OP_gt_eq x3 x2 x3250 x3500) x3250 x3500) x3250 x3500

d_C_readNewestFlatCurry :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readNewestFlatCurry x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_flatCurryFileNewer x1 x3250 x3500) (Curry_Prelude.d_C_maybe (Curry_FlatCurry.d_C_readFlatCurry x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_FlatCurry.d_C_readFlatCurryFile Curry_FlatCurry.d_C_flatCurryFileName x3250 x3500)) x3250 x3500

d_C_readNewestFlatCurryInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_C_readNewestFlatCurryInt x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_flatCurryFileNewer x1 x3250 x3500) (Curry_Prelude.d_C_maybe (Curry_FlatCurry.d_C_readFlatCurryInt x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_FlatCurry.d_C_readFlatCurryFile Curry_FlatCurry.d_C_flatCurryIntName x3250 x3500)) x3250 x3500

d_OP__case_0 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_0 x2 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x1
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
