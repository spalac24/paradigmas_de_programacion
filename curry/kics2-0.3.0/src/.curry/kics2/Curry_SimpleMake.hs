{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SimpleMake (d_C_smake) where

import Basics
import qualified Curry_Directory
import qualified Curry_Prelude
import qualified Curry_Time
d_C_smake :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_smake x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getDestTime x1 x3250 x3500) (d_OP_smake_dot___hash_lambda1 x4 x3 x2) x3250 x3500

d_OP_smake_dot_make_dot_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_Maybe Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_smake_dot_make_dot_2 x1 x2 x3 x4 x3250 x3500 = case x3 of
     Curry_Prelude.C_Nothing -> x2
     (Curry_Prelude.C_Just x5) -> d_OP__case_1 x4 x5 x1 x2 (d_C_outOfDate x5 x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_smake_dot_make_dot_2 x1 x2 x1002 x4 x3250 x3500) (d_OP_smake_dot_make_dot_2 x1 x2 x1003 x4 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_smake_dot_make_dot_2 x1 x2 z x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_smake_dot_make_dot_2 x1 x2 x1002 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_smake_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_smake_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (d_C_getDepTimes x3250 x3500) x3 x3250 x3500) (d_OP_smake_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x4) x3250 x3500

d_OP_smake_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_Maybe Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_smake_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3250 x3500 = d_OP_smake_dot_make_dot_2 x1 x2 x3 x4 x3250 x3500

d_C_getDestTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)
d_C_getDestTime x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_getDestTime_dot___hash_lambda3 x1) x3250 x3500

d_OP_getDestTime_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_Time.C_ClockTime)
d_OP_getDestTime_dot___hash_lambda3 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_liftIO (acceptCs id Curry_Prelude.C_Just) (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getDestTime_dot___hash_lambda3 x1 x1002 x3250 x3500) (d_OP_getDestTime_dot___hash_lambda3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getDestTime_dot___hash_lambda3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getDestTime_dot___hash_lambda3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getDepTimes :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Time.C_ClockTime)
d_C_getDepTimes x3250 x3500 = Curry_Prelude.d_C_mapIO Curry_Directory.d_C_getModificationTime x3250 x3500

nd_C_getDepTimes :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Time.C_ClockTime))
nd_C_getDepTimes x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_mapIO (wrapDX id Curry_Directory.d_C_getModificationTime) x2000 x3250 x3500))

d_C_outOfDate :: Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_outOfDate x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_gt) x1) x3250 x3500) x2 x3250 x3500

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP__case_1 x4 x5 x1 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> d_OP__case_0 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x5 x1 x2 x1002 x3250 x3500) (d_OP__case_1 x4 x5 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x5 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP__case_0 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3250 x3500) (d_OP__case_0 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
