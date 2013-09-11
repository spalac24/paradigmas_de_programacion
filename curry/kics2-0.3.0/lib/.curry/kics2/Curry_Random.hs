{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Random (d_C_nextInt, d_C_nextIntRange, d_C_nextBoolean, d_C_getRandomSeed) where

import Basics
import qualified Curry_Prelude
import qualified Curry_System
import qualified Curry_Time
d_C_zfact :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_zfact x3250 x3500 = Curry_Prelude.C_Int 36969#

d_C_wfact :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_wfact x3250 x3500 = Curry_Prelude.C_Int 18000#

d_C_two16 :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_two16 x3250 x3500 = Curry_Prelude.C_Int 65536#

d_C_large :: Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_large x3250 x3500 = Curry_Prelude.C_Int 536870911#

d_C_nextInt :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_nextInt x1 x3250 x3500 = let
     x2 = d_OP__case_2 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
      in (d_OP_nextInt_dot_next2_dot_15 x2 x2 x3250 x3500)

d_OP_nextInt_dot_next2_dot_15 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_nextInt_dot_next2_dot_15 x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star (d_C_zfact x3250 x3500) (Curry_Prelude.d_C_mod x2 (d_C_two16 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_star x2 (d_C_two16 x3250 x3500) x3250 x3500) x3250 x3500
     x4 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star (d_C_wfact x3250 x3500) (Curry_Prelude.d_C_mod x1 (d_C_two16 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_star x1 (d_C_two16 x3250 x3500) x3250 x3500) x3250 x3500
     x5 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_div x3 (d_C_two16 x3250 x3500) x3250 x3500) x4 x3250 x3500
     x6 = d_OP__case_1 x5 (Curry_Prelude.d_OP_lt x5 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
      in (Curry_Prelude.OP_Cons x6 (d_OP_nextInt_dot_next2_dot_15 x4 x3 x3250 x3500))

d_C_nextIntRange :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_nextIntRange x1 x2 x3250 x3500 = d_OP__case_0 x2 x1 (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_C_nextBoolean :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Bool
d_C_nextBoolean x1 x3250 x3500 = Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Int 0#)) (d_C_nextInt x1 x3250 x3500) x3250 x3500

d_C_getRandomSeed :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_getRandomSeed x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Time.d_C_getClockTime x3250 x3500) d_OP_getRandomSeed_dot___hash_lambda1 x3250 x3500

d_OP_getRandomSeed_dot___hash_lambda1 :: Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getCPUTime x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2 x1) x3250 x3500

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Time.C_ClockTime -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3250 x3500 = let
     x3 = Curry_Time.d_C_toUTCTime x1 x3250 x3500
     x4 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y x3 x3250 x3500
     x5 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo x3 x3250 x3500
     x6 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d x3 x3250 x3500
     x7 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h x3 x3250 x3500
     x8 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m x3 x3250 x3500
     x9 = d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s x3 x3250 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_mod (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_plus x4 x5 x3250 x3500) x6 x3250 x3500) x7 x3250 x3500) (Curry_Prelude.d_OP_star (Curry_Prelude.d_OP_star x8 x9 x3250 x3500) (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_two16 x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x2
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x3
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mo x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x4
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x5
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_h x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x6
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_m x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s :: Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s x1 x3250 x3500 = case x1 of
     (Curry_Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x7
     (Curry_Time.Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s x1002 x3250 x3500) (d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s x1003 x3250 x3500)
     (Curry_Time.Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s z x3250 x3500) x1002
     (Curry_Time.Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getRandomSeed_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP7_hash_s x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Time.Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_0 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_map (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_C_mod) x2) (d_C_nextInt x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1 x1002 x3250 x3500) (d_OP__case_0 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_1 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus x5 (d_C_large x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x5 x1002 x3250 x3500) (d_OP__case_1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_2 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3250 x3500) (d_OP__case_2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
