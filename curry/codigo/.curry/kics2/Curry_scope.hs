{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_scope (nd_C_f, d_C_pow, d_C_fact, d_C_fact_slow, d_C_zipp1, d_C_zipp2) where

import Basics
import qualified Curry_Prelude
nd_C_f :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_C_f x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (nd_OP___cond_0_f (Curry_Prelude.d_OP_eq_colon_eq x1 x3 x3500) x2000 x3500) (nd_C_f x1 x4 x2001 x3500) x2002 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_f x1 x1002 x3000 x3500) (nd_C_f x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_f x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_f x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0_f x1 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.C_True
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_f x1002 x3500) (d_OP___cond_0_f x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_f z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_f x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_f x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.C_True
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_f x1002 x3000 x3500) (nd_OP___cond_0_f x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_f z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_f x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_pow :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_pow x1 x2 x3500 = d_OP__case_2 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500

d_C_fact :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_fact x1 x3500 = d_OP_fact_dot_fact_aux_dot_15 (Curry_Prelude.C_Int 1#) x1 x3500

d_OP_fact_dot_fact_aux_dot_15 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_fact_dot_fact_aux_dot_15 x1 x2 x3500 = d_OP__case_1 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 1#) x3500) x3500

d_C_fact_slow :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_fact_slow x1 x3500 = d_OP__case_0 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_zipp1 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)
d_C_zipp1 x1 x3500 = Curry_Prelude.d_C_zip x1 (Curry_Prelude.d_C_map d_OP_zipp1_dot_f_dot_27 x1 x3500) x3500

d_OP_zipp1_dot_g_dot_27 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_zipp1_dot_g_dot_27 x1 x3500 = Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3500

d_OP_zipp1_dot_f_dot_27 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_zipp1_dot_f_dot_27 x1 x3500 = d_OP_zipp1_dot_g_dot_27 x1 x3500

d_C_zipp2 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)
d_C_zipp2 x1 x3500 = Curry_Prelude.d_C_zip x1 (Curry_Prelude.d_C_map d_OP_zipp2_dot_f_dot_35 x1 x3500) x3500

d_OP_zipp2_dot_g_dot_35 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_zipp2_dot_g_dot_35 x1 x3500 = Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 2#) x3500

d_OP_zipp2_dot_f_dot_35 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_zipp2_dot_f_dot_35 x1 x3500 = d_OP_zipp2_dot_g_dot_35 x1 x3500

d_OP__case_0 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_fact_slow (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_fact_slow (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP_fact_dot_fact_aux_dot_15 (Curry_Prelude.d_OP_star x2 x1 x3500) (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x1002 x3500) (d_OP__case_1 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP_fact_dot_fact_aux_dot_15 (Curry_Prelude.d_OP_star x2 x1 x3500) (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_pow x1 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x1002 x3500) (d_OP__case_2 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_pow x1 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
