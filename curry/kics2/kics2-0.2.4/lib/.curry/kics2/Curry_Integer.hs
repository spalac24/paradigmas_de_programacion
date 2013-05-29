{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Integer (d_C_pow, d_C_ilog, d_C_isqrt, d_C_factorial, d_C_binomial, d_C_abs, d_C_max3, d_C_min3, d_C_maxlist, d_C_minlist, d_C_bitTrunc, d_C_bitAnd, d_C_bitOr, d_C_bitNot, d_C_bitXor, d_C_even, d_C_odd) where

import Basics
import qualified Curry_Prelude
d_C_pow :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_pow x1 x2 x3500 = d_OP__case_23 x1 x2 (Curry_Prelude.d_OP_gt_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500

d_OP_pow_dot_powaux_dot_2 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_pow_dot_powaux_dot_2 x1 x2 x3 x3500 = d_OP__case_22 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_ilog :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_ilog x1 x3500 = d_OP__case_20 x1 (Curry_Prelude.d_OP_gt x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_isqrt :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_isqrt x1 x3500 = d_OP__case_18 x1 (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_OP_isqrt_dot_aux_dot_20 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_isqrt_dot_aux_dot_20 x1 x2 x3 x3500 = d_OP__case_15 x1 x2 x3 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.d_OP_plus x2 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500

d_C_factorial :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_factorial x1 x3500 = d_OP__case_13 x1 (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_binomial :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_binomial x1 x2 x3500 = d_OP__case_11 x1 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 0#) x3500) (Curry_Prelude.d_OP_gt_eq x1 x2 x3500) x3500) x3500

d_OP_binomial_dot_aux_dot_40 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_binomial_dot_aux_dot_40 x1 x2 x3500 = d_OP__case_10 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_abs :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_abs x1 x3500 = d_OP__case_9 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_max3 :: Curry_Prelude.Curry t0 => t0 -> t0 -> t0 -> ConstStore -> t0
d_C_max3 x1 x2 x3 x3500 = Curry_Prelude.d_C_max x1 (Curry_Prelude.d_C_max x2 x3 x3500) x3500

d_C_min3 :: Curry_Prelude.Curry t0 => t0 -> t0 -> t0 -> ConstStore -> t0
d_C_min3 x1 x2 x3 x3500 = Curry_Prelude.d_C_min x1 (Curry_Prelude.d_C_min x2 x3 x3500) x3500

d_C_maxlist :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_maxlist x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_8 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maxlist x1002 x3500) (d_C_maxlist x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maxlist z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maxlist x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_minlist :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_minlist x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_7 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_minlist x1002 x3500) (d_C_minlist x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_minlist z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_minlist x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_bitTrunc :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_bitTrunc x1 x2 x3500 = d_C_bitAnd (Curry_Prelude.d_OP_minus (d_C_pow (Curry_Prelude.C_Int 2#) x1 x3500) (Curry_Prelude.C_Int 1#) x3500) x2 x3500

d_C_bitAnd :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_bitAnd x1 x2 x3500 = d_OP__case_6 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_bitOr :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_bitOr x1 x2 x3500 = d_OP__case_4 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_bitNot :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_bitNot x1 x3500 = d_OP_bitNot_dot_aux_dot_95 (Curry_Prelude.C_Int 32#) x1 x3500

d_OP_bitNot_dot_aux_dot_95 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_bitNot_dot_aux_dot_95 x1 x2 x3500 = d_OP__case_2 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_bitXor :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_bitXor x1 x2 x3500 = d_OP__case_1 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500

d_C_even :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Bool
d_C_even x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#) x3500

d_C_odd :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Bool
d_C_odd x1 x3500 = Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#) x3500

d_OP__case_1 x1 x2 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitXor (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
          x4 = d_OP__case_0 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
           in (Curry_Prelude.d_OP_plus x3 x4 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x1002 x3500) (d_OP__case_1 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitXor (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
               x4 = nd_OP__case_0 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500) x3500) x2000 x3500
                in (Curry_Prelude.d_OP_plus x3 x4 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 1#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x1002 x3500) (d_OP__case_0 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 1#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> let
          x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_OP_bitNot_dot_aux_dot_95 (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
          x4 = Curry_Prelude.d_OP_minus (Curry_Prelude.C_Int 1#) (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) x3500
           in (Curry_Prelude.d_OP_plus x3 x4 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x1002 x3500) (d_OP__case_2 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> let
          x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_OP_bitNot_dot_aux_dot_95 (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
          x4 = Curry_Prelude.d_OP_minus (Curry_Prelude.C_Int 1#) (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) x3500
           in (Curry_Prelude.d_OP_plus x3 x4 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitOr (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
          x4 = d_OP__case_3 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 1#) x3500) x3500
           in (Curry_Prelude.d_OP_plus x3 x4 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x1002 x3500) (d_OP__case_4 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitOr (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
               x4 = nd_OP__case_3 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 1#) x3500) x2000 x3500
                in (Curry_Prelude.d_OP_plus x3 x4 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x1002 x3500) (d_OP__case_3 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> let
          x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitAnd (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
          x4 = d_OP__case_5 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#) x3500) x3500
           in (Curry_Prelude.d_OP_plus x3 x4 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x1002 x3500) (d_OP__case_6 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x3 = Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 2#) (d_C_bitAnd (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.d_C_div x2 (Curry_Prelude.C_Int 2#) x3500) x3500) x3500
               x4 = nd_OP__case_5 x1 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#) x3500) x2000 x3500
                in (Curry_Prelude.d_OP_plus x3 x4 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x1002 x3500) (d_OP__case_5 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.d_C_mod x1 (Curry_Prelude.C_Int 2#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_C_min x2 (d_C_minlist (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x1002 x3500) (d_OP__case_7 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_C_min x2 (d_C_minlist (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x2 x1002 x3000 x3500) (nd_OP__case_7 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_C_max x2 (d_C_maxlist (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x1002 x3500) (d_OP__case_8 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_C_max x2 (d_C_maxlist (Curry_Prelude.OP_Cons x4 x5) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x1002 x3000 x3500) (nd_OP__case_8 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_negate x1 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x1002 x3500) (d_OP__case_9 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_negate x1 x3500
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x1002 x3000 x3500) (nd_OP__case_9 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x2 (d_OP_binomial_dot_aux_dot_40 (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x2 x1002 x3500) (d_OP__case_10 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x2 (d_OP_binomial_dot_aux_dot_40 (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x2 x1002 x3000 x3500) (nd_OP__case_10 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_div (d_OP_binomial_dot_aux_dot_40 x2 x1 x3500) (d_C_factorial x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x2 x1002 x3500) (d_OP__case_11 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_div (d_OP_binomial_dot_aux_dot_40 x2 x1 x3500) (d_C_factorial x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x2 x1002 x3000 x3500) (nd_OP__case_11 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP__case_12 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x1002 x3500) (d_OP__case_13 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x1002 x3000 x3500) (nd_OP__case_13 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_factorial (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x1002 x3500) (d_OP__case_12 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_star x1 (d_C_factorial (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x1002 x3000 x3500) (nd_OP__case_12 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x2 x3 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x4 = Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus x3 x2 x3500) (Curry_Prelude.C_Int 2#) x3500
           in (d_OP__case_14 x1 x2 x3 x4 (Curry_Prelude.d_OP_gt (Curry_Prelude.d_OP_star x4 x4 x3500) x1 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x2 x3 x1002 x3500) (d_OP__case_15 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x2 x3 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (let
               x4 = Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus x3 x2 x3500) (Curry_Prelude.C_Int 2#) x3500
                in (nd_OP__case_14 x1 x2 x3 x4 (Curry_Prelude.d_OP_gt (Curry_Prelude.d_OP_star x4 x4 x3500) x1 x3500) x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_15 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP_isqrt_dot_aux_dot_20 x1 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP_isqrt_dot_aux_dot_20 x1 x4 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x2 x3 x4 x1002 x3500) (d_OP__case_14 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP_isqrt_dot_aux_dot_20 x1 x2 x4 x3500
     Curry_Prelude.C_False -> d_OP_isqrt_dot_aux_dot_20 x1 x4 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_14 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP__case_17 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x1 x1002 x3500) (d_OP__case_18 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x1 x1002 x3000 x3500) (nd_OP__case_18 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> d_OP__case_16 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 4#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x1002 x3500) (d_OP__case_17 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 4#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x1002 x3000 x3500) (nd_OP__case_17 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> d_OP_isqrt_dot_aux_dot_20 x1 (Curry_Prelude.C_Int 2#) x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x1 x1002 x3500) (d_OP__case_16 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> d_OP_isqrt_dot_aux_dot_20 x1 (Curry_Prelude.C_Int 2#) x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x1 x1002 x3000 x3500) (nd_OP__case_16 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> d_OP__case_19 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 10#) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x1002 x3500) (d_OP__case_20 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 10#) x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x1002 x3000 x3500) (nd_OP__case_20 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 1#) (d_C_ilog (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 10#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x1 x1002 x3500) (d_OP__case_19 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 0#
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 1#) (d_C_ilog (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 10#) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x1 x1002 x3000 x3500) (nd_OP__case_19 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP_pow_dot_powaux_dot_2 (Curry_Prelude.d_OP_star x1 (d_OP__case_21 x2 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x3 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 1#) x3500) x3500) x3500) (Curry_Prelude.d_OP_star x2 x2 x3500) (Curry_Prelude.d_C_div x3 (Curry_Prelude.C_Int 2#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x3 x1002 x3500) (d_OP__case_22 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (d_OP_pow_dot_powaux_dot_2 (Curry_Prelude.d_OP_star x1 (nd_OP__case_21 x2 x3 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x3 (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 1#) x3500) x2000 x3500) x3500) (Curry_Prelude.d_OP_star x2 x2 x3500) (Curry_Prelude.d_C_div x3 (Curry_Prelude.C_Int 2#) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 1#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x2 x3 x1002 x3500) (d_OP__case_21 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 1#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x2 x3 x1002 x3000 x3500) (nd_OP__case_21 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_pow_dot_powaux_dot_2 (Curry_Prelude.C_Int 1#) x1 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x2 x1002 x3500) (d_OP__case_23 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP_pow_dot_powaux_dot_2 (Curry_Prelude.C_Int 1#) x1 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x2 x1002 x3000 x3500) (nd_OP__case_23 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
