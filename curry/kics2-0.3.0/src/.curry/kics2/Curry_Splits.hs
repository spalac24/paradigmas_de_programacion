{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Splits (d_C_mkSplits) where

import Basics
import qualified Curry_Prelude
d_C_mkSplits :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int))
d_C_mkSplits x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '3'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_0 x1 x2 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mkSplits x1 x1002 x3250 x3500) (d_C_mkSplits x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mkSplits x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mkSplits x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP10_hash_ys :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP10_hash_ys x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP10_hash_ys x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP10_hash_ys x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP10_hash_ys z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP10_hash_ys x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP11_hash_zs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Int) (Curry_Prelude.OP_List Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP11_hash_zs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP11_hash_zs x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP11_hash_zs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP11_hash_zs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP11_hash_zs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP7_hash_sl :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP7_hash_sl x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP7_hash_sl x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP7_hash_sl x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP7_hash_sl z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP7_hash_sl x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP8_hash_nextl :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP8_hash_nextl x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP8_hash_nextl x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP8_hash_nextl x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP8_hash_nextl z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP8_hash_nextl x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP9_hash_spsl :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)
d_OP_mkSplits_dot___hash_selFP9_hash_spsl x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP9_hash_spsl x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP9_hash_spsl x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP9_hash_spsl z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP9_hash_spsl x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP4_hash_sr :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP4_hash_sr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP4_hash_sr x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP4_hash_sr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP4_hash_sr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP4_hash_sr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP5_hash_nextr :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_mkSplits_dot___hash_selFP5_hash_nextr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP5_hash_nextr x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP5_hash_nextr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP5_hash_nextr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP5_hash_nextr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mkSplits_dot___hash_selFP6_hash_spsr :: Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int)
d_OP_mkSplits_dot___hash_selFP6_hash_spsr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mkSplits_dot___hash_selFP6_hash_spsr x1002 x3250 x3500) (d_OP_mkSplits_dot___hash_selFP6_hash_spsr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mkSplits_dot___hash_selFP6_hash_spsr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mkSplits_dot___hash_selFP6_hash_spsr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_half :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_C_half x1 x3250 x3500 = Curry_Prelude.d_C_splitAt (Curry_Prelude.d_C_div (Curry_Prelude.d_C_length x1 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x1 x3250 x3500

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int))
d_OP__case_0 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple3 x3 x1 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_C_half x2 x3250 x3500
          x8 = d_OP_mkSplits_dot___hash_selFP10_hash_ys x7 x3250 x3500
          x9 = d_OP_mkSplits_dot___hash_selFP11_hash_zs x7 x3250 x3500
          x10 = d_C_mkSplits (Curry_Prelude.d_OP_plus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x8 x3250 x3500
          x11 = d_OP_mkSplits_dot___hash_selFP7_hash_sl x10 x3250 x3500
          x12 = d_OP_mkSplits_dot___hash_selFP8_hash_nextl x10 x3250 x3500
          x13 = d_OP_mkSplits_dot___hash_selFP9_hash_spsl x10 x3250 x3500
          x14 = d_C_mkSplits x12 x9 x3250 x3500
          x15 = d_OP_mkSplits_dot___hash_selFP4_hash_sr x14 x3250 x3500
          x16 = d_OP_mkSplits_dot___hash_selFP5_hash_nextr x14 x3250 x3500
          x17 = d_OP_mkSplits_dot___hash_selFP6_hash_spsr x14 x3250 x3500
           in (Curry_Prelude.OP_Tuple3 x1 x16 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 x1 x11 x15) (Curry_Prelude.d_OP_plus_plus x13 x17 x3250 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x3 x1002 x3250 x3500) (d_OP__case_0 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
