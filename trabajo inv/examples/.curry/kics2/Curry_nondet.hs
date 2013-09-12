{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_nondet (nd_C_coin, d_C_double, nd_C_double_coin, nd_C_insert, nd_C_perm, d_C_sorted, nd_C_mySort) where

import Basics
import qualified Curry_Prelude
nd_C_coin :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Int
nd_C_coin x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.C_Int 0#) (Curry_Prelude.C_Int 1#) x2000 x3250 x3500))

d_C_double :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_double x1 x3250 x3500 = Curry_Prelude.d_OP_plus x1 x1 x3250 x3500

nd_C_double_coin :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Int
nd_C_double_coin x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus (nd_C_coin x2000 x3250 x3500) (nd_C_coin x2001 x3250 x3500) x3250 x3500)))))

nd_C_insert :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_insert x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x3 x4)) (Curry_Prelude.OP_Cons x3 (nd_C_insert x1 x4 x2000 x3250 x3500)) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_insert x1 x1002 x3000 x3250 x3500) (nd_C_insert x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_insert x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_insert x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_perm :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_perm x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_insert x2 (nd_C_perm x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_perm x1002 x3000 x3250 x3500) (nd_C_perm x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_perm z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_perm x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_sorted :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sorted x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_0 x2 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sorted x1002 x3250 x3500) (d_C_sorted x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sorted z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sorted x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_mySort :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_mySort x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (d_C_sorted (nd_C_perm x1 x2000 x3250 x3500) x3250 x3500))

d_OP__case_0 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_0 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP___cond_0__case_0 x2 x4 x5 (Curry_Prelude.d_OP_eq_colon_eq ((Curry_Prelude.d_OP_lt x2 x4 x3250 x3500 :: Curry_Prelude.C_Bool)) Curry_Prelude.C_True x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3250 x3500) (d_OP__case_0 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP___cond_0__case_0 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP___cond_0__case_0 x1 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_Success -> Curry_Prelude.OP_Cons x1 (d_C_sorted (Curry_Prelude.OP_Cons x2 x3) x3250 x3500)
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__case_0 x1 x2 x3 x1002 x3250 x3500) (d_OP___cond_0__case_0 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__case_0 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__case_0 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
