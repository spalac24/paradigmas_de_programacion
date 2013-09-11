{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Combinatorial (nd_C_permute, nd_C_subset, nd_C_splitSet, nd_C_sizedSubset, nd_C_partition) where

import Basics
import qualified Curry_Prelude
import qualified Curry_SetFunctions
nd_C_permute :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_permute x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_permute_dot_ndinsert_dot_4 x2 (nd_C_permute x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_permute x1002 x3000 x3250 x3500) (nd_C_permute x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_permute z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_permute x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_permute_dot_ndinsert_dot_4 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP_permute_dot_ndinsert_dot_4 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x3 x4)) (Curry_Prelude.OP_Cons x3 (nd_OP_permute_dot_ndinsert_dot_4 x1 x4 x2000 x3250 x3500)) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_permute_dot_ndinsert_dot_4 x1 x1002 x3000 x3250 x3500) (nd_OP_permute_dot_ndinsert_dot_4 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_permute_dot_ndinsert_dot_4 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_permute_dot_ndinsert_dot_4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_subset :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_subset x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Cons x2 (nd_C_subset x3 x2000 x3250 x3500)) (nd_C_subset x3 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_subset x1002 x3000 x3250 x3500) (nd_C_subset x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_subset z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_subset x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_allSubsets :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_C_allSubsets x1 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_SetFunctions.nd_C_sortValues x2000 x3250 x3500) (Curry_SetFunctions.nd_C_set1 (wrapNX id nd_C_subset) x1 x2001 x3250 x3500) x2002 x3250 x3500))))))))

nd_C_splitSet :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_C_splitSet x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (let
                    x4 = nd_C_splitSet x3 x2000 x3250 x3500
                    x5 = d_OP_splitSet_dot___hash_selFP2_hash_u x4 x3250 x3500
                    x6 = d_OP_splitSet_dot___hash_selFP3_hash_v x4 x3250 x3500
                     in (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x5) x6) (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.OP_Cons x2 x6)) x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_splitSet x1002 x3000 x3250 x3500) (nd_C_splitSet x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_splitSet z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_splitSet x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitSet_dot___hash_selFP2_hash_u :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_splitSet_dot___hash_selFP2_hash_u x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitSet_dot___hash_selFP2_hash_u x1002 x3250 x3500) (d_OP_splitSet_dot___hash_selFP2_hash_u x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitSet_dot___hash_selFP2_hash_u z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitSet_dot___hash_selFP2_hash_u x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_splitSet_dot___hash_selFP3_hash_v :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_splitSet_dot___hash_selFP3_hash_v x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitSet_dot___hash_selFP3_hash_v x1002 x3250 x3500) (d_OP_splitSet_dot___hash_selFP3_hash_v x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitSet_dot___hash_selFP3_hash_v z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitSet_dot___hash_selFP3_hash_v x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_sizedSubset :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_sizedSubset x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_0 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x2000 x3250 x3500))

nd_OP_sizedSubset_dot_aux_dot_25 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP_sizedSubset_dot_aux_dot_25 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Cons x3 (nd_C_sizedSubset (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x4 x2000 x3250 x3500)) (nd_C_sizedSubset x1 x4 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_sizedSubset_dot_aux_dot_25 x1 x1002 x3000 x3250 x3500) (nd_OP_sizedSubset_dot_aux_dot_25 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_sizedSubset_dot_aux_dot_25 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_sizedSubset_dot_aux_dot_25 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_partition :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_C_partition x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP_partition_dot_insert_dot_33 x2 (nd_C_partition x3 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_partition x1002 x3000 x3250 x3500) (nd_C_partition x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_partition z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_partition x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_partition_dot_insert_dot_33 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP_partition_dot_insert_dot_33 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x1 x3) x4) (Curry_Prelude.OP_Cons x3 (nd_OP_partition_dot_insert_dot_33 x1 x4 x2000 x3250 x3500)) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_partition_dot_insert_dot_33 x1 x1002 x3000 x3250 x3500) (nd_OP_partition_dot_insert_dot_33 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_partition_dot_insert_dot_33 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_partition_dot_insert_dot_33 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_0 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP_sizedSubset_dot_aux_dot_25 x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_0 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
