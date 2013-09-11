{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Sort (d_C_quickSort, nd_C_quickSort, d_C_mergeSort, nd_C_mergeSort, d_C_leqList, nd_C_leqList, d_C_cmpList, nd_C_cmpList, d_C_leqChar, nd_C_leqChar, d_C_cmpChar, nd_C_cmpChar, d_C_leqCharIgnoreCase, d_C_leqString, nd_C_leqString, d_C_cmpString, nd_C_cmpString, d_C_leqStringIgnoreCase, nd_C_leqStringIgnoreCase, d_C_leqLexGerman) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
d_C_quickSort :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_quickSort x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = d_OP_quickSort_dot_split_dot_5 x1 x3 x4 x3250 x3500
          x6 = d_OP_quickSort_dot___hash_selFP5_hash_l x5 x3250 x3500
          x7 = d_OP_quickSort_dot___hash_selFP6_hash_r x5 x3250 x3500
           in (Curry_Prelude.d_OP_plus_plus (d_C_quickSort x1 x6 x3250 x3500) (Curry_Prelude.OP_Cons x3 (d_C_quickSort x1 x7 x3250 x3500)) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_quickSort x1 x1002 x3250 x3500) (d_C_quickSort x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_quickSort x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_quickSort x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_quickSort :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_quickSort x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x5 = nd_OP_quickSort_dot_split_dot_5 x1 x3 x4 x2000 x3250 x3500
                    x6 = d_OP_quickSort_dot___hash_selFP5_hash_l x5 x3250 x3500
                    x7 = d_OP_quickSort_dot___hash_selFP6_hash_r x5 x3250 x3500
                     in (let
                         x2001 = leftSupply x2003
                         x2002 = rightSupply x2003
                          in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_plus_plus (nd_C_quickSort x1 x6 x2001 x3250 x3500) (Curry_Prelude.OP_Cons x3 (nd_C_quickSort x1 x7 x2002 x3250 x3500)) x3250 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_quickSort x1 x1002 x3000 x3250 x3500) (nd_C_quickSort x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_quickSort x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_quickSort x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quickSort_dot_split_dot_5 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_OP_quickSort_dot_split_dot_5 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_OP_quickSort_dot_split_dot_5 x1 x2 x5 x3250 x3500
          x7 = d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x6 x3250 x3500
          x8 = d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x6 x3250 x3500
           in (d_OP__case_28 x2 x4 x1 x8 x7 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x2 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quickSort_dot_split_dot_5 x1 x2 x1002 x3250 x3500) (d_OP_quickSort_dot_split_dot_5 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quickSort_dot_split_dot_5 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quickSort_dot_split_dot_5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_quickSort_dot_split_dot_5 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_quickSort_dot_split_dot_5 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2000 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2000 (seq x2005 (let
                    x6 = nd_OP_quickSort_dot_split_dot_5 x1 x2 x5 x2000 x3250 x3500
                    x7 = d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x6 x3250 x3500
                    x8 = d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x6 x3250 x3500
                     in (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (nd_OP__case_28 x2 x4 x1 x8 x7 (let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2001 x3250 x3500) x2 x2002 x3250 x3500)))) x2004 x3250 x3500)))))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_quickSort_dot_split_dot_5 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_quickSort_dot_split_dot_5 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_quickSort_dot_split_dot_5 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_quickSort_dot_split_dot_5 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x1002 x3250 x3500) (d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quickSort_dot_split_dot_5_dot___hash_selFP2_hash_l x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x1002 x3250 x3500) (d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quickSort_dot_split_dot_5_dot___hash_selFP3_hash_r x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quickSort_dot___hash_selFP5_hash_l :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_quickSort_dot___hash_selFP5_hash_l x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quickSort_dot___hash_selFP5_hash_l x1002 x3250 x3500) (d_OP_quickSort_dot___hash_selFP5_hash_l x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quickSort_dot___hash_selFP5_hash_l z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quickSort_dot___hash_selFP5_hash_l x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_quickSort_dot___hash_selFP6_hash_r :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_quickSort_dot___hash_selFP6_hash_r x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_quickSort_dot___hash_selFP6_hash_r x1002 x3250 x3500) (d_OP_quickSort_dot___hash_selFP6_hash_r x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_quickSort_dot___hash_selFP6_hash_r z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_quickSort_dot___hash_selFP6_hash_r x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_mergeSort :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_mergeSort x1 x2 x3250 x3500 = d_OP_mergeSort_dot_mergeLists_dot_15 x1 (d_OP_mergeSort_dot_genRuns_dot_15 x1 x2 x3250 x3500) x3250 x3500

nd_C_mergeSort :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_mergeSort x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP_mergeSort_dot_mergeLists_dot_15 x1 (nd_OP_mergeSort_dot_genRuns_dot_15 x1 x2 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_OP_mergeSort_dot_genRuns_dot_15 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP_mergeSort_dot_genRuns_dot_15 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_26 x3 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mergeSort_dot_genRuns_dot_15 x1 x1002 x3250 x3500) (d_OP_mergeSort_dot_genRuns_dot_15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mergeSort_dot_genRuns_dot_15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mergeSort_dot_genRuns_dot_15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_mergeSort_dot_genRuns_dot_15 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP_mergeSort_dot_genRuns_dot_15 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_26 x3 x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_mergeSort_dot_genRuns_dot_15 x1 x1002 x3000 x3250 x3500) (nd_OP_mergeSort_dot_genRuns_dot_15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_mergeSort_dot_genRuns_dot_15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_mergeSort_dot_genRuns_dot_15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mergeSort_dot_mergePairs_dot_15 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP_mergeSort_dot_mergePairs_dot_15 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_23 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mergeSort_dot_mergePairs_dot_15 x1 x1002 x3250 x3500) (d_OP_mergeSort_dot_mergePairs_dot_15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mergeSort_dot_mergePairs_dot_15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mergeSort_dot_mergePairs_dot_15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_mergeSort_dot_mergePairs_dot_15 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_23 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x1002 x3000 x3250 x3500) (nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_mergeSort_dot_mergePairs_dot_15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_mergeSort_dot_mergeLists_dot_15 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_mergeSort_dot_mergeLists_dot_15 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_22 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mergeSort_dot_mergeLists_dot_15 x1 x1002 x3250 x3500) (d_OP_mergeSort_dot_mergeLists_dot_15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mergeSort_dot_mergeLists_dot_15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mergeSort_dot_mergeLists_dot_15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_mergeSort_dot_mergeLists_dot_15 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP_mergeSort_dot_mergeLists_dot_15 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_22 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_mergeSort_dot_mergeLists_dot_15 x1 x1002 x3000 x3250 x3500) (nd_OP_mergeSort_dot_mergeLists_dot_15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_mergeSort_dot_mergeLists_dot_15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_mergeSort_dot_mergeLists_dot_15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_merge :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_merge x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_21 x4 x1 x5 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_merge x1 x1002 x3 x3250 x3500) (d_C_merge x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_merge x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_merge x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_merge :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_merge x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_21 x4 x1 x5 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_merge x1 x1002 x3 x3000 x3250 x3500) (nd_C_merge x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_merge x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_merge x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leqList :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqList x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_18 x4 x1 x5 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqList x1 x1002 x3 x3250 x3500) (d_C_leqList x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqList x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqList x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_leqList :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_leqList x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x4 x1 x5 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_leqList x1 x1002 x3 x3000 x3250 x3500) (nd_C_leqList x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_leqList x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_leqList x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_cmpList :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpList x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_15 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_14 x6 x1 x7 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cmpList x1 x1002 x3 x3250 x3500) (d_C_cmpList x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cmpList x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cmpList x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_cmpList :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
nd_C_cmpList x1 x2 x3 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_15 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x6 x1 x7 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_cmpList x1 x1002 x3 x3000 x3250 x3500) (nd_C_cmpList x1 x1003 x3 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_cmpList x1 z x3 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_cmpList x1 x1002 x3 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_leqChar :: Cover -> ConstStore -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqChar x3250 x3500 = acceptCs id Curry_Prelude.d_OP_lt_eq

nd_C_leqChar :: IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Char (Func Curry_Prelude.C_Char Curry_Prelude.C_Bool)
nd_C_leqChar x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt_eq)

d_C_cmpChar :: Cover -> ConstStore -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpChar x3250 x3500 = acceptCs id Curry_Prelude.d_C_compare

nd_C_cmpChar :: IDSupply -> Cover -> ConstStore -> Func Curry_Prelude.C_Char (Func Curry_Prelude.C_Char Curry_Prelude.C_Ordering)
nd_C_cmpChar x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_C_compare)

d_C_leqCharIgnoreCase :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqCharIgnoreCase x1 x2 x3250 x3500 = Curry_Prelude.d_OP_lt_eq (Curry_Char.d_C_toUpper x1 x3250 x3500) (Curry_Char.d_C_toUpper x2 x3250 x3500) x3250 x3500

d_C_leqString :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqString x3250 x3500 = acceptCs id Curry_Prelude.d_OP_lt_eq

nd_C_leqString :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_C_leqString x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt_eq)

d_C_cmpString :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpString x3250 x3500 = acceptCs id Curry_Prelude.d_C_compare

nd_C_cmpString :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Ordering)
nd_C_cmpString x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_C_compare)

d_C_leqStringIgnoreCase :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqStringIgnoreCase x3250 x3500 = acceptCs id (d_C_leqList (acceptCs id d_C_leqCharIgnoreCase))

nd_C_leqStringIgnoreCase :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool)
nd_C_leqStringIgnoreCase x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id (nd_C_leqList (wrapDX (wrapDX id) (acceptCs id d_C_leqCharIgnoreCase))))

d_C_leqLexGerman :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_leqLexGerman x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_11 x3 x4 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_leqLexGerman x1002 x2 x3250 x3500) (d_C_leqLexGerman x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_leqLexGerman z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_leqLexGerman x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_leqLexGerman_dot_glex_dot_88 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_leqLexGerman_dot_glex_dot_88 x1 x3250 x3500 = d_OP__case_8 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt_eq x1 (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_lt_eq x1 (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'Z'#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP__case_8 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_8 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus x1 (Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'a'#) x3250 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_7 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 228#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3250 x3500) (d_OP__case_8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_7 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'a'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_6 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 246#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x1002 x3250 x3500) (d_OP__case_7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_6 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'o'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_5 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 252#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x1002 x3250 x3500) (d_OP__case_6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_5 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'u'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 196#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x1002 x3250 x3500) (d_OP__case_5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_4 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'a'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_3 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 214#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3250 x3500) (d_OP__case_4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_3 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'o'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 220#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x1002 x3250 x3500) (d_OP__case_3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_2 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'u'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 223#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3250 x3500) (d_OP__case_2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_1 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 's'#) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3250 x3500) (d_OP__case_1 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_0 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3250 x3500) (d_OP__case_0 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_11 x3 x4 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_OP_leqLexGerman_dot_glex_dot_88 (Curry_Prelude.d_C_ord x3 x3250 x3500) x3250 x3500
          x8 = d_OP_leqLexGerman_dot_glex_dot_88 (Curry_Prelude.d_C_ord x5 x3250 x3500) x3250 x3500
           in (d_OP__case_10 x8 x7 x6 x4 (Curry_Prelude.d_OP_eq_eq x7 x8 x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x3 x4 x1002 x3250 x3500) (d_OP__case_11 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_10 x8 x7 x6 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_leqLexGerman x4 x6 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_9 x8 x7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x8 x7 x6 x4 x1002 x3250 x3500) (d_OP__case_10 x8 x7 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x8 x7 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x8 x7 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_9 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_lt x7 x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x8 x7 x1002 x3250 x3500) (d_OP__case_9 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_14 x6 x1 x7 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_GT
     (Curry_Prelude.OP_Cons x8 x9) -> d_OP__case_13 x8 x6 x1 x9 x7 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x6 x3250 x3500) x8 x3250 x3500) Curry_Prelude.C_EQ x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x6 x1 x7 x1002 x3250 x3500) (d_OP__case_14 x6 x1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x6 x1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x6 x1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_14 :: Curry_Prelude.Curry t0 => t0 -> Func t0 (Func t0 Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
nd_OP__case_14 x6 x1 x7 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_GT
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_13 x8 x6 x1 x9 x7 (Curry_Prelude.d_OP_eq_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x6 x2000 x3250 x3500) x8 x2001 x3250 x3500)))) Curry_Prelude.C_EQ x3250 x3500) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x6 x1 x7 x1002 x3000 x3250 x3500) (nd_OP__case_14 x6 x1 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x6 x1 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x6 x1 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_13 x8 x6 x1 x9 x7 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_C_cmpList x1 x7 x9 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_12 x8 x6 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x8 x6 x1 x9 x7 x1002 x3250 x3500) (d_OP__case_13 x8 x6 x1 x9 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x8 x6 x1 x9 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x8 x6 x1 x9 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_13 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Ordering) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
nd_OP__case_13 x8 x6 x1 x9 x7 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_cmpList x1 x7 x9 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x8 x6 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x8 x6 x1 x9 x7 x1002 x3000 x3250 x3500) (nd_OP__case_13 x8 x6 x1 x9 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x8 x6 x1 x9 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x8 x6 x1 x9 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_12 x8 x6 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x6 x3250 x3500) x8 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x8 x6 x1 x1002 x3250 x3500) (d_OP__case_12 x8 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x8 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x8 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_12 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Ordering) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
nd_OP__case_12 x8 x6 x1 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x6 x2000 x3250 x3500) x8 x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x8 x6 x1 x1002 x3000 x3250 x3500) (nd_OP__case_12 x8 x6 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x8 x6 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x8 x6 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_15 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_EQ
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.C_LT
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1002 x3250 x3500) (d_OP__case_15 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_18 x4 x1 x5 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_17 x6 x4 x1 x7 x5 (Curry_Prelude.d_OP_eq_eq x4 x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x4 x1 x5 x1002 x3250 x3500) (d_OP__case_18 x4 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x4 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x4 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_18 :: Curry_Prelude.Curry t0 => t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP__case_18 x4 x1 x5 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x6 x4 x1 x7 x5 (Curry_Prelude.d_OP_eq_eq x4 x6 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x4 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_18 x4 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x4 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x4 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_17 x6 x4 x1 x7 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_leqList x1 x5 x7 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_16 x6 x4 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x6 x4 x1 x7 x5 x1002 x3250 x3500) (d_OP__case_17 x6 x4 x1 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x6 x4 x1 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x6 x4 x1 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_17 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP__case_17 x6 x4 x1 x7 x5 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_leqList x1 x5 x7 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x6 x4 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x6 x4 x1 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_17 x6 x4 x1 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x6 x4 x1 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x6 x4 x1 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_16 x6 x4 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x6 x4 x1 x1002 x3250 x3500) (d_OP__case_16 x6 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x6 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x6 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_16 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP__case_16 x6 x4 x1 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) x6 x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x6 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_16 x6 x4 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x6 x4 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x6 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_21 x4 x1 x5 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x4 x5
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_20 x6 x4 x1 x7 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x4 x1 x5 x1002 x3250 x3500) (d_OP__case_21 x4 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x4 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x4 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_21 :: Curry_Prelude.Curry t0 => t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_21 x4 x1 x5 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x4 x5
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_20 x6 x4 x1 x7 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) x6 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x4 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_21 x4 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x4 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x4 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_20 x6 x4 x1 x7 x5 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x4 (d_C_merge x1 x5 (Curry_Prelude.OP_Cons x6 x7) x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_19 x7 x5 x4 x1 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x6 x4 x1 x7 x5 x1002 x3250 x3500) (d_OP__case_20 x6 x4 x1 x7 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x6 x4 x1 x7 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x6 x4 x1 x7 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_20 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_20 x6 x4 x1 x7 x5 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x4 (nd_C_merge x1 x5 (Curry_Prelude.OP_Cons x6 x7) x2000 x3250 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_19 x7 x5 x4 x1 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x6 x4 x1 x7 x5 x1002 x3000 x3250 x3500) (nd_OP__case_20 x6 x4 x1 x7 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x6 x4 x1 x7 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x6 x4 x1 x7 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_19 x7 x5 x4 x1 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x6 (d_C_merge x1 (Curry_Prelude.OP_Cons x4 x5) x7 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x7 x5 x4 x1 x6 x1002 x3250 x3500) (d_OP__case_19 x7 x5 x4 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x7 x5 x4 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x7 x5 x4 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_19 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_19 x7 x5 x4 x1 x6 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x6 (nd_C_merge x1 (Curry_Prelude.OP_Cons x4 x5) x7 x2000 x3250 x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x7 x5 x4 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_19 x7 x5 x4 x1 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x7 x5 x4 x1 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x7 x5 x4 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_22 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP_mergeSort_dot_mergeLists_dot_15 x1 (Curry_Prelude.OP_Cons (d_C_merge x1 x3 x5 x3250 x3500) (d_OP_mergeSort_dot_mergePairs_dot_15 x1 x6 x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x3 x1002 x3250 x3500) (d_OP__case_22 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_22 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_22 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP_mergeSort_dot_mergeLists_dot_15 x1 (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_merge x1 x3 x5 x2000 x3250 x3500) (nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x6 x2001 x3250 x3500))))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_22 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP__case_23 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.OP_Cons (d_C_merge x1 x3 x5 x3250 x3500) (d_OP_mergeSort_dot_mergePairs_dot_15 x1 x6 x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x3 x1002 x3250 x3500) (d_OP__case_23 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_23 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP__case_23 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (nd_C_merge x1 x3 x5 x2000 x3250 x3500) (nd_OP_mergeSort_dot_mergePairs_dot_15 x1 x6 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_23 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.Curry t0 => t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP__case_26 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_25 x5 x3 x1 x6 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x3 x1 x1002 x3250 x3500) (d_OP__case_26 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_26 :: Curry_Prelude.Curry t0 => t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP__case_26 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_25 x5 x3 x1 x6 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x5 x2001 x3250 x3500)))) x2003 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_26 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_26 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_26 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_26 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP__case_25 x5 x3 x1 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List)) (d_OP_mergeSort_dot_genRuns_dot_15 x1 x6 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_24 x6 x1 x3 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x5 x3 x1 x6 x1002 x3250 x3500) (d_OP__case_25 x5 x3 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x5 x3 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x5 x3 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_25 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP__case_25 x5 x3 x1 x6 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List)) (nd_OP_mergeSort_dot_genRuns_dot_15 x1 x6 x2000 x3250 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_24 x6 x1 x3 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_25 x5 x3 x1 x6 x1002 x3000 x3250 x3500) (nd_OP__case_25 x5 x3 x1 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_25 x5 x3 x1 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_25 x5 x3 x1 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP__case_24 x6 x1 x3 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x5 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List)) (d_OP_mergeSort_dot_genRuns_dot_15 x1 x6 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x6 x1 x3 x5 x1002 x3250 x3500) (d_OP__case_24 x6 x1 x3 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x6 x1 x3 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x6 x1 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_24 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_OP__case_24 x6 x1 x3 x5 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x5 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List)) (nd_OP_mergeSort_dot_genRuns_dot_15 x1 x6 x2000 x3250 x3500)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_24 x6 x1 x3 x5 x1002 x3000 x3250 x3500) (nd_OP__case_24 x6 x1 x3 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_24 x6 x1 x3 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_24 x6 x1 x3 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.Curry t0 => t0 -> t0 -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_OP__case_28 x2 x4 x1 x8 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x4 x7) x8
     Curry_Prelude.C_False -> d_OP__case_27 x8 x4 x7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x2 x4 x1 x8 x7 x1002 x3250 x3500) (d_OP__case_28 x2 x4 x1 x8 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x2 x4 x1 x8 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x2 x4 x1 x8 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_28 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP__case_28 x2 x4 x1 x8 x7 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x4 x7) x8
     Curry_Prelude.C_False -> d_OP__case_27 x8 x4 x7 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_28 x2 x4 x1 x8 x7 x1002 x3000 x3250 x3500) (nd_OP__case_28 x2 x4 x1 x8 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_28 x2 x4 x1 x8 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_28 x2 x4 x1 x8 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_OP__case_27 x8 x4 x7 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x7 (Curry_Prelude.OP_Cons x4 x8)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x8 x4 x7 x1002 x3250 x3500) (d_OP__case_27 x8 x4 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x8 x4 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x8 x4 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
