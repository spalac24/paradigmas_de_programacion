{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_List (d_C_elemIndex, nd_C_elemIndex, d_C_elemIndices, nd_C_elemIndices, d_C_find, nd_C_find, d_C_findIndex, nd_C_findIndex, d_C_findIndices, nd_C_findIndices, d_C_nub, d_C_nubBy, nd_C_nubBy, d_C_delete, nd_C_delete, d_C_deleteBy, nd_C_deleteBy, d_OP_backslash_backslash, d_C_union, d_C_intersect, d_C_intersperse, d_C_intercalate, d_C_transpose, d_C_permutations, d_C_partition, nd_C_partition, d_C_group, nd_C_group, d_C_groupBy, nd_C_groupBy, d_C_splitOn, d_C_split, nd_C_split, d_C_inits, d_C_tails, d_C_replace, d_C_isPrefixOf, d_C_isSuffixOf, d_C_isInfixOf, d_C_sortBy, nd_C_sortBy, d_C_insertBy, nd_C_insertBy, d_C_last, d_C_init, d_C_sum, d_C_product, d_C_maximum, d_C_minimum, d_C_scanl, nd_C_scanl, d_C_scanl1, nd_C_scanl1, d_C_scanr, nd_C_scanr, d_C_scanr1, nd_C_scanr1, d_C_mapAccumL, nd_C_mapAccumL, d_C_mapAccumR, nd_C_mapAccumR, d_C_cycle, d_C_unfoldr, nd_C_unfoldr) where

import Basics
import qualified Curry_Maybe
import qualified Curry_Prelude
d_C_elemIndex :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int
d_C_elemIndex x1 x3500 = d_C_findIndex (Curry_Prelude.d_OP_eq_eq x1) x3500

nd_C_elemIndex :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.C_Maybe Curry_Prelude.C_Int)
nd_C_elemIndex x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_findIndex (wrapDX id (Curry_Prelude.d_OP_eq_eq x1)) x2000 x3500))

d_C_elemIndices :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_elemIndices x1 x3500 = d_C_findIndices (Curry_Prelude.d_OP_eq_eq x1)

nd_C_elemIndices :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_elemIndices x1 x3000 x3500 = wrapNX id (nd_C_findIndices (wrapDX id (Curry_Prelude.d_OP_eq_eq x1)))

d_C_find :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Maybe t0
d_C_find x1 x3500 = Curry_Prelude.d_OP_dot Curry_Maybe.d_C_listToMaybe (Curry_Prelude.d_C_filter x1) x3500

nd_C_find :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.C_Maybe t0)
nd_C_find x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Maybe.d_C_listToMaybe) (wrapNX id (Curry_Prelude.nd_C_filter x1)) x2000 x3500))

d_C_findIndex :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int
d_C_findIndex x1 x3500 = Curry_Prelude.d_OP_dot Curry_Maybe.d_C_listToMaybe (d_C_findIndices x1) x3500

nd_C_findIndex :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.C_Maybe Curry_Prelude.C_Int)
nd_C_findIndex x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Maybe.d_C_listToMaybe) (wrapNX id (nd_C_findIndices x1)) x2000 x3500))

d_C_findIndices :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_findIndices x1 x2 x3500 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_findIndices_dot___hash_lambda3 x1)) Curry_Prelude.OP_List (Curry_Prelude.d_C_zip x2 (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 0#) x3500) x3500) x3500

nd_C_findIndices :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
nd_C_findIndices x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_findIndices_dot___hash_lambda3 x1))) Curry_Prelude.OP_List (Curry_Prelude.d_C_zip x2 (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 0#) x3500) x3500) x2000 x3500))

d_OP_findIndices_dot___hash_lambda3 :: Curry_Prelude.Curry t6 => (t6 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_Tuple2 t6 Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_findIndices_dot___hash_lambda3 x1 x2 x3 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_21 x1 x4 x5 (Curry_Prelude.d_C_apply x1 x4 x3500) x3500) x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_findIndices_dot___hash_lambda3 x1 x1002 x3 x3500) (d_OP_findIndices_dot___hash_lambda3 x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_findIndices_dot___hash_lambda3 x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_findIndices_dot___hash_lambda3 x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_findIndices_dot___hash_lambda3 :: Curry_Prelude.Curry t6 => Func t6 Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 t6 Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
nd_OP_findIndices_dot___hash_lambda3 x1 x2 x3 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_21 x1 x4 x5 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x2001 x3500)))) x3 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_findIndices_dot___hash_lambda3 x1 x1002 x3 x3000 x3500) (nd_OP_findIndices_dot___hash_lambda3 x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_findIndices_dot___hash_lambda3 x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_findIndices_dot___hash_lambda3 x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_nub :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_nub x1 x3500 = d_C_nubBy (acceptCs id Curry_Prelude.d_OP_eq_eq) x1 x3500

d_C_nubBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_nubBy x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.OP_Cons x3 (d_C_nubBy x1 (Curry_Prelude.d_C_filter (d_OP_nubBy_dot___hash_lambda5 x1 x3) x4 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_nubBy x1 x1002 x3500) (d_C_nubBy x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_nubBy x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_nubBy x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_nubBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_nubBy x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Cons x3 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_nubBy x1 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_nubBy_dot___hash_lambda5 x1 x3)) x4 x2000 x3500) x2001 x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_nubBy x1 x1002 x3000 x3500) (nd_C_nubBy x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_nubBy x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_nubBy x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_nubBy_dot___hash_lambda5 :: Curry_Prelude.Curry t44 => (t44 -> ConstStore -> t44 -> ConstStore -> Curry_Prelude.C_Bool) -> t44 -> t44 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_nubBy_dot___hash_lambda5 x1 x2 x3 x3500 = Curry_Prelude.d_C_not (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x3 x3500) x3500

nd_OP_nubBy_dot___hash_lambda5 :: Curry_Prelude.Curry t44 => Func t44 (Func t44 Curry_Prelude.C_Bool) -> t44 -> t44 -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_nubBy_dot___hash_lambda5 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (Curry_Prelude.d_C_not (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x3 x2001 x3500)))) x3500))

d_C_delete :: Curry_Prelude.Curry t0 => ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_delete x3500 = acceptCs id (d_C_deleteBy (acceptCs id Curry_Prelude.d_OP_eq_eq))

nd_C_delete :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func t0 (Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0))
nd_C_delete x3000 x3500 = wrapDX (wrapNX id) (acceptCs id (nd_C_deleteBy (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq))))

d_C_deleteBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_deleteBy x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_20 x1 x2 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteBy x1 x2 x1002 x3500) (d_C_deleteBy x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteBy x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteBy x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_deleteBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_deleteBy x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_20 x1 x2 x4 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_deleteBy x1 x2 x1002 x3000 x3500) (nd_C_deleteBy x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_deleteBy x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_deleteBy x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_backslash_backslash :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_backslash_backslash x1 x2 x3500 = Curry_Prelude.d_C_foldl (acceptCs id (Curry_Prelude.d_C_flip (d_C_delete x3500))) x1 x2 x3500

d_C_union :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_union x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_19 x2 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x3 x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_union x1002 x2 x3500) (d_C_union x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_union z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_union x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_intersect :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_intersect x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_18 x2 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x3 x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_intersect x1002 x2 x3500) (d_C_intersect x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_intersect z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_intersect x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_intersperse :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_intersperse x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_17 x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_intersperse x1 x1002 x3500) (d_C_intersperse x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_intersperse x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_intersperse x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_intercalate :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> ConstStore -> Curry_Prelude.OP_List t0
d_C_intercalate x1 x2 x3500 = Curry_Prelude.d_C_concat (d_C_intersperse x1 x2 x3500) x3500

d_C_transpose :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_transpose x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_16 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_transpose x1002 x3500) (d_C_transpose x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_transpose z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_transpose x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_permutations :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_permutations x1 x3500 = Curry_Prelude.OP_Cons x1 (d_OP_permutations_dot_perms_dot_57 x1 Curry_Prelude.OP_List x3500)

d_OP_permutations_dot_perms_dot_57 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_OP_permutations_dot_perms_dot_57 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_C_foldr (acceptCs id (d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62 x3 x4)) (d_OP_permutations_dot_perms_dot_57 x4 (Curry_Prelude.OP_Cons x3 x2) x3500) (d_C_permutations x2 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_permutations_dot_perms_dot_57 x1002 x2 x3500) (d_OP_permutations_dot_perms_dot_57 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_permutations_dot_perms_dot_57 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_permutations_dot_perms_dot_57 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 :: (Curry_Prelude.Curry t161,Curry_Prelude.Curry t0) => t161 -> Curry_Prelude.OP_List t161 -> (Curry_Prelude.OP_List t161 -> ConstStore -> t0) -> Curry_Prelude.OP_List t161 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t161) (Curry_Prelude.OP_List t0)
d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x4 x5 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 x5
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 (Curry_Prelude.d_OP_dot x3 (acceptCs id (Curry_Prelude.OP_Cons x6)) x3500) x7 x5 x3500
          x9 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x8 x3500
          x10 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x8 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x6 x9) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply x3 (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x6 x9)) x3500) x10))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1002 x5 x3500) (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1003 x5 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 z x5 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1002 x5) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 :: (Curry_Prelude.Curry t161,Curry_Prelude.Curry t0) => t161 -> Curry_Prelude.OP_List t161 -> Func (Curry_Prelude.OP_List t161) t0 -> Curry_Prelude.OP_List t161 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t161) (Curry_Prelude.OP_List t0)
nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x4 x5 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 x5
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x8 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 (Curry_Prelude.nd_OP_dot x3 (wrapDX id (acceptCs id (Curry_Prelude.OP_Cons x6))) x2000 x3500) x7 x5 x2001 x3500)))
                    x9 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x8 x3500
                    x10 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x8 x3500
                     in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x6 x9) (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_apply x3 (Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x6 x9)) x2003 x3500) x10)))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1002 x5 x3000 x3500) (nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1003 x5 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 z x5 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 x3 x1002 x5 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us :: (Curry_Prelude.Curry t167,Curry_Prelude.Curry t161) => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t161) (Curry_Prelude.OP_List t167) -> ConstStore -> Curry_Prelude.OP_List t161
d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x1002 x3500) (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP2_hash_us x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs :: (Curry_Prelude.Curry t161,Curry_Prelude.Curry t167) => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t161) (Curry_Prelude.OP_List t167) -> ConstStore -> Curry_Prelude.OP_List t167
d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x1002 x3500) (d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62_dot___hash_selFP3_hash_zs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62 :: Curry_Prelude.Curry t161 => t161 -> Curry_Prelude.OP_List t161 -> Curry_Prelude.OP_List t161 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t161) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t161)
d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62 x1 x2 x3 x4 x3500 = let
     x5 = d_OP_permutations_dot_perms_dot_57_dot_interleave'_dot_62 x1 x2 Curry_Prelude.d_C_id x3 x4 x3500
      in (d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs x5 x3500)

d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs :: Curry_Prelude.Curry t161 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t161) (Curry_Prelude.OP_List (Curry_Prelude.OP_List t161)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t161)
d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs x1002 x3500) (d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_permutations_dot_perms_dot_57_dot_interleave_dot_62_dot___hash_selFP5_hash_zs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_partition :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
d_C_partition x1 x2 x3500 = Curry_Prelude.d_C_foldr (acceptCs id (d_OP_partition_dot_select_dot_76 x1)) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x2 x3500

nd_C_partition :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_C_partition x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_OP_partition_dot_select_dot_76 x1))) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) x2 x2000 x3500))

d_OP_partition_dot_select_dot_76 :: Curry_Prelude.Curry t211 => (t211 -> ConstStore -> Curry_Prelude.C_Bool) -> t211 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t211) (Curry_Prelude.OP_List t211) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t211) (Curry_Prelude.OP_List t211)
d_OP_partition_dot_select_dot_76 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_15 x1 x2 x4 x5 (Curry_Prelude.d_C_apply x1 x2 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_partition_dot_select_dot_76 x1 x2 x1002 x3500) (d_OP_partition_dot_select_dot_76 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_partition_dot_select_dot_76 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_partition_dot_select_dot_76 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_partition_dot_select_dot_76 :: Curry_Prelude.Curry t211 => Func t211 Curry_Prelude.C_Bool -> t211 -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t211) (Curry_Prelude.OP_List t211) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t211) (Curry_Prelude.OP_List t211)
nd_OP_partition_dot_select_dot_76 x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_15 x1 x2 x4 x5 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_partition_dot_select_dot_76 x1 x2 x1002 x3000 x3500) (nd_OP_partition_dot_select_dot_76 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_partition_dot_select_dot_76 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_partition_dot_select_dot_76 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_group :: Curry_Prelude.Curry t0 => ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_group x3500 = d_C_groupBy (acceptCs id Curry_Prelude.d_OP_eq_eq)

nd_C_group :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List (Curry_Prelude.OP_List t0))
nd_C_group x3000 x3500 = wrapNX id (nd_C_groupBy (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)))

d_C_groupBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_groupBy x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_C_span (Curry_Prelude.d_C_apply x1 x3 x3500) x4 x3500
          x6 = d_OP_groupBy_dot___hash_selFP7_hash_ys x5 x3500
          x7 = d_OP_groupBy_dot___hash_selFP8_hash_zs x5 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 x6) (d_C_groupBy x1 x7 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_groupBy x1 x1002 x3500) (d_C_groupBy x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_groupBy x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_groupBy x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_groupBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_C_groupBy x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x5 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_span (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4 x2001 x3500)))
                    x6 = d_OP_groupBy_dot___hash_selFP7_hash_ys x5 x3500
                    x7 = d_OP_groupBy_dot___hash_selFP8_hash_zs x5 x3500
                     in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 x6) (nd_C_groupBy x1 x7 x2003 x3500)))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_groupBy x1 x1002 x3000 x3500) (nd_C_groupBy x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_groupBy x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_groupBy x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_groupBy_dot___hash_selFP7_hash_ys :: Curry_Prelude.Curry t219 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t219) (Curry_Prelude.OP_List t219) -> ConstStore -> Curry_Prelude.OP_List t219
d_OP_groupBy_dot___hash_selFP7_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_groupBy_dot___hash_selFP7_hash_ys x1002 x3500) (d_OP_groupBy_dot___hash_selFP7_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_groupBy_dot___hash_selFP7_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_groupBy_dot___hash_selFP7_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_groupBy_dot___hash_selFP8_hash_zs :: Curry_Prelude.Curry t219 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t219) (Curry_Prelude.OP_List t219) -> ConstStore -> Curry_Prelude.OP_List t219
d_OP_groupBy_dot___hash_selFP8_hash_zs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_groupBy_dot___hash_selFP8_hash_zs x1002 x3500) (d_OP_groupBy_dot___hash_selFP8_hash_zs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_groupBy_dot___hash_selFP8_hash_zs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_groupBy_dot___hash_selFP8_hash_zs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitOn :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_splitOn x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_14 x1 x2 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitOn x1002 x2 x3500) (d_C_splitOn x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitOn z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitOn x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitOn_dot_go_dot_96 :: Curry_Prelude.Curry t297 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t297 -> Curry_Prelude.OP_List t297 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t297)
d_OP_splitOn_dot_go_dot_96 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_13 x1 x2 x3 x4 x5 (d_C_isPrefixOf x2 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitOn_dot_go_dot_96 x1 x2 x1002 x3500) (d_OP_splitOn_dot_go_dot_96 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitOn_dot_go_dot_96 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitOn_dot_go_dot_96 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs :: Curry_Prelude.Curry t297 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t297) -> ConstStore -> Curry_Prelude.OP_List t297
d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x1002 x3500) (d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss :: Curry_Prelude.Curry t297 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t297) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t297)
d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x3
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x1002 x3500) (d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_split :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_split x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_11 x1 x3 x4 (Curry_Prelude.d_C_apply x1 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_split x1 x1002 x3500) (d_C_split x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_split x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_split x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_split :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
nd_C_split x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_11 x1 x3 x4 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_split x1 x1002 x3000 x3500) (nd_C_split x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_split x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_split x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_split_dot___hash_selFP13_hash_ys :: Curry_Prelude.Curry t239 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t239) -> ConstStore -> Curry_Prelude.OP_List t239
d_OP_split_dot___hash_selFP13_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_split_dot___hash_selFP13_hash_ys x1002 x3500) (d_OP_split_dot___hash_selFP13_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_split_dot___hash_selFP13_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_split_dot___hash_selFP13_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_split_dot___hash_selFP14_hash_yss :: Curry_Prelude.Curry t239 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t239) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t239)
d_OP_split_dot___hash_selFP14_hash_yss x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x3
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_split_dot___hash_selFP14_hash_yss x1002 x3500) (d_OP_split_dot___hash_selFP14_hash_yss x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_split_dot___hash_selFP14_hash_yss z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_split_dot___hash_selFP14_hash_yss x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_inits :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_inits x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List) (Curry_Prelude.d_C_map (acceptCs id (Curry_Prelude.OP_Cons x2)) (d_C_inits x3 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_inits x1002 x3500) (d_C_inits x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_inits z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_inits x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_tails :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0)
d_C_tails x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.OP_Cons x1 (d_C_tails x3 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tails x1002 x3500) (d_C_tails x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tails z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tails x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_replace :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_replace x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_9 x1 x2 x4 x5 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_replace x1 x2 x1002 x3500) (d_C_replace x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_replace x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_replace x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isPrefixOf :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isPrefixOf x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_7 x3 x4 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isPrefixOf x1002 x2 x3500) (d_C_isPrefixOf x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isPrefixOf z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isPrefixOf x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isSuffixOf :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSuffixOf x1 x2 x3500 = d_C_isPrefixOf (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x1 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x2 x3500) x3500

d_C_isInfixOf :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isInfixOf x1 x2 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (d_C_isPrefixOf x1) x3500) (d_C_tails x2 x3500) x3500

d_C_sortBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sortBy x1 x3500 = Curry_Prelude.d_C_foldr (acceptCs id (d_C_insertBy x1)) Curry_Prelude.OP_List

nd_C_sortBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_C_sortBy x1 x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id (nd_C_insertBy x1))) Curry_Prelude.OP_List)

d_C_insertBy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_insertBy x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_6 x1 x2 x4 x5 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_insertBy x1 x2 x1002 x3500) (d_C_insertBy x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_insertBy x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_insertBy x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_insertBy :: Curry_Prelude.Curry t0 => Func t0 (Func t0 Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_insertBy x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_OP__case_6 x1 x2 x4 x5 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_insertBy x1 x2 x1002 x3000 x3500) (nd_C_insertBy x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_insertBy x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_insertBy x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_last :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_last x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_5 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_last x1002 x3500) (d_C_last x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_last z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_last x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_init :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_init x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_init x1002 x3500) (d_C_init x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_init z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_init x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_sum :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_sum x1 x3500 = Curry_Prelude.d_C_foldl (acceptCs id Curry_Prelude.d_OP_plus) (Curry_Prelude.C_Int 0#) x1 x3500

d_C_product :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_product x1 x3500 = Curry_Prelude.d_C_foldl (acceptCs id Curry_Prelude.d_OP_star) (Curry_Prelude.C_Int 1#) x1 x3500

d_C_maximum :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_maximum x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_C_foldl1 (acceptCs id Curry_Prelude.d_C_max) x1 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maximum x1002 x3500) (d_C_maximum x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maximum z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maximum x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_minimum :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> t0
d_C_minimum x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_C_foldl1 (acceptCs id Curry_Prelude.d_C_max) x1 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_minimum x1002 x3500) (d_C_minimum x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_minimum z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_minimum x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanl :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> t1 -> ConstStore -> t0) -> t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_scanl x1 x2 x3 x3500 = Curry_Prelude.OP_Cons x2 (d_OP__case_3 x1 x2 x3 x3500)

nd_C_scanl :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Func t1 t0) -> t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_scanl x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Cons x2 (nd_OP__case_3 x1 x2 x3 x2000 x3500)))

d_C_scanl1 :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> t0) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_scanl1 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_C_scanl x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanl1 x1 x1002 x3500) (d_C_scanl1 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanl1 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanl1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_scanl1 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 t0) -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_scanl1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_scanl x1 x3 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_scanl1 x1 x1002 x3000 x3500) (nd_C_scanl1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_scanl1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_scanl1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanr :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t1 -> ConstStore -> t1) -> t1 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_scanr x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_C_scanr x1 x2 x5 x3500
          x7 = d_OP_scanr_dot___hash_selFP16_hash_qs x6 x3500
          x8 = d_OP_scanr_dot___hash_selFP17_hash_q x6 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3500) x8 x3500) x7)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanr x1 x2 x1002 x3500) (d_C_scanr x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanr x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanr x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_scanr :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Func t1 t1) -> t1 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t1
nd_C_scanr x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x6 = nd_C_scanr x1 x2 x5 x2000 x3500
                    x7 = d_OP_scanr_dot___hash_selFP16_hash_qs x6 x3500
                    x8 = d_OP_scanr_dot___hash_selFP17_hash_q x6 x3500
                     in (Curry_Prelude.OP_Cons (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2001 x3500) x8 x2002 x3500)))) x7))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_scanr x1 x2 x1002 x3000 x3500) (nd_C_scanr x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_scanr x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_scanr x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanr_dot___hash_selFP16_hash_qs :: Curry_Prelude.Curry t458 => Curry_Prelude.OP_List t458 -> ConstStore -> Curry_Prelude.OP_List t458
d_OP_scanr_dot___hash_selFP16_hash_qs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanr_dot___hash_selFP16_hash_qs x1002 x3500) (d_OP_scanr_dot___hash_selFP16_hash_qs x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanr_dot___hash_selFP16_hash_qs z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanr_dot___hash_selFP16_hash_qs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanr_dot___hash_selFP17_hash_q :: Curry_Prelude.Curry t458 => Curry_Prelude.OP_List t458 -> ConstStore -> t458
d_OP_scanr_dot___hash_selFP17_hash_q x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanr_dot___hash_selFP17_hash_q x1002 x3500) (d_OP_scanr_dot___hash_selFP17_hash_q x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanr_dot___hash_selFP17_hash_q z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanr_dot___hash_selFP17_hash_q x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_scanr1 :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0 -> ConstStore -> t0) -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_scanr1 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x1 x3 x4 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_scanr1 x1 x1002 x3500) (d_C_scanr1 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_scanr1 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_scanr1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_scanr1 :: Curry_Prelude.Curry t0 => Func t0 (Func t0 t0) -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_scanr1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 x3 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_scanr1 x1 x1002 x3000 x3500) (nd_C_scanr1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_scanr1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_scanr1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanr1_dot___hash_selFP19_hash_qs :: Curry_Prelude.Curry t482 => Curry_Prelude.OP_List t482 -> ConstStore -> Curry_Prelude.OP_List t482
d_OP_scanr1_dot___hash_selFP19_hash_qs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanr1_dot___hash_selFP19_hash_qs x1002 x3500) (d_OP_scanr1_dot___hash_selFP19_hash_qs x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanr1_dot___hash_selFP19_hash_qs z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanr1_dot___hash_selFP19_hash_qs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_scanr1_dot___hash_selFP20_hash_q :: Curry_Prelude.Curry t482 => Curry_Prelude.OP_List t482 -> ConstStore -> t482
d_OP_scanr1_dot___hash_selFP20_hash_q x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_scanr1_dot___hash_selFP20_hash_q x1002 x3500) (d_OP_scanr1_dot___hash_selFP20_hash_q x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_scanr1_dot___hash_selFP20_hash_q z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_scanr1_dot___hash_selFP20_hash_q x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mapAccumL :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0,Curry_Prelude.Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t2) -> t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t2)
d_C_mapAccumL x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x4 x3500
          x7 = d_OP_mapAccumL_dot___hash_selFP25_hash_s' x6 x3500
          x8 = d_OP_mapAccumL_dot___hash_selFP26_hash_y x6 x3500
          x9 = d_C_mapAccumL x1 x7 x5 x3500
          x10 = d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x9 x3500
          x11 = d_OP_mapAccumL_dot___hash_selFP24_hash_ys x9 x3500
           in (Curry_Prelude.OP_Tuple2 x10 (Curry_Prelude.OP_Cons x8 x11))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mapAccumL x1 x2 x1002 x3500) (d_C_mapAccumL x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mapAccumL x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mapAccumL x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_mapAccumL :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0,Curry_Prelude.Curry t2) => Func t0 (Func t1 (Curry_Prelude.OP_Tuple2 t0 t2)) -> t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t2)
nd_C_mapAccumL x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (let
                    x6 = let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))
                    x7 = d_OP_mapAccumL_dot___hash_selFP25_hash_s' x6 x3500
                    x8 = d_OP_mapAccumL_dot___hash_selFP26_hash_y x6 x3500
                    x9 = nd_C_mapAccumL x1 x7 x5 x2003 x3500
                    x10 = d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x9 x3500
                    x11 = d_OP_mapAccumL_dot___hash_selFP24_hash_ys x9 x3500
                     in (Curry_Prelude.OP_Tuple2 x10 (Curry_Prelude.OP_Cons x8 x11)))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mapAccumL x1 x2 x1002 x3000 x3500) (nd_C_mapAccumL x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mapAccumL x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mapAccumL x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumL_dot___hash_selFP25_hash_s' :: (Curry_Prelude.Curry t503,Curry_Prelude.Curry t498) => Curry_Prelude.OP_Tuple2 t498 t503 -> ConstStore -> t498
d_OP_mapAccumL_dot___hash_selFP25_hash_s' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumL_dot___hash_selFP25_hash_s' x1002 x3500) (d_OP_mapAccumL_dot___hash_selFP25_hash_s' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumL_dot___hash_selFP25_hash_s' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumL_dot___hash_selFP25_hash_s' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumL_dot___hash_selFP26_hash_y :: (Curry_Prelude.Curry t498,Curry_Prelude.Curry t503) => Curry_Prelude.OP_Tuple2 t498 t503 -> ConstStore -> t503
d_OP_mapAccumL_dot___hash_selFP26_hash_y x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumL_dot___hash_selFP26_hash_y x1002 x3500) (d_OP_mapAccumL_dot___hash_selFP26_hash_y x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumL_dot___hash_selFP26_hash_y z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumL_dot___hash_selFP26_hash_y x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumL_dot___hash_selFP23_hash_s'' :: (Curry_Prelude.Curry t503,Curry_Prelude.Curry t498) => Curry_Prelude.OP_Tuple2 t498 (Curry_Prelude.OP_List t503) -> ConstStore -> t498
d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x1002 x3500) (d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumL_dot___hash_selFP23_hash_s'' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumL_dot___hash_selFP23_hash_s'' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumL_dot___hash_selFP24_hash_ys :: (Curry_Prelude.Curry t498,Curry_Prelude.Curry t503) => Curry_Prelude.OP_Tuple2 t498 (Curry_Prelude.OP_List t503) -> ConstStore -> Curry_Prelude.OP_List t503
d_OP_mapAccumL_dot___hash_selFP24_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumL_dot___hash_selFP24_hash_ys x1002 x3500) (d_OP_mapAccumL_dot___hash_selFP24_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumL_dot___hash_selFP24_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumL_dot___hash_selFP24_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mapAccumR :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0,Curry_Prelude.Curry t2) => (t0 -> ConstStore -> t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t2) -> t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t2)
d_C_mapAccumR x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_C_mapAccumR x1 x2 x5 x3500
          x7 = d_OP_mapAccumR_dot___hash_selFP31_hash_s' x6 x3500
          x8 = d_OP_mapAccumR_dot___hash_selFP32_hash_ys x6 x3500
          x9 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x7 x3500) x4 x3500
          x10 = d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x9 x3500
          x11 = d_OP_mapAccumR_dot___hash_selFP30_hash_y x9 x3500
           in (Curry_Prelude.OP_Tuple2 x10 (Curry_Prelude.OP_Cons x11 x8))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mapAccumR x1 x2 x1002 x3500) (d_C_mapAccumR x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mapAccumR x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mapAccumR x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_mapAccumR :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0,Curry_Prelude.Curry t2) => Func t0 (Func t1 (Curry_Prelude.OP_Tuple2 t0 t2)) -> t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_List t2)
nd_C_mapAccumR x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x6 = nd_C_mapAccumR x1 x2 x5 x2000 x3500
                    x7 = d_OP_mapAccumR_dot___hash_selFP31_hash_s' x6 x3500
                    x8 = d_OP_mapAccumR_dot___hash_selFP32_hash_ys x6 x3500
                    x9 = let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x7 x2001 x3500) x4 x2002 x3500)))
                    x10 = d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x9 x3500
                    x11 = d_OP_mapAccumR_dot___hash_selFP30_hash_y x9 x3500
                     in (Curry_Prelude.OP_Tuple2 x10 (Curry_Prelude.OP_Cons x11 x8)))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mapAccumR x1 x2 x1002 x3000 x3500) (nd_C_mapAccumR x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mapAccumR x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mapAccumR x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumR_dot___hash_selFP31_hash_s' :: (Curry_Prelude.Curry t530,Curry_Prelude.Curry t522) => Curry_Prelude.OP_Tuple2 t522 (Curry_Prelude.OP_List t530) -> ConstStore -> t522
d_OP_mapAccumR_dot___hash_selFP31_hash_s' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumR_dot___hash_selFP31_hash_s' x1002 x3500) (d_OP_mapAccumR_dot___hash_selFP31_hash_s' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumR_dot___hash_selFP31_hash_s' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumR_dot___hash_selFP31_hash_s' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumR_dot___hash_selFP32_hash_ys :: (Curry_Prelude.Curry t522,Curry_Prelude.Curry t530) => Curry_Prelude.OP_Tuple2 t522 (Curry_Prelude.OP_List t530) -> ConstStore -> Curry_Prelude.OP_List t530
d_OP_mapAccumR_dot___hash_selFP32_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumR_dot___hash_selFP32_hash_ys x1002 x3500) (d_OP_mapAccumR_dot___hash_selFP32_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumR_dot___hash_selFP32_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumR_dot___hash_selFP32_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumR_dot___hash_selFP29_hash_s'' :: (Curry_Prelude.Curry t530,Curry_Prelude.Curry t522) => Curry_Prelude.OP_Tuple2 t522 t530 -> ConstStore -> t522
d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x1002 x3500) (d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumR_dot___hash_selFP29_hash_s'' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumR_dot___hash_selFP29_hash_s'' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_mapAccumR_dot___hash_selFP30_hash_y :: (Curry_Prelude.Curry t522,Curry_Prelude.Curry t530) => Curry_Prelude.OP_Tuple2 t522 t530 -> ConstStore -> t530
d_OP_mapAccumR_dot___hash_selFP30_hash_y x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_mapAccumR_dot___hash_selFP30_hash_y x1002 x3500) (d_OP_mapAccumR_dot___hash_selFP30_hash_y x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_mapAccumR_dot___hash_selFP30_hash_y z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_mapAccumR_dot___hash_selFP30_hash_y x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_cycle :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_cycle x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = Curry_Prelude.d_OP_plus_plus x1 x4 x3500
           in x4
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cycle x1002 x3500) (d_C_cycle x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cycle z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cycle x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_unfoldr :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t1 t0)) -> t0 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_unfoldr x1 x2 x3500 = d_OP__case_1 x1 x2 (Curry_Prelude.d_C_apply x1 x2 x3500) x3500

nd_C_unfoldr :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t1 t0)) -> t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t1
nd_C_unfoldr x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_1 x1 x2 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x2001 x3500)))))

d_OP__case_1 x1 x2 x4 x3500 = case x4 of
     (Curry_Prelude.C_Just x3) -> d_OP__case_0 x1 x3 x3500
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x1002 x3500) (d_OP__case_1 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.C_Just x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x3 x2000 x3500))
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Cons x4 (d_C_unfoldr x1 x5 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x4 (nd_C_unfoldr x1 x5 x2000 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = d_C_scanr1 x1 x4 x3500
          x8 = d_OP_scanr1_dot___hash_selFP19_hash_qs x7 x3500
          x9 = d_OP_scanr1_dot___hash_selFP20_hash_q x7 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3500) x9 x3500) x8)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x3 x1002 x3500) (d_OP__case_2 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2000 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2000 (seq x2003 (let
                    x7 = nd_C_scanr1 x1 x4 x2000 x3500
                    x8 = d_OP_scanr1_dot___hash_selFP19_hash_qs x7 x3500
                    x9 = d_OP_scanr1_dot___hash_selFP20_hash_q x7 x3500
                     in (Curry_Prelude.OP_Cons (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2001 x3500) x9 x2002 x3500)))) x8))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x3 x1002 x3000 x3500) (nd_OP__case_2 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_C_scanl x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x4 x3500) x5 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x1002 x3500) (d_OP__case_3 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (nd_C_scanl x1 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))) x5 x2003 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.OP_Cons x2 (d_C_init (Curry_Prelude.OP_Cons x4 x5) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3500) (d_OP__case_4 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.OP_Cons x2 (d_C_init (Curry_Prelude.OP_Cons x4 x5) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x2 x1002 x3000 x3500) (nd_OP__case_4 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> d_C_last (Curry_Prelude.OP_Cons x4 x5) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x1002 x3500) (d_OP__case_5 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x4 x5) -> d_C_last (Curry_Prelude.OP_Cons x4 x5) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x1002 x3000 x3500) (nd_OP__case_5 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons x4 x5)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x4 (d_C_insertBy x1 x2 x5 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x4 x5 x1002 x3500) (d_OP__case_6 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x2 (Curry_Prelude.OP_Cons x4 x5)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x4 (nd_C_insertBy x1 x2 x5 x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x3 x4 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 x5 x3500) (d_C_isPrefixOf x4 x6 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x3 x4 x1002 x3500) (d_OP__case_7 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x3 x4 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x3 x5 x3500) (d_C_isPrefixOf x4 x6 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x3 x4 x1002 x3000 x3500) (nd_OP__case_7 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 x5
     Curry_Prelude.C_False -> d_OP__case_8 x1 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x2 x4 x5 x1002 x3500) (d_OP__case_9 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 x5
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_9 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x4 (d_C_replace x1 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x5 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x4 x5 x1002 x3500) (d_OP__case_8 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x4 (d_C_replace x1 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x5 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List (d_C_split x1 x4 x3500)
     Curry_Prelude.C_False -> d_OP__case_10 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x3 x4 x1002 x3500) (d_OP__case_11 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons Curry_Prelude.OP_List (nd_C_split x1 x4 x2000 x3500)))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x1 x3 x4 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_11 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x1 x3 x4 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x5 = d_C_split x1 x4 x3500
          x6 = d_OP_split_dot___hash_selFP13_hash_ys x5 x3500
          x7 = d_OP_split_dot___hash_selFP14_hash_yss x5 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 x6) x7)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x3 x4 x1002 x3500) (d_OP__case_10 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x1 x3 x4 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (let
               x5 = nd_C_split x1 x4 x2000 x3500
               x6 = d_OP_split_dot___hash_selFP13_hash_ys x5 x3500
               x7 = d_OP_split_dot___hash_selFP14_hash_yss x5 x3500
                in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x3 x6) x7)))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_10 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List (d_OP_splitOn_dot_go_dot_96 x1 x2 (Curry_Prelude.d_C_drop x1 x3 x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_12 x1 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_13 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List (d_OP_splitOn_dot_go_dot_96 x1 x2 (Curry_Prelude.d_C_drop x1 x3 x3500) x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x2 x4 x5 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_13 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x4 x5 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x6 = d_OP_splitOn_dot_go_dot_96 x1 x2 x5 x3500
          x7 = d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x6 x3500
          x8 = d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x6 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 x7) x8)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x2 x4 x5 x1002 x3500) (d_OP__case_12 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x4 x5 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x6 = d_OP_splitOn_dot_go_dot_96 x1 x2 x5 x3500
          x7 = d_OP_splitOn_dot_go_dot_96_dot___hash_selFP10_hash_zs x6 x3500
          x8 = d_OP_splitOn_dot_go_dot_96_dot___hash_selFP11_hash_zss x6 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 x7) x8)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_12 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_C_split (Curry_Prelude.d_OP_eq_eq x3) x2 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = Curry_Prelude.d_C_length x1 x3500
           in (d_OP_splitOn_dot_go_dot_96 x7 x1 x2 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x2 x3 x1002 x3500) (d_OP__case_14 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_C_split (wrapDX id (Curry_Prelude.d_OP_eq_eq x3)) x2 x2000 x3500))
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = Curry_Prelude.d_C_length x1 x3500
           in (d_OP_splitOn_dot_go_dot_96 x7 x1 x2 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_14 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x4) x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Cons x2 x5)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x2 x4 x5 x1002 x3500) (d_OP__case_15 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x4) x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Cons x2 x5)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_15 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x3 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_C_transpose x3 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 (Curry_Prelude.d_C_map Curry_Prelude.d_C_head x3 x3500)) (d_C_transpose (Curry_Prelude.OP_Cons x5 (Curry_Prelude.d_C_map Curry_Prelude.d_C_tail x3 x3500)) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x1002 x3500) (d_OP__case_16 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x3 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_C_transpose x3 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_head) x3 x2000 x3500)) (d_C_transpose (Curry_Prelude.OP_Cons x5 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_tail) x3 x2001 x3500)) x3500))))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x3 x1002 x3000 x3500) (nd_OP__case_16 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x1 x3 x4 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x1 (d_C_intersperse x1 (Curry_Prelude.OP_Cons x5 x6) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x3 x1002 x3500) (d_OP__case_17 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x1 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x5 x6) -> Curry_Prelude.OP_Cons x3 (Curry_Prelude.OP_Cons x1 (d_C_intersperse x1 (Curry_Prelude.OP_Cons x5 x6) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x1 x3 x1002 x3000 x3500) (nd_OP__case_17 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x3 (d_C_intersect x4 x2 x3500)
     Curry_Prelude.C_False -> d_C_intersect x4 x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x2 x3 x4 x1002 x3500) (d_OP__case_18 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x3 (d_C_intersect x4 x2 x3500)
     Curry_Prelude.C_False -> d_C_intersect x4 x2 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_18 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_union x4 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x3 (d_C_union x4 x2 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x2 x3 x4 x1002 x3500) (d_OP__case_19 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> d_C_union x4 x2 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x3 (d_C_union x4 x2 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_19 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x4 (d_C_deleteBy x1 x2 x5 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x2 x4 x5 x1002 x3500) (d_OP__case_20 x1 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x4 (nd_C_deleteBy x1 x2 x5 x2000 x3500)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_20 x1 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x4 x5 x1002 x3500) (d_OP__case_21 x1 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x4 x5 x1002 x3000 x3500) (nd_OP__case_21 x1 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
