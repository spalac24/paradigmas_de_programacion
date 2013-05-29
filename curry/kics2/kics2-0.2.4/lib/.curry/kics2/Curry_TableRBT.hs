{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_TableRBT (C_TableRBT, d_C_emptyTableRBT, nd_C_emptyTableRBT, d_C_isEmptyTable, nd_C_isEmptyTable, d_C_lookupRBT, nd_C_lookupRBT, d_C_updateRBT, nd_C_updateRBT, d_C_tableRBT2list, nd_C_tableRBT2list, d_C_deleteRBT, nd_C_deleteRBT) where

import Basics
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
type C_TableRBT t0 t1 = Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)

d_C_emptyTableRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.C_Bool) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_emptyTableRBT x1 x3500 = Curry_RedBlackTree.d_C_empty (acceptCs id d_OP_emptyTableRBT_dot___hash_lambda1) (acceptCs id d_OP_emptyTableRBT_dot___hash_lambda2) (acceptCs id (d_OP_emptyTableRBT_dot___hash_lambda3 x1)) x3500

nd_C_emptyTableRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Func t0 Curry_Prelude.C_Bool) -> IDSupply -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)
nd_C_emptyTableRBT x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_RedBlackTree.nd_C_empty (wrapDX (wrapDX id) (acceptCs id d_OP_emptyTableRBT_dot___hash_lambda1)) (wrapDX (wrapDX id) (acceptCs id d_OP_emptyTableRBT_dot___hash_lambda2)) (wrapDX (wrapNX id) (acceptCs id (nd_OP_emptyTableRBT_dot___hash_lambda3 x1))) x2000 x3500))

d_OP_emptyTableRBT_dot___hash_lambda1 :: (Curry_Prelude.Curry t21,Curry_Prelude.Curry t22) => Curry_Prelude.OP_Tuple2 t21 t22 -> Curry_Prelude.OP_Tuple2 t21 t22 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_emptyTableRBT_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x1 x3500) (Curry_Prelude.d_C_fst x2 x3500) x3500

d_OP_emptyTableRBT_dot___hash_lambda2 :: (Curry_Prelude.Curry t21,Curry_Prelude.Curry t22) => Curry_Prelude.OP_Tuple2 t21 t22 -> Curry_Prelude.OP_Tuple2 t21 t22 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_emptyTableRBT_dot___hash_lambda2 x1 x2 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst x1 x3500) (Curry_Prelude.d_C_fst x2 x3500) x3500

d_OP_emptyTableRBT_dot___hash_lambda3 :: (Curry_Prelude.Curry t21,Curry_Prelude.Curry t22) => (t21 -> ConstStore -> t21 -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_Tuple2 t21 t22 -> Curry_Prelude.OP_Tuple2 t21 t22 -> ConstStore -> Curry_Prelude.C_Bool
d_OP_emptyTableRBT_dot___hash_lambda3 x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_fst x2 x3500) x3500) (Curry_Prelude.d_C_fst x3 x3500) x3500

nd_OP_emptyTableRBT_dot___hash_lambda3 :: (Curry_Prelude.Curry t21,Curry_Prelude.Curry t22) => Func t21 (Func t21 Curry_Prelude.C_Bool) -> Curry_Prelude.OP_Tuple2 t21 t22 -> Curry_Prelude.OP_Tuple2 t21 t22 -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_emptyTableRBT_dot___hash_lambda3 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.d_C_fst x2 x3500) x2000 x3500) (Curry_Prelude.d_C_fst x3 x3500) x2001 x3500)))))

d_C_isEmptyTable :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmptyTable x3500 = Curry_RedBlackTree.d_C_isEmpty

nd_C_isEmptyTable :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => IDSupply -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)) Curry_Prelude.C_Bool
nd_C_isEmptyTable x3000 x3500 = wrapNX id Curry_RedBlackTree.nd_C_isEmpty

d_C_lookupRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_Prelude.C_Maybe t1
d_C_lookupRBT x1 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_maybe Curry_Prelude.C_Nothing (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) Curry_Prelude.d_C_snd x3500)) (Curry_RedBlackTree.d_C_lookup (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_failed x3500))) x3500

nd_C_lookupRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> IDSupply -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_Prelude.C_Maybe t1)
nd_C_lookupRBT x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_maybe Curry_Prelude.C_Nothing (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id Curry_Prelude.C_Just)) (wrapDX id Curry_Prelude.d_C_snd) x2000 x3500))) (wrapNX id (Curry_RedBlackTree.nd_C_lookup (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_failed x3500)))) x2001 x3500)))))

d_C_updateRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_updateRBT x1 x2 x3500 = Curry_RedBlackTree.d_C_update (Curry_Prelude.OP_Tuple2 x1 x2)

nd_C_updateRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> IDSupply -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1))
nd_C_updateRBT x1 x2 x3000 x3500 = wrapNX id (Curry_RedBlackTree.nd_C_update (Curry_Prelude.OP_Tuple2 x1 x2))

d_C_tableRBT2list :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_tableRBT2list x3500 = Curry_RedBlackTree.d_C_tree2list

nd_C_tableRBT2list :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => IDSupply -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1))
nd_C_tableRBT2list x3000 x3500 = wrapNX id Curry_RedBlackTree.nd_C_tree2list

d_C_deleteRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_deleteRBT x1 x3500 = Curry_RedBlackTree.d_C_delete (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_failed x3500))

nd_C_deleteRBT :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> IDSupply -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_RedBlackTree.C_RedBlackTree (Curry_Prelude.OP_Tuple2 t0 t1))
nd_C_deleteRBT x1 x3000 x3500 = wrapNX id (Curry_RedBlackTree.nd_C_delete (Curry_Prelude.OP_Tuple2 x1 (Curry_Prelude.d_C_failed x3500)))
