{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SetRBT (C_SetRBT, d_C_emptySetRBT, nd_C_emptySetRBT, d_C_isEmptySetRBT, nd_C_isEmptySetRBT, d_C_elemRBT, nd_C_elemRBT, d_C_insertRBT, nd_C_insertRBT, d_C_insertMultiRBT, nd_C_insertMultiRBT, d_C_deleteRBT, nd_C_deleteRBT, d_C_setRBT2list, nd_C_setRBT2list, d_C_unionRBT, nd_C_unionRBT, d_C_intersectRBT, nd_C_intersectRBT, d_C_sortRBT, nd_C_sortRBT) where

import Basics
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_RedBlackTree
type C_SetRBT t0 = Curry_RedBlackTree.C_RedBlackTree t0

d_C_emptySetRBT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_emptySetRBT x3250 x3500 = Curry_RedBlackTree.d_C_empty (acceptCs id Curry_Prelude.d_OP_eq_eq) (acceptCs id Curry_Prelude.d_OP_eq_eq)

nd_C_emptySetRBT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Func t0 (Func t0 Curry_Prelude.C_Bool)) (Curry_RedBlackTree.C_RedBlackTree t0)
nd_C_emptySetRBT x3000 x3250 x3500 = wrapNX id (Curry_RedBlackTree.nd_C_empty (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)))

d_C_isEmptySetRBT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmptySetRBT x3250 x3500 = Curry_RedBlackTree.d_C_isEmpty

nd_C_isEmptySetRBT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree t0) Curry_Prelude.C_Bool
nd_C_isEmptySetRBT x3000 x3250 x3500 = wrapNX id Curry_RedBlackTree.nd_C_isEmpty

d_C_elemRBT :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_elemRBT x1 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Maybe.d_C_isJust (Curry_RedBlackTree.d_C_lookup x1) x3250 x3500

nd_C_elemRBT :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree t0) Curry_Prelude.C_Bool
nd_C_elemRBT x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Maybe.d_C_isJust) (wrapNX id (Curry_RedBlackTree.nd_C_lookup x1)) x2000 x3250 x3500))

d_C_insertRBT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_insertRBT x3250 x3500 = acceptCs id Curry_RedBlackTree.d_C_update

nd_C_insertRBT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func t0 (Func (Curry_RedBlackTree.C_RedBlackTree t0) (Curry_RedBlackTree.C_RedBlackTree t0))
nd_C_insertRBT x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id Curry_RedBlackTree.nd_C_update)

d_C_insertMultiRBT :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_insertMultiRBT x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_RedBlackTree.d_C_setInsertEquivalence (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.d_OP_dot (Curry_RedBlackTree.d_C_update x1) (Curry_RedBlackTree.d_C_setInsertEquivalence (acceptCs id d_OP_insertMultiRBT_dot___hash_lambda1)) x3250 x3500) x3250 x3500

nd_C_insertMultiRBT :: Curry_Prelude.Curry t0 => t0 -> IDSupply -> Cover -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree t0) (Curry_RedBlackTree.C_RedBlackTree t0)
nd_C_insertMultiRBT x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_RedBlackTree.nd_C_setInsertEquivalence (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)))) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_RedBlackTree.nd_C_update x1)) (wrapNX id (Curry_RedBlackTree.nd_C_setInsertEquivalence (wrapDX (wrapDX id) (acceptCs id d_OP_insertMultiRBT_dot___hash_lambda1)))) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_OP_insertMultiRBT_dot___hash_lambda1 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_insertMultiRBT_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.C_False

d_C_deleteRBT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_deleteRBT x3250 x3500 = acceptCs id Curry_RedBlackTree.d_C_delete

nd_C_deleteRBT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func t0 (Func (Curry_RedBlackTree.C_RedBlackTree t0) (Curry_RedBlackTree.C_RedBlackTree t0))
nd_C_deleteRBT x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id Curry_RedBlackTree.nd_C_delete)

d_C_setRBT2list :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_setRBT2list x3250 x3500 = Curry_RedBlackTree.d_C_tree2list

nd_C_setRBT2list :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_RedBlackTree.C_RedBlackTree t0) (Curry_Prelude.OP_List t0)
nd_C_setRBT2list x3000 x3250 x3500 = wrapNX id Curry_RedBlackTree.nd_C_tree2list

d_C_unionRBT :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_unionRBT x1 x2 x3250 x3500 = Curry_Prelude.d_C_foldr (d_C_insertRBT x3250 x3500) x2 (Curry_Prelude.d_C_apply (d_C_setRBT2list x3250 x3500) x1 x3250 x3500) x3250 x3500

nd_C_unionRBT :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> Curry_RedBlackTree.C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
nd_C_unionRBT x1 x2 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_foldr (nd_C_insertRBT x2000 x3250 x3500) x2 (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_setRBT2list x2001 x3250 x3500) x1 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_C_intersectRBT :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> Curry_RedBlackTree.C_RedBlackTree t0 -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
d_C_intersectRBT x1 x2 x3250 x3500 = Curry_Prelude.d_C_foldr (d_C_insertRBT x3250 x3500) (Curry_RedBlackTree.d_C_newTreeLike x1 x3250 x3500) (Curry_Prelude.d_C_filter (d_OP_intersectRBT_dot___hash_lambda2 x2) (Curry_Prelude.d_C_apply (d_C_setRBT2list x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_intersectRBT :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> Curry_RedBlackTree.C_RedBlackTree t0 -> IDSupply -> Cover -> ConstStore -> Curry_RedBlackTree.C_RedBlackTree t0
nd_C_intersectRBT x1 x2 x3000 x3250 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2009 = leftSupply x2008
          x2010 = rightSupply x2008
           in (seq x2009 (seq x2010 (let
               x2007 = leftSupply x2009
               x2000 = rightSupply x2009
                in (seq x2007 (seq x2000 (let
                    x2001 = leftSupply x2010
                    x2006 = rightSupply x2010
                     in (seq x2001 (seq x2006 (Curry_Prelude.nd_C_foldr (nd_C_insertRBT x2000 x3250 x3500) (Curry_RedBlackTree.nd_C_newTreeLike x1 x2001 x3250 x3500) (let
                         x2005 = leftSupply x2006
                         x2004 = rightSupply x2006
                          in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_filter (wrapNX id (nd_OP_intersectRBT_dot___hash_lambda2 x2)) (let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (nd_C_setRBT2list x2002 x3250 x3500) x1 x2003 x3250 x3500)))) x2005 x3250 x3500)))) x2007 x3250 x3500)))))))))))

d_OP_intersectRBT_dot___hash_lambda2 :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_intersectRBT_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply (d_C_elemRBT x2 x3250 x3500) x1 x3250 x3500

nd_OP_intersectRBT_dot___hash_lambda2 :: Curry_Prelude.Curry t0 => Curry_RedBlackTree.C_RedBlackTree t0 -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_intersectRBT_dot___hash_lambda2 x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_elemRBT x2 x2000 x3250 x3500) x1 x2001 x3250 x3500)))))

d_C_sortRBT :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_sortRBT x3250 x3500 = acceptCs id Curry_RedBlackTree.d_C_sort

nd_C_sortRBT :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Func t0 (Func t0 Curry_Prelude.C_Bool)) (Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0))
nd_C_sortRBT x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id Curry_RedBlackTree.nd_C_sort)
