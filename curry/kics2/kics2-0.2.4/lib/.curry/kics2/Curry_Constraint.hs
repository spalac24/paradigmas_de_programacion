{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Constraint (d_OP_lt_colon, d_OP_gt_colon, d_OP_lt_eq_colon, d_OP_gt_eq_colon, d_C_andC, nd_C_andC, nd_C_orC, d_C_allC, nd_C_allC, nd_C_anyC) where

import Basics
import qualified Curry_Prelude
d_OP_lt_colon :: Curry_Prelude.Curry t0 => t0 -> t0 -> ConstStore -> Curry_Prelude.C_Success
d_OP_lt_colon x1 x2 x3500 = Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_OP_lt x1 x2 x3500) Curry_Prelude.C_True x3500

d_OP_gt_colon :: Curry_Prelude.Curry t0 => t0 -> t0 -> ConstStore -> Curry_Prelude.C_Success
d_OP_gt_colon x1 x2 x3500 = Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_OP_gt x1 x2 x3500) Curry_Prelude.C_True x3500

d_OP_lt_eq_colon :: Curry_Prelude.Curry t0 => t0 -> t0 -> ConstStore -> Curry_Prelude.C_Success
d_OP_lt_eq_colon x1 x2 x3500 = Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_OP_lt_eq x1 x2 x3500) Curry_Prelude.C_True x3500

d_OP_gt_eq_colon :: Curry_Prelude.Curry t0 => t0 -> t0 -> ConstStore -> Curry_Prelude.C_Success
d_OP_gt_eq_colon x1 x2 x3500 = Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_OP_gt_eq x1 x2 x3500) Curry_Prelude.C_True x3500

d_C_andC :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Success -> ConstStore -> Curry_Prelude.C_Success
d_C_andC x3500 = Curry_Prelude.d_C_foldr (acceptCs id Curry_Prelude.d_OP_ampersand) (Curry_Prelude.d_C_success x3500)

nd_C_andC :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Success) Curry_Prelude.C_Success
nd_C_andC x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_ampersand)) (Curry_Prelude.d_C_success x3500))

nd_C_orC :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Success) Curry_Prelude.C_Success
nd_C_orC x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id Curry_Prelude.nd_OP_qmark)) (Curry_Prelude.d_C_failed x3500))

d_C_allC :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Success) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Success
d_C_allC x1 x3500 = Curry_Prelude.d_OP_dot (d_C_andC x3500) (Curry_Prelude.d_C_map x1) x3500

nd_C_allC :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) Curry_Prelude.C_Success
nd_C_allC x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_andC x2000 x3500) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3500)))))

nd_C_anyC :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) Curry_Prelude.C_Success
nd_C_anyC x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (nd_C_orC x2000 x3500) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2001 x3500)))))
