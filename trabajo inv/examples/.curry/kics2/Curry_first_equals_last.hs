{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_first_equals_last (nd_C_firsteqlast) where

import Basics
import qualified Curry_Prelude
nd_C_firsteqlast :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_firsteqlast x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x2 = generate x2001 x3250
                in (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_OP_eq_colon_eq x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_unknown x2000 x3250 x3500) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_success x3250 x3500) x3250 x3500))))))
