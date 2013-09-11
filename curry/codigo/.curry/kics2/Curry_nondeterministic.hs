{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_nondeterministic (nd_C_choose, nd_C_one23) where

import Basics
import qualified Curry_Prelude
nd_C_choose :: Curry_Prelude.Curry t0 => t0 -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_choose x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_qmark x1 x2 x2000 x3250 x3500))

nd_C_one23 :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Int
nd_C_one23 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_C_choose (Curry_Prelude.C_Int 1#) (nd_C_choose (Curry_Prelude.C_Int 2#) (Curry_Prelude.C_Int 3#) x2000 x3250 x3500) x2001 x3250 x3500)))))
