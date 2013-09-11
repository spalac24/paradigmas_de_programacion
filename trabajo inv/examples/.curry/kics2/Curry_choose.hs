{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_choose (nd_C_choose) where

import Basics
import qualified Curry_Prelude
nd_C_choose :: Curry_Prelude.Curry t0 => t0 -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_choose x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_qmark x1 x2 x2000 x3250 x3500))
