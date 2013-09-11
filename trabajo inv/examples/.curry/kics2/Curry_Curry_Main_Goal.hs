{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Curry_Main_Goal (nd_C_kics2MainGoal) where

import Basics
import qualified Curry_Prelude
import qualified Curry_nondet
nd_C_kics2MainGoal :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Int
nd_C_kics2MainGoal x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_nondet.nd_C_coin x2000 x3250 x3500))
