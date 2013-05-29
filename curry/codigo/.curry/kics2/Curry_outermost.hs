{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_outermost (d_C_loop) where

import Basics
import qualified Curry_Prelude
d_C_loop :: ConstStore -> Curry_Prelude.C_Int
d_C_loop x3500 = Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 1#) (d_C_loop x3500) x3500
