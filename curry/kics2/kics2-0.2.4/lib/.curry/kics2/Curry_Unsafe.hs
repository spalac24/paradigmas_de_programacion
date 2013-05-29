{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Unsafe (d_C_trace, d_C_unsafePerformIO) where

import Basics
import qualified Curry_Prelude
import System.IO.Unsafe(unsafePerformIO)
import qualified Curry_Prelude as CP


d_C_trace :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> ConstStore -> t0
d_C_trace x1 x2 x3500 = d_C_unsafePerformIO (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr x1 x3500) (Curry_Prelude.d_C_return x2 x3500) x3500) x3500

d_C_unsafePerformIO :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> ConstStore -> t0
d_C_unsafePerformIO x1 x3500 = external_d_C_unsafePerformIO x1 x3500
external_d_C_unsafePerformIO :: CP.C_IO a -> ConstStore -> a
external_d_C_unsafePerformIO io cs = unsafePerformIO  (toIO io cs)

-----------------------------------------------------------------------

