{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_noinductive (d_C_eq) where

import Basics
import qualified Curry_Prelude
d_C_eq :: Curry_Prelude.Curry t0 => t0 -> t0 -> Cover -> ConstStore -> t0
d_C_eq x1 x2 x3250 x3500 = d_OP___cond_0_eq x1 (Curry_Prelude.d_OP_eq_colon_eq x1 x2 x3250 x3500) x3250 x3500

d_OP___cond_0_eq :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Success -> Cover -> ConstStore -> t0
d_OP___cond_0_eq x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_eq x1 x1002 x3250 x3500) (d_OP___cond_0_eq x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_eq x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_eq x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
