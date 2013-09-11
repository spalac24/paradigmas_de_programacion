{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_trs (nd_C_add) where

import Basics
import qualified Curry_Prelude
nd_C_add :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Int
nd_C_add x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x3 = generate x2000 x3250
           in (d_OP___cond_0_add x3 (Curry_Prelude.d_OP_eq_colon_eq x3 (Curry_Prelude.d_OP_plus x1 x2 x3250 x3500) x3250 x3500) x3250 x3500)))

d_OP___cond_0_add :: Curry_Prelude.C_Int -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP___cond_0_add x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_add x1 x1002 x3250 x3500) (d_OP___cond_0_add x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_add x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_add x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
