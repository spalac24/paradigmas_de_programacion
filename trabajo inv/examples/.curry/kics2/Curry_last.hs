{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_last (nd_C_last) where

import Basics
import qualified Curry_Prelude
nd_C_last :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_last x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x2 = generate x2001 x3250
                in (d_OP___cond_0_last x2 (Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_unknown x2000 x3250 x3500) (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) x3250 x3500) x1 x3250 x3500) x3250 x3500))))))

d_OP___cond_0_last :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Success -> Cover -> ConstStore -> t0
d_OP___cond_0_last x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_last x1 x1002 x3250 x3500) (d_OP___cond_0_last x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_last x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_last x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
