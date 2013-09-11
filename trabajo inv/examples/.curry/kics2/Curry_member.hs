{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_member (nd_C_myMember) where

import Basics
import qualified Curry_Prelude
nd_C_myMember :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_myMember x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (d_OP___cond_0_myMember (Curry_Prelude.d_OP_eq_colon_eq x2 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_unknown x2000 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) (Curry_Prelude.nd_C_unknown x2001 x3250 x3500) x3250 x3500) x3250 x3500)))) x3250 x3500) x3250 x3500))

d_OP___cond_0_myMember :: Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP___cond_0_myMember x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_Success -> Curry_Prelude.C_True
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_myMember x1002 x3250 x3500) (d_OP___cond_0_myMember x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_myMember z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_myMember x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
