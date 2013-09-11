{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_sublists (nd_C_sublists) where

import Basics
import qualified Curry_Prelude
nd_C_sublists :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_sublists x1 x3000 x3250 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x2006 = leftSupply x2008
          x2007 = rightSupply x2008
           in (seq x2006 (seq x2007 (let
               x2 = generate x2007 x3250
                in (d_OP___cond_0_sublists x2 (let
                    x2002 = leftSupply x2006
                    x2005 = rightSupply x2006
                     in (seq x2002 (seq x2005 (Curry_Prelude.d_OP_ampersand (Curry_Prelude.d_OP_eq_colon_eq (let
                         x2000 = leftSupply x2002
                         x2001 = rightSupply x2002
                          in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_unknown x2000 x3250 x3500) x2 x3250 x3500) (Curry_Prelude.nd_C_unknown x2001 x3250 x3500) x3250 x3500)))) x1 x3250 x3500) (Curry_Prelude.d_OP_eq_colon_eq x2 (let
                         x2003 = leftSupply x2005
                         x2004 = rightSupply x2005
                          in (seq x2003 (seq x2004 (Curry_Prelude.OP_Cons (Curry_Prelude.nd_C_unknown x2003 x3250 x3500) (Curry_Prelude.nd_C_unknown x2004 x3250 x3500))))) x3250 x3500) x3250 x3500)))) x3250 x3500))))))

d_OP___cond_0_sublists :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP___cond_0_sublists x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_sublists x1 x1002 x3250 x3500) (d_OP___cond_0_sublists x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_sublists x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_sublists x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
