{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Utils (d_C_unless, d_C_notNull, nd_C_notNull, d_C_when, d_C_strip, nd_C_strip) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
d_C_unless :: Curry_Prelude.C_Bool -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_unless x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3250 x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unless x1002 x2 x3250 x3500) (d_C_unless x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unless z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unless x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_notNull :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_notNull x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3250 x3500

nd_C_notNull :: Curry_Prelude.Curry t0 => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List t0) Curry_Prelude.C_Bool
nd_C_notNull x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (wrapDX id Curry_Prelude.d_C_null) x2000 x3250 x3500))

d_C_when :: Curry_Prelude.C_Bool -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_when x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_when x1002 x2 x3250 x3500) (d_C_when x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_when z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_when x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_strip :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_strip x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_strip :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_strip x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2000 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2000 (seq x2005 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2000 x3250 x3500) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id Curry_Char.d_C_isSpace))) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2001 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id Curry_Char.d_C_isSpace))) x2002 x3250 x3500)))) x2004 x3250 x3500)))) x2006 x3250 x3500))))))))
