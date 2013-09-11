{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_take (nd_C_myTake) where

import Basics
import qualified Curry_Prelude
nd_C_myTake :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_myTake x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_qmark (d_OP__case_1 x1 x3250 x3500) (nd_OP__case_0 x1 x2 x2000 x3250 x3500) x2001 x3250 x3500)))))

nd_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_OP__case_0 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Cons x3 (nd_C_myTake (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x4 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3250 x3500) (nd_OP__case_0 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_1 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.C_Int 0#) -> Curry_Prelude.OP_List
     (Curry_Prelude.C_CurryInt x5000) -> matchInteger [(0,Curry_Prelude.OP_List)] x5000 x3250 x3500
     (Curry_Prelude.Choice_C_Int x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Int x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Int x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Int x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
