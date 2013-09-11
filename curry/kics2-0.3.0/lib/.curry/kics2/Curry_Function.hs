{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Function (d_C_fix, nd_C_fix, d_C_on, nd_C_on, d_C_first, nd_C_first, d_C_second, nd_C_second, d_OP_star_star_star, nd_OP_star_star_star, d_OP_ampersand_ampersand_ampersand, nd_OP_ampersand_ampersand_ampersand, d_C_both, nd_C_both) where

import Basics
import qualified Curry_Prelude
d_C_fix :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> t0
d_C_fix x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_apply x1 x2 x3250 x3500
      in x2

nd_C_fix :: Curry_Prelude.Curry t0 => Func t0 t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_fix x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x2 = Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500
           in x2))

d_C_on :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t0) -> t2 -> t2 -> Cover -> ConstStore -> t1
d_C_on x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 (Curry_Prelude.d_C_apply x2 x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply x2 x4 x3250 x3500) x3250 x3500

nd_C_on :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => Func t0 (Func t0 t1) -> Func t2 t0 -> t2 -> t2 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_on x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2002 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x1 (Curry_Prelude.nd_C_apply x2 x3 x2000 x3250 x3500) x2001 x3250 x3500)))) (Curry_Prelude.nd_C_apply x2 x4 x2003 x3250 x3500) x2004 x3250 x3500))))))))

d_C_first :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> Cover -> ConstStore -> t1) -> Curry_Prelude.OP_Tuple2 t0 t2 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
d_C_first x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_first x1 x1002 x3250 x3500) (d_C_first x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_first x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_first x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_first :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 t1 -> Curry_Prelude.OP_Tuple2 t0 t2 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
nd_C_first x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x4))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_first x1 x1002 x3000 x3250 x3500) (nd_C_first x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_first x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_first x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_second :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> Curry_Prelude.OP_Tuple2 t2 t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t1
d_C_second x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_apply x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_second x1 x1002 x3250 x3500) (d_C_second x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_second x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_second x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_second :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => Func t0 t1 -> Curry_Prelude.OP_Tuple2 t2 t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t1
nd_C_second x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_second x1 x1002 x3000 x3250 x3500) (nd_C_second x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_second x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_second x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_star_star_star :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1,Curry_Prelude.Curry t3) => (t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> t3) -> Curry_Prelude.OP_Tuple2 t0 t2 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t3
d_OP_star_star_star x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) (Curry_Prelude.d_C_apply x2 x5 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_star_star_star x1 x2 x1002 x3250 x3500) (d_OP_star_star_star x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_star_star_star x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_star_star_star x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_star_star_star :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1,Curry_Prelude.Curry t3) => Func t0 t1 -> Func t2 t3 -> Curry_Prelude.OP_Tuple2 t0 t2 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t3
nd_OP_star_star_star x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x2 x5 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_star_star_star x1 x2 x1002 x3000 x3250 x3500) (nd_OP_star_star_star x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_star_star_star x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_star_star_star x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_ampersand_ampersand_ampersand :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> Cover -> ConstStore -> t1) -> (t0 -> Cover -> ConstStore -> t2) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
d_OP_ampersand_ampersand_ampersand x1 x2 x3 x3250 x3500 = Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) (Curry_Prelude.d_C_apply x2 x3 x3250 x3500)

nd_OP_ampersand_ampersand_ampersand :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 t1 -> Func t0 t2 -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
nd_OP_ampersand_ampersand_ampersand x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x2 x3 x2001 x3250 x3500))))))

d_C_both :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> Curry_Prelude.OP_Tuple2 t0 t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t1
d_C_both x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) (Curry_Prelude.d_C_apply x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_both x1 x1002 x3250 x3500) (d_C_both x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_both x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_both x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_both :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> Curry_Prelude.OP_Tuple2 t0 t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t1
nd_C_both x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x1 x4 x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_both x1 x1002 x3000 x3250 x3500) (nd_C_both x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_both x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_both x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
