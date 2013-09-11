{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_State (C_State, d_C_bindS, nd_C_bindS, d_C_bindS_, nd_C_bindS_, d_C_returnS, d_C_getS, d_C_putS, d_C_modifyS, nd_C_modifyS, d_C_sequenceS, nd_C_sequenceS, d_C_runState, nd_C_runState, d_C_evalState, nd_C_evalState, d_C_execState, nd_C_execState, d_C_liftS, nd_C_liftS, d_C_liftS2, nd_C_liftS2) where

import Basics
import qualified Curry_Prelude
type C_State t0 t1 = t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0

d_C_bindS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> (t1 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0
d_C_bindS x1 x2 x3 x3250 x3500 = let
     x4 = Curry_Prelude.d_C_apply x1 x3 x3250 x3500
     x5 = d_OP_bindS_dot___hash_selFP2_hash_x x4 x3250 x3500
     x6 = d_OP_bindS_dot___hash_selFP3_hash_newS x4 x3250 x3500
      in (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3250 x3500) x6 x3250 x3500)

nd_C_bindS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> Func t1 (Func t0 (Curry_Prelude.OP_Tuple2 t2 t0)) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0
nd_C_bindS x1 x2 x3 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x4 = Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500
               x5 = d_OP_bindS_dot___hash_selFP2_hash_x x4 x3250 x3500
               x6 = d_OP_bindS_dot___hash_selFP3_hash_newS x4 x3250 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2001 x3250 x3500) x6 x2002 x3250 x3500)))))))))

d_OP_bindS_dot___hash_selFP2_hash_x :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t0
d_OP_bindS_dot___hash_selFP2_hash_x x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bindS_dot___hash_selFP2_hash_x x1002 x3250 x3500) (d_OP_bindS_dot___hash_selFP2_hash_x x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bindS_dot___hash_selFP2_hash_x z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bindS_dot___hash_selFP2_hash_x x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_bindS_dot___hash_selFP3_hash_newS :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> t1
d_OP_bindS_dot___hash_selFP3_hash_newS x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bindS_dot___hash_selFP3_hash_newS x1002 x3250 x3500) (d_OP_bindS_dot___hash_selFP3_hash_newS x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bindS_dot___hash_selFP3_hash_newS z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bindS_dot___hash_selFP3_hash_newS x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_bindS_ :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0
d_C_bindS_ x1 x2 x3250 x3500 = d_C_bindS x1 (d_OP_bindS__dot___hash_lambda1 x2)

nd_C_bindS_ :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> Func t0 (Curry_Prelude.OP_Tuple2 t2 t0) -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 t2 t0)
nd_C_bindS_ x1 x2 x3000 x3250 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_bindS__dot___hash_lambda1 x2)))

d_OP_bindS__dot___hash_lambda1 :: (Curry_Prelude.Curry t2,Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t2 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0
d_OP_bindS__dot___hash_lambda1 x1 x2 x3250 x3500 = x1

nd_OP_bindS__dot___hash_lambda1 :: (Curry_Prelude.Curry t2,Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t2 -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 t1 t0)
nd_OP_bindS__dot___hash_lambda1 x1 x2 x3000 x3250 x3500 = x1

d_C_returnS :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t1
d_C_returnS x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 x1 x2

d_C_getS :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t0
d_C_getS x1 x3250 x3500 = Curry_Prelude.OP_Tuple2 x1 x1

d_C_putS :: Curry_Prelude.Curry t0 => t0 -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
d_C_putS x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit x1

d_C_modifyS :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> t0) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
d_C_modifyS x1 x2 x3250 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit (Curry_Prelude.d_C_apply x1 x2 x3250 x3500)

nd_C_modifyS :: Curry_Prelude.Curry t0 => Func t0 t0 -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
nd_C_modifyS x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500)))

d_C_sequenceS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Cover -> ConstStore -> Curry_Prelude.OP_List (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0
d_C_sequenceS x3250 x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_sequenceS_dot___hash_lambda2) (d_C_returnS Curry_Prelude.OP_List)

nd_C_sequenceS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Func t0 (Curry_Prelude.OP_Tuple2 t1 t0))) (Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0))
nd_C_sequenceS x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_sequenceS_dot___hash_lambda2)) (wrapDX id (d_C_returnS Curry_Prelude.OP_List)))

d_OP_sequenceS_dot___hash_lambda2 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0
d_OP_sequenceS_dot___hash_lambda2 x1 x2 x3250 x3500 = d_C_bindS x1 (d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 x2)

nd_OP_sequenceS_dot___hash_lambda2 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0) -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0)
nd_OP_sequenceS_dot___hash_lambda2 x1 x2 x3000 x3250 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 x2)))

d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0) -> t1 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0
d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3250 x3500 = d_C_bindS x1 (d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x2)

nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0) -> t1 -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0)
nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3000 x3250 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x2)))

d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) t1
d_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3250 x3500 = d_C_returnS (Curry_Prelude.OP_Cons x1 x2)

nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) t1)
nd_OP_sequenceS_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3000 x3250 x3500 = wrapDX id (d_C_returnS (Curry_Prelude.OP_Cons x1 x2))

d_C_runState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0
d_C_runState x1 x2 x3250 x3500 = Curry_Prelude.d_C_apply x1 x2 x3250 x3500

nd_C_runState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0
nd_C_runState x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500))

d_C_evalState :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> Cover -> ConstStore -> t1
d_C_evalState x1 x2 x3250 x3500 = Curry_Prelude.d_C_fst (d_C_runState x1 x2 x3250 x3500) x3250 x3500

nd_C_evalState :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> Cover -> ConstStore -> t1
nd_C_evalState x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_fst (nd_C_runState x1 x2 x2000 x3250 x3500) x3250 x3500))

d_C_execState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> Cover -> ConstStore -> t0
d_C_execState x1 x2 x3250 x3500 = Curry_Prelude.d_C_snd (d_C_runState x1 x2 x3250 x3500) x3250 x3500

nd_C_execState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_execState x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_snd (nd_C_runState x1 x2 x2000 x3250 x3500) x3250 x3500))

d_C_liftS :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> Cover -> ConstStore -> t1) -> (t2 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t2) -> Cover -> ConstStore -> t2 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
d_C_liftS x1 x2 x3250 x3500 = d_C_bindS x2 (Curry_Prelude.d_OP_dot (acceptCs id d_C_returnS) x1 x3250 x3500)

nd_C_liftS :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 t1 -> Func t2 (Curry_Prelude.OP_Tuple2 t0 t2) -> IDSupply -> Cover -> ConstStore -> Func t2 (Curry_Prelude.OP_Tuple2 t1 t2)
nd_C_liftS x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (wrapNX id (nd_C_bindS x2 (Curry_Prelude.nd_OP_dot (wrapDX (wrapDX id) (acceptCs id d_C_returnS)) x1 x2000 x3250 x3500))))

d_C_liftS2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> (t3 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t3) -> (t3 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t3) -> Cover -> ConstStore -> t3 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t3
d_C_liftS2 x1 x2 x3 x3250 x3500 = d_C_bindS x2 (d_OP_liftS2_dot___hash_lambda5 x3 x1)

nd_C_liftS2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Func t0 (Func t1 t2) -> Func t3 (Curry_Prelude.OP_Tuple2 t0 t3) -> Func t3 (Curry_Prelude.OP_Tuple2 t1 t3) -> IDSupply -> Cover -> ConstStore -> Func t3 (Curry_Prelude.OP_Tuple2 t2 t3)
nd_C_liftS2 x1 x2 x3 x3000 x3250 x3500 = wrapNX id (nd_C_bindS x2 (wrapNX id (nd_OP_liftS2_dot___hash_lambda5 x3 x1)))

d_OP_liftS2_dot___hash_lambda5 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> (t2 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t3) -> t2 -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t3 t0
d_OP_liftS2_dot___hash_lambda5 x1 x2 x3 x3250 x3500 = d_C_bindS x1 (d_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 x2 x3)

nd_OP_liftS2_dot___hash_lambda5 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> Func t2 (Func t1 t3) -> t2 -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_Tuple2 t3 t0)
nd_OP_liftS2_dot___hash_lambda5 x1 x2 x3 x3000 x3250 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 x2 x3)))

d_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => (t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> t0 -> t1 -> Cover -> ConstStore -> t3 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t3
d_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = d_C_returnS (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) x3 x3250 x3500)

nd_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t3) => Func t0 (Func t1 t2) -> t0 -> t1 -> IDSupply -> Cover -> ConstStore -> Func t3 (Curry_Prelude.OP_Tuple2 t2 t3)
nd_OP_liftS2_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (wrapDX id (d_C_returnS (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) x3 x2001 x3250 x3500)))))))
