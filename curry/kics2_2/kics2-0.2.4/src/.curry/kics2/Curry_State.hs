{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_State (C_State, d_C_bindS, nd_C_bindS, d_C_returnS, d_C_sequenceS, nd_C_sequenceS, d_C_getS, d_C_setS, d_C_modifyS, nd_C_modifyS, d_C_runState, nd_C_runState, d_C_evalState, nd_C_evalState, d_C_execState, nd_C_execState) where

import Basics
import qualified Curry_Prelude
type C_State t0 t1 = t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t1

d_C_bindS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> (t1 -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0) -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0
d_C_bindS x1 x2 x3 x3500 = let
     x4 = Curry_Prelude.d_C_apply x1 x3 x3500
     x5 = d_OP_bindS_dot___hash_selFP2_hash_x x4 x3500
     x6 = d_OP_bindS_dot___hash_selFP3_hash_newS x4 x3500
      in (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x5 x3500) x6 x3500)

nd_C_bindS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> Func t1 (Func t0 (Curry_Prelude.OP_Tuple2 t2 t0)) -> t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t0
nd_C_bindS x1 x2 x3 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x4 = Curry_Prelude.nd_C_apply x1 x3 x2000 x3500
               x5 = d_OP_bindS_dot___hash_selFP2_hash_x x4 x3500
               x6 = d_OP_bindS_dot___hash_selFP3_hash_newS x4 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x5 x2001 x3500) x6 x2002 x3500)))))))))

d_OP_bindS_dot___hash_selFP2_hash_x :: (Curry_Prelude.Curry t8,Curry_Prelude.Curry t7) => Curry_Prelude.OP_Tuple2 t7 t8 -> ConstStore -> t7
d_OP_bindS_dot___hash_selFP2_hash_x x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bindS_dot___hash_selFP2_hash_x x1002 x3500) (d_OP_bindS_dot___hash_selFP2_hash_x x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bindS_dot___hash_selFP2_hash_x z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bindS_dot___hash_selFP2_hash_x x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_bindS_dot___hash_selFP3_hash_newS :: (Curry_Prelude.Curry t7,Curry_Prelude.Curry t8) => Curry_Prelude.OP_Tuple2 t7 t8 -> ConstStore -> t8
d_OP_bindS_dot___hash_selFP3_hash_newS x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_bindS_dot___hash_selFP3_hash_newS x1002 x3500) (d_OP_bindS_dot___hash_selFP3_hash_newS x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_bindS_dot___hash_selFP3_hash_newS z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_bindS_dot___hash_selFP3_hash_newS x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_returnS :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t1
d_C_returnS x1 x2 x3500 = Curry_Prelude.OP_Tuple2 x1 x2

d_C_sequenceS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => ConstStore -> Curry_Prelude.OP_List (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> ConstStore -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0
d_C_sequenceS x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_sequenceS_dot___hash_lambda1) (d_C_returnS Curry_Prelude.OP_List)

nd_C_sequenceS :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Func t0 (Curry_Prelude.OP_Tuple2 t1 t0))) (Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) t0))
nd_C_sequenceS x3000 x3500 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapNX id) (acceptCs id nd_OP_sequenceS_dot___hash_lambda1)) (wrapDX id (d_C_returnS Curry_Prelude.OP_List)))

d_OP_sequenceS_dot___hash_lambda1 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t37) => (t37 -> ConstStore -> Curry_Prelude.OP_Tuple2 t38 t37) -> (t37 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37) -> ConstStore -> t37 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37
d_OP_sequenceS_dot___hash_lambda1 x1 x2 x3500 = d_C_bindS x1 (d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 x2)

nd_OP_sequenceS_dot___hash_lambda1 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t37) => Func t37 (Curry_Prelude.OP_Tuple2 t38 t37) -> Func t37 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37) -> IDSupply -> ConstStore -> Func t37 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37)
nd_OP_sequenceS_dot___hash_lambda1 x1 x2 x3000 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 x2)))

d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t37) => (t37 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37) -> t38 -> ConstStore -> t37 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37
d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3500 = d_C_bindS x1 (d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x2)

nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t37) => Func t37 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37) -> t38 -> IDSupply -> ConstStore -> Func t37 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t37)
nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3000 x3500 = wrapNX id (nd_C_bindS x1 (wrapNX id (nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x2)))

d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t71) => t38 -> Curry_Prelude.OP_List t38 -> ConstStore -> t71 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t71
d_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3500 = d_C_returnS (Curry_Prelude.OP_Cons x1 x2)

nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t71) => t38 -> Curry_Prelude.OP_List t38 -> IDSupply -> ConstStore -> Func t71 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t38) t71)
nd_OP_sequenceS_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3000 x3500 = wrapDX id (d_C_returnS (Curry_Prelude.OP_Cons x1 x2))

d_C_getS :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 t0
d_C_getS x1 x3500 = Curry_Prelude.OP_Tuple2 x1 x1

d_C_setS :: Curry_Prelude.Curry t0 => t0 -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
d_C_setS x1 x2 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit x1

d_C_modifyS :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> t0) -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
d_C_modifyS x1 x2 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit (Curry_Prelude.d_C_apply x1 x2 x3500)

nd_C_modifyS :: Curry_Prelude.Curry t0 => Func t0 t0 -> t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit t0
nd_C_modifyS x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_Unit (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500)))

d_C_runState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0
d_C_runState x1 x2 x3500 = Curry_Prelude.d_C_apply x1 x2 x3500

nd_C_runState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0
nd_C_runState x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500))

d_C_evalState :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> ConstStore -> t1
d_C_evalState x1 x2 x3500 = Curry_Prelude.d_C_fst (d_C_runState x1 x2 x3500) x3500

nd_C_evalState :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> ConstStore -> t1
nd_C_evalState x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_fst (nd_C_runState x1 x2 x2000 x3500) x3500))

d_C_execState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> ConstStore -> t0
d_C_execState x1 x2 x3500 = Curry_Prelude.d_C_snd (d_C_runState x1 x2 x3500) x3500

nd_C_execState :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 t1 t0) -> t0 -> IDSupply -> ConstStore -> t0
nd_C_execState x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_C_snd (nd_C_runState x1 x2 x2000 x3500) x3500))
