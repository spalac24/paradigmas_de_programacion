{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AllSolutions (d_C_getAllValues, d_C_getOneValue, nd_C_getAllSolutions, nd_C_getOneSolution, nd_C_getAllFailures) where

import Basics
import qualified Curry_Prelude
import qualified Curry_SearchTree
d_C_getAllValues :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_C_getAllValues x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_SearchTree.d_C_getSearchTree x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_SearchTree.d_C_allValuesDFS x3500) x3500) x3500

d_C_getOneValue :: Curry_Prelude.Curry t0 => t0 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)
d_C_getOneValue x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_SearchTree.d_C_getSearchTree x1 x3500) d_OP_getOneValue_dot___hash_lambda1 x3500

d_OP_getOneValue_dot___hash_lambda1 :: Curry_Prelude.Curry t12 => Curry_SearchTree.C_SearchTree t12 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t12)
d_OP_getOneValue_dot___hash_lambda1 x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_SearchTree.d_C_allValuesDFS x3500) x1 x3500
      in (Curry_Prelude.d_C_return (d_OP__case_1 x2 (Curry_Prelude.d_C_null x2 x3500) x3500) x3500)

nd_C_getAllSolutions :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_C_getAllSolutions x1 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2002 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllValues (let
                    x2000 = leftSupply x2002
                    x2001 = rightSupply x2002
                     in (seq x2000 (seq x2001 (let
                         x2 = generate x2001
                          in (Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500)))))) x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst))) x2003 x3500) x2004 x3500))))))))

nd_C_getOneSolution :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)
nd_C_getOneSolution x1 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getAllSolutions x1 x2000 x3500) (wrapDX id d_OP_getOneSolution_dot___hash_lambda2) x2001 x3500)))))

d_OP_getOneSolution_dot___hash_lambda2 :: Curry_Prelude.Curry t42 => Curry_Prelude.OP_List t42 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t42)
d_OP_getOneSolution_dot___hash_lambda2 x1 x3500 = Curry_Prelude.d_C_return (d_OP__case_0 x1 (Curry_Prelude.d_C_null x1 x3500) x3500) x3500

nd_C_getAllFailures :: Curry_Prelude.Curry t0 => t0 -> Func t0 Curry_Prelude.C_Success -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_C_getAllFailures x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_getAllValues x1 x3500) (wrapNX id (nd_OP_getAllFailures_dot___hash_lambda3 x2)) x2000 x3500))

nd_OP_getAllFailures_dot___hash_lambda3 :: Curry_Prelude.Curry t76 => Func t76 Curry_Prelude.C_Success -> Curry_Prelude.OP_List t76 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t76)
nd_OP_getAllFailures_dot___hash_lambda3 x1 x2 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO (wrapNX id (nd_C_naf x1)) x2000 x3500) x2 x2001 x3500)))) (wrapDX id d_OP_getAllFailures_dot___hash_lambda3_dot___hash_lambda4) x2003 x3500)))))

d_OP_getAllFailures_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.Curry t76 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t76) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t76)
d_OP_getAllFailures_dot___hash_lambda3_dot___hash_lambda4 x1 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_C_concat x1 x3500) x3500

nd_C_naf :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
nd_C_naf x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_getOneSolution (wrapNX id (nd_C_lambda x1 x2)) x2000 x3500) (wrapDX id (d_C_returner x2)) x2001 x3500)))))

d_C_lambda :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Success) -> t0 -> Curry_Prelude.OP_Unit -> ConstStore -> Curry_Prelude.C_Success
d_C_lambda x1 x2 x3 x3500 = Curry_Prelude.d_C_apply x1 x2 x3500

nd_C_lambda :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Success -> t0 -> Curry_Prelude.OP_Unit -> IDSupply -> ConstStore -> Curry_Prelude.C_Success
nd_C_lambda x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500))

d_C_returner :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t0 -> Curry_Prelude.C_Maybe t1 -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t0)
d_C_returner x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) (Curry_Prelude.d_C_const Curry_Prelude.OP_List) x2 x3500) x3500

d_OP__case_0 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head x1 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head x1 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head x2 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x1002 x3500) (d_OP__case_1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head x2 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x1002 x3000 x3500) (nd_OP__case_1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
