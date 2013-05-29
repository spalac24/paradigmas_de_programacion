{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Maybe (d_C_isJust, d_C_isNothing, d_C_fromJust, d_C_fromMaybe, d_C_maybeToList, d_C_listToMaybe, d_C_catMaybes, d_C_mapMaybe, nd_C_mapMaybe, d_OP_gt_gt_minus, nd_OP_gt_gt_minus, d_C_sequenceMaybe, d_C_mapMMaybe, nd_C_mapMMaybe) where

import Basics
import qualified Curry_Prelude
d_C_isJust :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isJust x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> Curry_Prelude.C_True
     Curry_Prelude.C_Nothing -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isJust x1002 x3500) (d_C_isJust x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isJust z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isJust x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isNothing :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isNothing x1 x3500 = case x1 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.C_True
     (Curry_Prelude.C_Just x2) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isNothing x1002 x3500) (d_C_isNothing x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isNothing z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isNothing x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fromJust :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> ConstStore -> t0
d_C_fromJust x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> x2
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))))))))))))))) x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fromJust x1002 x3500) (d_C_fromJust x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fromJust z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fromJust x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_fromMaybe :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.C_Maybe t0 -> ConstStore -> t0
d_C_fromMaybe x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> x1
     (Curry_Prelude.C_Just x3) -> x3
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fromMaybe x1 x1002 x3500) (d_C_fromMaybe x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fromMaybe x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fromMaybe x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_maybeToList :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_maybeToList x1 x3500 = case x1 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_List
     (Curry_Prelude.C_Just x2) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_maybeToList x1002 x3500) (d_C_maybeToList x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_maybeToList z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_maybeToList x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_listToMaybe :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Maybe t0
d_C_listToMaybe x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.C_Just x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_listToMaybe x1002 x3500) (d_C_listToMaybe x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_listToMaybe z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_listToMaybe x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_catMaybes :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.C_Maybe t0) -> ConstStore -> Curry_Prelude.OP_List t0
d_C_catMaybes x1 x3500 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_catMaybes_dot___hash_lambda3) Curry_Prelude.OP_List x1 x3500

d_OP_catMaybes_dot___hash_lambda3 :: Curry_Prelude.Curry t36 => Curry_Prelude.C_Maybe t36 -> Curry_Prelude.OP_List t36 -> ConstStore -> Curry_Prelude.OP_List t36
d_OP_catMaybes_dot___hash_lambda3 x1 x2 x3500 = case x1 of
     (Curry_Prelude.C_Just x3) -> Curry_Prelude.OP_Cons x3 x2
     Curry_Prelude.C_Nothing -> x2
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_catMaybes_dot___hash_lambda3 x1002 x2 x3500) (d_OP_catMaybes_dot___hash_lambda3 x1003 x2 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_catMaybes_dot___hash_lambda3 z x2 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_catMaybes_dot___hash_lambda3 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mapMaybe :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.C_Maybe t1) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t1
d_C_mapMaybe x1 x3500 = Curry_Prelude.d_OP_dot d_C_catMaybes (Curry_Prelude.d_C_map x1) x3500

nd_C_mapMaybe :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.C_Maybe t1) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t1)
nd_C_mapMaybe x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_catMaybes) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2000 x3500))

d_OP_gt_gt_minus :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.C_Maybe t0 -> (t0 -> ConstStore -> Curry_Prelude.C_Maybe t1) -> ConstStore -> Curry_Prelude.C_Maybe t1
d_OP_gt_gt_minus x1 x2 x3500 = case x1 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.C_Nothing
     (Curry_Prelude.C_Just x3) -> Curry_Prelude.d_C_apply x2 x3 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_gt_gt_minus x1002 x2 x3500) (d_OP_gt_gt_minus x1003 x2 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_gt_gt_minus z x2 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_gt_gt_minus x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_gt_gt_minus :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.C_Maybe t0 -> Func t0 (Curry_Prelude.C_Maybe t1) -> IDSupply -> ConstStore -> Curry_Prelude.C_Maybe t1
nd_OP_gt_gt_minus x1 x2 x3000 x3500 = case x1 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.C_Nothing
     (Curry_Prelude.C_Just x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x2 x3 x2000 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_gt_gt_minus x1002 x2 x3000 x3500) (nd_OP_gt_gt_minus x1003 x2 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_gt_gt_minus z x2 x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_gt_gt_minus x1002 x2 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_sequenceMaybe :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.C_Maybe t0) -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t0)
d_C_sequenceMaybe x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP_gt_gt_minus x2 (d_OP_sequenceMaybe_dot___hash_lambda5 x3) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sequenceMaybe x1002 x3500) (d_C_sequenceMaybe x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sequenceMaybe z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sequenceMaybe x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_sequenceMaybe_dot___hash_lambda5 :: Curry_Prelude.Curry t65 => Curry_Prelude.OP_List (Curry_Prelude.C_Maybe t65) -> t65 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t65)
d_OP_sequenceMaybe_dot___hash_lambda5 x1 x2 x3500 = d_OP_gt_gt_minus (d_C_sequenceMaybe x1 x3500) (d_OP_sequenceMaybe_dot___hash_lambda5_dot___hash_lambda6 x2) x3500

d_OP_sequenceMaybe_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.Curry t65 => t65 -> Curry_Prelude.OP_List t65 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t65)
d_OP_sequenceMaybe_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3500 = Curry_Prelude.C_Just (Curry_Prelude.OP_Cons x1 x2)

d_C_mapMMaybe :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> Curry_Prelude.C_Maybe t1) -> ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t1)
d_C_mapMMaybe x1 x3500 = Curry_Prelude.d_OP_dot d_C_sequenceMaybe (Curry_Prelude.d_C_map x1) x3500

nd_C_mapMMaybe :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.C_Maybe t1) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List t1))
nd_C_mapMMaybe x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id d_C_sequenceMaybe) (wrapNX id (Curry_Prelude.nd_C_map x1)) x2000 x3500))
