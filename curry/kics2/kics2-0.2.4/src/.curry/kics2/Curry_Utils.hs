{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Utils (d_C_foldIO, nd_C_foldIO, d_C_liftIO, nd_C_liftIO, d_C_unless, d_C_notNull, nd_C_notNull, d_C_when, d_C_mapFst, nd_C_mapFst, d_C_mapSnd, nd_C_mapSnd, d_C_strip, nd_C_strip) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
d_C_foldIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> ConstStore -> t1 -> ConstStore -> Curry_Prelude.C_IO t0) -> t0 -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.C_IO t0
d_C_foldIO x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return x2 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x2 x3500) x4 x3500) (d_OP_foldIO_dot___hash_lambda1 x1 x5) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_foldIO x1 x2 x1002 x3500) (d_C_foldIO x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_foldIO x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_foldIO x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_foldIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Func t1 (Curry_Prelude.C_IO t0)) -> t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t0
nd_C_foldIO x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return x2 x3500
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) x4 x2001 x3500)))) (wrapNX id (nd_OP_foldIO_dot___hash_lambda1 x1 x5)) x2003 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_foldIO x1 x2 x1002 x3000 x3500) (nd_C_foldIO x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_foldIO x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_foldIO x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_foldIO_dot___hash_lambda1 :: (Curry_Prelude.Curry t10,Curry_Prelude.Curry t18) => (t18 -> ConstStore -> t10 -> ConstStore -> Curry_Prelude.C_IO t18) -> Curry_Prelude.OP_List t10 -> t18 -> ConstStore -> Curry_Prelude.C_IO t18
d_OP_foldIO_dot___hash_lambda1 x1 x2 x3 x3500 = d_C_foldIO x1 x3 x2 x3500

nd_OP_foldIO_dot___hash_lambda1 :: (Curry_Prelude.Curry t10,Curry_Prelude.Curry t18) => Func t18 (Func t10 (Curry_Prelude.C_IO t18)) -> Curry_Prelude.OP_List t10 -> t18 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t18
nd_OP_foldIO_dot___hash_lambda1 x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_foldIO x1 x3 x2 x2000 x3500))

d_C_liftIO :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t1) -> Curry_Prelude.C_IO t0 -> ConstStore -> Curry_Prelude.C_IO t1
d_C_liftIO x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq x2 (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return x1 x3500) x3500

nd_C_liftIO :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> Curry_Prelude.C_IO t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO t1
nd_C_liftIO x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq x2 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) x1 x2000 x3500) x2001 x3500)))))

d_C_unless :: Curry_Prelude.C_Bool -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_unless x1 x2 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_unless x1002 x2 x3500) (d_C_unless x1003 x2 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_unless z x2 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_unless x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_notNull :: Curry_Prelude.Curry t0 => ConstStore -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_notNull x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3500

nd_C_notNull :: Curry_Prelude.Curry t0 => IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) Curry_Prelude.C_Bool
nd_C_notNull x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_not) (wrapDX id Curry_Prelude.d_C_null) x2000 x3500))

d_C_when :: Curry_Prelude.C_Bool -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_when x1 x2 x3500 = case x1 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_when x1002 x2 x3500) (d_C_when x1003 x2 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_when z x2 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_when x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mapFst :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (t0 -> ConstStore -> t1) -> Curry_Prelude.OP_Tuple2 t0 t2 -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
d_C_mapFst x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply x1 x3 x3500) x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mapFst x1 x1002 x3500) (d_C_mapFst x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mapFst x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mapFst x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_mapFst :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func t0 t1 -> Curry_Prelude.OP_Tuple2 t0 t2 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t1 t2
nd_C_mapFst x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mapFst x1 x1002 x3000 x3500) (nd_C_mapFst x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mapFst x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mapFst x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_mapSnd :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => (t0 -> ConstStore -> t1) -> Curry_Prelude.OP_Tuple2 t2 t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t1
d_C_mapSnd x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.d_C_apply x1 x4 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_mapSnd x1 x1002 x3500) (d_C_mapSnd x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_mapSnd x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_mapSnd x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_mapSnd :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t2,Curry_Prelude.Curry t1) => Func t0 t1 -> Curry_Prelude.OP_Tuple2 t2 t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 t2 t1
nd_C_mapSnd x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x3 (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_mapSnd x1 x1002 x3000 x3500) (nd_C_mapSnd x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_mapSnd x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_mapSnd x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_strip :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_strip x3500 = Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace) (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace) x3500) x3500) x3500

nd_C_strip :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_strip x3000 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2000 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2000 (seq x2005 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2000 x3500) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id Curry_Char.d_C_isSpace))) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (Curry_Prelude.nd_C_reverse x2001 x3500) (wrapNX id (Curry_Prelude.nd_C_dropWhile (wrapDX id Curry_Char.d_C_isSpace))) x2002 x3500)))) x2004 x3500)))) x2006 x3500))))))))
