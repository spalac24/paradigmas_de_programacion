{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Traversal (C_Traversable, d_C_noChildren, nd_C_noChildren, d_C_children, nd_C_children, d_C_replaceChildren, nd_C_replaceChildren, d_C_mapChildren, nd_C_mapChildren, d_C_family, nd_C_family, d_C_childFamilies, nd_C_childFamilies, d_C_mapFamily, nd_C_mapFamily, d_C_mapChildFamilies, nd_C_mapChildFamilies, d_C_evalFamily, nd_C_evalFamily, d_C_evalChildFamilies, nd_C_evalChildFamilies, d_C_fold, nd_C_fold, d_C_foldChildren, nd_C_foldChildren, d_C_replaceChildrenIO, nd_C_replaceChildrenIO, d_C_mapChildrenIO, nd_C_mapChildrenIO, d_C_mapFamilyIO, nd_C_mapFamilyIO, d_C_mapChildFamiliesIO, nd_C_mapChildFamiliesIO, d_C_evalFamilyIO, nd_C_evalFamilyIO, d_C_evalChildFamiliesIO, nd_C_evalChildFamiliesIO) where

import Basics
import qualified Curry_Prelude
type C_Traversable t0 t1 = t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)

type C_FunList t0 = Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0

d_C_noChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)
d_C_noChildren x1 x3250 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (Curry_Prelude.d_C_const x1)

nd_C_noChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)
nd_C_noChildren x1 x3000 x3250 x3500 = Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List (wrapDX id (Curry_Prelude.d_C_const x1))

d_C_children :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
d_C_children x1 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst x1 x3250 x3500

nd_C_children :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.OP_List t1)
nd_C_children x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) x1 x2000 x3250 x3500))

d_C_replaceChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0
d_C_replaceChildren x1 x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd x1 x3250 x3500

nd_C_replaceChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> IDSupply -> Cover -> ConstStore -> Func t0 (Func (Curry_Prelude.OP_List t1) t0)
nd_C_replaceChildren x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) x1 x2000 x3250 x3500))

d_C_mapChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> t1) -> t0 -> Cover -> ConstStore -> t0
d_C_mapChildren x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_replaceChildren x1 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_C_map x2 (Curry_Prelude.d_C_apply (d_C_children x1 x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_mapChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 t1 -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_mapChildren x1 x2 x3 x3000 x3250 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2008 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2008 (seq x2010 (let
               x2002 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2002 (seq x2007 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_replaceChildren x1 x2000 x3250 x3500) x3 x2001 x3250 x3500)))) (let
                    x2006 = leftSupply x2007
                    x2005 = rightSupply x2007
                     in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_map x2 (let
                         x2004 = leftSupply x2005
                         x2003 = rightSupply x2005
                          in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (nd_C_children x1 x2003 x3250 x3500) x3 x2004 x3250 x3500)))) x2006 x3250 x3500)))) x2008 x3250 x3500))))))))

d_C_family :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_family x1 x2 x3250 x3500 = d_C_familyFL x1 x2 Curry_Prelude.OP_List x3250 x3500

nd_C_family :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_family x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_familyFL x1 x2 Curry_Prelude.OP_List x2000 x3250 x3500))

d_C_childFamilies :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
d_C_childFamilies x1 x2 x3 x3250 x3500 = d_C_childFamiliesFL x1 x2 x3 Curry_Prelude.OP_List x3250 x3500

nd_C_childFamilies :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
nd_C_childFamilies x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_childFamiliesFL x1 x2 x3 Curry_Prelude.OP_List x2000 x3250 x3500))

d_C_mapFamily :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> (t0 -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0
d_C_mapFamily x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot x2 (Curry_Prelude.d_C_apply (d_C_mapChildFamilies x1 x1 x3250 x3500) x2 x3250 x3500) x3250 x3500

nd_C_mapFamily :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> Func t0 t0 -> IDSupply -> Cover -> ConstStore -> Func t0 t0
nd_C_mapFamily x1 x2 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot x2 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mapChildFamilies x1 x1 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_C_mapChildFamilies :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> Cover -> ConstStore -> (t1 -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0
d_C_mapChildFamilies x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id (d_C_mapChildren x1)) (d_C_mapFamily x2) x3250 x3500

nd_C_mapChildFamilies :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> IDSupply -> Cover -> ConstStore -> Func (Func t1 t1) (Func t0 t0)
nd_C_mapChildFamilies x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX (wrapNX id) (acceptCs id (nd_C_mapChildren x1))) (wrapNX id (nd_C_mapFamily x2)) x2000 x3250 x3500))

d_C_evalFamily :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0
d_C_evalFamily x1 x2 x3250 x3500 = d_C_mapFamily x1 (d_OP_evalFamily_dot_g_dot_18 x2 x1) x3250 x3500

nd_C_evalFamily :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> Func t0 (Curry_Prelude.C_Maybe t0) -> IDSupply -> Cover -> ConstStore -> Func t0 t0
nd_C_evalFamily x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_mapFamily x1 (wrapNX id (nd_OP_evalFamily_dot_g_dot_18 x2 x1)) x2000 x3250 x3500))

d_OP_evalFamily_dot_g_dot_18 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t0) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> t0 -> Cover -> ConstStore -> t0
d_OP_evalFamily_dot_g_dot_18 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_maybe x3 (d_C_mapFamily x2 (d_OP_evalFamily_dot_g_dot_18 x1 x2) x3250 x3500) (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x3250 x3500

nd_OP_evalFamily_dot_g_dot_18 :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.C_Maybe t0) -> Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_OP_evalFamily_dot_g_dot_18 x1 x2 x3 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_maybe x3 (nd_C_mapFamily x2 (wrapNX id (nd_OP_evalFamily_dot_g_dot_18 x1 x2)) x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x1 x3 x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_evalChildFamilies :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> Cover -> ConstStore -> (t1 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe t1) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t0
d_C_evalChildFamilies x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id (d_C_mapChildren x1)) (d_C_evalFamily x2) x3250 x3500

nd_C_evalChildFamilies :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> IDSupply -> Cover -> ConstStore -> Func (Func t1 (Curry_Prelude.C_Maybe t1)) (Func t0 t0)
nd_C_evalChildFamilies x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX (wrapNX id) (acceptCs id (nd_C_mapChildren x1))) (wrapNX id (nd_C_evalFamily x2)) x2000 x3250 x3500))

d_C_fold :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> t1
d_C_fold x1 x2 x3250 x3500 = d_C_foldChildren x1 x1 x2 x2

nd_C_fold :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> Func t0 (Func (Curry_Prelude.OP_List t1) t1) -> IDSupply -> Cover -> ConstStore -> Func t0 t1
nd_C_fold x1 x2 x3000 x3250 x3500 = wrapNX id (nd_C_foldChildren x1 x1 x2 x2)

d_C_foldChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0,Curry_Prelude.Curry t3) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t2 -> Cover -> ConstStore -> t3) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_List t2 -> Cover -> ConstStore -> t2) -> t0 -> Cover -> ConstStore -> t3
d_C_foldChildren x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x3 x5 x3250 x3500) (Curry_Prelude.d_C_map (d_C_fold x2 x4 x3250 x3500) (Curry_Prelude.d_C_apply (d_C_children x1 x3250 x3500) x5 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_foldChildren :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t2,Curry_Prelude.Curry t0,Curry_Prelude.Curry t3) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> Func t0 (Func (Curry_Prelude.OP_List t2) t3) -> Func t1 (Func (Curry_Prelude.OP_List t2) t2) -> t0 -> IDSupply -> Cover -> ConstStore -> t3
nd_C_foldChildren x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2009 = x3000
      in (seq x2009 (let
          x2008 = leftSupply x2009
          x2010 = rightSupply x2009
           in (seq x2008 (seq x2010 (let
               x2000 = leftSupply x2010
               x2006 = rightSupply x2010
                in (seq x2000 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x3 x5 x2000 x3250 x3500) (let
                    x2005 = leftSupply x2006
                    x2007 = rightSupply x2006
                     in (seq x2005 (seq x2007 (let
                         x2001 = leftSupply x2007
                         x2004 = rightSupply x2007
                          in (seq x2001 (seq x2004 (Curry_Prelude.nd_C_map (nd_C_fold x2 x4 x2001 x3250 x3500) (let
                              x2003 = leftSupply x2004
                              x2002 = rightSupply x2004
                               in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (nd_C_children x1 x2002 x3250 x3500) x5 x2003 x3250 x3500)))) x2005 x3250 x3500))))))) x2008 x3250 x3500))))))))

d_C_replaceChildrenIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t1) -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_replaceChildrenIO x1 x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id d_C_liftIO) (d_C_replaceChildren x1 x3250 x3500) x3250 x3500

nd_C_replaceChildrenIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> IDSupply -> Cover -> ConstStore -> Func t0 (Func (Curry_Prelude.C_IO (Curry_Prelude.OP_List t1)) (Curry_Prelude.C_IO t0))
nd_C_replaceChildrenIO x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX (wrapNX id) (acceptCs id nd_C_liftIO)) (nd_C_replaceChildren x1 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_mapChildrenIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.C_IO t1) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_mapChildrenIO x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_replaceChildrenIO x1 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO x2 x3250 x3500) (Curry_Prelude.d_C_apply (d_C_children x1 x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_mapChildrenIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.C_IO t1) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
nd_C_mapChildrenIO x1 x2 x3 x3000 x3250 x3500 = let
     x2011 = x3000
      in (seq x2011 (let
          x2010 = leftSupply x2011
          x2012 = rightSupply x2011
           in (seq x2010 (seq x2012 (let
               x2002 = leftSupply x2012
               x2008 = rightSupply x2012
                in (seq x2002 (seq x2008 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_replaceChildrenIO x1 x2000 x3250 x3500) x3 x2001 x3250 x3500)))) (let
                    x2007 = leftSupply x2008
                    x2009 = rightSupply x2008
                     in (seq x2007 (seq x2009 (let
                         x2003 = leftSupply x2009
                         x2006 = rightSupply x2009
                          in (seq x2003 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO x2 x2003 x3250 x3500) (let
                              x2005 = leftSupply x2006
                              x2004 = rightSupply x2006
                               in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (nd_C_children x1 x2004 x3250 x3500) x3 x2005 x3250 x3500)))) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))))

d_C_mapFamilyIO :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_mapFamilyIO x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_mapChildFamiliesIO x1 x1 x3250 x3500) x2 x3250 x3500) x3 x3250 x3500) x2 x3250 x3500

nd_C_mapFamilyIO :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> Func t0 (Curry_Prelude.C_IO t0) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
nd_C_mapFamilyIO x1 x2 x3 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_mapChildFamiliesIO x1 x1 x2000 x3250 x3500) x2 x2001 x3250 x3500)))) x3 x2003 x3250 x3500)))) x2 x2005 x3250 x3500)))))

d_C_mapChildFamiliesIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> Cover -> ConstStore -> (t1 -> Cover -> ConstStore -> Curry_Prelude.C_IO t1) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_mapChildFamiliesIO x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id (d_C_mapChildrenIO x1)) (acceptCs id (d_C_mapFamilyIO x2)) x3250 x3500

nd_C_mapChildFamiliesIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> IDSupply -> Cover -> ConstStore -> Func (Func t1 (Curry_Prelude.C_IO t1)) (Func t0 (Curry_Prelude.C_IO t0))
nd_C_mapChildFamiliesIO x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX (wrapNX id) (acceptCs id (nd_C_mapChildrenIO x1))) (wrapDX (wrapNX id) (acceptCs id (nd_C_mapFamilyIO x2))) x2000 x3250 x3500))

d_C_evalFamilyIO :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_evalFamilyIO x1 x2 x3250 x3500 = d_C_mapFamilyIO x1 (d_OP_evalFamilyIO_dot_g_dot_36 x2 x1)

nd_C_evalFamilyIO :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> Func t0 (Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)) -> IDSupply -> Cover -> ConstStore -> Func t0 (Curry_Prelude.C_IO t0)
nd_C_evalFamilyIO x1 x2 x3000 x3250 x3500 = wrapNX id (nd_C_mapFamilyIO x1 (wrapNX id (nd_OP_evalFamilyIO_dot_g_dot_36 x2 x1)))

d_OP_evalFamilyIO_dot_g_dot_36 :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)) -> (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_evalFamilyIO_dot_g_dot_36 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) (Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return x3 x3250 x3500) (d_C_mapFamilyIO x2 (d_OP_evalFamilyIO_dot_g_dot_36 x1 x2))) x3250 x3500

nd_OP_evalFamilyIO_dot_g_dot_36 :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t0)) -> Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
nd_OP_evalFamilyIO_dot_g_dot_36 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_maybe (Curry_Prelude.d_C_return x3 x3250 x3500) (wrapNX id (nd_C_mapFamilyIO x2 (wrapNX id (nd_OP_evalFamilyIO_dot_g_dot_36 x1 x2)))))) x2001 x3250 x3500)))))

d_C_evalChildFamiliesIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> Cover -> ConstStore -> (t1 -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t1)) -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_evalChildFamiliesIO x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dot (acceptCs id (d_C_mapChildrenIO x1)) (d_C_evalFamilyIO x2) x3250 x3500

nd_C_evalChildFamiliesIO :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> IDSupply -> Cover -> ConstStore -> Func (Func t1 (Curry_Prelude.C_IO (Curry_Prelude.C_Maybe t1))) (Func t0 (Curry_Prelude.C_IO t0))
nd_C_evalChildFamiliesIO x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX (wrapNX id) (acceptCs id (nd_C_mapChildrenIO x1))) (wrapNX id (nd_C_evalFamilyIO x2)) x2000 x3250 x3500))

d_C_concatFL :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0) -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_concatFL x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_C_apply x3 (d_C_concatFL x4 x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_concatFL x1002 x2 x3250 x3500) (d_C_concatFL x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_concatFL z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_concatFL x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_concatFL :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)) -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_concatFL x1 x2 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x3 (nd_C_concatFL x4 x2 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_concatFL x1002 x2 x3000 x3250 x3500) (nd_C_concatFL x1003 x2 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_concatFL z x2 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_concatFL x1002 x2 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_familyFL :: Curry_Prelude.Curry t0 => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> t0)) -> t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_familyFL x1 x2 x3 x3250 x3500 = Curry_Prelude.OP_Cons x2 (d_C_childFamiliesFL x1 x1 x2 x3 x3250 x3500)

nd_C_familyFL :: Curry_Prelude.Curry t0 => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t0) t0)) -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_familyFL x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.OP_Cons x2 (nd_C_childFamiliesFL x1 x1 x2 x3 x2000 x3250 x3500)))

d_C_childFamiliesFL :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t0)) -> (t1 -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> t1)) -> t0 -> Curry_Prelude.OP_List t1 -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
d_C_childFamiliesFL x1 x2 x3 x4 x3250 x3500 = d_C_concatFL (Curry_Prelude.d_C_map (acceptCs id (d_C_familyFL x2)) (Curry_Prelude.d_C_apply (d_C_children x1 x3250 x3500) x3 x3250 x3500) x3250 x3500) x4 x3250 x3500

nd_C_childFamiliesFL :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t0)) -> Func t1 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t1) (Func (Curry_Prelude.OP_List t1) t1)) -> t0 -> Curry_Prelude.OP_List t1 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t1
nd_C_childFamiliesFL x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (nd_C_concatFL (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_map (wrapDX (wrapNX id) (acceptCs id (nd_C_familyFL x2))) (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_children x1 x2000 x3250 x3500) x3 x2001 x3250 x3500)))) x2003 x3250 x3500)))) x4 x2005 x3250 x3500)))))

d_C_liftIO :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => (t0 -> Cover -> ConstStore -> t1) -> Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t1
d_C_liftIO x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x2 (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500

nd_C_liftIO :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 t1 -> Curry_Prelude.C_IO t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO t1
nd_C_liftIO x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq x2 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) x1 x2000 x3250 x3500) x2001 x3250 x3500)))))
