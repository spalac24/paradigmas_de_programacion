{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Parser (C_Parser, C_ParserRep, nd_OP_lt_bar_gt, nd_OP_lt_bar_bar_gt, nd_OP_lt_star_gt, nd_OP_gt_gt_gt, d_C_empty, d_C_terminal, d_C_satisfy, nd_C_satisfy, nd_C_star, nd_C_some) where

import Basics
import qualified Curry_Prelude
type C_Parser t0 = Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0

type C_ParserRep t0 t1 = t0 -> ConstStore -> Curry_Prelude.OP_List t1 -> ConstStore -> Curry_Prelude.OP_List t1

nd_OP_lt_bar_gt :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_lt_bar_gt x1 x2 x3000 x3500 = wrapNX id (nd_OP_lt_bar_gt_dot___hash_lambda1 x1 x2)

nd_OP_lt_bar_gt_dot___hash_lambda1 :: Curry_Prelude.Curry t1 => Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1) -> Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1) -> Curry_Prelude.OP_List t1 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t1
nd_OP_lt_bar_gt_dot___hash_lambda1 x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_qmark (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_apply x2 x3 x2001 x3500) x2002 x3500))))))))

nd_OP_lt_bar_bar_gt :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1)) -> Func t0 (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1)) -> IDSupply -> ConstStore -> Func t0 (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1))
nd_OP_lt_bar_bar_gt x1 x2 x3000 x3500 = wrapNX id (nd_OP_lt_bar_bar_gt_dot___hash_lambda2 x1 x2)

nd_OP_lt_bar_bar_gt_dot___hash_lambda2 :: (Curry_Prelude.Curry t14,Curry_Prelude.Curry t15) => Func t14 (Func (Curry_Prelude.OP_List t15) (Curry_Prelude.OP_List t15)) -> Func t14 (Func (Curry_Prelude.OP_List t15) (Curry_Prelude.OP_List t15)) -> t14 -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t15) (Curry_Prelude.OP_List t15)
nd_OP_lt_bar_bar_gt_dot___hash_lambda2 x1 x2 x3 x3000 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (nd_OP_lt_bar_gt (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) (Curry_Prelude.nd_C_apply x2 x3 x2001 x3500) x2002 x3500))))))))

nd_OP_lt_star_gt :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0)
nd_OP_lt_star_gt x1 x2 x3000 x3500 = wrapNX id (nd_OP_lt_star_gt_dot_seq_dot_8 x1 x2)

nd_OP_lt_star_gt_dot_seq_dot_8 :: Curry_Prelude.Curry t20 => Func (Curry_Prelude.OP_List t20) (Curry_Prelude.OP_List t20) -> Func (Curry_Prelude.OP_List t20) (Curry_Prelude.OP_List t20) -> Curry_Prelude.OP_List t20 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t20
nd_OP_lt_star_gt_dot_seq_dot_8 x1 x2 x3 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x4 = generate x2003
                in (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x2 x4 (Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.nd_C_apply x1 x3 x2000 x3500) x4 x3500) x2001 x3500)))))))))

d_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_Success -> Curry_Prelude.d_C_apply x1 x2 x3500
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1002 x3500) (d_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_Success -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500))
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1002 x3000 x3500) (nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__lt_star_gt_dot_seq_dot_8 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_gt_gt_gt :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> t1 -> IDSupply -> ConstStore -> Func t1 (Func (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0))
nd_OP_gt_gt_gt x1 x2 x3000 x3500 = wrapDX (wrapNX id) (acceptCs id (nd_OP_gt_gt_gt_dot_attach_dot_12 x1 x2))

nd_OP_gt_gt_gt_dot_attach_dot_12 :: (Curry_Prelude.Curry t38,Curry_Prelude.Curry t33) => Func (Curry_Prelude.OP_List t33) (Curry_Prelude.OP_List t33) -> t38 -> t38 -> Curry_Prelude.OP_List t33 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t33
nd_OP_gt_gt_gt_dot_attach_dot_12 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2002 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2002 (seq x2003 (let
               x5 = generate x2003
                in (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x5 (Curry_Prelude.d_OP_ampersand_gt (Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) x5 x3500) (Curry_Prelude.d_OP_eq_colon_eq x2 x3 x3500) x3500) x2001 x3500)))))))))

d_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1002 x3500) (d_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1002 x3000 x3500) (nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0__gt_gt_gt_dot_attach_dot_12 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_empty :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_empty x1 x3500 = x1

d_C_terminal :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_terminal x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP___cond_0_terminal x4 (Curry_Prelude.d_OP_eq_colon_eq x1 x3 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_terminal x1 x1002 x3500) (d_C_terminal x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_terminal x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_terminal x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0_terminal x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_terminal x1 x1002 x3500) (d_OP___cond_0_terminal x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_terminal x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_terminal x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_terminal x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_terminal x1 x1002 x3000 x3500) (nd_OP___cond_0_terminal x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_terminal x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_terminal x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_satisfy :: Curry_Prelude.Curry t0 => (t0 -> ConstStore -> Curry_Prelude.C_Bool) -> t0 -> Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_satisfy x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP___cond_0_satisfy x5 (Curry_Prelude.d_OP_ampersand (Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.d_C_apply x1 x4 x3500) Curry_Prelude.C_True x3500) (Curry_Prelude.d_OP_eq_colon_eq x2 x4 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_satisfy x1 x2 x1002 x3500) (d_C_satisfy x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_satisfy x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_satisfy x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_satisfy :: Curry_Prelude.Curry t0 => Func t0 Curry_Prelude.C_Bool -> t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_satisfy x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP___cond_0_satisfy x5 (Curry_Prelude.d_OP_ampersand (Curry_Prelude.d_OP_eq_colon_eq (Curry_Prelude.nd_C_apply x1 x4 x2000 x3500) Curry_Prelude.C_True x3500) (Curry_Prelude.d_OP_eq_colon_eq x2 x4 x3500) x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_satisfy x1 x2 x1002 x3000 x3500) (nd_C_satisfy x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_satisfy x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_satisfy x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP___cond_0_satisfy x1 x2 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP___cond_0_satisfy x1 x1002 x3500) (d_OP___cond_0_satisfy x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP___cond_0_satisfy x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP___cond_0_satisfy x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP___cond_0_satisfy x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_Success -> x1
     (Curry_Prelude.Choice_C_Success x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP___cond_0_satisfy x1 x1002 x3000 x3500) (nd_OP___cond_0_satisfy x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Success x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP___cond_0_satisfy x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Success x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP___cond_0_satisfy x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Success x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_star :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1)) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1))
nd_C_star x1 x3000 x3500 = let
     x2015 = x3000
      in (seq x2015 (let
          x2011 = leftSupply x2015
          x2016 = rightSupply x2015
           in (seq x2011 (seq x2016 (let
               x2013 = leftSupply x2016
               x2014 = rightSupply x2016
                in (seq x2013 (seq x2014 (let
                    x2 = generate x2013
                    x3 = generate x2014
                     in (let
                         x2010 = leftSupply x2011
                         x2012 = rightSupply x2011
                          in (seq x2010 (seq x2012 (let
                              x2008 = leftSupply x2012
                              x2009 = rightSupply x2012
                               in (seq x2008 (seq x2009 (nd_OP_lt_bar_bar_gt (let
                                   x2007 = leftSupply x2008
                                   x2005 = rightSupply x2008
                                    in (seq x2007 (seq x2005 (nd_OP_gt_gt_gt (let
                                        x2004 = leftSupply x2005
                                        x2006 = rightSupply x2005
                                         in (seq x2004 (seq x2006 (let
                                             x2000 = leftSupply x2006
                                             x2003 = rightSupply x2006
                                              in (seq x2000 (seq x2003 (nd_OP_lt_star_gt (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) (let
                                                  x2002 = leftSupply x2003
                                                  x2001 = rightSupply x2003
                                                   in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_star x1 x2001 x3500) x3 x2002 x3500)))) x2004 x3500))))))) (Curry_Prelude.OP_Cons x2 x3) x2007 x3500)))) (nd_OP_gt_gt_gt (wrapDX id d_C_empty) Curry_Prelude.OP_List x2009 x3500) x2010 x3500)))))))))))))))

nd_C_some :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Func t0 (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1)) -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List t0) (Func (Curry_Prelude.OP_List t1) (Curry_Prelude.OP_List t1))
nd_C_some x1 x3000 x3500 = let
     x2011 = x3000
      in (seq x2011 (let
          x2008 = leftSupply x2011
          x2012 = rightSupply x2011
           in (seq x2008 (seq x2012 (let
               x2009 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2009 (seq x2010 (let
                    x2 = generate x2009
                    x3 = generate x2010
                     in (let
                         x2007 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2007 (seq x2005 (nd_OP_gt_gt_gt (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2000 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2000 (seq x2003 (nd_OP_lt_star_gt (Curry_Prelude.nd_C_apply x1 x2 x2000 x3500) (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_star x1 x2001 x3500) x3 x2002 x3500)))) x2004 x3500))))))) (Curry_Prelude.OP_Cons x2 x3) x2007 x3500))))))))))))
