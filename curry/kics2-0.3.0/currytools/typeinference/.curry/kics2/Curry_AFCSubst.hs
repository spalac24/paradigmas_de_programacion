{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AFCSubst (C_AFCSubst, d_C_showAFCSubst, nd_C_showAFCSubst, d_C_emptyAFCSubst, nd_C_emptyAFCSubst, d_C_lookupAFCSubst, nd_C_lookupAFCSubst, d_C_substFunc, nd_C_substFunc, d_C_substRule, nd_C_substRule, d_C_substExpr, nd_C_substExpr, d_C_substSnd, nd_C_substSnd, d_C_substBranch, nd_C_substBranch, d_C_substPattern, nd_C_substPattern, d_C_subst, nd_C_subst) where

import Basics
import qualified Curry_AnnotatedFlatCurry
import qualified Curry_FiniteMap
import qualified Curry_FlatCurry
import qualified Curry_Prelude
type C_AFCSubst = Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr

d_C_showAFCSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showAFCSubst x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_unlines (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_map d_OP_showAFCSubst_dot_showOne_dot_2) Curry_FiniteMap.d_C_fmToList x3250 x3500) x3250 x3500

nd_C_showAFCSubst :: IDSupply -> Cover -> ConstStore -> Func (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showAFCSubst x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_unlines) (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_OP_showAFCSubst_dot_showOne_dot_2))) (wrapNX id Curry_FiniteMap.nd_C_fmToList) x2000 x3250 x3500) x2001 x3250 x3500)))))

d_OP_showAFCSubst_dot_showOne_dot_2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 t0 t1 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showAFCSubst_dot_showOne_dot_2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_C_show x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_showAFCSubst_dot_showOne_dot_2 x1002 x3250 x3500) (d_OP_showAFCSubst_dot_showOne_dot_2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_showAFCSubst_dot_showOne_dot_2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_showAFCSubst_dot_showOne_dot_2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_emptyAFCSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr
d_C_emptyAFCSubst x3250 x3500 = Curry_FiniteMap.d_C_emptyFM (acceptCs id Curry_Prelude.d_OP_lt) x3250 x3500

nd_C_emptyAFCSubst :: IDSupply -> Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr
nd_C_emptyAFCSubst x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_FiniteMap.nd_C_emptyFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x2000 x3250 x3500))

d_C_lookupAFCSubst :: Cover -> ConstStore -> Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_FlatCurry.C_TypeExpr
d_C_lookupAFCSubst x3250 x3500 = acceptCs id Curry_FiniteMap.d_C_lookupFM

nd_C_lookupAFCSubst :: IDSupply -> Cover -> ConstStore -> Func (Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr) (Func Curry_Prelude.C_Int (Curry_Prelude.C_Maybe Curry_FlatCurry.C_TypeExpr))
nd_C_lookupAFCSubst x3000 x3250 x3500 = wrapDX (wrapNX id) (acceptCs id Curry_FiniteMap.nd_C_lookupFM)

d_C_substFunc :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_AFuncDecl Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_AFuncDecl Curry_FlatCurry.C_TypeExpr
d_C_substFunc x1 x2 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_AFunc x3 x4 x5 x6 x7) -> Curry_AnnotatedFlatCurry.C_AFunc x3 x4 x5 (d_C_subst x1 x6 x3250 x3500) (d_C_substRule x1 x7 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choice_C_AFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substFunc x1 x1002 x3250 x3500) (d_C_substFunc x1 x1003 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_AFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substFunc x1 z x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_AFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substFunc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_AFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substFunc :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_AFuncDecl Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_AFuncDecl Curry_FlatCurry.C_TypeExpr
nd_C_substFunc x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_AFunc x3 x4 x5 x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AnnotatedFlatCurry.C_AFunc x3 x4 x5 (nd_C_subst x1 x6 x2000 x3250 x3500) (nd_C_substRule x1 x7 x2001 x3250 x3500))))))
     (Curry_AnnotatedFlatCurry.Choice_C_AFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substFunc x1 x1002 x3000 x3250 x3500) (nd_C_substFunc x1 x1003 x3000 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_AFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substFunc x1 z x3000 x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_AFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substFunc x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_AFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substRule :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_ARule Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_ARule Curry_FlatCurry.C_TypeExpr
d_C_substRule x1 x2 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_ARule x3 x4 x5) -> Curry_AnnotatedFlatCurry.C_ARule (d_C_subst x1 x3 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substSnd x1) x4 x3250 x3500) (d_C_substExpr x1 x5 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_AExternal x6 x7) -> Curry_AnnotatedFlatCurry.C_AExternal (d_C_subst x1 x6 x3250 x3500) x7
     (Curry_AnnotatedFlatCurry.Choice_C_ARule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substRule x1 x1002 x3250 x3500) (d_C_substRule x1 x1003 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_ARule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substRule x1 z x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_ARule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substRule x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_ARule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substRule :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_ARule Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_ARule Curry_FlatCurry.C_TypeExpr
nd_C_substRule x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_ARule x3 x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_ARule (nd_C_subst x1 x3 x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substSnd x1)) x4 x2001 x3250 x3500) (nd_C_substExpr x1 x5 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_AExternal x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AnnotatedFlatCurry.C_AExternal (nd_C_subst x1 x6 x2000 x3250 x3500) x7))
     (Curry_AnnotatedFlatCurry.Choice_C_ARule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substRule x1 x1002 x3000 x3250 x3500) (nd_C_substRule x1 x1003 x3000 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_ARule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substRule x1 z x3000 x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_ARule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substRule x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_ARule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substExpr :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr
d_C_substExpr x1 x2 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_AComb x3 x4 x5 x6) -> Curry_AnnotatedFlatCurry.C_AComb (d_C_subst x1 x3 x3250 x3500) x4 (d_C_substSnd x1 x5 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substExpr x1) x6 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_AVar x7 x8) -> Curry_AnnotatedFlatCurry.C_AVar (d_C_subst x1 x7 x3250 x3500) x8
     (Curry_AnnotatedFlatCurry.C_ACase x9 x10 x11 x12) -> Curry_AnnotatedFlatCurry.C_ACase (d_C_subst x1 x9 x3250 x3500) x10 (d_C_substExpr x1 x11 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substBranch x1) x12 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_ALit x13 x14) -> Curry_AnnotatedFlatCurry.C_ALit (d_C_subst x1 x13 x3250 x3500) x14
     (Curry_AnnotatedFlatCurry.C_AOr x15 x16 x17) -> Curry_AnnotatedFlatCurry.C_AOr (d_C_subst x1 x15 x3250 x3500) (d_C_substExpr x1 x16 x3250 x3500) (d_C_substExpr x1 x17 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_ALet x18 x19 x20) -> Curry_AnnotatedFlatCurry.C_ALet (d_C_subst x1 x18 x3250 x3500) (Curry_Prelude.d_C_map (d_OP_substExpr_dot_substBinding_dot_26 x1) x19 x3250 x3500) (d_C_substExpr x1 x20 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_AFree x21 x22 x23) -> Curry_AnnotatedFlatCurry.C_AFree (d_C_subst x1 x21 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substSnd x1) x22 x3250 x3500) (d_C_substExpr x1 x23 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_ATyped x24 x25 x26) -> Curry_AnnotatedFlatCurry.C_ATyped (d_C_subst x1 x24 x3250 x3500) (d_C_substExpr x1 x25 x3250 x3500) (d_C_subst x1 x26 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choice_C_AExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substExpr x1 x1002 x3250 x3500) (d_C_substExpr x1 x1003 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_AExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substExpr x1 z x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_AExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substExpr x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_AExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substExpr :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr
nd_C_substExpr x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_AComb x3 x4 x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_AComb (nd_C_subst x1 x3 x2000 x3250 x3500) x4 (nd_C_substSnd x1 x5 x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substExpr x1)) x6 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_AVar x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AnnotatedFlatCurry.C_AVar (nd_C_subst x1 x7 x2000 x3250 x3500) x8))
     (Curry_AnnotatedFlatCurry.C_ACase x9 x10 x11 x12) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_ACase (nd_C_subst x1 x9 x2000 x3250 x3500) x10 (nd_C_substExpr x1 x11 x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substBranch x1)) x12 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_ALit x13 x14) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AnnotatedFlatCurry.C_ALit (nd_C_subst x1 x13 x2000 x3250 x3500) x14))
     (Curry_AnnotatedFlatCurry.C_AOr x15 x16 x17) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_AOr (nd_C_subst x1 x15 x2000 x3250 x3500) (nd_C_substExpr x1 x16 x2001 x3250 x3500) (nd_C_substExpr x1 x17 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_ALet x18 x19 x20) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_ALet (nd_C_subst x1 x18 x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_substExpr_dot_substBinding_dot_26 x1)) x19 x2001 x3250 x3500) (nd_C_substExpr x1 x20 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_AFree x21 x22 x23) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_AFree (nd_C_subst x1 x21 x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substSnd x1)) x22 x2001 x3250 x3500) (nd_C_substExpr x1 x23 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_ATyped x24 x25 x26) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_ATyped (nd_C_subst x1 x24 x2000 x3250 x3500) (nd_C_substExpr x1 x25 x2001 x3250 x3500) (nd_C_subst x1 x26 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.Choice_C_AExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substExpr x1 x1002 x3000 x3250 x3500) (nd_C_substExpr x1 x1003 x3000 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_AExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substExpr x1 z x3000 x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_AExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substExpr x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_AExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_substExpr_dot_substBinding_dot_26 :: Curry_Prelude.Curry t0 => Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 t0 (Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr)
d_OP_substExpr_dot_substBinding_dot_26 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (d_C_substExpr x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_substExpr_dot_substBinding_dot_26 x1 x1002 x3250 x3500) (d_OP_substExpr_dot_substBinding_dot_26 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_substExpr_dot_substBinding_dot_26 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_substExpr_dot_substBinding_dot_26 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_substExpr_dot_substBinding_dot_26 :: Curry_Prelude.Curry t0 => Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 t0 (Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 (Curry_AnnotatedFlatCurry.C_AExpr Curry_FlatCurry.C_TypeExpr)
nd_OP_substExpr_dot_substBinding_dot_26 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x3 (nd_C_substExpr x1 x4 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_substExpr_dot_substBinding_dot_26 x1 x1002 x3000 x3250 x3500) (nd_OP_substExpr_dot_substBinding_dot_26 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_substExpr_dot_substBinding_dot_26 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_substExpr_dot_substBinding_dot_26 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substSnd :: Curry_Prelude.Curry t0 => Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 t0 Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 Curry_FlatCurry.C_TypeExpr
d_C_substSnd x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 (d_C_subst x1 x4 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substSnd x1 x1002 x3250 x3500) (d_C_substSnd x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substSnd x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substSnd x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substSnd :: Curry_Prelude.Curry t0 => Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_Prelude.OP_Tuple2 t0 Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 t0 Curry_FlatCurry.C_TypeExpr
nd_C_substSnd x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x3 (nd_C_subst x1 x4 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substSnd x1 x1002 x3000 x3250 x3500) (nd_C_substSnd x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substSnd x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substSnd x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substBranch :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_ABranchExpr Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_ABranchExpr Curry_FlatCurry.C_TypeExpr
d_C_substBranch x1 x2 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_ABranch x3 x4) -> Curry_AnnotatedFlatCurry.C_ABranch (d_C_substPattern x1 x3 x3250 x3500) (d_C_substExpr x1 x4 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choice_C_ABranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substBranch x1 x1002 x3250 x3500) (d_C_substBranch x1 x1003 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_ABranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substBranch x1 z x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_ABranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substBranch x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_ABranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substBranch :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_ABranchExpr Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_ABranchExpr Curry_FlatCurry.C_TypeExpr
nd_C_substBranch x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_ABranch x3 x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_AnnotatedFlatCurry.C_ABranch (nd_C_substPattern x1 x3 x2000 x3250 x3500) (nd_C_substExpr x1 x4 x2001 x3250 x3500))))))
     (Curry_AnnotatedFlatCurry.Choice_C_ABranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substBranch x1 x1002 x3000 x3250 x3500) (nd_C_substBranch x1 x1003 x3000 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_ABranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substBranch x1 z x3000 x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_ABranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substBranch x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_ABranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_substPattern :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_APattern Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_APattern Curry_FlatCurry.C_TypeExpr
d_C_substPattern x1 x2 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_APattern x3 x4 x5) -> Curry_AnnotatedFlatCurry.C_APattern (d_C_subst x1 x3 x3250 x3500) (d_C_substSnd x1 x4 x3250 x3500) (Curry_Prelude.d_C_map (d_C_substSnd x1) x5 x3250 x3500)
     (Curry_AnnotatedFlatCurry.C_ALPattern x6 x7) -> Curry_AnnotatedFlatCurry.C_ALPattern (d_C_subst x1 x6 x3250 x3500) x7
     (Curry_AnnotatedFlatCurry.Choice_C_APattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_substPattern x1 x1002 x3250 x3500) (d_C_substPattern x1 x1003 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_APattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_substPattern x1 z x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_APattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_substPattern x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_APattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_substPattern :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_AnnotatedFlatCurry.C_APattern Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_AnnotatedFlatCurry.C_APattern Curry_FlatCurry.C_TypeExpr
nd_C_substPattern x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AnnotatedFlatCurry.C_APattern x3 x4 x5) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2000 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2000 (seq x2004 (let
                    x2001 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2001 (seq x2002 (Curry_AnnotatedFlatCurry.C_APattern (nd_C_subst x1 x3 x2000 x3250 x3500) (nd_C_substSnd x1 x4 x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_substSnd x1)) x5 x2002 x3250 x3500)))))))))
     (Curry_AnnotatedFlatCurry.C_ALPattern x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (Curry_AnnotatedFlatCurry.C_ALPattern (nd_C_subst x1 x6 x2000 x3250 x3500) x7))
     (Curry_AnnotatedFlatCurry.Choice_C_APattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_substPattern x1 x1002 x3000 x3250 x3500) (nd_C_substPattern x1 x1003 x3000 x3250 x3500)
     (Curry_AnnotatedFlatCurry.Choices_C_APattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_substPattern x1 z x3000 x3250 x3500) x1002
     (Curry_AnnotatedFlatCurry.Guard_C_APattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_substPattern x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AnnotatedFlatCurry.Fail_C_APattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_subst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_TypeExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeExpr
d_C_subst x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_TVar x3) -> Curry_Prelude.d_C_maybe x2 Curry_Prelude.d_C_id (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupAFCSubst x3250 x3500) x1 x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_TCons x4 x5) -> Curry_FlatCurry.C_TCons x4 (Curry_Prelude.d_C_map (d_C_subst x1) x5 x3250 x3500)
     (Curry_FlatCurry.C_FuncType x6 x7) -> Curry_FlatCurry.C_FuncType (d_C_subst x1 x6 x3250 x3500) (d_C_subst x1 x7 x3250 x3500)
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_subst x1 x1002 x3250 x3500) (d_C_subst x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_subst x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_subst x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_subst :: Curry_FiniteMap.C_FM Curry_Prelude.C_Int Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_TypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeExpr
nd_C_subst x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_TVar x3) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_maybe x2 (wrapDX id Curry_Prelude.d_C_id) (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_lookupAFCSubst x2000 x3250 x3500) x1 x2001 x3250 x3500)))) x3 x2003 x3250 x3500)))) x2005 x3250 x3500)))))
     (Curry_FlatCurry.C_TCons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (Curry_FlatCurry.C_TCons x4 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_subst x1)) x5 x2000 x3250 x3500)))
     (Curry_FlatCurry.C_FuncType x6 x7) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_FlatCurry.C_FuncType (nd_C_subst x1 x6 x2000 x3250 x3500) (nd_C_subst x1 x7 x2001 x3250 x3500))))))
     (Curry_FlatCurry.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_subst x1 x1002 x3000 x3250 x3500) (nd_C_subst x1 x1003 x3000 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_subst x1 z x3000 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_subst x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
