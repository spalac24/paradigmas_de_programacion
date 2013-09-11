{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Demandedness (C_DemandDomain (..), C_DemandedArgs, d_C_showDemand, d_C_lub, d_C_demandAnalysis, nd_C_demandAnalysis, d_C_daFunc, d_C_daFuncRule, d_C_prelude) where

import Basics
import qualified Curry_Analysis
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_FlatCurryGoodies
type C_DemandedArgs = Curry_Prelude.OP_List Curry_Prelude.C_Int

data C_DemandDomain
     = C_Bot
     | C_Top
     | Choice_C_DemandDomain Cover ID C_DemandDomain C_DemandDomain
     | Choices_C_DemandDomain Cover ID ([C_DemandDomain])
     | Fail_C_DemandDomain Cover FailInfo
     | Guard_C_DemandDomain Cover Constraints C_DemandDomain

instance Show C_DemandDomain where
  showsPrec d (Choice_C_DemandDomain cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_DemandDomain cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_DemandDomain cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_DemandDomain cd info) = showChar '!'
  showsPrec _ C_Bot = showString "Bot"
  showsPrec _ C_Top = showString "Top"


instance Read C_DemandDomain where
  readsPrec _ s = (readParen False (\r -> [ (C_Bot,r0) | (_,r0) <- readQualified "Demandedness" "Bot" r]) s) ++ (readParen False (\r -> [ (C_Top,r0) | (_,r0) <- readQualified "Demandedness" "Top" r]) s)


instance NonDet C_DemandDomain where
  choiceCons = Choice_C_DemandDomain
  choicesCons = Choices_C_DemandDomain
  failCons = Fail_C_DemandDomain
  guardCons = Guard_C_DemandDomain
  try (Choice_C_DemandDomain cd i x y) = tryChoice cd i x y
  try (Choices_C_DemandDomain cd i xs) = tryChoices cd i xs
  try (Fail_C_DemandDomain cd info) = Fail cd info
  try (Guard_C_DemandDomain cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_DemandDomain cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_DemandDomain cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_DemandDomain cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_DemandDomain cd i _) = error ("Demandedness.DemandDomain.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_DemandDomain cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_DemandDomain cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_DemandDomain where
  generate s c = Choices_C_DemandDomain c (freeID [0,0] s) [C_Bot,C_Top]


instance NormalForm C_DemandDomain where
  ($!!) cont C_Bot d cs = cont C_Bot d cs
  ($!!) cont C_Top d cs = cont C_Top d cs
  ($!!) cont (Choice_C_DemandDomain cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_DemandDomain cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_DemandDomain cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_DemandDomain cd info) _ _ = failCons cd info
  ($##) cont C_Bot d cs = cont C_Bot d cs
  ($##) cont C_Top d cs = cont C_Top d cs
  ($##) cont (Choice_C_DemandDomain cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_DemandDomain cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_DemandDomain cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_DemandDomain cd info) _ _ = failCons cd info
  searchNF _ cont C_Bot = cont C_Bot
  searchNF _ cont C_Top = cont C_Top
  searchNF _ _ x = error ("Demandedness.DemandDomain.searchNF: no constructor: " ++ (show x))


instance Unifiable C_DemandDomain where
  (=.=) C_Bot C_Bot d cs = C_Success
  (=.=) C_Top C_Top d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Bot C_Bot d cs = C_Success
  (=.<=) C_Top C_Top d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Bot = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_Top = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_DemandDomain cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_DemandDomain cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_DemandDomain cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_DemandDomain cd i _) = error ("Demandedness.DemandDomain.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_DemandDomain cd info) = [(Unsolvable info)]
  bind d i (Guard_C_DemandDomain cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Bot = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_Top = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_DemandDomain cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_DemandDomain cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_DemandDomain cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_DemandDomain cd i _) = error ("Demandedness.DemandDomain.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_DemandDomain cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_DemandDomain cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_DemandDomain where
  (=?=) (Choice_C_DemandDomain cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_DemandDomain cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_DemandDomain cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_DemandDomain cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_DemandDomain cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_DemandDomain cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_DemandDomain cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_DemandDomain cd info) _ _ = failCons cd info
  (=?=) C_Bot C_Bot d cs = Curry_Prelude.C_True
  (=?=) C_Top C_Top d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_DemandDomain cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_DemandDomain cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_DemandDomain cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_DemandDomain cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_DemandDomain cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_DemandDomain cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_DemandDomain cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_DemandDomain cd info) _ _ = failCons cd info
  (<?=) C_Bot C_Bot d cs = Curry_Prelude.C_True
  (<?=) C_Bot C_Top _ _ = Curry_Prelude.C_True
  (<?=) C_Top C_Top d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_showDemand :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showDemand x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> d_OP__case_11 x1 x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_plus_plus (d_OP__case_10 x1 (Curry_Prelude.d_OP_eq_eq x1 Curry_Analysis.C_AText x3250 x3500) x3250 x3500) (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map Curry_Prelude.d_C_show (Curry_Prelude.OP_Cons x3 x4) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_showDemand x1 x1002 x3250 x3500) (d_C_showDemand x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_showDemand x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_showDemand x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lub :: C_DemandDomain -> C_DemandDomain -> Cover -> ConstStore -> C_DemandDomain
d_C_lub x1 x2 x3250 x3500 = case x1 of
     C_Bot -> x2
     C_Top -> C_Top
     (Choice_C_DemandDomain x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lub x1002 x2 x3250 x3500) (d_C_lub x1003 x2 x3250 x3500)
     (Choices_C_DemandDomain x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lub z x2 x3250 x3500) x1002
     (Guard_C_DemandDomain x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lub x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_DemandDomain x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_demandAnalysis :: Cover -> ConstStore -> Curry_Analysis.C_Analysis (Curry_Prelude.OP_List Curry_Prelude.C_Int)
d_C_demandAnalysis x3250 x3500 = Curry_Analysis.d_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3250 x3500) (acceptCs id d_C_daFunc) x3250 x3500

nd_C_demandAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Analysis.C_Analysis (Curry_Prelude.OP_List Curry_Prelude.C_Int)
nd_C_demandAnalysis x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Analysis.nd_C_dependencyFuncAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3250 x3500) (wrapDX (wrapDX id) (acceptCs id d_C_daFunc)) x2000 x3250 x3500))

d_C_daFunc :: Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_daFunc x1 x2 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> d_OP__case_9 x7 x2 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_daFunc x1002 x2 x3250 x3500) (d_C_daFunc x1003 x2 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_daFunc z x2 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_daFunc x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_daFuncRule :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_C_daFuncRule x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_External x3) -> Curry_Prelude.OP_List
     (Curry_FlatCurry.C_Rule x4 x5) -> Curry_Prelude.d_C_map Curry_Prelude.d_C_fst (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) C_Bot) Curry_Prelude.d_C_snd x3250 x3500) (Curry_Prelude.d_C_map (d_OP_daFuncRule_dot___hash_lambda3 x1 x5) x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_daFuncRule x1 x1002 x3250 x3500) (d_C_daFuncRule x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_daFuncRule x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_daFuncRule x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_daFuncRule_dot_absEvalExpr_dot_30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> C_DemandDomain
d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x2 x3 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Var x4) -> d_OP__case_5 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x4 x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Lit x5) -> C_Top
     (Curry_FlatCurry.C_Comb x6 x7 x8) -> d_OP__case_4 x6 x1 x7 x8 x3 (Curry_Prelude.d_OP_eq_eq x6 Curry_FlatCurry.C_FuncCall x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x9 x10) -> d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x10 x3 x3250 x3500
     (Curry_FlatCurry.C_Let x11 x12) -> d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x12 (d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x11 x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Or x13 x14) -> d_C_lub (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x13 x3 x3250 x3500) (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x14 x3 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x15 x16 x17) -> d_OP__case_3 x3 x16 x1 x17 (Curry_Prelude.d_OP_eq_eq (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x16 x3 x3250 x3500) C_Bot x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x18 x19) -> d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x18 x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x1002 x3 x3250 x3500) (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x1003 x3 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 z x3 x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> C_DemandDomain
d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_C_map (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2) (Curry_Prelude.d_C_zip (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3250 x3500) x3 x3250 x3500) x3250 x3500
     x6 = Curry_List.d_OP_backslash_backslash x4 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) C_Bot) Curry_Prelude.d_C_snd x3250 x3500) x5 x3250 x3500) x3250 x3500) x3250 x3500
      in (d_OP__case_2 x6 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500)

d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int C_DemandDomain
d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.OP_Tuple2 x4 (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x2 x5 x1 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1002 x3250 x3500) (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_DemandDomain
d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Branch x4 x5) -> d_OP_daFuncRule_dot_absEvalExpr_dot_30 x2 x5 x1 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x1 x2 x1002 x3250 x3500) (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x1 x2 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x1 x2 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_daFuncRule_dot_absEvalBindings_dot_30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> x3
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_1 x3 x1 x5 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x1002 x3 x3250 x3500) (d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_daFuncRule_dot___hash_lambda3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_FlatCurry.C_Expr -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int C_DemandDomain
d_OP_daFuncRule_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = Curry_Prelude.OP_Tuple2 x3 (d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x2 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3250 x3500)

d_C_prelude :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_1 x3 x1 x5 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x8 = d_OP_daFuncRule_dot_absEvalExpr_dot_30 x1 x7 x3 x3250 x3500
           in (d_OP__case_0 x8 x3 x5 x1 x6 (Curry_Prelude.d_OP_eq_eq x8 C_Bot x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x1 x5 x1002 x3250 x3500) (d_OP__case_1 x3 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: C_DemandDomain -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_FlatCurry.C_Expr) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_0 x8 x3 x5 x1 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x5 (Curry_Prelude.OP_Cons x6 x3) x3250 x3500
     Curry_Prelude.C_False -> d_OP_daFuncRule_dot_absEvalBindings_dot_30 x1 x5 x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x8 x3 x5 x1 x6 x1002 x3250 x3500) (d_OP__case_0 x8 x3 x5 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x8 x3 x5 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x8 x3 x5 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_DemandDomain
d_OP__case_2 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> C_Top
     Curry_Prelude.C_False -> C_Bot
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x6 x1002 x3250 x3500) (d_OP__case_2 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_DemandDomain
d_OP__case_3 x3 x16 x1 x17 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> C_Bot
     Curry_Prelude.C_False -> Curry_Prelude.d_C_foldr (acceptCs id d_C_lub) C_Bot (Curry_Prelude.d_C_map (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot_absEvalBranch_dot_54 x3 x1) x17 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x16 x1 x17 x1002 x3250 x3500) (d_OP__case_3 x3 x16 x1 x17 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x16 x1 x17 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x16 x1 x17 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_FlatCurry.C_CombType -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_DemandDomain
d_OP__case_4 x6 x1 x7 x8 x3 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x7 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_daFuncRule_dot_absEvalExpr_dot_30_dot___hash_lambda1 x3 x1 x8) (Curry_Prelude.d_C_lookup x7 x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> C_Top
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x6 x1 x7 x8 x3 x1002 x3250 x3500) (d_OP__case_4 x6 x1 x7 x8 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x6 x1 x7 x8 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x6 x1 x7 x8 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_DemandDomain
d_OP__case_5 x3 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> C_Bot
     Curry_Prelude.C_False -> C_Top
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x4 x1002 x3250 x3500) (d_OP__case_5 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_FlatCurry.C_Rule -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_9 x7 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x10 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List))))))))))))))))
          x11 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))) Curry_Prelude.OP_List)))))
           in (d_OP__case_8 x8 x10 x9 x11 x7 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x9 x3250 x3500) x10 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x8 (d_C_prelude x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x7 x2 x1002 x3250 x3500) (d_OP__case_9 x7 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x7 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x7 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Rule -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_8 x8 x10 x9 x11 x7 x2 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 1#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 2#) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> d_OP__case_7 x8 x11 x9 x7 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x9 x3250 x3500) x11 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x8 (d_C_prelude x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x8 x10 x9 x11 x7 x2 x1002 x3250 x3500) (d_OP__case_8 x8 x10 x9 x11 x7 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x8 x10 x9 x11 x7 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x8 x10 x9 x11 x7 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FlatCurry.C_Rule -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_7 x8 x11 x9 x7 x2 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 1#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_6 x7 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x8 x11 x9 x7 x2 x1002 x3250 x3500) (d_OP__case_7 x8 x11 x9 x7 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x8 x11 x9 x7 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x8 x11 x9 x7 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_FlatCurry.C_Rule -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Int)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Int
d_OP__case_6 x7 x2 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_daFuncRule x2 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x7 x2 x1002 x3250 x3500) (d_OP__case_6 x7 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x7 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x7 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Analysis.C_AOutFormat -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_10 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x1 x1002 x3250 x3500) (d_OP__case_10 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_11 x1 x3250 x3500 = case x1 of
     Curry_Analysis.C_AText -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))
     Curry_Analysis.C_ANote -> Curry_Prelude.OP_List
     (Curry_Analysis.Choice_C_AOutFormat x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1002 x3250 x3500) (d_OP__case_11 x1003 x3250 x3500)
     (Curry_Analysis.Choices_C_AOutFormat x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 z x3250 x3500) x1002
     (Curry_Analysis.Guard_C_AOutFormat x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Analysis.Fail_C_AOutFormat x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
