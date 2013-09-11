{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlexRigid (C_FlexRigidResult (..), d_C_getFlexRigid) where

import Basics
import qualified Curry_FlatCurry
import qualified Curry_Prelude
data C_FlexRigidResult
     = C_UnknownFR
     | C_ConflictFR
     | C_KnownFlex
     | C_KnownRigid
     | Choice_C_FlexRigidResult Cover ID C_FlexRigidResult C_FlexRigidResult
     | Choices_C_FlexRigidResult Cover ID ([C_FlexRigidResult])
     | Fail_C_FlexRigidResult Cover FailInfo
     | Guard_C_FlexRigidResult Cover Constraints C_FlexRigidResult

instance Show C_FlexRigidResult where
  showsPrec d (Choice_C_FlexRigidResult cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_FlexRigidResult cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_FlexRigidResult cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_FlexRigidResult cd info) = showChar '!'
  showsPrec _ C_UnknownFR = showString "UnknownFR"
  showsPrec _ C_ConflictFR = showString "ConflictFR"
  showsPrec _ C_KnownFlex = showString "KnownFlex"
  showsPrec _ C_KnownRigid = showString "KnownRigid"


instance Read C_FlexRigidResult where
  readsPrec _ s = (readParen False (\r -> [ (C_UnknownFR,r0) | (_,r0) <- readQualified "FlexRigid" "UnknownFR" r]) s) ++ ((readParen False (\r -> [ (C_ConflictFR,r0) | (_,r0) <- readQualified "FlexRigid" "ConflictFR" r]) s) ++ ((readParen False (\r -> [ (C_KnownFlex,r0) | (_,r0) <- readQualified "FlexRigid" "KnownFlex" r]) s) ++ (readParen False (\r -> [ (C_KnownRigid,r0) | (_,r0) <- readQualified "FlexRigid" "KnownRigid" r]) s)))


instance NonDet C_FlexRigidResult where
  choiceCons = Choice_C_FlexRigidResult
  choicesCons = Choices_C_FlexRigidResult
  failCons = Fail_C_FlexRigidResult
  guardCons = Guard_C_FlexRigidResult
  try (Choice_C_FlexRigidResult cd i x y) = tryChoice cd i x y
  try (Choices_C_FlexRigidResult cd i xs) = tryChoices cd i xs
  try (Fail_C_FlexRigidResult cd info) = Fail cd info
  try (Guard_C_FlexRigidResult cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_FlexRigidResult cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_FlexRigidResult cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_FlexRigidResult cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_FlexRigidResult cd i _) = error ("FlexRigid.FlexRigidResult.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_FlexRigidResult cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_FlexRigidResult cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_FlexRigidResult where
  generate s c = Choices_C_FlexRigidResult c (freeID [0,0,0,0] s) [C_UnknownFR,C_ConflictFR,C_KnownFlex,C_KnownRigid]


instance NormalForm C_FlexRigidResult where
  ($!!) cont C_UnknownFR d cs = cont C_UnknownFR d cs
  ($!!) cont C_ConflictFR d cs = cont C_ConflictFR d cs
  ($!!) cont C_KnownFlex d cs = cont C_KnownFlex d cs
  ($!!) cont C_KnownRigid d cs = cont C_KnownRigid d cs
  ($!!) cont (Choice_C_FlexRigidResult cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_FlexRigidResult cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_FlexRigidResult cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_FlexRigidResult cd info) _ _ = failCons cd info
  ($##) cont C_UnknownFR d cs = cont C_UnknownFR d cs
  ($##) cont C_ConflictFR d cs = cont C_ConflictFR d cs
  ($##) cont C_KnownFlex d cs = cont C_KnownFlex d cs
  ($##) cont C_KnownRigid d cs = cont C_KnownRigid d cs
  ($##) cont (Choice_C_FlexRigidResult cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_FlexRigidResult cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_FlexRigidResult cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_FlexRigidResult cd info) _ _ = failCons cd info
  searchNF _ cont C_UnknownFR = cont C_UnknownFR
  searchNF _ cont C_ConflictFR = cont C_ConflictFR
  searchNF _ cont C_KnownFlex = cont C_KnownFlex
  searchNF _ cont C_KnownRigid = cont C_KnownRigid
  searchNF _ _ x = error ("FlexRigid.FlexRigidResult.searchNF: no constructor: " ++ (show x))


instance Unifiable C_FlexRigidResult where
  (=.=) C_UnknownFR C_UnknownFR d cs = C_Success
  (=.=) C_ConflictFR C_ConflictFR d cs = C_Success
  (=.=) C_KnownFlex C_KnownFlex d cs = C_Success
  (=.=) C_KnownRigid C_KnownRigid d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_UnknownFR C_UnknownFR d cs = C_Success
  (=.<=) C_ConflictFR C_ConflictFR d cs = C_Success
  (=.<=) C_KnownFlex C_KnownFlex d cs = C_Success
  (=.<=) C_KnownRigid C_KnownRigid d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_UnknownFR = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_ConflictFR = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_KnownFlex = ((i :=: (ChooseN 2 0)):(concat []))
  bind cd i C_KnownRigid = ((i :=: (ChooseN 3 0)):(concat []))
  bind d i (Choice_C_FlexRigidResult cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_FlexRigidResult cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_FlexRigidResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_FlexRigidResult cd i _) = error ("FlexRigid.FlexRigidResult.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_FlexRigidResult cd info) = [(Unsolvable info)]
  bind d i (Guard_C_FlexRigidResult cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_UnknownFR = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_ConflictFR = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_KnownFlex = [(i :=: (ChooseN 2 0))]
  lazyBind cd i C_KnownRigid = [(i :=: (ChooseN 3 0))]
  lazyBind d i (Choice_C_FlexRigidResult cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_FlexRigidResult cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_FlexRigidResult cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_FlexRigidResult cd i _) = error ("FlexRigid.FlexRigidResult.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_FlexRigidResult cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_FlexRigidResult cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_FlexRigidResult where
  (=?=) (Choice_C_FlexRigidResult cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_FlexRigidResult cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_FlexRigidResult cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_FlexRigidResult cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_FlexRigidResult cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_FlexRigidResult cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_FlexRigidResult cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_FlexRigidResult cd info) _ _ = failCons cd info
  (=?=) C_UnknownFR C_UnknownFR d cs = Curry_Prelude.C_True
  (=?=) C_ConflictFR C_ConflictFR d cs = Curry_Prelude.C_True
  (=?=) C_KnownFlex C_KnownFlex d cs = Curry_Prelude.C_True
  (=?=) C_KnownRigid C_KnownRigid d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_FlexRigidResult cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_FlexRigidResult cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_FlexRigidResult cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_FlexRigidResult cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_FlexRigidResult cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_FlexRigidResult cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_FlexRigidResult cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_FlexRigidResult cd info) _ _ = failCons cd info
  (<?=) C_UnknownFR C_UnknownFR d cs = Curry_Prelude.C_True
  (<?=) C_UnknownFR C_ConflictFR _ _ = Curry_Prelude.C_True
  (<?=) C_UnknownFR C_KnownFlex _ _ = Curry_Prelude.C_True
  (<?=) C_UnknownFR C_KnownRigid _ _ = Curry_Prelude.C_True
  (<?=) C_ConflictFR C_ConflictFR d cs = Curry_Prelude.C_True
  (<?=) C_ConflictFR C_KnownFlex _ _ = Curry_Prelude.C_True
  (<?=) C_ConflictFR C_KnownRigid _ _ = Curry_Prelude.C_True
  (<?=) C_KnownFlex C_KnownFlex d cs = Curry_Prelude.C_True
  (<?=) C_KnownFlex C_KnownRigid _ _ = Curry_Prelude.C_True
  (<?=) C_KnownRigid C_KnownRigid d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_getFlexRigid :: Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> C_FlexRigidResult
d_C_getFlexRigid x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Var x2) -> C_UnknownFR
     (Curry_FlatCurry.C_Lit x3) -> C_UnknownFR
     (Curry_FlatCurry.C_Comb x4 x5 x6) -> Curry_Prelude.d_C_foldr (acceptCs id d_C_joinCaseTypes) C_UnknownFR (Curry_Prelude.d_C_map d_C_getFlexRigid x6 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Let x7 x8) -> d_C_getFlexRigid x8 x3250 x3500
     (Curry_FlatCurry.C_Free x9 x10) -> d_C_getFlexRigid x10 x3250 x3500
     (Curry_FlatCurry.C_Or x11 x12) -> d_C_joinCaseTypes (d_C_getFlexRigid x11 x3250 x3500) (d_C_getFlexRigid x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x13 x14 x15) -> Curry_Prelude.d_C_foldr (acceptCs id d_C_joinCaseTypes) (d_OP__case_4 x13 (Curry_Prelude.d_OP_eq_eq x13 Curry_FlatCurry.C_Flex x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map d_C_getFlexRigid (Curry_Prelude.OP_Cons x14 (Curry_Prelude.d_C_map d_OP_getFlexRigid_dot___hash_lambda1 x15 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Typed x16 x17) -> d_C_getFlexRigid x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getFlexRigid x1002 x3250 x3500) (d_C_getFlexRigid x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getFlexRigid z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getFlexRigid x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getFlexRigid_dot___hash_lambda1 :: Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> Curry_FlatCurry.C_Expr
d_OP_getFlexRigid_dot___hash_lambda1 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Branch x2 x3) -> x3
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getFlexRigid_dot___hash_lambda1 x1002 x3250 x3500) (d_OP_getFlexRigid_dot___hash_lambda1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getFlexRigid_dot___hash_lambda1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getFlexRigid_dot___hash_lambda1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_joinCaseTypes :: C_FlexRigidResult -> C_FlexRigidResult -> Cover -> ConstStore -> C_FlexRigidResult
d_C_joinCaseTypes x1 x2 x3250 x3500 = case x1 of
     C_ConflictFR -> d_OP__case_3 x2 x3250 x3500
     C_UnknownFR -> d_OP__case_2 x2 x3250 x3500
     C_KnownFlex -> d_OP__case_1 x2 x3250 x3500
     C_KnownRigid -> d_OP__case_0 x2 x3250 x3500
     (Choice_C_FlexRigidResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_joinCaseTypes x1002 x2 x3250 x3500) (d_C_joinCaseTypes x1003 x2 x3250 x3500)
     (Choices_C_FlexRigidResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_joinCaseTypes z x2 x3250 x3500) x1002
     (Guard_C_FlexRigidResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_joinCaseTypes x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_FlexRigidResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: C_FlexRigidResult -> Cover -> ConstStore -> C_FlexRigidResult
d_OP__case_0 x2 x3250 x3500 = case x2 of
     C_ConflictFR -> C_ConflictFR
     C_UnknownFR -> C_KnownRigid
     C_KnownFlex -> C_ConflictFR
     C_KnownRigid -> C_KnownRigid
     (Choice_C_FlexRigidResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Choices_C_FlexRigidResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Guard_C_FlexRigidResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FlexRigidResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: C_FlexRigidResult -> Cover -> ConstStore -> C_FlexRigidResult
d_OP__case_1 x2 x3250 x3500 = case x2 of
     C_ConflictFR -> C_ConflictFR
     C_UnknownFR -> C_KnownFlex
     C_KnownFlex -> C_KnownFlex
     C_KnownRigid -> C_ConflictFR
     (Choice_C_FlexRigidResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Choices_C_FlexRigidResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Guard_C_FlexRigidResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FlexRigidResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: C_FlexRigidResult -> Cover -> ConstStore -> C_FlexRigidResult
d_OP__case_2 x2 x3250 x3500 = case x2 of
     C_ConflictFR -> C_ConflictFR
     C_UnknownFR -> C_UnknownFR
     C_KnownFlex -> C_KnownFlex
     C_KnownRigid -> C_KnownRigid
     (Choice_C_FlexRigidResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1002 x3250 x3500) (d_OP__case_2 x1003 x3250 x3500)
     (Choices_C_FlexRigidResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 z x3250 x3500) x1002
     (Guard_C_FlexRigidResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FlexRigidResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: C_FlexRigidResult -> Cover -> ConstStore -> C_FlexRigidResult
d_OP__case_3 x2 x3250 x3500 = case x2 of
     C_ConflictFR -> C_ConflictFR
     C_UnknownFR -> C_ConflictFR
     C_KnownFlex -> C_ConflictFR
     C_KnownRigid -> C_ConflictFR
     (Choice_C_FlexRigidResult x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3250 x3500) (d_OP__case_3 x1003 x3250 x3500)
     (Choices_C_FlexRigidResult x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3250 x3500) x1002
     (Guard_C_FlexRigidResult x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_FlexRigidResult x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_FlatCurry.C_CaseType -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_FlexRigidResult
d_OP__case_4 x13 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> C_KnownFlex
     Curry_Prelude.C_False -> C_KnownRigid
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x13 x1002 x3250 x3500) (d_OP__case_4 x13 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x13 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x13 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
