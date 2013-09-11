{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_PatternComplete (C_CompletenessType (..), d_C_analyseTotallyDefined, nd_C_analyseTotallyDefined, d_C_analyseCompleteness) where

import Basics
import qualified Curry_Dependency
import qualified Curry_FlatCurry
import qualified Curry_Prelude
data C_CompletenessType
     = C_Complete
     | C_InComplete
     | C_InCompleteOr
     | Choice_C_CompletenessType Cover ID C_CompletenessType C_CompletenessType
     | Choices_C_CompletenessType Cover ID ([C_CompletenessType])
     | Fail_C_CompletenessType Cover FailInfo
     | Guard_C_CompletenessType Cover Constraints C_CompletenessType

instance Show C_CompletenessType where
  showsPrec d (Choice_C_CompletenessType cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CompletenessType cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CompletenessType cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CompletenessType cd info) = showChar '!'
  showsPrec _ C_Complete = showString "Complete"
  showsPrec _ C_InComplete = showString "InComplete"
  showsPrec _ C_InCompleteOr = showString "InCompleteOr"


instance Read C_CompletenessType where
  readsPrec _ s = (readParen False (\r -> [ (C_Complete,r0) | (_,r0) <- readQualified "PatternComplete" "Complete" r]) s) ++ ((readParen False (\r -> [ (C_InComplete,r0) | (_,r0) <- readQualified "PatternComplete" "InComplete" r]) s) ++ (readParen False (\r -> [ (C_InCompleteOr,r0) | (_,r0) <- readQualified "PatternComplete" "InCompleteOr" r]) s))


instance NonDet C_CompletenessType where
  choiceCons = Choice_C_CompletenessType
  choicesCons = Choices_C_CompletenessType
  failCons = Fail_C_CompletenessType
  guardCons = Guard_C_CompletenessType
  try (Choice_C_CompletenessType cd i x y) = tryChoice cd i x y
  try (Choices_C_CompletenessType cd i xs) = tryChoices cd i xs
  try (Fail_C_CompletenessType cd info) = Fail cd info
  try (Guard_C_CompletenessType cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CompletenessType cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CompletenessType cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CompletenessType cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CompletenessType cd i _) = error ("PatternComplete.CompletenessType.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CompletenessType cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CompletenessType cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CompletenessType where
  generate s c = Choices_C_CompletenessType c (freeID [0,0,0] s) [C_Complete,C_InComplete,C_InCompleteOr]


instance NormalForm C_CompletenessType where
  ($!!) cont C_Complete d cs = cont C_Complete d cs
  ($!!) cont C_InComplete d cs = cont C_InComplete d cs
  ($!!) cont C_InCompleteOr d cs = cont C_InCompleteOr d cs
  ($!!) cont (Choice_C_CompletenessType cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CompletenessType cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CompletenessType cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CompletenessType cd info) _ _ = failCons cd info
  ($##) cont C_Complete d cs = cont C_Complete d cs
  ($##) cont C_InComplete d cs = cont C_InComplete d cs
  ($##) cont C_InCompleteOr d cs = cont C_InCompleteOr d cs
  ($##) cont (Choice_C_CompletenessType cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CompletenessType cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CompletenessType cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CompletenessType cd info) _ _ = failCons cd info
  searchNF _ cont C_Complete = cont C_Complete
  searchNF _ cont C_InComplete = cont C_InComplete
  searchNF _ cont C_InCompleteOr = cont C_InCompleteOr
  searchNF _ _ x = error ("PatternComplete.CompletenessType.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CompletenessType where
  (=.=) C_Complete C_Complete d cs = C_Success
  (=.=) C_InComplete C_InComplete d cs = C_Success
  (=.=) C_InCompleteOr C_InCompleteOr d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_Complete C_Complete d cs = C_Success
  (=.<=) C_InComplete C_InComplete d cs = C_Success
  (=.<=) C_InCompleteOr C_InCompleteOr d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_Complete = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_InComplete = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_InCompleteOr = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_CompletenessType cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CompletenessType cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CompletenessType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CompletenessType cd i _) = error ("PatternComplete.CompletenessType.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CompletenessType cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CompletenessType cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Complete = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_InComplete = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_InCompleteOr = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_CompletenessType cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CompletenessType cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CompletenessType cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CompletenessType cd i _) = error ("PatternComplete.CompletenessType.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CompletenessType cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CompletenessType cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CompletenessType where
  (=?=) (Choice_C_CompletenessType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CompletenessType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CompletenessType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CompletenessType cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CompletenessType cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CompletenessType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CompletenessType cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CompletenessType cd info) _ _ = failCons cd info
  (=?=) C_Complete C_Complete d cs = Curry_Prelude.C_True
  (=?=) C_InComplete C_InComplete d cs = Curry_Prelude.C_True
  (=?=) C_InCompleteOr C_InCompleteOr d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_CompletenessType cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CompletenessType cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CompletenessType cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CompletenessType cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CompletenessType cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CompletenessType cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CompletenessType cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CompletenessType cd info) _ _ = failCons cd info
  (<?=) C_Complete C_Complete d cs = Curry_Prelude.C_True
  (<?=) C_Complete C_InComplete _ _ = Curry_Prelude.C_True
  (<?=) C_Complete C_InCompleteOr _ _ = Curry_Prelude.C_True
  (<?=) C_InComplete C_InComplete d cs = Curry_Prelude.C_True
  (<?=) C_InComplete C_InCompleteOr _ _ = Curry_Prelude.C_True
  (<?=) C_InCompleteOr C_InCompleteOr d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_analyseTotallyDefined :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CompletenessType)
d_C_analyseTotallyDefined x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_foldr (acceptCs id d_C_combineAndResults) C_Complete
      in (Curry_Dependency.d_C_analyseWithDependencies (d_C_analyseCompleteness x1) x2)

nd_C_analyseTotallyDefined :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) C_CompletenessType))
nd_C_analyseTotallyDefined x1 x3000 x3250 x3500 = let
     x2 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_C_combineAndResults)) C_Complete)
      in (wrapNX id (Curry_Dependency.nd_C_analyseWithDependencies (wrapDX id (d_C_analyseCompleteness x1)) x2))

d_C_analyseCompleteness :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> C_CompletenessType
d_C_analyseCompleteness x1 x2 x3250 x3500 = d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 x2 x3250 x3500

d_OP_analyseCompleteness_dot_anaFun_dot_5 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> C_CompletenessType
d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Func x3 x4 x5 x6 x7) -> d_OP__case_17 x1 x7 x3250 x3500
     (Curry_FlatCurry.Choice_C_FuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 x1002 x3250 x3500) (d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_FuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_FuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_analyseCompleteness_dot_anaFun_dot_5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_FuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isComplete :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Expr -> Cover -> ConstStore -> C_CompletenessType
d_C_isComplete x1 x2 x3250 x3500 = case x2 of
     (Curry_FlatCurry.C_Var x3) -> C_Complete
     (Curry_FlatCurry.C_Lit x4) -> C_Complete
     (Curry_FlatCurry.C_Comb x5 x6 x7) -> d_OP__case_16 x7 x6 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x7 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Free x8 x9) -> C_Complete
     (Curry_FlatCurry.C_Let x10 x11) -> C_Complete
     (Curry_FlatCurry.C_Or x12 x13) -> d_C_combineOrResults (d_C_isComplete x1 x12 x3250 x3500) (d_C_isComplete x1 x13 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.C_Case x14 x15 x16) -> d_OP__case_15 x1 x16 x3250 x3500
     (Curry_FlatCurry.Choice_C_Expr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isComplete x1 x1002 x3250 x3500) (d_C_isComplete x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Expr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isComplete x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Expr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isComplete x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Expr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_isComplete_dot_checkAllCons_dot_60 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_CompletenessType
d_OP_isComplete_dot_checkAllCons_dot_60 x1 x2 x3 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> C_Complete
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_12 x1 x5 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_isComplete_dot_checkAllCons_dot_60 x1 x1002 x3 x3250 x3500) (d_OP_isComplete_dot_checkAllCons_dot_60 x1 x1003 x3 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_isComplete_dot_checkAllCons_dot_60 x1 z x3 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_isComplete_dot_checkAllCons_dot_60 x1 x1002 x3 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_combineOrResults :: C_CompletenessType -> C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_C_combineOrResults x1 x2 x3250 x3500 = case x1 of
     C_Complete -> C_Complete
     C_InComplete -> d_OP__case_9 x2 x3250 x3500
     C_InCompleteOr -> d_OP__case_8 x2 x3250 x3500
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineOrResults x1002 x2 x3250 x3500) (d_C_combineOrResults x1003 x2 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineOrResults z x2 x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineOrResults x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_combineAndResults :: C_CompletenessType -> C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_C_combineAndResults x1 x2 x3250 x3500 = case x1 of
     C_InComplete -> C_InComplete
     C_Complete -> d_OP__case_7 x2 x3250 x3500
     C_InCompleteOr -> d_OP__case_6 x2 x3250 x3500
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_combineAndResults x1002 x2 x3250 x3500) (d_C_combineAndResults x1003 x2 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_combineAndResults z x2 x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_combineAndResults x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getConstructors :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_C_getConstructors x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_5 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_getConstructors x1 x1002 x3250 x3500) (d_C_getConstructors x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_getConstructors x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_getConstructors x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_getConstructors_dot_hasCons_dot_121 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_getConstructors_dot_hasCons_dot_121 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_3 x4 x1 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getConstructors_dot_hasCons_dot_121 x1 x1002 x3250 x3500) (d_OP_getConstructors_dot_hasCons_dot_121 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getConstructors_dot_hasCons_dot_121 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getConstructors_dot_hasCons_dot_121 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_removeConstructor :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_C_removeConstructor x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_removeConstructor x1 x1002 x3250 x3500) (d_C_removeConstructor x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_removeConstructor x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_removeConstructor x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP__case_2 x1 x4 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Cons x5 x6 x7 x8) -> d_OP__case_1 x5 x1 x4 x8 x7 x6 (Curry_Prelude.d_OP_eq_eq x1 x5 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x4 x1002 x3250 x3500) (d_OP__case_2 x1 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP__case_1 x5 x1 x4 x8 x7 x6 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_0 x4 x1 x8 x7 x6 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x5 x1 x4 x8 x7 x6 x1002 x3250 x3500) (d_OP__case_1 x5 x1 x4 x8 x7 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x5 x1 x4 x8 x7 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x5 x1 x4 x8 x7 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeExpr -> Curry_FlatCurry.C_Visibility -> Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP__case_0 x4 x1 x8 x7 x6 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Cons x5 x6 x7 x8) (d_C_removeConstructor x1 x4 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x4 x1 x8 x7 x6 x5 x1002 x3250 x3500) (d_OP__case_0 x4 x1 x8 x7 x6 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x4 x1 x8 x7 x6 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x4 x1 x8 x7 x6 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_3 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_Cons x5 x6 x7 x8) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 x5 x3250 x3500) (d_OP_getConstructors_dot_hasCons_dot_121 x1 x4 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_ConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x4 x1 x1002 x3250 x3500) (d_OP__case_3 x4 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_ConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x4 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_ConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_ConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP__case_5 x1 x4 x3 x3250 x3500 = case x3 of
     (Curry_FlatCurry.C_TypeSyn x5 x6 x7 x8) -> d_C_getConstructors x1 x4 x3250 x3500
     (Curry_FlatCurry.C_Type x9 x10 x11 x12) -> d_OP__case_4 x12 x1 x4 (d_OP_getConstructors_dot_hasCons_dot_121 x1 x12 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_TypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x4 x1002 x3250 x3500) (d_OP__case_5 x1 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_TypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_TypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_TypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl
d_OP__case_4 x12 x1 x4 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> x12
     Curry_Prelude.C_False -> d_C_getConstructors x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x12 x1 x4 x1002 x3250 x3500) (d_OP__case_4 x12 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x12 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x12 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_6 x2 x3250 x3500 = case x2 of
     C_Complete -> C_InCompleteOr
     C_InComplete -> C_InComplete
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3250 x3500) (d_OP__case_6 x1003 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_7 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InComplete
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_8 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InCompleteOr
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1002 x3250 x3500) (d_OP__case_8 x1003 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 z x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: C_CompletenessType -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_9 x2 x3250 x3500 = case x2 of
     C_Complete -> C_Complete
     C_InComplete -> C_InCompleteOr
     C_InCompleteOr -> C_InCompleteOr
     (Choice_C_CompletenessType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1002 x3250 x3500) (d_OP__case_9 x1003 x3250 x3500)
     (Choices_C_CompletenessType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 z x3250 x3500) x1002
     (Guard_C_CompletenessType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CompletenessType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_ConsDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_12 x1 x5 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_11 x1 x7 x5 x4 x6 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x5 x4 x1002 x3250 x3500) (d_OP__case_12 x1 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_11 x1 x7 x5 x4 x6 x3250 x3500 = case x6 of
     (Curry_FlatCurry.C_Branch x8 x9) -> d_OP__case_10 x9 x1 x7 x5 x4 x8 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x1 x7 x5 x4 x1002 x3250 x3500) (d_OP__case_11 x1 x7 x5 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x1 x7 x5 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x1 x7 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_ConsDecl -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_10 x9 x1 x7 x5 x4 x8 x3250 x3500 = case x8 of
     (Curry_FlatCurry.C_LPattern x10) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x11 x12) -> d_C_combineAndResults (d_OP_isComplete_dot_checkAllCons_dot_60 x1 (d_C_removeConstructor x11 (Curry_Prelude.OP_Cons x4 x5) x3250 x3500) x7 x3250 x3500) (d_C_isComplete x1 x9 x3250 x3500) x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x9 x1 x7 x5 x4 x1002 x3250 x3500) (d_OP__case_10 x9 x1 x7 x5 x4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x9 x1 x7 x5 x4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x9 x1 x7 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_15 x1 x16 x3250 x3500 = case x16 of
     Curry_Prelude.OP_List -> C_InComplete
     (Curry_Prelude.OP_Cons x17 x18) -> d_OP__case_14 x18 x1 x17 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x1 x1002 x3250 x3500) (d_OP__case_15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_BranchExpr -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_14 x18 x1 x17 x3250 x3500 = case x17 of
     (Curry_FlatCurry.C_Branch x19 x20) -> d_OP__case_13 x18 x20 x1 x19 x3250 x3500
     (Curry_FlatCurry.Choice_C_BranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x18 x1 x1002 x3250 x3500) (d_OP__case_14 x18 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_BranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x18 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_BranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x18 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_BranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_FlatCurry.C_BranchExpr -> Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Pattern -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_13 x18 x20 x1 x19 x3250 x3500 = case x19 of
     (Curry_FlatCurry.C_LPattern x21) -> C_InComplete
     (Curry_FlatCurry.C_Pattern x22 x23) -> d_OP_isComplete_dot_checkAllCons_dot_60 x1 (d_C_getConstructors x22 x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_FlatCurry.C_Branch (Curry_FlatCurry.C_Pattern x22 x23) x20) x18) x3250 x3500
     (Curry_FlatCurry.Choice_C_Pattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x18 x20 x1 x1002 x3250 x3500) (d_OP__case_13 x18 x20 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Pattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x18 x20 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Pattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x18 x20 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Pattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_FlatCurry.C_Expr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_16 x7 x6 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_isComplete x1 (Curry_Prelude.d_C_head x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> C_Complete
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x7 x6 x1 x1002 x3250 x3500) (d_OP__case_16 x7 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x7 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x7 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_FlatCurry.C_Rule -> Cover -> ConstStore -> C_CompletenessType
d_OP__case_17 x1 x7 x3250 x3500 = case x7 of
     (Curry_FlatCurry.C_Rule x8 x9) -> d_C_isComplete x1 x9 x3250 x3500
     (Curry_FlatCurry.C_External x10) -> C_Complete
     (Curry_FlatCurry.Choice_C_Rule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1 x1002 x3250 x3500) (d_OP__case_17 x1 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Rule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x1 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Rule x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Rule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
