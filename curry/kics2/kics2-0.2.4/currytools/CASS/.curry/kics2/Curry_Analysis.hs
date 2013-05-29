{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Analysis (C_Analysis (..), d_C_simpleFuncAnalysis, nd_C_simpleFuncAnalysis, d_C_simpleTypeAnalysis, nd_C_simpleTypeAnalysis, d_C_simpleConstructorAnalysis, nd_C_simpleConstructorAnalysis, d_C_dependencyFuncAnalysis, nd_C_dependencyFuncAnalysis, d_C_dependencyTypeAnalysis, nd_C_dependencyTypeAnalysis, d_C_combinedSimpleFuncAnalysis, nd_C_combinedSimpleFuncAnalysis, d_C_combinedSimpleTypeAnalysis, nd_C_combinedSimpleTypeAnalysis, d_C_combinedDependencyFuncAnalysis, nd_C_combinedDependencyFuncAnalysis, d_C_combinedDependencyTypeAnalysis, nd_C_combinedDependencyTypeAnalysis, d_C_isSimpleAnalysis, nd_C_isSimpleAnalysis, d_C_isCombinedAnalysis, nd_C_isCombinedAnalysis, d_C_analysisName, nd_C_analysisName, d_C_baseAnalysisName, nd_C_baseAnalysisName, d_C_startValue, nd_C_startValue) where

import Basics
import qualified Curry_CurryFiles
import qualified Curry_FlatCurry
import qualified Curry_GenericProgInfo
import qualified Curry_LoadAnalysis
import qualified Curry_Prelude
import qualified Curry_FlatCurryGoodies
data C_Analysis t0
     = C_SimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_FuncDecl -> ConstStore -> t0)
     | HO_C_SimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_FuncDecl t0)
     | C_SimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_TypeDecl -> ConstStore -> t0)
     | HO_C_SimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_TypeDecl t0)
     | C_SimpleConstructorAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> t0)
     | HO_C_SimpleConstructorAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_ConsDecl (Func Curry_FlatCurry.C_TypeDecl t0))
     | C_DependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0)
     | HO_C_DependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))
     | C_DependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0)
     | HO_C_DependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))
     | C_CombinedSimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_FuncDecl -> ConstStore -> t0))
     | HO_C_CombinedSimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_FuncDecl t0)))
     | C_CombinedSimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_TypeDecl -> ConstStore -> t0))
     | HO_C_CombinedSimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_TypeDecl t0)))
     | C_CombinedDependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0))
     | HO_C_CombinedDependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))))
     | C_CombinedDependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0))
     | HO_C_CombinedDependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))))
     | Choice_C_Analysis Cover ID (C_Analysis t0) (C_Analysis t0)
     | Choices_C_Analysis Cover ID ([C_Analysis t0])
     | Fail_C_Analysis Cover FailInfo
     | Guard_C_Analysis Cover Constraints (C_Analysis t0)

instance Show t0 => Show (C_Analysis t0) where
  showsPrec d (Choice_C_Analysis cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Analysis cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Analysis cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Analysis cd info) = showChar '!'
  showsPrec _ (C_SimpleFuncAnalysis x1 x2) = (showString "(SimpleFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_SimpleFuncAnalysis x1 x2) = (showString "(SimpleFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_SimpleTypeAnalysis x1 x2) = (showString "(SimpleTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_SimpleTypeAnalysis x1 x2) = (showString "(SimpleTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_SimpleConstructorAnalysis x1 x2) = (showString "(SimpleConstructorAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (HO_C_SimpleConstructorAnalysis x1 x2) = (showString "(SimpleConstructorAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_DependencyFuncAnalysis x1 x2 x3) = (showString "(DependencyFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (HO_C_DependencyFuncAnalysis x1 x2 x3) = (showString "(DependencyFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_DependencyTypeAnalysis x1 x2 x3) = (showString "(DependencyTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (HO_C_DependencyTypeAnalysis x1 x2 x3) = (showString "(DependencyTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = (showString "(CombinedSimpleFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = (showString "(CombinedSimpleFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = (showString "(CombinedSimpleTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = (showString "(CombinedSimpleTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))
  showsPrec _ (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = (showString "(CombinedDependencyFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))
  showsPrec _ (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = (showString "(CombinedDependencyFuncAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))
  showsPrec _ (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = (showString "(CombinedDependencyTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))
  showsPrec _ (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = (showString "(CombinedDependencyTypeAnalysis") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read t0 => Read (C_Analysis t0) where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_SimpleFuncAnalysis x1 x2,r2) | (_,r0) <- readQualified "Analysis" "SimpleFuncAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_SimpleTypeAnalysis x1 x2,r2) | (_,r0) <- readQualified "Analysis" "SimpleTypeAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_SimpleConstructorAnalysis x1 x2,r2) | (_,r0) <- readQualified "Analysis" "SimpleConstructorAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_DependencyFuncAnalysis x1 x2 x3,r3) | (_,r0) <- readQualified "Analysis" "DependencyFuncAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_DependencyTypeAnalysis x1 x2 x3,r3) | (_,r0) <- readQualified "Analysis" "DependencyTypeAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s) ++ ((readParen (d > 10) (\r -> [ (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4,r4) | (_,r0) <- readQualified "Analysis" "CombinedSimpleFuncAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ ((readParen (d > 10) (\r -> [ (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4,r4) | (_,r0) <- readQualified "Analysis" "CombinedSimpleTypeAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s) ++ ((readParen (d > 10) (\r -> [ (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "Analysis" "CombinedDependencyFuncAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s) ++ (readParen (d > 10) (\r -> [ (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "Analysis" "CombinedDependencyTypeAnalysis" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s))))))))


instance NonDet (C_Analysis t0) where
  choiceCons = Choice_C_Analysis
  choicesCons = Choices_C_Analysis
  failCons = Fail_C_Analysis
  guardCons = Guard_C_Analysis
  try (Choice_C_Analysis cd i x y) = tryChoice cd i x y
  try (Choices_C_Analysis cd i xs) = tryChoices cd i xs
  try (Fail_C_Analysis cd info) = Fail cd info
  try (Guard_C_Analysis cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Analysis cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Analysis cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Analysis cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Analysis cd i _) = error ("Analysis.Analysis.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Analysis cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Analysis cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Analysis t0) where
  generate s = Choices_C_Analysis defCover (freeID [2,2,2,3,3,4,4,5,5] s) [(C_SimpleFuncAnalysis (generate (leftSupply s)) (generate (rightSupply s))),(C_SimpleTypeAnalysis (generate (leftSupply s)) (generate (rightSupply s))),(C_SimpleConstructorAnalysis (generate (leftSupply s)) (generate (rightSupply s))),(C_DependencyFuncAnalysis (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s))),(C_DependencyTypeAnalysis (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s))),(C_CombinedSimpleFuncAnalysis (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s)))),(C_CombinedSimpleTypeAnalysis (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s)))),(C_CombinedDependencyFuncAnalysis (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s)))),(C_CombinedDependencyTypeAnalysis (generate (leftSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply (leftSupply s)))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance NormalForm t0 => NormalForm (C_Analysis t0) where
  ($!!) cont (C_SimpleFuncAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleFuncAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_SimpleFuncAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleFuncAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_SimpleTypeAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleTypeAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_SimpleTypeAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleTypeAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_SimpleConstructorAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleConstructorAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_SimpleConstructorAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleConstructorAnalysis y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_DependencyFuncAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DependencyFuncAnalysis y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_DependencyFuncAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_DependencyFuncAnalysis y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_DependencyTypeAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DependencyTypeAnalysis y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_DependencyTypeAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_DependencyTypeAnalysis y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs) $!! x5) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Analysis cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Analysis cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Analysis cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Analysis cd info) _ = failCons cd info
  ($##) cont (C_SimpleFuncAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleFuncAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_SimpleFuncAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleFuncAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_SimpleTypeAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleTypeAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_SimpleTypeAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleTypeAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_SimpleConstructorAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_SimpleConstructorAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_SimpleConstructorAnalysis x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (HO_C_SimpleConstructorAnalysis y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_DependencyFuncAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DependencyFuncAnalysis y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_DependencyFuncAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_DependencyFuncAnalysis y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_DependencyTypeAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_DependencyTypeAnalysis y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_DependencyTypeAnalysis x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_DependencyTypeAnalysis y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> ((\y5 cs -> cont (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs) $## x5) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Analysis cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Analysis cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Analysis cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Analysis cd info) _ = failCons cd info
  searchNF search cont (C_SimpleFuncAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (C_SimpleFuncAnalysis y1 y2)) x2) x1
  searchNF search cont (HO_C_SimpleFuncAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_SimpleFuncAnalysis y1 y2)) x2) x1
  searchNF search cont (C_SimpleTypeAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (C_SimpleTypeAnalysis y1 y2)) x2) x1
  searchNF search cont (HO_C_SimpleTypeAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_SimpleTypeAnalysis y1 y2)) x2) x1
  searchNF search cont (C_SimpleConstructorAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (C_SimpleConstructorAnalysis y1 y2)) x2) x1
  searchNF search cont (HO_C_SimpleConstructorAnalysis x1 x2) = search (\y1 -> search (\y2 -> cont (HO_C_SimpleConstructorAnalysis y1 y2)) x2) x1
  searchNF search cont (C_DependencyFuncAnalysis x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_DependencyFuncAnalysis y1 y2 y3)) x3) x2) x1
  searchNF search cont (HO_C_DependencyFuncAnalysis x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (HO_C_DependencyFuncAnalysis y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_DependencyTypeAnalysis x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_DependencyTypeAnalysis y1 y2 y3)) x3) x2) x1
  searchNF search cont (HO_C_DependencyTypeAnalysis x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (HO_C_DependencyTypeAnalysis y1 y2 y3)) x3) x2) x1
  searchNF search cont (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF search cont (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF search cont (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF search cont (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF search cont (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Analysis.Analysis.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Analysis t0) where
  (=.=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((x5 =:= y5) cs)) cs)) cs)) cs)) cs
  (=.=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((x5 =:= y5) cs)) cs)) cs)) cs)) cs
  (=.=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((x5 =:= y5) cs)) cs)) cs)) cs)) cs
  (=.=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((((x4 =:= y4) cs) & ((x5 =:= y5) cs)) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((x5 =:<= y5) cs)) cs)) cs)) cs)) cs
  (=.<=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((x5 =:<= y5) cs)) cs)) cs)) cs)) cs
  (=.<=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((x5 =:<= y5) cs)) cs)) cs)) cs)) cs
  (=.<=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((((x4 =:<= y4) cs) & ((x5 =:<= y5) cs)) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_SimpleFuncAnalysis x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_SimpleFuncAnalysis x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_SimpleTypeAnalysis x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_SimpleTypeAnalysis x2 x3) = ((i :=: (ChooseN 1 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_SimpleConstructorAnalysis x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (HO_C_SimpleConstructorAnalysis x2 x3) = ((i :=: (ChooseN 2 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (C_DependencyFuncAnalysis x2 x3 x4) = ((i :=: (ChooseN 3 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (HO_C_DependencyFuncAnalysis x2 x3 x4) = ((i :=: (ChooseN 3 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (C_DependencyTypeAnalysis x2 x3 x4) = ((i :=: (ChooseN 4 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (HO_C_DependencyTypeAnalysis x2 x3 x4) = ((i :=: (ChooseN 4 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) = ((i :=: (ChooseN 5 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) = ((i :=: (ChooseN 5 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (C_CombinedSimpleTypeAnalysis x2 x3 x4 x5) = ((i :=: (ChooseN 6 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (HO_C_CombinedSimpleTypeAnalysis x2 x3 x4 x5) = ((i :=: (ChooseN 6 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (C_CombinedDependencyFuncAnalysis x2 x3 x4 x5 x6) = ((i :=: (ChooseN 7 5)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (rightID i)) x5),(bind (rightID (rightID i)) x6)]))
  bind i (HO_C_CombinedDependencyFuncAnalysis x2 x3 x4 x5 x6) = ((i :=: (ChooseN 7 5)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (rightID i)) x5),(bind (rightID (rightID i)) x6)]))
  bind i (C_CombinedDependencyTypeAnalysis x2 x3 x4 x5 x6) = ((i :=: (ChooseN 8 5)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (rightID i)) x5),(bind (rightID (rightID i)) x6)]))
  bind i (HO_C_CombinedDependencyTypeAnalysis x2 x3 x4 x5 x6) = ((i :=: (ChooseN 8 5)):(concat [(bind (leftID (leftID (leftID i))) x2),(bind (rightID (leftID (leftID i))) x3),(bind (rightID (leftID i)) x4),(bind (leftID (rightID i)) x5),(bind (rightID (rightID i)) x6)]))
  bind i (Choice_C_Analysis cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Analysis cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Analysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Analysis cd i _) = error ("Analysis.Analysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Analysis cd info) = [(Unsolvable info)]
  bind i (Guard_C_Analysis cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_SimpleFuncAnalysis x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_SimpleFuncAnalysis x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_SimpleTypeAnalysis x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_SimpleTypeAnalysis x2 x3) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_SimpleConstructorAnalysis x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (HO_C_SimpleConstructorAnalysis x2 x3) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (C_DependencyFuncAnalysis x2 x3 x4) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (HO_C_DependencyFuncAnalysis x2 x3 x4) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (C_DependencyTypeAnalysis x2 x3 x4) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (HO_C_DependencyTypeAnalysis x2 x3 x4) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) = [(i :=: (ChooseN 5 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) = [(i :=: (ChooseN 5 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (C_CombinedSimpleTypeAnalysis x2 x3 x4 x5) = [(i :=: (ChooseN 6 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (HO_C_CombinedSimpleTypeAnalysis x2 x3 x4 x5) = [(i :=: (ChooseN 6 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (C_CombinedDependencyFuncAnalysis x2 x3 x4 x5 x6) = [(i :=: (ChooseN 7 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x6)))]
  lazyBind i (HO_C_CombinedDependencyFuncAnalysis x2 x3 x4 x5 x6) = [(i :=: (ChooseN 7 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x6)))]
  lazyBind i (C_CombinedDependencyTypeAnalysis x2 x3 x4 x5 x6) = [(i :=: (ChooseN 8 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x6)))]
  lazyBind i (HO_C_CombinedDependencyTypeAnalysis x2 x3 x4 x5 x6) = [(i :=: (ChooseN 8 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind (leftID (leftID (leftID i))) x2))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind (rightID (leftID (leftID i))) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x6)))]
  lazyBind i (Choice_C_Analysis cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Analysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Analysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Analysis cd i _) = error ("Analysis.Analysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Analysis cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Analysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Analysis t0) where
  (=?=) (Choice_C_Analysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Analysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Analysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Analysis cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Analysis cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Analysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Analysis cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Analysis cd info) _ = failCons cd info
  (=?=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (=?=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (=?=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (=?=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (=?=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (=?=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.=?= y5) cs) cs) cs) cs) cs
  (=?=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.=?= y5) cs) cs) cs) cs) cs
  (=?=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.=?= y5) cs) cs) cs) cs) cs
  (=?=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.=?= y5) cs) cs) cs) cs) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Analysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Analysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Analysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Analysis cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Analysis cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Analysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Analysis cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Analysis cd info) _ = failCons cd info
  (<?=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_SimpleFuncAnalysis _ _) (C_SimpleTypeAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_SimpleTypeAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_SimpleTypeAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_SimpleTypeAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_SimpleTypeAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.<?= y5) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.<?= y5) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.<?= y5) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x4 Curry_Prelude.=?= y4) cs) ((x5 Curry_Prelude.<?= y5) cs) cs) cs) cs) cs) cs) cs) cs) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable t0 => Coverable (C_Analysis t0) where
  cover (C_SimpleFuncAnalysis x1 x2) = C_SimpleFuncAnalysis (cover x1) (cover x2)
  cover (HO_C_SimpleFuncAnalysis x1 x2) = HO_C_SimpleFuncAnalysis (cover x1) (cover x2)
  cover (C_SimpleTypeAnalysis x1 x2) = C_SimpleTypeAnalysis (cover x1) (cover x2)
  cover (HO_C_SimpleTypeAnalysis x1 x2) = HO_C_SimpleTypeAnalysis (cover x1) (cover x2)
  cover (C_SimpleConstructorAnalysis x1 x2) = C_SimpleConstructorAnalysis (cover x1) (cover x2)
  cover (HO_C_SimpleConstructorAnalysis x1 x2) = HO_C_SimpleConstructorAnalysis (cover x1) (cover x2)
  cover (C_DependencyFuncAnalysis x1 x2 x3) = C_DependencyFuncAnalysis (cover x1) (cover x2) (cover x3)
  cover (HO_C_DependencyFuncAnalysis x1 x2 x3) = HO_C_DependencyFuncAnalysis (cover x1) (cover x2) (cover x3)
  cover (C_DependencyTypeAnalysis x1 x2 x3) = C_DependencyTypeAnalysis (cover x1) (cover x2) (cover x3)
  cover (HO_C_DependencyTypeAnalysis x1 x2 x3) = HO_C_DependencyTypeAnalysis (cover x1) (cover x2) (cover x3)
  cover (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = C_CombinedSimpleFuncAnalysis (cover x1) (cover x2) (cover x3) (cover x4)
  cover (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) = HO_C_CombinedSimpleFuncAnalysis (cover x1) (cover x2) (cover x3) (cover x4)
  cover (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = C_CombinedSimpleTypeAnalysis (cover x1) (cover x2) (cover x3) (cover x4)
  cover (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) = HO_C_CombinedSimpleTypeAnalysis (cover x1) (cover x2) (cover x3) (cover x4)
  cover (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = C_CombinedDependencyFuncAnalysis (cover x1) (cover x2) (cover x3) (cover x4) (cover x5)
  cover (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) = HO_C_CombinedDependencyFuncAnalysis (cover x1) (cover x2) (cover x3) (cover x4) (cover x5)
  cover (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = C_CombinedDependencyTypeAnalysis (cover x1) (cover x2) (cover x3) (cover x4) (cover x5)
  cover (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) = HO_C_CombinedDependencyTypeAnalysis (cover x1) (cover x2) (cover x3) (cover x4) (cover x5)
  cover (Choice_C_Analysis cd i x y) = Choice_C_Analysis (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Analysis cd i xs) = Choices_C_Analysis (incCover cd) i (map cover xs)
  cover (Fail_C_Analysis cd info) = Fail_C_Analysis (incCover cd) info
  cover (Guard_C_Analysis cd c e) = Guard_C_Analysis (incCover cd) c (cover e)


d_C_simpleFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_FuncDecl -> ConstStore -> t0) -> ConstStore -> C_Analysis t0
d_C_simpleFuncAnalysis x1 x2 x3500 = C_SimpleFuncAnalysis x1 x2

nd_C_simpleFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_FuncDecl t0 -> IDSupply -> ConstStore -> C_Analysis t0
nd_C_simpleFuncAnalysis x1 x2 x3000 x3500 = HO_C_SimpleFuncAnalysis x1 x2

d_C_simpleTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_TypeDecl -> ConstStore -> t0) -> ConstStore -> C_Analysis t0
d_C_simpleTypeAnalysis x1 x2 x3500 = C_SimpleTypeAnalysis x1 x2

nd_C_simpleTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_TypeDecl t0 -> IDSupply -> ConstStore -> C_Analysis t0
nd_C_simpleTypeAnalysis x1 x2 x3000 x3500 = HO_C_SimpleTypeAnalysis x1 x2

d_C_simpleConstructorAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_ConsDecl -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> t0) -> ConstStore -> C_Analysis t0
d_C_simpleConstructorAnalysis x1 x2 x3500 = C_SimpleConstructorAnalysis x1 x2

nd_C_simpleConstructorAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_ConsDecl (Func Curry_FlatCurry.C_TypeDecl t0) -> IDSupply -> ConstStore -> C_Analysis t0
nd_C_simpleConstructorAnalysis x1 x2 x3000 x3500 = HO_C_SimpleConstructorAnalysis x1 x2

d_C_dependencyFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> (Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0) -> ConstStore -> C_Analysis t0
d_C_dependencyFuncAnalysis x1 x2 x3 x3500 = C_DependencyFuncAnalysis x1 x2 x3

nd_C_dependencyFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0) -> IDSupply -> ConstStore -> C_Analysis t0
nd_C_dependencyFuncAnalysis x1 x2 x3 x3000 x3500 = HO_C_DependencyFuncAnalysis x1 x2 x3

d_C_dependencyTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> (Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> ConstStore -> t0) -> ConstStore -> C_Analysis t0
d_C_dependencyTypeAnalysis x1 x2 x3 x3500 = C_DependencyTypeAnalysis x1 x2 x3

nd_C_dependencyTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0) -> IDSupply -> ConstStore -> C_Analysis t0
nd_C_dependencyTypeAnalysis x1 x2 x3 x3000 x3500 = HO_C_DependencyTypeAnalysis x1 x2 x3

d_C_combinedSimpleFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> t1) -> ConstStore -> C_Analysis t1
d_C_combinedSimpleFuncAnalysis x1 x2 x3 x3500 = let
     x4 = d_C_analysisName x2 x3500
      in (C_CombinedSimpleFuncAnalysis x4 x1 Curry_Prelude.C_True (d_C_runWithBaseAnalysis x2 x3))

nd_C_combinedSimpleFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_FuncDecl t1) -> IDSupply -> ConstStore -> C_Analysis t1
nd_C_combinedSimpleFuncAnalysis x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = nd_C_analysisName x2 x2000 x3500
           in (HO_C_CombinedSimpleFuncAnalysis x4 x1 Curry_Prelude.C_True (wrapNX id (nd_C_runWithBaseAnalysis x2 x3)))))

d_C_combinedSimpleTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> t1) -> ConstStore -> C_Analysis t1
d_C_combinedSimpleTypeAnalysis x1 x2 x3 x3500 = let
     x4 = d_C_analysisName x2 x3500
      in (C_CombinedSimpleTypeAnalysis x4 x1 Curry_Prelude.C_True (d_C_runWithBaseAnalysis x2 x3))

nd_C_combinedSimpleTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_TypeDecl t1) -> IDSupply -> ConstStore -> C_Analysis t1
nd_C_combinedSimpleTypeAnalysis x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = nd_C_analysisName x2 x2000 x3500
           in (HO_C_CombinedSimpleTypeAnalysis x4 x1 Curry_Prelude.C_True (wrapNX id (nd_C_runWithBaseAnalysis x2 x3)))))

d_C_combinedDependencyFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1) -> ConstStore -> t1) -> ConstStore -> C_Analysis t1
d_C_combinedDependencyFuncAnalysis x1 x2 x3 x4 x3500 = let
     x5 = d_C_analysisName x2 x3500
      in (C_CombinedDependencyFuncAnalysis x5 x1 Curry_Prelude.C_True x3 (d_C_runWithBaseAnalysis x2 x4))

nd_C_combinedDependencyFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1)) t1)) -> IDSupply -> ConstStore -> C_Analysis t1
nd_C_combinedDependencyFuncAnalysis x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = nd_C_analysisName x2 x2000 x3500
           in (HO_C_CombinedDependencyFuncAnalysis x5 x1 Curry_Prelude.C_True x3 (wrapNX id (nd_C_runWithBaseAnalysis x2 x4)))))

d_C_combinedDependencyTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1) -> ConstStore -> t1) -> ConstStore -> C_Analysis t1
d_C_combinedDependencyTypeAnalysis x1 x2 x3 x4 x3500 = let
     x5 = d_C_analysisName x2 x3500
      in (C_CombinedDependencyTypeAnalysis x5 x1 Curry_Prelude.C_True x3 (d_C_runWithBaseAnalysis x2 x4))

nd_C_combinedDependencyTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1)) t1)) -> IDSupply -> ConstStore -> C_Analysis t1
nd_C_combinedDependencyTypeAnalysis x1 x2 x3 x4 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = nd_C_analysisName x2 x2000 x3500
           in (HO_C_CombinedDependencyTypeAnalysis x5 x1 Curry_Prelude.C_True x3 (wrapNX id (nd_C_runWithBaseAnalysis x2 x4)))))

d_C_runWithBaseAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> ConstStore -> t1 -> ConstStore -> t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (t1 -> ConstStore -> t2)
d_C_runWithBaseAnalysis x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x3 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1 x2 x1 x3) x3500

nd_C_runWithBaseAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func t1 t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Func t1 t2)
nd_C_runWithBaseAnalysis x1 x2 x3 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x3 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1 x2 x1 x3)) x2000 x3500))

d_OP_runWithBaseAnalysis_dot___hash_lambda1 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => (Curry_GenericProgInfo.C_ProgInfo t71 -> ConstStore -> t64 -> ConstStore -> t65) -> C_Analysis t71 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (t64 -> ConstStore -> t65)
d_OP_runWithBaseAnalysis_dot___hash_lambda1 x1 x2 x3 x4 x3500 = let
     x5 = d_C_analysisName x2 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_getInterfaceInfos x5 x4 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x5 x3) x3500)

nd_OP_runWithBaseAnalysis_dot___hash_lambda1 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => Func (Curry_GenericProgInfo.C_ProgInfo t71) (Func t64 t65) -> C_Analysis t71 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Func t64 t65)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1 x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = nd_C_analysisName x2 x2000 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_getInterfaceInfos x5 x4 x2001 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x5 x3)) x2002 x3500)))))))))

d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => (Curry_GenericProgInfo.C_ProgInfo t71 -> ConstStore -> t64 -> ConstStore -> t65) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t71 -> ConstStore -> Curry_Prelude.C_IO (t64 -> ConstStore -> t65)
d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_loadCompleteAnalysis x2 x3 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x4) x3500

nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => Func (Curry_GenericProgInfo.C_ProgInfo t71) (Func t64 t65) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t71 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Func t64 t65)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_loadCompleteAnalysis x2 x3 x2000 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x4)) x2001 x3500)))))

d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => (Curry_GenericProgInfo.C_ProgInfo t71 -> ConstStore -> t64 -> ConstStore -> t65) -> Curry_GenericProgInfo.C_ProgInfo t71 -> Curry_GenericProgInfo.C_ProgInfo t71 -> ConstStore -> Curry_Prelude.C_IO (t64 -> ConstStore -> t65)
d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3500 = let
     x4 = Curry_GenericProgInfo.d_C_combineProgInfo x2 x3 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x1 x4 x3500) x3500)

nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t71,Curry_Prelude.Curry t64,Curry_Prelude.Curry t65) => Func (Curry_GenericProgInfo.C_ProgInfo t71) (Func t64 t65) -> Curry_GenericProgInfo.C_ProgInfo t71 -> Curry_GenericProgInfo.C_ProgInfo t71 -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Func t64 t65)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x4 = Curry_GenericProgInfo.nd_C_combineProgInfo x2 x3 x2000 x3500
                in (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x1 x4 x2001 x3500) x3500))))))

d_C_isSimpleAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSimpleAnalysis x1 x3500 = case x1 of
     (C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (C_SimpleTypeAnalysis x4 x5) -> Curry_Prelude.C_True
     (C_SimpleConstructorAnalysis x6 x7) -> Curry_Prelude.C_True
     (C_CombinedSimpleFuncAnalysis x8 x9 x10 x11) -> Curry_Prelude.C_True
     (C_CombinedSimpleTypeAnalysis x12 x13 x14 x15) -> Curry_Prelude.C_True
     (C_DependencyFuncAnalysis x16 x17 x18) -> Curry_Prelude.C_False
     (C_DependencyTypeAnalysis x19 x20 x21) -> Curry_Prelude.C_False
     (C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> Curry_Prelude.C_False
     (C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSimpleAnalysis x1002 x3500) (d_C_isSimpleAnalysis x1003 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSimpleAnalysis z x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSimpleAnalysis x1002) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_isSimpleAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isSimpleAnalysis x1 x3000 x3500 = case x1 of
     (HO_C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (HO_C_SimpleTypeAnalysis x4 x5) -> Curry_Prelude.C_True
     (HO_C_SimpleConstructorAnalysis x6 x7) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleFuncAnalysis x8 x9 x10 x11) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleTypeAnalysis x12 x13 x14 x15) -> Curry_Prelude.C_True
     (HO_C_DependencyFuncAnalysis x16 x17 x18) -> Curry_Prelude.C_False
     (HO_C_DependencyTypeAnalysis x19 x20 x21) -> Curry_Prelude.C_False
     (HO_C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> Curry_Prelude.C_False
     (HO_C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isSimpleAnalysis x1002 x3000 x3500) (nd_C_isSimpleAnalysis x1003 x3000 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isSimpleAnalysis z x3000 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isSimpleAnalysis x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isCombinedAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCombinedAnalysis x1 x3500 = case x1 of
     (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> Curry_Prelude.C_True
     (C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> Curry_Prelude.C_True
     (C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> Curry_Prelude.C_True
     (C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> Curry_Prelude.C_True
     (C_SimpleFuncAnalysis x20 x21) -> Curry_Prelude.C_False
     (C_SimpleTypeAnalysis x22 x23) -> Curry_Prelude.C_False
     (C_SimpleConstructorAnalysis x24 x25) -> Curry_Prelude.C_False
     (C_DependencyFuncAnalysis x26 x27 x28) -> Curry_Prelude.C_False
     (C_DependencyTypeAnalysis x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCombinedAnalysis x1002 x3500) (d_C_isCombinedAnalysis x1003 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCombinedAnalysis z x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCombinedAnalysis x1002) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_isCombinedAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isCombinedAnalysis x1 x3000 x3500 = case x1 of
     (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> Curry_Prelude.C_True
     (HO_C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> Curry_Prelude.C_True
     (HO_C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> Curry_Prelude.C_True
     (HO_C_SimpleFuncAnalysis x20 x21) -> Curry_Prelude.C_False
     (HO_C_SimpleTypeAnalysis x22 x23) -> Curry_Prelude.C_False
     (HO_C_SimpleConstructorAnalysis x24 x25) -> Curry_Prelude.C_False
     (HO_C_DependencyFuncAnalysis x26 x27 x28) -> Curry_Prelude.C_False
     (HO_C_DependencyTypeAnalysis x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isCombinedAnalysis x1002 x3000 x3500) (nd_C_isCombinedAnalysis x1003 x3000 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isCombinedAnalysis z x3000 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isCombinedAnalysis x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_analysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_analysisName x1 x3500 = case x1 of
     (C_SimpleFuncAnalysis x2 x3) -> x2
     (C_SimpleTypeAnalysis x4 x5) -> x4
     (C_SimpleConstructorAnalysis x6 x7) -> x6
     (C_DependencyFuncAnalysis x8 x9 x10) -> x8
     (C_DependencyTypeAnalysis x11 x12 x13) -> x11
     (C_CombinedSimpleFuncAnalysis x14 x15 x16 x17) -> x15
     (C_CombinedSimpleTypeAnalysis x18 x19 x20 x21) -> x19
     (C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> x23
     (C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> x28
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_analysisName x1002 x3500) (d_C_analysisName x1003 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_analysisName z x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_analysisName x1002) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_analysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_analysisName x1 x3000 x3500 = case x1 of
     (HO_C_SimpleFuncAnalysis x2 x3) -> x2
     (HO_C_SimpleTypeAnalysis x4 x5) -> x4
     (HO_C_SimpleConstructorAnalysis x6 x7) -> x6
     (HO_C_DependencyFuncAnalysis x8 x9 x10) -> x8
     (HO_C_DependencyTypeAnalysis x11 x12 x13) -> x11
     (HO_C_CombinedSimpleFuncAnalysis x14 x15 x16 x17) -> x15
     (HO_C_CombinedSimpleTypeAnalysis x18 x19 x20 x21) -> x19
     (HO_C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> x23
     (HO_C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> x28
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_analysisName x1002 x3000 x3500) (nd_C_analysisName x1003 x3000 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_analysisName z x3000 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_analysisName x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_baseAnalysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_baseAnalysisName x1 x3500 = case x1 of
     (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> x2
     (C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> x6
     (C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> x10
     (C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> x15
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_baseAnalysisName x1002 x3500) (d_C_baseAnalysisName x1003 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_baseAnalysisName z x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_baseAnalysisName x1002) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_baseAnalysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_baseAnalysisName x1 x3000 x3500 = case x1 of
     (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> x2
     (HO_C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> x6
     (HO_C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> x10
     (HO_C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> x15
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_baseAnalysisName x1002 x3000 x3500) (nd_C_baseAnalysisName x1003 x3000 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_baseAnalysisName z x3000 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_baseAnalysisName x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_startValue :: Curry_Prelude.Curry t0 => C_Analysis t0 -> ConstStore -> t0
d_C_startValue x1 x3500 = case x1 of
     (C_DependencyFuncAnalysis x2 x3 x4) -> x3
     (C_DependencyTypeAnalysis x5 x6 x7) -> x6
     (C_CombinedDependencyFuncAnalysis x8 x9 x10 x11 x12) -> x11
     (C_CombinedDependencyTypeAnalysis x13 x14 x15 x16 x17) -> x16
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_startValue x1002 x3500) (d_C_startValue x1003 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_startValue z x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_startValue x1002) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_startValue :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> ConstStore -> t0
nd_C_startValue x1 x3000 x3500 = case x1 of
     (HO_C_DependencyFuncAnalysis x2 x3 x4) -> x3
     (HO_C_DependencyTypeAnalysis x5 x6 x7) -> x6
     (HO_C_CombinedDependencyFuncAnalysis x8 x9 x10 x11 x12) -> x11
     (HO_C_CombinedDependencyTypeAnalysis x13 x14 x15 x16 x17) -> x16
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_startValue x1002 x3000 x3500) (nd_C_startValue x1003 x3000 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_startValue z x3000 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_startValue x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
