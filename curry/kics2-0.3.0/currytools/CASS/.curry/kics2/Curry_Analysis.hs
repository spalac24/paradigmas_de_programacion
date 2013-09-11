{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Analysis (C_Analysis (..), C_AOutFormat (..), d_C_simpleFuncAnalysis, nd_C_simpleFuncAnalysis, d_C_simpleTypeAnalysis, nd_C_simpleTypeAnalysis, d_C_simpleConstructorAnalysis, nd_C_simpleConstructorAnalysis, d_C_dependencyFuncAnalysis, nd_C_dependencyFuncAnalysis, d_C_dependencyTypeAnalysis, nd_C_dependencyTypeAnalysis, d_C_combinedSimpleFuncAnalysis, nd_C_combinedSimpleFuncAnalysis, d_C_combinedSimpleTypeAnalysis, nd_C_combinedSimpleTypeAnalysis, d_C_combinedDependencyFuncAnalysis, nd_C_combinedDependencyFuncAnalysis, d_C_combinedDependencyTypeAnalysis, nd_C_combinedDependencyTypeAnalysis, d_C_isSimpleAnalysis, nd_C_isSimpleAnalysis, d_C_isCombinedAnalysis, nd_C_isCombinedAnalysis, d_C_isFunctionAnalysis, nd_C_isFunctionAnalysis, d_C_analysisName, nd_C_analysisName, d_C_baseAnalysisName, nd_C_baseAnalysisName, d_C_startValue, nd_C_startValue) where

import Basics
import qualified Curry_CurryFiles
import qualified Curry_FlatCurry
import qualified Curry_GenericProgInfo
import qualified Curry_LoadAnalysis
import qualified Curry_Prelude
import qualified Curry_FlatCurryGoodies
data C_Analysis t0
     = C_SimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t0)
     | HO_C_SimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_FuncDecl t0)
     | C_SimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t0)
     | HO_C_SimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_TypeDecl t0)
     | C_SimpleConstructorAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t0)
     | HO_C_SimpleConstructorAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func Curry_FlatCurry.C_ConsDecl (Func Curry_FlatCurry.C_TypeDecl t0))
     | C_DependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0)
     | HO_C_DependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))
     | C_DependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0)
     | HO_C_DependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0 (Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))
     | C_CombinedSimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t0))
     | HO_C_CombinedSimpleFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_FuncDecl t0)))
     | C_CombinedSimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t0))
     | HO_C_CombinedSimpleTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_TypeDecl t0)))
     | C_CombinedDependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0))
     | HO_C_CombinedDependencyFuncAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0))))
     | C_CombinedDependencyTypeAnalysis (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0))
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
  generate s c = Choices_C_Analysis c (freeID [2,2,2,3,3,4,4,5,5] s) [(HO_C_SimpleFuncAnalysis (generate (leftSupply s) c) (generate (rightSupply s) c)),(HO_C_SimpleTypeAnalysis (generate (leftSupply s) c) (generate (rightSupply s) c)),(HO_C_SimpleConstructorAnalysis (generate (leftSupply s) c) (generate (rightSupply s) c)),(HO_C_DependencyFuncAnalysis (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(HO_C_DependencyTypeAnalysis (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c)),(HO_C_CombinedSimpleFuncAnalysis (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(HO_C_CombinedSimpleTypeAnalysis (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(HO_C_CombinedDependencyFuncAnalysis (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c)),(HO_C_CombinedDependencyTypeAnalysis (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_Analysis t0) where
  ($!!) cont (C_SimpleFuncAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleFuncAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_SimpleFuncAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleFuncAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_SimpleTypeAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleTypeAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_SimpleTypeAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleTypeAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_SimpleConstructorAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleConstructorAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_SimpleConstructorAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleConstructorAnalysis y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_DependencyFuncAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DependencyFuncAnalysis y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_DependencyFuncAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_DependencyTypeAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DependencyTypeAnalysis y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_DependencyTypeAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Analysis cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Analysis cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Analysis cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Analysis cd info) _ _ = failCons cd info
  ($##) cont (C_SimpleFuncAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleFuncAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_SimpleFuncAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleFuncAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_SimpleTypeAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleTypeAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_SimpleTypeAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleTypeAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_SimpleConstructorAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_SimpleConstructorAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_SimpleConstructorAnalysis x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (HO_C_SimpleConstructorAnalysis y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_DependencyFuncAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DependencyFuncAnalysis y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_DependencyFuncAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_DependencyTypeAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_DependencyTypeAnalysis y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_DependencyTypeAnalysis x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Analysis cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Analysis cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Analysis cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Analysis cd info) _ _ = failCons cd info
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
  (=.=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_SimpleFuncAnalysis x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_SimpleFuncAnalysis x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_SimpleTypeAnalysis x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_SimpleTypeAnalysis x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_SimpleConstructorAnalysis x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (HO_C_SimpleConstructorAnalysis x3 x4) = ((i :=: (ChooseN 2 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_DependencyFuncAnalysis x3 x4 x5) = ((i :=: (ChooseN 3 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (HO_C_DependencyFuncAnalysis x3 x4 x5) = ((i :=: (ChooseN 3 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_DependencyTypeAnalysis x3 x4 x5) = ((i :=: (ChooseN 4 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (HO_C_DependencyTypeAnalysis x3 x4 x5) = ((i :=: (ChooseN 4 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind cd i (C_CombinedSimpleFuncAnalysis x3 x4 x5 x6) = ((i :=: (ChooseN 5 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (HO_C_CombinedSimpleFuncAnalysis x3 x4 x5 x6) = ((i :=: (ChooseN 5 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_CombinedSimpleTypeAnalysis x3 x4 x5 x6) = ((i :=: (ChooseN 6 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (HO_C_CombinedSimpleTypeAnalysis x3 x4 x5 x6) = ((i :=: (ChooseN 6 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind cd i (C_CombinedDependencyFuncAnalysis x3 x4 x5 x6 x7) = ((i :=: (ChooseN 7 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind cd i (HO_C_CombinedDependencyFuncAnalysis x3 x4 x5 x6 x7) = ((i :=: (ChooseN 7 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind cd i (C_CombinedDependencyTypeAnalysis x3 x4 x5 x6 x7) = ((i :=: (ChooseN 8 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind cd i (HO_C_CombinedDependencyTypeAnalysis x3 x4 x5 x6 x7) = ((i :=: (ChooseN 8 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_Analysis cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Analysis cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Analysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Analysis cd i _) = error ("Analysis.Analysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Analysis cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Analysis cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_SimpleFuncAnalysis x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_SimpleFuncAnalysis x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_SimpleTypeAnalysis x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_SimpleTypeAnalysis x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_SimpleConstructorAnalysis x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (HO_C_SimpleConstructorAnalysis x3 x4) = [(i :=: (ChooseN 2 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_DependencyFuncAnalysis x3 x4 x5) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (HO_C_DependencyFuncAnalysis x3 x4 x5) = [(i :=: (ChooseN 3 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_DependencyTypeAnalysis x3 x4 x5) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (HO_C_DependencyTypeAnalysis x3 x4 x5) = [(i :=: (ChooseN 4 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind cd i (C_CombinedSimpleFuncAnalysis x3 x4 x5 x6) = [(i :=: (ChooseN 5 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (HO_C_CombinedSimpleFuncAnalysis x3 x4 x5 x6) = [(i :=: (ChooseN 5 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_CombinedSimpleTypeAnalysis x3 x4 x5 x6) = [(i :=: (ChooseN 6 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (HO_C_CombinedSimpleTypeAnalysis x3 x4 x5 x6) = [(i :=: (ChooseN 6 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind cd i (C_CombinedDependencyFuncAnalysis x3 x4 x5 x6 x7) = [(i :=: (ChooseN 7 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind cd i (HO_C_CombinedDependencyFuncAnalysis x3 x4 x5 x6 x7) = [(i :=: (ChooseN 7 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind cd i (C_CombinedDependencyTypeAnalysis x3 x4 x5 x6 x7) = [(i :=: (ChooseN 8 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind cd i (HO_C_CombinedDependencyTypeAnalysis x3 x4 x5 x6 x7) = [(i :=: (ChooseN 8 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_Analysis cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Analysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Analysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Analysis cd i _) = error ("Analysis.Analysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Analysis cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Analysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Analysis t0) where
  (=?=) (Choice_C_Analysis cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Analysis cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Analysis cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Analysis cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Analysis cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Analysis cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Analysis cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Analysis cd info) _ _ = failCons cd info
  (=?=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (=?=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (=?=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Analysis cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Analysis cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Analysis cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Analysis cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Analysis cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Analysis cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Analysis cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Analysis cd info) _ _ = failCons cd info
  (<?=) (C_SimpleFuncAnalysis x1 x2) (C_SimpleFuncAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_SimpleFuncAnalysis _ _) (C_SimpleTypeAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_SimpleTypeAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis x1 x2) (HO_C_SimpleFuncAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_SimpleTypeAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_SimpleTypeAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleFuncAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis x1 x2) (C_SimpleTypeAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_SimpleTypeAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis x1 x2) (HO_C_SimpleTypeAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_SimpleConstructorAnalysis _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleTypeAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis x1 x2) (C_SimpleConstructorAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis x1 x2) (HO_C_SimpleConstructorAnalysis y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_DependencyFuncAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_SimpleConstructorAnalysis _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis x1 x2 x3) (C_DependencyFuncAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis x1 x2 x3) (HO_C_DependencyFuncAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_DependencyTypeAnalysis _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyFuncAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis x1 x2 x3) (C_DependencyTypeAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis x1 x2 x3) (HO_C_DependencyTypeAnalysis y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_DependencyTypeAnalysis _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleFuncAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleFuncAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis x1 x2 x3 x4) (HO_C_CombinedSimpleTypeAnalysis y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedSimpleTypeAnalysis _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyFuncAnalysis _ _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedDependencyFuncAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyFuncAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) (C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (HO_C_CombinedDependencyFuncAnalysis _ _ _ _ _) (HO_C_CombinedDependencyTypeAnalysis _ _ _ _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_CombinedDependencyTypeAnalysis x1 x2 x3 x4 x5) (HO_C_CombinedDependencyTypeAnalysis y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


data C_AOutFormat
     = C_AText
     | C_ANote
     | Choice_C_AOutFormat Cover ID C_AOutFormat C_AOutFormat
     | Choices_C_AOutFormat Cover ID ([C_AOutFormat])
     | Fail_C_AOutFormat Cover FailInfo
     | Guard_C_AOutFormat Cover Constraints C_AOutFormat

instance Show C_AOutFormat where
  showsPrec d (Choice_C_AOutFormat cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_AOutFormat cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_AOutFormat cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_AOutFormat cd info) = showChar '!'
  showsPrec _ C_AText = showString "AText"
  showsPrec _ C_ANote = showString "ANote"


instance Read C_AOutFormat where
  readsPrec _ s = (readParen False (\r -> [ (C_AText,r0) | (_,r0) <- readQualified "Analysis" "AText" r]) s) ++ (readParen False (\r -> [ (C_ANote,r0) | (_,r0) <- readQualified "Analysis" "ANote" r]) s)


instance NonDet C_AOutFormat where
  choiceCons = Choice_C_AOutFormat
  choicesCons = Choices_C_AOutFormat
  failCons = Fail_C_AOutFormat
  guardCons = Guard_C_AOutFormat
  try (Choice_C_AOutFormat cd i x y) = tryChoice cd i x y
  try (Choices_C_AOutFormat cd i xs) = tryChoices cd i xs
  try (Fail_C_AOutFormat cd info) = Fail cd info
  try (Guard_C_AOutFormat cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_AOutFormat cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_AOutFormat cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_AOutFormat cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_AOutFormat cd i _) = error ("Analysis.AOutFormat.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_AOutFormat cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_AOutFormat cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_AOutFormat where
  generate s c = Choices_C_AOutFormat c (freeID [0,0] s) [C_AText,C_ANote]


instance NormalForm C_AOutFormat where
  ($!!) cont C_AText d cs = cont C_AText d cs
  ($!!) cont C_ANote d cs = cont C_ANote d cs
  ($!!) cont (Choice_C_AOutFormat cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_AOutFormat cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_AOutFormat cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_AOutFormat cd info) _ _ = failCons cd info
  ($##) cont C_AText d cs = cont C_AText d cs
  ($##) cont C_ANote d cs = cont C_ANote d cs
  ($##) cont (Choice_C_AOutFormat cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_AOutFormat cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_AOutFormat cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_AOutFormat cd info) _ _ = failCons cd info
  searchNF _ cont C_AText = cont C_AText
  searchNF _ cont C_ANote = cont C_ANote
  searchNF _ _ x = error ("Analysis.AOutFormat.searchNF: no constructor: " ++ (show x))


instance Unifiable C_AOutFormat where
  (=.=) C_AText C_AText d cs = C_Success
  (=.=) C_ANote C_ANote d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_AText C_AText d cs = C_Success
  (=.<=) C_ANote C_ANote d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_AText = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_ANote = ((i :=: (ChooseN 1 0)):(concat []))
  bind d i (Choice_C_AOutFormat cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_AOutFormat cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_AOutFormat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_AOutFormat cd i _) = error ("Analysis.AOutFormat.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_AOutFormat cd info) = [(Unsolvable info)]
  bind d i (Guard_C_AOutFormat cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_AText = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_ANote = [(i :=: (ChooseN 1 0))]
  lazyBind d i (Choice_C_AOutFormat cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_AOutFormat cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_AOutFormat cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_AOutFormat cd i _) = error ("Analysis.AOutFormat.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_AOutFormat cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_AOutFormat cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_AOutFormat where
  (=?=) (Choice_C_AOutFormat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_AOutFormat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_AOutFormat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_AOutFormat cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_AOutFormat cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_AOutFormat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_AOutFormat cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_AOutFormat cd info) _ _ = failCons cd info
  (=?=) C_AText C_AText d cs = Curry_Prelude.C_True
  (=?=) C_ANote C_ANote d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_AOutFormat cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_AOutFormat cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_AOutFormat cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_AOutFormat cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_AOutFormat cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_AOutFormat cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_AOutFormat cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_AOutFormat cd info) _ _ = failCons cd info
  (<?=) C_AText C_AText d cs = Curry_Prelude.C_True
  (<?=) C_AText C_ANote _ _ = Curry_Prelude.C_True
  (<?=) C_ANote C_ANote d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_simpleFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> C_Analysis t0
d_C_simpleFuncAnalysis x1 x2 x3250 x3500 = C_SimpleFuncAnalysis x1 x2

nd_C_simpleFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_FuncDecl t0 -> IDSupply -> Cover -> ConstStore -> C_Analysis t0
nd_C_simpleFuncAnalysis x1 x2 x3000 x3250 x3500 = HO_C_SimpleFuncAnalysis x1 x2

d_C_simpleTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> C_Analysis t0
d_C_simpleTypeAnalysis x1 x2 x3250 x3500 = C_SimpleTypeAnalysis x1 x2

nd_C_simpleTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_TypeDecl t0 -> IDSupply -> Cover -> ConstStore -> C_Analysis t0
nd_C_simpleTypeAnalysis x1 x2 x3000 x3250 x3500 = HO_C_SimpleTypeAnalysis x1 x2

d_C_simpleConstructorAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_FlatCurry.C_ConsDecl -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> C_Analysis t0
d_C_simpleConstructorAnalysis x1 x2 x3250 x3500 = C_SimpleConstructorAnalysis x1 x2

nd_C_simpleConstructorAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func Curry_FlatCurry.C_ConsDecl (Func Curry_FlatCurry.C_TypeDecl t0) -> IDSupply -> Cover -> ConstStore -> C_Analysis t0
nd_C_simpleConstructorAnalysis x1 x2 x3000 x3250 x3500 = HO_C_SimpleConstructorAnalysis x1 x2

d_C_dependencyFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> (Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> C_Analysis t0
d_C_dependencyFuncAnalysis x1 x2 x3 x3250 x3500 = C_DependencyFuncAnalysis x1 x2 x3

nd_C_dependencyFuncAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0) -> IDSupply -> Cover -> ConstStore -> C_Analysis t0
nd_C_dependencyFuncAnalysis x1 x2 x3 x3000 x3250 x3500 = HO_C_DependencyFuncAnalysis x1 x2 x3

d_C_dependencyTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> (Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0) -> Cover -> ConstStore -> t0) -> Cover -> ConstStore -> C_Analysis t0
d_C_dependencyTypeAnalysis x1 x2 x3 x3250 x3500 = C_DependencyTypeAnalysis x1 x2 x3

nd_C_dependencyTypeAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t0)) t0) -> IDSupply -> Cover -> ConstStore -> C_Analysis t0
nd_C_dependencyTypeAnalysis x1 x2 x3 x3000 x3250 x3500 = HO_C_DependencyTypeAnalysis x1 x2 x3

d_C_combinedSimpleFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> C_Analysis t1
d_C_combinedSimpleFuncAnalysis x1 x2 x3 x3250 x3500 = let
     x4 = d_C_analysisName x2 x3250 x3500
      in (C_CombinedSimpleFuncAnalysis x4 x1 Curry_Prelude.C_True (d_C_runWithBaseAnalysis x2 x3))

nd_C_combinedSimpleFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_FuncDecl t1) -> IDSupply -> Cover -> ConstStore -> C_Analysis t1
nd_C_combinedSimpleFuncAnalysis x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = nd_C_analysisName x2 x2000 x3250 x3500
           in (HO_C_CombinedSimpleFuncAnalysis x4 x1 Curry_Prelude.C_True (wrapNX id (nd_C_runWithBaseAnalysis x2 x3)))))

d_C_combinedSimpleTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> C_Analysis t1
d_C_combinedSimpleTypeAnalysis x1 x2 x3 x3250 x3500 = let
     x4 = d_C_analysisName x2 x3250 x3500
      in (C_CombinedSimpleTypeAnalysis x4 x1 Curry_Prelude.C_True (d_C_runWithBaseAnalysis x2 x3))

nd_C_combinedSimpleTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_TypeDecl t1) -> IDSupply -> Cover -> ConstStore -> C_Analysis t1
nd_C_combinedSimpleTypeAnalysis x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x4 = nd_C_analysisName x2 x2000 x3250 x3500
           in (HO_C_CombinedSimpleTypeAnalysis x4 x1 Curry_Prelude.C_True (wrapNX id (nd_C_runWithBaseAnalysis x2 x3)))))

d_C_combinedDependencyFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_FlatCurry.C_FuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1) -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> C_Analysis t1
d_C_combinedDependencyFuncAnalysis x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_C_analysisName x2 x3250 x3500
      in (C_CombinedDependencyFuncAnalysis x5 x1 Curry_Prelude.C_True x3 (d_C_runWithBaseAnalysis x2 x4))

nd_C_combinedDependencyFuncAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_FuncDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1)) t1)) -> IDSupply -> Cover -> ConstStore -> C_Analysis t1
nd_C_combinedDependencyFuncAnalysis x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = nd_C_analysisName x2 x2000 x3250 x3500
           in (HO_C_CombinedDependencyFuncAnalysis x5 x1 Curry_Prelude.C_True x3 (wrapNX id (nd_C_runWithBaseAnalysis x2 x4)))))

d_C_combinedDependencyTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_FlatCurry.C_TypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1) -> Cover -> ConstStore -> t1) -> Cover -> ConstStore -> C_Analysis t1
d_C_combinedDependencyTypeAnalysis x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_C_analysisName x2 x3250 x3500
      in (C_CombinedDependencyTypeAnalysis x5 x1 Curry_Prelude.C_True x3 (d_C_runWithBaseAnalysis x2 x4))

nd_C_combinedDependencyTypeAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_Analysis t0 -> t1 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func Curry_FlatCurry.C_TypeDecl (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) t1)) t1)) -> IDSupply -> Cover -> ConstStore -> C_Analysis t1
nd_C_combinedDependencyTypeAnalysis x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x5 = nd_C_analysisName x2 x2000 x3250 x3500
           in (HO_C_CombinedDependencyTypeAnalysis x5 x1 Curry_Prelude.C_True x3 (wrapNX id (nd_C_runWithBaseAnalysis x2 x4)))))

d_C_runWithBaseAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_Analysis t0 -> (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (t1 -> Cover -> ConstStore -> t2)
d_C_runWithBaseAnalysis x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x3 x3250 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1 x2 x1 x3) x3250 x3500

nd_C_runWithBaseAnalysis :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => C_Analysis t0 -> Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func t1 t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Func t1 t2)
nd_C_runWithBaseAnalysis x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x3 x3250 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1 x2 x1 x3)) x2000 x3250 x3500))

d_OP_runWithBaseAnalysis_dot___hash_lambda1 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (t1 -> Cover -> ConstStore -> t2)
d_OP_runWithBaseAnalysis_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = let
     x5 = d_C_analysisName x2 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_getInterfaceInfos x5 x4 x3250 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x5 x3) x3250 x3500)

nd_OP_runWithBaseAnalysis_dot___hash_lambda1 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func t1 t2) -> C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Func t1 t2)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = nd_C_analysisName x2 x2000 x3250 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_getInterfaceInfos x5 x4 x2001 x3250 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x5 x3)) x2002 x3250 x3500)))))))))

d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (t1 -> Cover -> ConstStore -> t2)
d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_loadCompleteAnalysis x2 x3 x3250 x3500) (d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x4) x3250 x3500

nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func t1 t2) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_GenericProgInfo.C_ProgInfo t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Func t1 t2)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_loadCompleteAnalysis x2 x3 x2000 x3250 x3500) (wrapNX id (nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x4)) x2001 x3250 x3500)))))

d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => (Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> t1 -> Cover -> ConstStore -> t2) -> Curry_GenericProgInfo.C_ProgInfo t0 -> Curry_GenericProgInfo.C_ProgInfo t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO (t1 -> Cover -> ConstStore -> t2)
d_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3250 x3500 = let
     x4 = Curry_GenericProgInfo.d_C_combineProgInfo x2 x3 x3250 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x3250 x3500)

nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1,Curry_Prelude.Curry t2) => Func (Curry_GenericProgInfo.C_ProgInfo t0) (Func t1 t2) -> Curry_GenericProgInfo.C_ProgInfo t0 -> Curry_GenericProgInfo.C_ProgInfo t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Func t1 t2)
nd_OP_runWithBaseAnalysis_dot___hash_lambda1_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x4 = Curry_GenericProgInfo.nd_C_combineProgInfo x2 x3 x2000 x3250 x3500
                in (Curry_Prelude.d_C_return (Curry_Prelude.nd_C_apply x1 x4 x2001 x3250 x3500) x3250 x3500))))))

d_C_isSimpleAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isSimpleAnalysis x1 x3250 x3500 = case x1 of
     (C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (C_SimpleTypeAnalysis x4 x5) -> Curry_Prelude.C_True
     (C_SimpleConstructorAnalysis x6 x7) -> Curry_Prelude.C_True
     (C_CombinedSimpleFuncAnalysis x8 x9 x10 x11) -> Curry_Prelude.C_True
     (C_CombinedSimpleTypeAnalysis x12 x13 x14 x15) -> Curry_Prelude.C_True
     (C_DependencyFuncAnalysis x16 x17 x18) -> Curry_Prelude.C_False
     (C_DependencyTypeAnalysis x19 x20 x21) -> Curry_Prelude.C_False
     (C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> Curry_Prelude.C_False
     (C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isSimpleAnalysis x1002 x3250 x3500) (d_C_isSimpleAnalysis x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isSimpleAnalysis z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isSimpleAnalysis x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isSimpleAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isSimpleAnalysis x1 x3000 x3250 x3500 = case x1 of
     (HO_C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (HO_C_SimpleTypeAnalysis x4 x5) -> Curry_Prelude.C_True
     (HO_C_SimpleConstructorAnalysis x6 x7) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleFuncAnalysis x8 x9 x10 x11) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleTypeAnalysis x12 x13 x14 x15) -> Curry_Prelude.C_True
     (HO_C_DependencyFuncAnalysis x16 x17 x18) -> Curry_Prelude.C_False
     (HO_C_DependencyTypeAnalysis x19 x20 x21) -> Curry_Prelude.C_False
     (HO_C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> Curry_Prelude.C_False
     (HO_C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isSimpleAnalysis x1002 x3000 x3250 x3500) (nd_C_isSimpleAnalysis x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isSimpleAnalysis z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isSimpleAnalysis x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isCombinedAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCombinedAnalysis x1 x3250 x3500 = case x1 of
     (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> Curry_Prelude.C_True
     (C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> Curry_Prelude.C_True
     (C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> Curry_Prelude.C_True
     (C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> Curry_Prelude.C_True
     (C_SimpleFuncAnalysis x20 x21) -> Curry_Prelude.C_False
     (C_SimpleTypeAnalysis x22 x23) -> Curry_Prelude.C_False
     (C_SimpleConstructorAnalysis x24 x25) -> Curry_Prelude.C_False
     (C_DependencyFuncAnalysis x26 x27 x28) -> Curry_Prelude.C_False
     (C_DependencyTypeAnalysis x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isCombinedAnalysis x1002 x3250 x3500) (d_C_isCombinedAnalysis x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isCombinedAnalysis z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isCombinedAnalysis x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isCombinedAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isCombinedAnalysis x1 x3000 x3250 x3500 = case x1 of
     (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> Curry_Prelude.C_True
     (HO_C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> Curry_Prelude.C_True
     (HO_C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> Curry_Prelude.C_True
     (HO_C_SimpleFuncAnalysis x20 x21) -> Curry_Prelude.C_False
     (HO_C_SimpleTypeAnalysis x22 x23) -> Curry_Prelude.C_False
     (HO_C_SimpleConstructorAnalysis x24 x25) -> Curry_Prelude.C_False
     (HO_C_DependencyFuncAnalysis x26 x27 x28) -> Curry_Prelude.C_False
     (HO_C_DependencyTypeAnalysis x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isCombinedAnalysis x1002 x3000 x3250 x3500) (nd_C_isCombinedAnalysis x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isCombinedAnalysis z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isCombinedAnalysis x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isFunctionAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isFunctionAnalysis x1 x3250 x3500 = case x1 of
     (C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (C_DependencyFuncAnalysis x4 x5 x6) -> Curry_Prelude.C_True
     (C_CombinedSimpleFuncAnalysis x7 x8 x9 x10) -> Curry_Prelude.C_True
     (C_CombinedDependencyFuncAnalysis x11 x12 x13 x14 x15) -> Curry_Prelude.C_True
     (C_SimpleTypeAnalysis x16 x17) -> Curry_Prelude.C_False
     (C_SimpleConstructorAnalysis x18 x19) -> Curry_Prelude.C_False
     (C_DependencyTypeAnalysis x20 x21 x22) -> Curry_Prelude.C_False
     (C_CombinedSimpleTypeAnalysis x23 x24 x25 x26) -> Curry_Prelude.C_False
     (C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isFunctionAnalysis x1002 x3250 x3500) (d_C_isFunctionAnalysis x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isFunctionAnalysis z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isFunctionAnalysis x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_isFunctionAnalysis :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_C_isFunctionAnalysis x1 x3000 x3250 x3500 = case x1 of
     (HO_C_SimpleFuncAnalysis x2 x3) -> Curry_Prelude.C_True
     (HO_C_DependencyFuncAnalysis x4 x5 x6) -> Curry_Prelude.C_True
     (HO_C_CombinedSimpleFuncAnalysis x7 x8 x9 x10) -> Curry_Prelude.C_True
     (HO_C_CombinedDependencyFuncAnalysis x11 x12 x13 x14 x15) -> Curry_Prelude.C_True
     (HO_C_SimpleTypeAnalysis x16 x17) -> Curry_Prelude.C_False
     (HO_C_SimpleConstructorAnalysis x18 x19) -> Curry_Prelude.C_False
     (HO_C_DependencyTypeAnalysis x20 x21 x22) -> Curry_Prelude.C_False
     (HO_C_CombinedSimpleTypeAnalysis x23 x24 x25 x26) -> Curry_Prelude.C_False
     (HO_C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> Curry_Prelude.C_False
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_isFunctionAnalysis x1002 x3000 x3250 x3500) (nd_C_isFunctionAnalysis x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_isFunctionAnalysis z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_isFunctionAnalysis x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_analysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_analysisName x1 x3250 x3500 = case x1 of
     (C_SimpleFuncAnalysis x2 x3) -> x2
     (C_SimpleTypeAnalysis x4 x5) -> x4
     (C_SimpleConstructorAnalysis x6 x7) -> x6
     (C_DependencyFuncAnalysis x8 x9 x10) -> x8
     (C_DependencyTypeAnalysis x11 x12 x13) -> x11
     (C_CombinedSimpleFuncAnalysis x14 x15 x16 x17) -> x15
     (C_CombinedSimpleTypeAnalysis x18 x19 x20 x21) -> x19
     (C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> x23
     (C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> x28
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_analysisName x1002 x3250 x3500) (d_C_analysisName x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_analysisName z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_analysisName x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_analysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_analysisName x1 x3000 x3250 x3500 = case x1 of
     (HO_C_SimpleFuncAnalysis x2 x3) -> x2
     (HO_C_SimpleTypeAnalysis x4 x5) -> x4
     (HO_C_SimpleConstructorAnalysis x6 x7) -> x6
     (HO_C_DependencyFuncAnalysis x8 x9 x10) -> x8
     (HO_C_DependencyTypeAnalysis x11 x12 x13) -> x11
     (HO_C_CombinedSimpleFuncAnalysis x14 x15 x16 x17) -> x15
     (HO_C_CombinedSimpleTypeAnalysis x18 x19 x20 x21) -> x19
     (HO_C_CombinedDependencyFuncAnalysis x22 x23 x24 x25 x26) -> x23
     (HO_C_CombinedDependencyTypeAnalysis x27 x28 x29 x30 x31) -> x28
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_analysisName x1002 x3000 x3250 x3500) (nd_C_analysisName x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_analysisName z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_analysisName x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_baseAnalysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_baseAnalysisName x1 x3250 x3500 = case x1 of
     (C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> x2
     (C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> x6
     (C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> x10
     (C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> x15
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_baseAnalysisName x1002 x3250 x3500) (d_C_baseAnalysisName x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_baseAnalysisName z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_baseAnalysisName x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_baseAnalysisName :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_baseAnalysisName x1 x3000 x3250 x3500 = case x1 of
     (HO_C_CombinedSimpleFuncAnalysis x2 x3 x4 x5) -> x2
     (HO_C_CombinedSimpleTypeAnalysis x6 x7 x8 x9) -> x6
     (HO_C_CombinedDependencyFuncAnalysis x10 x11 x12 x13 x14) -> x10
     (HO_C_CombinedDependencyTypeAnalysis x15 x16 x17 x18 x19) -> x15
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_baseAnalysisName x1002 x3000 x3250 x3500) (nd_C_baseAnalysisName x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_baseAnalysisName z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_baseAnalysisName x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_startValue :: Curry_Prelude.Curry t0 => C_Analysis t0 -> Cover -> ConstStore -> t0
d_C_startValue x1 x3250 x3500 = case x1 of
     (C_DependencyFuncAnalysis x2 x3 x4) -> x3
     (C_DependencyTypeAnalysis x5 x6 x7) -> x6
     (C_CombinedDependencyFuncAnalysis x8 x9 x10 x11 x12) -> x11
     (C_CombinedDependencyTypeAnalysis x13 x14 x15 x16 x17) -> x16
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_startValue x1002 x3250 x3500) (d_C_startValue x1003 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_startValue z x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_startValue x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_startValue :: Curry_Prelude.Curry t0 => C_Analysis t0 -> IDSupply -> Cover -> ConstStore -> t0
nd_C_startValue x1 x3000 x3250 x3500 = case x1 of
     (HO_C_DependencyFuncAnalysis x2 x3 x4) -> x3
     (HO_C_DependencyTypeAnalysis x5 x6 x7) -> x6
     (HO_C_CombinedDependencyFuncAnalysis x8 x9 x10 x11 x12) -> x11
     (HO_C_CombinedDependencyTypeAnalysis x13 x14 x15 x16 x17) -> x16
     (Choice_C_Analysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_startValue x1002 x3000 x3250 x3500) (nd_C_startValue x1003 x3000 x3250 x3500)
     (Choices_C_Analysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_startValue z x3000 x3250 x3500) x1002
     (Guard_C_Analysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_startValue x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_Analysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
