{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AnalysisCollection (C_RegisteredAnalysis, d_C_analysisInfos, d_C_functionAnalysisInfos, d_C_registeredAnalysisNames, d_C_lookupRegAnaWorker, nd_C_lookupRegAnaWorker, d_C_runAnalysisWithWorkers, nd_C_runAnalysisWithWorkers, d_C_analyzeMain, nd_C_analyzeMain) where

import Basics
import qualified Curry_Analysis
import qualified Curry_AnalysisDependencies
import qualified Curry_Configuration
import qualified Curry_CurryFiles
import qualified Curry_Deterministic
import qualified Curry_GenericProgInfo
import qualified Curry_HigherOrder
import qualified Curry_IO
import qualified Curry_Indeterministic
import qualified Curry_LoadAnalysis
import qualified Curry_Prelude
import qualified Curry_RightLinearity
import qualified Curry_ServerFunctions
import qualified Curry_SolutionCompleteness
import qualified Curry_TotallyDefined
import qualified Curry_WorkerFunctions
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_IOExts
import qualified Curry_XML
data C_RegisteredAnalysis
     = C_RegAna (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
     | HO_C_RegAna (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func Curry_Prelude.C_Bool (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit))
     | Choice_C_RegisteredAnalysis Cover ID C_RegisteredAnalysis C_RegisteredAnalysis
     | Choices_C_RegisteredAnalysis Cover ID ([C_RegisteredAnalysis])
     | Fail_C_RegisteredAnalysis Cover FailInfo
     | Guard_C_RegisteredAnalysis Cover Constraints C_RegisteredAnalysis

instance Show C_RegisteredAnalysis where
  showsPrec d (Choice_C_RegisteredAnalysis cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_RegisteredAnalysis cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_RegisteredAnalysis cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_RegisteredAnalysis cd info) = showChar '!'
  showsPrec _ (C_RegAna x1 x2 x3) = (showString "(RegAna") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))
  showsPrec _ (HO_C_RegAna x1 x2 x3) = (showString "(RegAna") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_RegisteredAnalysis where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_RegAna x1 x2 x3,r3) | (_,r0) <- readQualified "AnalysisCollection" "RegAna" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_RegisteredAnalysis where
  choiceCons = Choice_C_RegisteredAnalysis
  choicesCons = Choices_C_RegisteredAnalysis
  failCons = Fail_C_RegisteredAnalysis
  guardCons = Guard_C_RegisteredAnalysis
  try (Choice_C_RegisteredAnalysis cd i x y) = tryChoice cd i x y
  try (Choices_C_RegisteredAnalysis cd i xs) = tryChoices cd i xs
  try (Fail_C_RegisteredAnalysis cd info) = Fail cd info
  try (Guard_C_RegisteredAnalysis cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_RegisteredAnalysis cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_RegisteredAnalysis cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_RegisteredAnalysis cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_RegisteredAnalysis cd i _) = error ("AnalysisCollection.RegisteredAnalysis.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_RegisteredAnalysis cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_RegisteredAnalysis cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_RegisteredAnalysis where
  generate s = Choices_C_RegisteredAnalysis defCover (freeID [3] s) [(C_RegAna (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_RegisteredAnalysis where
  ($!!) cont (C_RegAna x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_RegAna y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (HO_C_RegAna x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_RegAna y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_RegisteredAnalysis cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_RegisteredAnalysis cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_RegisteredAnalysis cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_RegisteredAnalysis cd info) _ = failCons cd info
  ($##) cont (C_RegAna x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_RegAna y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (HO_C_RegAna x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (HO_C_RegAna y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_RegisteredAnalysis cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_RegisteredAnalysis cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_RegisteredAnalysis cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_RegisteredAnalysis cd info) _ = failCons cd info
  searchNF search cont (C_RegAna x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_RegAna y1 y2 y3)) x3) x2) x1
  searchNF search cont (HO_C_RegAna x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (HO_C_RegAna y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("AnalysisCollection.RegisteredAnalysis.searchNF: no constructor: " ++ (show x))


instance Unifiable C_RegisteredAnalysis where
  (=.=) (C_RegAna x1 x2 x3) (C_RegAna y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) (HO_C_RegAna x1 x2 x3) (HO_C_RegAna y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_RegAna x1 x2 x3) (C_RegAna y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) (HO_C_RegAna x1 x2 x3) (HO_C_RegAna y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_RegAna x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (HO_C_RegAna x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_RegisteredAnalysis cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_RegisteredAnalysis cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_RegisteredAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_RegisteredAnalysis cd i _) = error ("AnalysisCollection.RegisteredAnalysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_RegisteredAnalysis cd info) = [(Unsolvable info)]
  bind i (Guard_C_RegisteredAnalysis cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_RegAna x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (HO_C_RegAna x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_RegisteredAnalysis cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_RegisteredAnalysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_RegisteredAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_RegisteredAnalysis cd i _) = error ("AnalysisCollection.RegisteredAnalysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_RegisteredAnalysis cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_RegisteredAnalysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_RegisteredAnalysis where
  (=?=) (Choice_C_RegisteredAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_RegisteredAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_RegisteredAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_RegisteredAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_RegisteredAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_RegisteredAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_RegisteredAnalysis cd info) _ = failCons cd info
  (=?=) (C_RegAna x1 x2 x3) (C_RegAna y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (=?=) (HO_C_RegAna x1 x2 x3) (HO_C_RegAna y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_RegisteredAnalysis cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_RegisteredAnalysis cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_RegisteredAnalysis cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_RegisteredAnalysis cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_RegisteredAnalysis cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_RegisteredAnalysis cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_RegisteredAnalysis cd info) _ = failCons cd info
  (<?=) (C_RegAna x1 x2 x3) (C_RegAna y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs
  (<?=) (HO_C_RegAna x1 x2 x3) (HO_C_RegAna y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_RegisteredAnalysis where
  cover (C_RegAna x1 x2 x3) = C_RegAna (cover x1) (cover x2) (cover x3)
  cover (HO_C_RegAna x1 x2 x3) = HO_C_RegAna (cover x1) (cover x2) (cover x3)
  cover (Choice_C_RegisteredAnalysis cd i x y) = Choice_C_RegisteredAnalysis (incCover cd) i (cover x) (cover y)
  cover (Choices_C_RegisteredAnalysis cd i xs) = Choices_C_RegisteredAnalysis (incCover cd) i (map cover xs)
  cover (Fail_C_RegisteredAnalysis cd info) = Fail_C_RegisteredAnalysis (incCover cd) info
  cover (Guard_C_RegisteredAnalysis cd c e) = Guard_C_RegisteredAnalysis (incCover cd) c (cover e)


d_C_analysisInfos :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analysisInfos x3500 = Curry_Prelude.d_OP_plus_plus (d_C_functionAnalysisInfos x3500) (d_C_typeAnalysisInfos x3500) x3500

d_C_functionAnalysisInfos :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_functionAnalysisInfos x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) Curry_Prelude.OP_List))))))))

d_C_typeAnalysisInfos :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_typeAnalysisInfos x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) Curry_Prelude.OP_List))

d_C_registeredAnalysis :: ConstStore -> Curry_Prelude.OP_List C_RegisteredAnalysis
d_C_registeredAnalysis x3500 = Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_Deterministic.d_C_overlapAnalysis x3500) Curry_Deterministic.d_C_showOverlap x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_Deterministic.d_C_ndAnalysis x3500) Curry_Deterministic.d_C_showDet x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_Deterministic.d_C_setValAnalysis x3500) Curry_Deterministic.d_C_showSetValued x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_RightLinearity.d_C_rlinAnalysis x3500) Curry_RightLinearity.d_C_showRightLinear x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_SolutionCompleteness.d_C_solcompAnalysis x3500) Curry_SolutionCompleteness.d_C_showSolComplete x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_TotallyDefined.d_C_patCompAnalysis x3500) Curry_TotallyDefined.d_C_showComplete x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_TotallyDefined.d_C_totalAnalysis x3500) Curry_TotallyDefined.d_C_showTotally x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_Indeterministic.d_C_indetAnalysis x3500) Curry_Indeterministic.d_C_showIndet x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_HigherOrder.d_C_hiOrdType x3500) Curry_HigherOrder.d_C_showOrder x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_HigherOrder.d_C_hiOrdCons x3500) Curry_HigherOrder.d_C_showOrder x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_HigherOrder.d_C_hiOrdFunc x3500) Curry_Prelude.d_C_show x3500) (Curry_Prelude.OP_Cons (d_C_scAnalysis (Curry_TotallyDefined.d_C_siblingCons x3500) Curry_Prelude.d_C_show x3500) Curry_Prelude.OP_List)))))))))))

nd_C_registeredAnalysis :: IDSupply -> ConstStore -> Curry_Prelude.OP_List C_RegisteredAnalysis
nd_C_registeredAnalysis x3000 x3500 = let
     x2046 = x3000
      in (seq x2046 (let
          x2002 = leftSupply x2046
          x2045 = rightSupply x2046
           in (seq x2002 (seq x2045 (Curry_Prelude.OP_Cons (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_scAnalysis (Curry_Deterministic.nd_C_overlapAnalysis x2000 x3500) (wrapDX id Curry_Deterministic.d_C_showOverlap) x2001 x3500)))) (let
               x2005 = leftSupply x2045
               x2044 = rightSupply x2045
                in (seq x2005 (seq x2044 (Curry_Prelude.OP_Cons (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (nd_C_scAnalysis (Curry_Deterministic.nd_C_ndAnalysis x2003 x3500) (wrapDX id Curry_Deterministic.d_C_showDet) x2004 x3500)))) (let
                    x2008 = leftSupply x2044
                    x2043 = rightSupply x2044
                     in (seq x2008 (seq x2043 (Curry_Prelude.OP_Cons (let
                         x2007 = leftSupply x2008
                         x2006 = rightSupply x2008
                          in (seq x2007 (seq x2006 (nd_C_scAnalysis (Curry_Deterministic.nd_C_setValAnalysis x2006 x3500) (wrapDX id Curry_Deterministic.d_C_showSetValued) x2007 x3500)))) (let
                         x2011 = leftSupply x2043
                         x2042 = rightSupply x2043
                          in (seq x2011 (seq x2042 (Curry_Prelude.OP_Cons (let
                              x2010 = leftSupply x2011
                              x2009 = rightSupply x2011
                               in (seq x2010 (seq x2009 (nd_C_scAnalysis (Curry_RightLinearity.nd_C_rlinAnalysis x2009 x3500) (wrapDX id Curry_RightLinearity.d_C_showRightLinear) x2010 x3500)))) (let
                              x2014 = leftSupply x2042
                              x2041 = rightSupply x2042
                               in (seq x2014 (seq x2041 (Curry_Prelude.OP_Cons (let
                                   x2013 = leftSupply x2014
                                   x2012 = rightSupply x2014
                                    in (seq x2013 (seq x2012 (nd_C_scAnalysis (Curry_SolutionCompleteness.nd_C_solcompAnalysis x2012 x3500) (wrapDX id Curry_SolutionCompleteness.d_C_showSolComplete) x2013 x3500)))) (let
                                   x2017 = leftSupply x2041
                                   x2040 = rightSupply x2041
                                    in (seq x2017 (seq x2040 (Curry_Prelude.OP_Cons (let
                                        x2016 = leftSupply x2017
                                        x2015 = rightSupply x2017
                                         in (seq x2016 (seq x2015 (nd_C_scAnalysis (Curry_TotallyDefined.nd_C_patCompAnalysis x2015 x3500) (wrapDX id Curry_TotallyDefined.d_C_showComplete) x2016 x3500)))) (let
                                        x2020 = leftSupply x2040
                                        x2039 = rightSupply x2040
                                         in (seq x2020 (seq x2039 (Curry_Prelude.OP_Cons (let
                                             x2019 = leftSupply x2020
                                             x2018 = rightSupply x2020
                                              in (seq x2019 (seq x2018 (nd_C_scAnalysis (Curry_TotallyDefined.nd_C_totalAnalysis x2018 x3500) (wrapDX id Curry_TotallyDefined.d_C_showTotally) x2019 x3500)))) (let
                                             x2023 = leftSupply x2039
                                             x2038 = rightSupply x2039
                                              in (seq x2023 (seq x2038 (Curry_Prelude.OP_Cons (let
                                                  x2022 = leftSupply x2023
                                                  x2021 = rightSupply x2023
                                                   in (seq x2022 (seq x2021 (nd_C_scAnalysis (Curry_Indeterministic.nd_C_indetAnalysis x2021 x3500) (wrapDX id Curry_Indeterministic.d_C_showIndet) x2022 x3500)))) (let
                                                  x2026 = leftSupply x2038
                                                  x2037 = rightSupply x2038
                                                   in (seq x2026 (seq x2037 (Curry_Prelude.OP_Cons (let
                                                       x2025 = leftSupply x2026
                                                       x2024 = rightSupply x2026
                                                        in (seq x2025 (seq x2024 (nd_C_scAnalysis (Curry_HigherOrder.nd_C_hiOrdType x2024 x3500) (wrapDX id Curry_HigherOrder.d_C_showOrder) x2025 x3500)))) (let
                                                       x2029 = leftSupply x2037
                                                       x2036 = rightSupply x2037
                                                        in (seq x2029 (seq x2036 (Curry_Prelude.OP_Cons (let
                                                            x2028 = leftSupply x2029
                                                            x2027 = rightSupply x2029
                                                             in (seq x2028 (seq x2027 (nd_C_scAnalysis (Curry_HigherOrder.nd_C_hiOrdCons x2027 x3500) (wrapDX id Curry_HigherOrder.d_C_showOrder) x2028 x3500)))) (let
                                                            x2032 = leftSupply x2036
                                                            x2035 = rightSupply x2036
                                                             in (seq x2032 (seq x2035 (Curry_Prelude.OP_Cons (let
                                                                 x2031 = leftSupply x2032
                                                                 x2030 = rightSupply x2032
                                                                  in (seq x2031 (seq x2030 (nd_C_scAnalysis (Curry_HigherOrder.nd_C_hiOrdFunc x2030 x3500) (wrapDX id Curry_Prelude.d_C_show) x2031 x3500)))) (Curry_Prelude.OP_Cons (let
                                                                 x2034 = leftSupply x2035
                                                                 x2033 = rightSupply x2035
                                                                  in (seq x2034 (seq x2033 (nd_C_scAnalysis (Curry_TotallyDefined.nd_C_siblingCons x2033 x3500) (wrapDX id Curry_Prelude.d_C_show) x2034 x3500)))) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))

d_C_regAnaName :: C_RegisteredAnalysis -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_regAnaName x1 x3500 = case x1 of
     (C_RegAna x2 x3 x4) -> x2
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaName x1002 x3500) (d_C_regAnaName x1003 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaName z x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaName x1002) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_regAnaName :: C_RegisteredAnalysis -> IDSupply -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_regAnaName x1 x3000 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4) -> x2
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaName x1002 x3000 x3500) (nd_C_regAnaName x1003 x3000 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaName z x3000 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaName x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_regAnaServer :: C_RegisteredAnalysis -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_regAnaServer x1 x3500 = case x1 of
     (C_RegAna x2 x3 x4) -> x3
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaServer x1002 x3500) (d_C_regAnaServer x1003 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaServer z x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaServer x1002) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_regAnaServer :: C_RegisteredAnalysis -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func Curry_Prelude.C_Bool (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_C_regAnaServer x1 x3000 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4) -> x3
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaServer x1002 x3000 x3500) (nd_C_regAnaServer x1003 x3000 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaServer z x3000 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaServer x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_regAnaWorker :: C_RegisteredAnalysis -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_regAnaWorker x1 x3500 = case x1 of
     (C_RegAna x2 x3 x4) -> x4
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaWorker x1002 x3500) (d_C_regAnaWorker x1003 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaWorker z x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaWorker x1002) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_regAnaWorker :: C_RegisteredAnalysis -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_regAnaWorker x1 x3000 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4) -> x4
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaWorker x1002 x3000 x3500) (nd_C_regAnaWorker x1003 x3000 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaWorker z x3000 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaWorker x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_registeredAnalysisNames :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_registeredAnalysisNames x3500 = Curry_Prelude.d_C_map d_C_regAnaName (d_C_registeredAnalysis x3500) x3500

d_C_lookupRegAna :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
d_C_lookupRegAna x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_6 x1 x4 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupRegAna x1 x1002 x3500) (d_C_lookupRegAna x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupRegAna x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupRegAna x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_lookupRegAna :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> IDSupply -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
nd_C_lookupRegAna x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_lookupRegAna x1 x1002 x3000 x3500) (nd_C_lookupRegAna x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_lookupRegAna x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_lookupRegAna x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lookupRegAnaServer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_lookupRegAnaServer x1 x3500 = Curry_Prelude.d_C_maybe (acceptCs (acceptCs id) (d_OP_lookupRegAnaServer_dot___hash_lambda1 x1)) d_C_regAnaServer (d_C_lookupRegAna x1 (d_C_registeredAnalysis x3500) x3500) x3500

nd_C_lookupRegAnaServer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func Curry_Prelude.C_Bool (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_C_lookupRegAnaServer x1 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) (d_OP_lookupRegAnaServer_dot___hash_lambda1 x1))) (wrapNX id nd_C_regAnaServer) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_lookupRegAna x1 (nd_C_registeredAnalysis x2000 x3500) x2001 x3500)))) x2003 x3500)))))

d_OP_lookupRegAnaServer_dot___hash_lambda1 :: Curry_Prelude.Curry t199 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either t199 (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupRegAnaServer_dot___hash_lambda1 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.C_Right (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) x1 x3500)) x3500

d_C_lookupRegAnaWorker :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_lookupRegAnaWorker x1 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_const (Curry_Prelude.d_C_done x3500)) d_C_regAnaWorker (d_C_lookupRegAna x1 (d_C_registeredAnalysis x3500) x3500) x3500

nd_C_lookupRegAnaWorker :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_lookupRegAnaWorker x1 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (wrapDX id (Curry_Prelude.d_C_const (Curry_Prelude.d_C_done x3500))) (wrapNX id nd_C_regAnaWorker) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_lookupRegAna x1 (nd_C_registeredAnalysis x2000 x3500) x2001 x3500)))) x2003 x3500)))))

d_C_scAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> C_RegisteredAnalysis
d_C_scAnalysis x1 x2 x3500 = C_RegAna (Curry_Analysis.d_C_analysisName x1 x3500) (acceptCs (acceptCs id) (d_C_analyzeAsString x1 x2)) (Curry_WorkerFunctions.d_C_analysisClient x1)

nd_C_scAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> C_RegisteredAnalysis
nd_C_scAnalysis x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (HO_C_RegAna (Curry_Analysis.nd_C_analysisName x1 x2000 x3500) (wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) (nd_C_analyzeAsString x1 x2))) (wrapNX id (Curry_WorkerFunctions.nd_C_analysisClient x1))))

d_C_debugMessage :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_debugMessage x1 x2 x3500 = Curry_Configuration.d_C_debugMessageLevel x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) x2 x3500) x3500

d_C_runAnalysisWithWorkers :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_runAnalysisWithWorkers x1 x2 x3 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupRegAnaServer x1 x3500) x3 x3500) x2 x3500) Curry_Prelude.C_True x3500

nd_C_runAnalysisWithWorkers :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_runAnalysisWithWorkers x1 x2 x3 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_lookupRegAnaServer x1 x2000 x3500) x3 x2001 x3500)))) x2 x2003 x3500)))) Curry_Prelude.C_True x2005 x3500)))))

d_C_runAnalysisWithWorkersNoLoad :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_runAnalysisWithWorkersNoLoad x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupRegAnaServer x1 x3500) x3 x3500) x2 x3500) Curry_Prelude.C_False x3500) (Curry_Prelude.d_C_done x3500) x3500

d_C_analyzeAsString :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> (t0 -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeAsString x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_analyzeMain x1 x3 x4 x5 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_Prelude.d_C_either (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Left) (Curry_GenericProgInfo.d_C_mapProgInfo x2) x3500) (acceptCs id Curry_Prelude.C_Right)) x3500) x3500

nd_C_analyzeAsString :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_analyzeAsString x1 x2 x3 x4 x5 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_analyzeMain x1 x3 x4 x5 x2000 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapNX id (Curry_Prelude.nd_C_either (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id Curry_Prelude.C_Left)) (wrapNX id (Curry_GenericProgInfo.nd_C_mapProgInfo x2)) x2001 x3500) (wrapDX id (acceptCs id Curry_Prelude.C_Right)))) x2002 x3500)))) x2004 x3500))))))))

d_C_analyzeMain :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeMain x1 x2 x3 x4 x3500 = let
     x5 = Curry_Analysis.d_C_analysisName x1 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisDependencies.d_C_getModulesToAnalyze x1 x2 x3500) (d_OP_analyzeMain_dot___hash_lambda2 x1 x5 x3 x4 x2) x3500) x3500)

nd_C_analyzeMain :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_analyzeMain x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = Curry_Analysis.nd_C_analysisName x1 x2000 x3500
                in (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x5 x3500) x3500) x3500) x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AnalysisDependencies.nd_C_getModulesToAnalyze x1 x2 x2001 x3500) (wrapNX id (nd_OP_analyzeMain_dot___hash_lambda2 x1 x5 x3 x4 x2)) x2002 x3500)))) x3500))))))

d_OP_analyzeMain_dot___hash_lambda2 :: Curry_Prelude.Curry t104 => Curry_Analysis.C_Analysis t104 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda2 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP__case_4 x1 x2 x3 x5 x6 (Curry_Prelude.d_C_null x6 x3500) x3500) (d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 x2 x4 x5) x3500

nd_OP_analyzeMain_dot___hash_lambda2 :: Curry_Prelude.Curry t104 => Curry_Analysis.C_Analysis t104 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda2 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP__case_4 x1 x2 x3 x5 x6 (Curry_Prelude.d_C_null x6 x3500) x2000 x3500) (wrapNX id (nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 x2 x4 x5)) x2001 x3500)))))

d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x4 x5 x3500 = d_OP__case_3 x1 x2 x3 x4 x5 (Curry_Prelude.d_OP_gt x5 (Curry_Prelude.C_Int 0#) x3500) x3500

d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 :: Curry_Prelude.Curry t104 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_maybe (d_OP__case_2 x1 x3 x2 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Right) x3500) x4 x3500) d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5 x3500

nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 :: Curry_Prelude.Curry t104 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x3 x4 x3000 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2003 = rightSupply x2006
           in (seq x2005 (seq x2003 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_maybe (nd_OP__case_2 x1 x3 x2 x2000 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id Curry_Prelude.C_Right)) x2001 x3500) x4 x2002 x3500))))))) (wrapNX id nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5) x2005 x3500)))))

d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.Curry t104 => Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5 x1 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_C_either Curry_GenericProgInfo.d_C_showProgInfo Curry_Prelude.d_C_id x1 x3500) x3500) x3500) (Curry_Prelude.d_C_return x1 x3500) x3500

nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.Curry t104 => Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t104) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda4_dot___hash_lambda5 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.nd_C_either (wrapNX id Curry_GenericProgInfo.nd_C_showProgInfo) (wrapDX id Curry_Prelude.d_C_id) x1 x2000 x3500) x3500) x3500) (Curry_Prelude.d_C_return x1 x3500) x3500))

d_C_analyzeLocally :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeLocally x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x2 x3500) x3500) x3500) x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply (d_C_lookupRegAnaWorker x1 x3500) x2 x3500) (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500) x3500) x3500

d_C_prepareCombinedAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prepareCombinedAnalysis x1 x2 x3 x4 x3500 = let
     x5 = Curry_Analysis.d_C_baseAnalysisName x1 x3500
      in (d_OP__case_1 x1 x2 x3 x4 x5 (Curry_Analysis.d_C_isCombinedAnalysis x1 x3500) x3500)

nd_C_prepareCombinedAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_prepareCombinedAnalysis x1 x2 x3 x4 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = Curry_Analysis.nd_C_baseAnalysisName x1 x2000 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (nd_OP__case_1 x1 x2 x3 x4 x5 (Curry_Analysis.nd_C_isCombinedAnalysis x1 x2001 x3500) x2002 x3500)))))))))

d_OP_prepareCombinedAnalysis_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_prepareCombinedAnalysis_dot___hash_lambda6 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_runAnalysisWithWorkersNoLoad x1 x2) x3500) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3500) x3500

d_OP__case_1 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_0 x1 x2 x3 x4 x5 (Curry_Analysis.d_C_isSimpleAnalysis x1 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_1 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_0 x1 x2 x3 x4 x5 (Curry_Analysis.nd_C_isSimpleAnalysis x1 x2000 x3500) x2001 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x2 x3500) (d_OP_prepareCombinedAnalysis_dot___hash_lambda6 x5 x4 x2) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_runAnalysisWithWorkersNoLoad x5 x4) x3500) x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_0 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x2 x3500) (wrapDX id (d_OP_prepareCombinedAnalysis_dot___hash_lambda6 x5 x4 x2)) x2000 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO_ (wrapDX id (d_C_runAnalysisWithWorkersNoLoad x5 x4)) x2000 x3500) x3 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) x3 x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_loadCompleteAnalysis x1 x3 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Left) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.C_Left (Curry_GenericProgInfo.d_C_emptyProgInfo x3500)) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x3 x1002 x3500) (d_OP__case_2 x1 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) x3 x3500) x3500) (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_loadCompleteAnalysis x1 x3 x2000 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id Curry_Prelude.C_Left)) x2001 x3500) x2002 x3500))))))) x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.C_Left (Curry_GenericProgInfo.nd_C_emptyProgInfo x2000 x3500)) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x3 x1002 x3000 x3500) (nd_OP__case_2 x1 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (Curry_ServerFunctions.d_C_workerLoop x2 Curry_Prelude.OP_List x1 x3 x4 Curry_Prelude.OP_List x3500) x3500
     Curry_Prelude.C_False -> d_C_analyzeLocally x1 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x4 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x4 x5 x1002 x3500) (d_OP__case_3 x1 x2 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (Curry_ServerFunctions.d_C_workerLoop x2 Curry_Prelude.OP_List x1 x3 x4 Curry_Prelude.OP_List x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (d_C_analyzeLocally x1 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x4 x2000 x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (d_C_prepareCombinedAnalysis x1 x5 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x6 x3500) x3 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Configuration.d_C_numberOfWorkers x3500) (d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda3 x2 x3 x5 x6) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x5 x6 x1002 x3500) (d_OP__case_4 x1 x2 x3 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_gt_gt (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_prepareCombinedAnalysis x1 x5 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x6 x2000 x3500) x3 x2001 x3500)))) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Configuration.d_C_numberOfWorkers x3500) (wrapDX id (d_OP_analyzeMain_dot___hash_lambda2_dot___hash_lambda3 x2 x3 x5 x6)) x2003 x3500) x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x5 x6 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x4 x3 x3500 = case x3 of
     (C_RegAna x5 x6 x7) -> d_OP__case_5 x1 x3 x4 x5 (Curry_Prelude.d_OP_eq_eq x1 x5 x3500) x3500
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x4 x1002 x3500) (d_OP__case_6 x1 x4 x1003 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x4 z x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x4 x1002) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x4 x3 x3000 x3500 = case x3 of
     (HO_C_RegAna x5 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x3 x4 x5 (Curry_Prelude.d_OP_eq_eq x1 x5 x3500) x2000 x3500))
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x4 x1002 x3000 x3500) (nd_OP__case_6 x1 x4 x1003 x3000 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x4 z x3000 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x3
     Curry_Prelude.C_False -> d_C_lookupRegAna x1 x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x3 x4 x5 x1002 x3500) (d_OP__case_5 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x3
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_lookupRegAna x1 x4 x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_5 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
