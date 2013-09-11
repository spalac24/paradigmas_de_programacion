{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_AnalysisCollection (C_RegisteredAnalysis, d_C_registeredAnalysisNames, d_C_functionAnalysisInfos, d_C_lookupRegAnaWorker, nd_C_lookupRegAnaWorker, d_C_runAnalysisWithWorkers, nd_C_runAnalysisWithWorkers, d_C_analyzeMain, nd_C_analyzeMain) where

import Basics
import qualified Curry_Analysis
import qualified Curry_AnalysisDependencies
import qualified Curry_Configuration
import qualified Curry_CurryFiles
import qualified Curry_Demandedness
import qualified Curry_Deterministic
import qualified Curry_GenericProgInfo
import qualified Curry_Groundness
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
     = C_RegAna (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
     | HO_C_RegAna (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Bool (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func (Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat) (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))) (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit))
     | Choice_C_RegisteredAnalysis Cover ID C_RegisteredAnalysis C_RegisteredAnalysis
     | Choices_C_RegisteredAnalysis Cover ID ([C_RegisteredAnalysis])
     | Fail_C_RegisteredAnalysis Cover FailInfo
     | Guard_C_RegisteredAnalysis Cover Constraints C_RegisteredAnalysis

instance Show C_RegisteredAnalysis where
  showsPrec d (Choice_C_RegisteredAnalysis cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_RegisteredAnalysis cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_RegisteredAnalysis cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_RegisteredAnalysis cd info) = showChar '!'
  showsPrec _ (C_RegAna x1 x2 x3 x4 x5) = (showString "(RegAna") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))
  showsPrec _ (HO_C_RegAna x1 x2 x3 x4 x5) = (showString "(RegAna") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . (showChar ')')))))))))))


instance Read C_RegisteredAnalysis where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_RegAna x1 x2 x3 x4 x5,r5) | (_,r0) <- readQualified "AnalysisCollection" "RegAna" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4]) s


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
  generate s c = Choices_C_RegisteredAnalysis c (freeID [5] s) [(HO_C_RegAna (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_RegisteredAnalysis where
  ($!!) cont (C_RegAna x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_RegAna y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (HO_C_RegAna x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_RegAna y1 y2 y3 y4 y5) d cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_RegisteredAnalysis cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_RegisteredAnalysis cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_RegisteredAnalysis cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  ($##) cont (C_RegAna x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (C_RegAna y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (HO_C_RegAna x1 x2 x3 x4 x5) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> cont (HO_C_RegAna y1 y2 y3 y4 y5) d cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_RegisteredAnalysis cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_RegisteredAnalysis cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_RegisteredAnalysis cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  searchNF search cont (C_RegAna x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (C_RegAna y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF search cont (HO_C_RegAna x1 x2 x3 x4 x5) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> cont (HO_C_RegAna y1 y2 y3 y4 y5)) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("AnalysisCollection.RegisteredAnalysis.searchNF: no constructor: " ++ (show x))


instance Unifiable C_RegisteredAnalysis where
  (=.=) (C_RegAna x1 x2 x3 x4 x5) (C_RegAna y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) (HO_C_RegAna x1 x2 x3 x4 x5) (HO_C_RegAna y1 y2 y3 y4 y5) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & (((x5 =:= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_RegAna x1 x2 x3 x4 x5) (C_RegAna y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) (HO_C_RegAna x1 x2 x3 x4 x5) (HO_C_RegAna y1 y2 y3 y4 y5) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & (((x5 =:<= y5) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_RegAna x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind cd i (HO_C_RegAna x3 x4 x5 x6 x7) = ((i :=: (ChooseN 0 5)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (rightID (leftID i)) x5),(bind cd (leftID (rightID i)) x6),(bind cd (rightID (rightID i)) x7)]))
  bind d i (Choice_C_RegisteredAnalysis cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_RegisteredAnalysis cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_RegisteredAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_RegisteredAnalysis cd i _) = error ("AnalysisCollection.RegisteredAnalysis.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_RegisteredAnalysis cd info) = [(Unsolvable info)]
  bind d i (Guard_C_RegisteredAnalysis cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_RegAna x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind cd i (HO_C_RegAna x3 x4 x5 x6 x7) = [(i :=: (ChooseN 0 5)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x5))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x6))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x7)))]
  lazyBind d i (Choice_C_RegisteredAnalysis cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_RegisteredAnalysis cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_RegisteredAnalysis cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_RegisteredAnalysis cd i _) = error ("AnalysisCollection.RegisteredAnalysis.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_RegisteredAnalysis cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_RegisteredAnalysis cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_RegisteredAnalysis where
  (=?=) (Choice_C_RegisteredAnalysis cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_RegisteredAnalysis cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_RegisteredAnalysis cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_RegisteredAnalysis cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_RegisteredAnalysis cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_RegisteredAnalysis cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_RegisteredAnalysis cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  (=?=) (C_RegAna x1 x2 x3 x4 x5) (C_RegAna y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (=?=) (HO_C_RegAna x1 x2 x3 x4 x5) (HO_C_RegAna y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.=?= y5) d) cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_RegisteredAnalysis cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_RegisteredAnalysis cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_RegisteredAnalysis cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_RegisteredAnalysis cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_RegisteredAnalysis cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_RegisteredAnalysis cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_RegisteredAnalysis cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_RegisteredAnalysis cd info) _ _ = failCons cd info
  (<?=) (C_RegAna x1 x2 x3 x4 x5) (C_RegAna y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (HO_C_RegAna x1 x2 x3 x4 x5) (HO_C_RegAna y1 y2 y3 y4 y5) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (((x5 Curry_Prelude.<?= y5) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_registeredAnalysis :: Cover -> ConstStore -> Curry_Prelude.OP_List C_RegisteredAnalysis
d_C_registeredAnalysis x3250 x3500 = Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Deterministic.d_C_overlapAnalysis x3250 x3500) (acceptCs id Curry_Deterministic.d_C_showOverlap) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Deterministic.d_C_nondetAnalysis x3250 x3500) (acceptCs id Curry_Deterministic.d_C_showDet) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_RightLinearity.d_C_rlinAnalysis x3250 x3500) (acceptCs id Curry_RightLinearity.d_C_showRightLinear) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_SolutionCompleteness.d_C_solcompAnalysis x3250 x3500) (acceptCs id Curry_SolutionCompleteness.d_C_showSolComplete) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_TotallyDefined.d_C_patCompAnalysis x3250 x3500) (acceptCs id Curry_TotallyDefined.d_C_showComplete) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_TotallyDefined.d_C_totalAnalysis x3250 x3500) (acceptCs id Curry_TotallyDefined.d_C_showTotally) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Indeterministic.d_C_indetAnalysis x3250 x3500) (acceptCs id Curry_Indeterministic.d_C_showIndet) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Demandedness.d_C_demandAnalysis x3250 x3500) (acceptCs id Curry_Demandedness.d_C_showDemand) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) (Curry_Groundness.d_C_groundAnalysis x3250 x3500) (acceptCs id Curry_Groundness.d_C_showGround) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Groundness.d_C_ndEffectAnalysis x3250 x3500) (acceptCs id Curry_Groundness.d_C_showNDEffect) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_HigherOrder.d_C_hiOrdType x3250 x3500) (acceptCs id Curry_HigherOrder.d_C_showOrder) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_HigherOrder.d_C_hiOrdCons x3250 x3500) (acceptCs id Curry_HigherOrder.d_C_showOrder) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_HigherOrder.d_C_hiOrdFunc x3250 x3500) (acceptCs id Curry_HigherOrder.d_C_showOrder) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_TotallyDefined.d_C_siblingCons x3250 x3500) Curry_TotallyDefined.d_C_showSibling x3250 x3500) Curry_Prelude.OP_List)))))))))))))

nd_C_registeredAnalysis :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List C_RegisteredAnalysis
nd_C_registeredAnalysis x3000 x3250 x3500 = let
     x2054 = x3000
      in (seq x2054 (let
          x2002 = leftSupply x2054
          x2053 = rightSupply x2054
           in (seq x2002 (seq x2053 (Curry_Prelude.OP_Cons (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))) (Curry_Deterministic.nd_C_overlapAnalysis x2000 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Deterministic.d_C_showOverlap)) x2001 x3250 x3500)))) (let
               x2005 = leftSupply x2053
               x2052 = rightSupply x2053
                in (seq x2005 (seq x2052 (Curry_Prelude.OP_Cons (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))) (Curry_Deterministic.nd_C_nondetAnalysis x2003 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Deterministic.d_C_showDet)) x2004 x3250 x3500)))) (let
                    x2008 = leftSupply x2052
                    x2051 = rightSupply x2052
                     in (seq x2008 (seq x2051 (Curry_Prelude.OP_Cons (let
                         x2007 = leftSupply x2008
                         x2006 = rightSupply x2008
                          in (seq x2007 (seq x2006 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_RightLinearity.nd_C_rlinAnalysis x2006 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_RightLinearity.d_C_showRightLinear)) x2007 x3250 x3500)))) (let
                         x2011 = leftSupply x2051
                         x2050 = rightSupply x2051
                          in (seq x2011 (seq x2050 (Curry_Prelude.OP_Cons (let
                              x2010 = leftSupply x2011
                              x2009 = rightSupply x2011
                               in (seq x2010 (seq x2009 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_SolutionCompleteness.nd_C_solcompAnalysis x2009 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_SolutionCompleteness.d_C_showSolComplete)) x2010 x3250 x3500)))) (let
                              x2014 = leftSupply x2050
                              x2049 = rightSupply x2050
                               in (seq x2014 (seq x2049 (Curry_Prelude.OP_Cons (let
                                   x2013 = leftSupply x2014
                                   x2012 = rightSupply x2014
                                    in (seq x2013 (seq x2012 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_TotallyDefined.nd_C_patCompAnalysis x2012 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_TotallyDefined.d_C_showComplete)) x2013 x3250 x3500)))) (let
                                   x2017 = leftSupply x2049
                                   x2048 = rightSupply x2049
                                    in (seq x2017 (seq x2048 (Curry_Prelude.OP_Cons (let
                                        x2016 = leftSupply x2017
                                        x2015 = rightSupply x2017
                                         in (seq x2016 (seq x2015 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_TotallyDefined.nd_C_totalAnalysis x2015 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_TotallyDefined.d_C_showTotally)) x2016 x3250 x3500)))) (let
                                        x2020 = leftSupply x2048
                                        x2047 = rightSupply x2048
                                         in (seq x2020 (seq x2047 (Curry_Prelude.OP_Cons (let
                                             x2019 = leftSupply x2020
                                             x2018 = rightSupply x2020
                                              in (seq x2019 (seq x2018 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Indeterministic.nd_C_indetAnalysis x2018 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Indeterministic.d_C_showIndet)) x2019 x3250 x3500)))) (let
                                             x2023 = leftSupply x2047
                                             x2046 = rightSupply x2047
                                              in (seq x2023 (seq x2046 (Curry_Prelude.OP_Cons (let
                                                  x2022 = leftSupply x2023
                                                  x2021 = rightSupply x2023
                                                   in (seq x2022 (seq x2021 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))) (Curry_Demandedness.nd_C_demandAnalysis x2021 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Demandedness.d_C_showDemand)) x2022 x3250 x3500)))) (let
                                                  x2026 = leftSupply x2046
                                                  x2045 = rightSupply x2046
                                                   in (seq x2026 (seq x2045 (Curry_Prelude.OP_Cons (let
                                                       x2025 = leftSupply x2026
                                                       x2024 = rightSupply x2026
                                                        in (seq x2025 (seq x2024 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) (Curry_Groundness.nd_C_groundAnalysis x2024 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Groundness.d_C_showGround)) x2025 x3250 x3500)))) (let
                                                       x2029 = leftSupply x2045
                                                       x2044 = rightSupply x2045
                                                        in (seq x2029 (seq x2044 (Curry_Prelude.OP_Cons (let
                                                            x2028 = leftSupply x2029
                                                            x2027 = rightSupply x2029
                                                             in (seq x2028 (seq x2027 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Groundness.nd_C_ndEffectAnalysis x2027 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_Groundness.d_C_showNDEffect)) x2028 x3250 x3500)))) (let
                                                            x2032 = leftSupply x2044
                                                            x2043 = rightSupply x2044
                                                             in (seq x2032 (seq x2043 (Curry_Prelude.OP_Cons (let
                                                                 x2031 = leftSupply x2032
                                                                 x2030 = rightSupply x2032
                                                                  in (seq x2031 (seq x2030 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_HigherOrder.nd_C_hiOrdType x2030 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_HigherOrder.d_C_showOrder)) x2031 x3250 x3500)))) (let
                                                                 x2035 = leftSupply x2043
                                                                 x2042 = rightSupply x2043
                                                                  in (seq x2035 (seq x2042 (Curry_Prelude.OP_Cons (let
                                                                      x2034 = leftSupply x2035
                                                                      x2033 = rightSupply x2035
                                                                       in (seq x2034 (seq x2033 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_HigherOrder.nd_C_hiOrdCons x2033 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_HigherOrder.d_C_showOrder)) x2034 x3250 x3500)))) (let
                                                                      x2038 = leftSupply x2042
                                                                      x2041 = rightSupply x2042
                                                                       in (seq x2038 (seq x2041 (Curry_Prelude.OP_Cons (let
                                                                           x2037 = leftSupply x2038
                                                                           x2036 = rightSupply x2038
                                                                            in (seq x2037 (seq x2036 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) (Curry_HigherOrder.nd_C_hiOrdFunc x2036 x3250 x3500) (wrapDX (wrapDX id) (acceptCs id Curry_HigherOrder.d_C_showOrder)) x2037 x3250 x3500)))) (Curry_Prelude.OP_Cons (let
                                                                           x2040 = leftSupply x2041
                                                                           x2039 = rightSupply x2041
                                                                            in (seq x2040 (seq x2039 (nd_C_cassAnalysis (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))) (Curry_TotallyDefined.nd_C_siblingCons x2039 x3250 x3500) (wrapNX id Curry_TotallyDefined.nd_C_showSibling) x2040 x3250 x3500)))) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))

d_C_cassAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_Analysis t0 -> (Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> C_RegisteredAnalysis
d_C_cassAnalysis x1 x2 x3 x3250 x3500 = C_RegAna (Curry_Analysis.d_C_analysisName x2 x3250 x3500) (Curry_Analysis.d_C_isFunctionAnalysis x2 x3250 x3500) x1 (acceptCs (acceptCs id) (d_C_analyzeAsString x2 x3)) (Curry_WorkerFunctions.d_C_analysisClient x2)

nd_C_cassAnalysis :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_Analysis t0 -> Func Curry_Analysis.C_AOutFormat (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> C_RegisteredAnalysis
nd_C_cassAnalysis x1 x2 x3 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (HO_C_RegAna (Curry_Analysis.nd_C_analysisName x2 x2000 x3250 x3500) (Curry_Analysis.nd_C_isFunctionAnalysis x2 x2001 x3250 x3500) x1 (wrapDX (wrapDX (wrapNX id)) (acceptCs (acceptCs id) (nd_C_analyzeAsString x2 x3))) (wrapNX id (Curry_WorkerFunctions.nd_C_analysisClient x2)))))))

d_C_regAnaName :: C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_regAnaName x1 x3250 x3500 = case x1 of
     (C_RegAna x2 x3 x4 x5 x6) -> x2
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaName x1002 x3250 x3500) (d_C_regAnaName x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaName z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaName x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_regAnaName :: C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_regAnaName x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4 x5 x6) -> x2
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaName x1002 x3000 x3250 x3500) (nd_C_regAnaName x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaName z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaName x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_regAnaServer :: C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_regAnaServer x1 x3250 x3500 = case x1 of
     (C_RegAna x2 x3 x4 x5 x6) -> x5
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaServer x1002 x3250 x3500) (d_C_regAnaServer x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaServer z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaServer x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_regAnaServer :: C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func (Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat) (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_C_regAnaServer x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4 x5 x6) -> x5
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaServer x1002 x3000 x3250 x3500) (nd_C_regAnaServer x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaServer z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaServer x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_regAnaWorker :: C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_regAnaWorker x1 x3250 x3500 = case x1 of
     (C_RegAna x2 x3 x4 x5 x6) -> x6
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_regAnaWorker x1002 x3250 x3500) (d_C_regAnaWorker x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_regAnaWorker z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_regAnaWorker x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_regAnaWorker :: C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_regAnaWorker x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4 x5 x6) -> x6
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_regAnaWorker x1002 x3000 x3250 x3500) (nd_C_regAnaWorker x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_regAnaWorker z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_regAnaWorker x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_registeredAnalysisNames :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_registeredAnalysisNames x3250 x3500 = Curry_Prelude.d_C_map d_C_regAnaName (d_C_registeredAnalysis x3250 x3500) x3250 x3500

d_C_functionAnalysisInfos :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_functionAnalysisInfos x3250 x3500 = Curry_Prelude.d_C_map d_OP_functionAnalysisInfos_dot___hash_lambda1 (Curry_Prelude.d_C_filter d_OP_functionAnalysisInfos_dot___hash_lambda2 (d_C_registeredAnalysis x3250 x3500) x3250 x3500) x3250 x3500

d_OP_functionAnalysisInfos_dot___hash_lambda1 :: C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_functionAnalysisInfos_dot___hash_lambda1 x1 x3250 x3500 = case x1 of
     (C_RegAna x2 x3 x4 x5 x6) -> Curry_Prelude.OP_Tuple2 x2 x4
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_functionAnalysisInfos_dot___hash_lambda1 x1002 x3250 x3500) (d_OP_functionAnalysisInfos_dot___hash_lambda1 x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_functionAnalysisInfos_dot___hash_lambda1 z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_functionAnalysisInfos_dot___hash_lambda1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_functionAnalysisInfos_dot___hash_lambda1 :: C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_OP_functionAnalysisInfos_dot___hash_lambda1 x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4 x5 x6) -> Curry_Prelude.OP_Tuple2 x2 x4
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_functionAnalysisInfos_dot___hash_lambda1 x1002 x3000 x3250 x3500) (nd_OP_functionAnalysisInfos_dot___hash_lambda1 x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_functionAnalysisInfos_dot___hash_lambda1 z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_functionAnalysisInfos_dot___hash_lambda1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_functionAnalysisInfos_dot___hash_lambda2 :: C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_functionAnalysisInfos_dot___hash_lambda2 x1 x3250 x3500 = case x1 of
     (C_RegAna x2 x3 x4 x5 x6) -> x3
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_functionAnalysisInfos_dot___hash_lambda2 x1002 x3250 x3500) (d_OP_functionAnalysisInfos_dot___hash_lambda2 x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_functionAnalysisInfos_dot___hash_lambda2 z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_functionAnalysisInfos_dot___hash_lambda2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_functionAnalysisInfos_dot___hash_lambda2 :: C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_functionAnalysisInfos_dot___hash_lambda2 x1 x3000 x3250 x3500 = case x1 of
     (HO_C_RegAna x2 x3 x4 x5 x6) -> x3
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_functionAnalysisInfos_dot___hash_lambda2 x1002 x3000 x3250 x3500) (nd_OP_functionAnalysisInfos_dot___hash_lambda2 x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_functionAnalysisInfos_dot___hash_lambda2 z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_functionAnalysisInfos_dot___hash_lambda2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lookupRegAna :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
d_C_lookupRegAna x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_6 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_lookupRegAna x1 x1002 x3250 x3500) (d_C_lookupRegAna x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_lookupRegAna x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_lookupRegAna x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_lookupRegAna :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
nd_C_lookupRegAna x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x4 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_lookupRegAna x1 x1002 x3000 x3250 x3500) (nd_C_lookupRegAna x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_lookupRegAna x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_lookupRegAna x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_lookupRegAnaServer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_lookupRegAnaServer x1 x3250 x3500 = Curry_Prelude.d_C_maybe (acceptCs (acceptCs id) (d_OP_lookupRegAnaServer_dot___hash_lambda3 x1)) d_C_regAnaServer (d_C_lookupRegAna x1 (d_C_registeredAnalysis x3250 x3500) x3250 x3500) x3250 x3500

nd_C_lookupRegAnaServer :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List Curry_IO.C_Handle) (Func (Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat) (Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))))
nd_C_lookupRegAnaServer x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (wrapDX (wrapDX (wrapDX id)) (acceptCs (acceptCs id) (d_OP_lookupRegAnaServer_dot___hash_lambda3 x1))) (wrapNX id nd_C_regAnaServer) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_lookupRegAna x1 (nd_C_registeredAnalysis x2000 x3250 x3500) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_OP_lookupRegAnaServer_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupRegAnaServer_dot___hash_lambda3 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.C_Right (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))) x1 x3250 x3500)) x3250 x3500

d_C_lookupRegAnaWorker :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_lookupRegAnaWorker x1 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_const (Curry_Prelude.d_C_done x3250 x3500)) d_C_regAnaWorker (d_C_lookupRegAna x1 (d_C_registeredAnalysis x3250 x3500) x3250 x3500) x3250 x3500

nd_C_lookupRegAnaWorker :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_lookupRegAnaWorker x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (wrapDX id (Curry_Prelude.d_C_const (Curry_Prelude.d_C_done x3250 x3500))) (wrapNX id nd_C_regAnaWorker) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_lookupRegAna x1 (nd_C_registeredAnalysis x2000 x3250 x3500) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

d_C_debugMessage :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_debugMessage x1 x2 x3250 x3500 = Curry_Configuration.d_C_debugMessageLevel x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) x2 x3250 x3500) x3250 x3500

d_C_runAnalysisWithWorkers :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_AOutFormat -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_runAnalysisWithWorkers x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupRegAnaServer x1 x3250 x3500) x4 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.C_Just x2) x3250 x3500

nd_C_runAnalysisWithWorkers :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_AOutFormat -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_runAnalysisWithWorkers x1 x2 x3 x4 x3000 x3250 x3500 = let
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
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (nd_C_lookupRegAnaServer x1 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x3 x2003 x3250 x3500)))) (Curry_Prelude.C_Just x2) x2005 x3250 x3500)))))

d_C_runAnalysisWithWorkersNoLoad :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_runAnalysisWithWorkersNoLoad x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (d_C_lookupRegAnaServer x1 x3250 x3500) x3 x3250 x3500) x2 x3250 x3500) Curry_Prelude.C_Nothing x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500

d_C_analyzeAsString :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> (Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeAsString x1 x2 x3 x4 x5 x3250 x3500 = let
     x6 = Curry_Prelude.d_C_maybe Curry_Analysis.C_AText Curry_Prelude.d_C_id x5 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (d_C_analyzeMain x1 x3 x4 (Curry_Prelude.d_OP_slash_eq x5 Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_Prelude.d_C_either (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Left) (Curry_GenericProgInfo.d_C_mapProgInfo (Curry_Prelude.d_C_apply x2 x6 x3250 x3500)) x3250 x3500) (acceptCs id Curry_Prelude.C_Right)) x3250 x3500) x3250 x3500)

nd_C_analyzeAsString :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Func Curry_Analysis.C_AOutFormat (Func t0 (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Maybe Curry_Analysis.C_AOutFormat -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_analyzeAsString x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2010 = x3000
      in (seq x2010 (let
          x2000 = leftSupply x2010
          x2008 = rightSupply x2010
           in (seq x2000 (seq x2008 (let
               x6 = Curry_Prelude.nd_C_maybe Curry_Analysis.C_AText (wrapDX id Curry_Prelude.d_C_id) x5 x2000 x3250 x3500
                in (let
                    x2007 = leftSupply x2008
                    x2009 = rightSupply x2008
                     in (seq x2007 (seq x2009 (let
                         x2001 = leftSupply x2009
                         x2006 = rightSupply x2009
                          in (seq x2001 (seq x2006 (Curry_Prelude.nd_OP_gt_gt_eq (nd_C_analyzeMain x1 x3 x4 (Curry_Prelude.d_OP_slash_eq x5 Curry_Prelude.C_Nothing x3250 x3500) x2001 x3250 x3500) (let
                              x2005 = leftSupply x2006
                              x2004 = rightSupply x2006
                               in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapNX id (Curry_Prelude.nd_C_either (let
                                   x2003 = leftSupply x2004
                                   x2002 = rightSupply x2004
                                    in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_dot (wrapDX id (acceptCs id Curry_Prelude.C_Left)) (wrapNX id (Curry_GenericProgInfo.nd_C_mapProgInfo (Curry_Prelude.nd_C_apply x2 x6 x2002 x3250 x3500))) x2003 x3250 x3500)))) (wrapDX id (acceptCs id Curry_Prelude.C_Right)))) x2005 x3250 x3500)))) x2007 x3250 x3500))))))))))))

d_C_analyzeMain :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeMain x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Analysis.d_C_analysisName x1 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisDependencies.d_C_getModulesToAnalyze x1 x2 x3250 x3500) (d_OP_analyzeMain_dot___hash_lambda4 x1 x5 x3 x4 x2) x3250 x3500) x3250 x3500)

nd_C_analyzeMain :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_analyzeMain x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = Curry_Analysis.nd_C_analysisName x1 x2000 x3250 x3500
                in (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AnalysisDependencies.nd_C_getModulesToAnalyze x1 x2 x2001 x3250 x3500) (wrapNX id (nd_OP_analyzeMain_dot___hash_lambda4 x1 x5 x3 x4 x2)) x2002 x3250 x3500)))) x3250 x3500))))))

d_OP_analyzeMain_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP__case_4 x6 x5 x3 x2 x1 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) (d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 x2 x4 x5) x3250 x3500

nd_OP_analyzeMain_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (nd_OP__case_4 x6 x5 x3 x2 x1 (Curry_Prelude.d_C_null x6 x3250 x3500) x2000 x3250 x3500) (wrapNX id (nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 x2 x4 x5)) x2001 x3250 x3500)))))

d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_3 x5 x4 x1 x3 x2 (Curry_Prelude.d_OP_gt x5 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_maybe (d_OP__case_2 x3 x1 x2 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Right) x3250 x3500) x4 x3250 x3500) d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7 x3250 x3500

nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6 x1 x2 x3 x4 x3000 x3250 x3500 = let
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
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_maybe (nd_OP__case_2 x3 x1 x2 x2000 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id Curry_Prelude.C_Right)) x2001 x3250 x3500) x4 x2002 x3250 x3500))))))) (wrapNX id nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7) x2005 x3250 x3500)))))

d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.d_C_either Curry_GenericProgInfo.d_C_showProgInfo Curry_Prelude.d_C_id x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500

nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda6_dot___hash_lambda7 x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 4#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.nd_C_either (wrapNX id Curry_GenericProgInfo.nd_C_showProgInfo) (wrapDX id Curry_Prelude.d_C_id) x1 x2000 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500))

d_C_analyzeLocally :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_analyzeLocally x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_apply (d_C_lookupRegAnaWorker x1 x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500) x3250 x3500

d_C_prepareCombinedAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prepareCombinedAnalysis x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Analysis.d_C_baseAnalysisName x1 x3250 x3500
      in (d_OP__case_1 x1 x3 x4 x5 x2 (Curry_Analysis.d_C_isCombinedAnalysis x1 x3250 x3500) x3250 x3500)

nd_C_prepareCombinedAnalysis :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_prepareCombinedAnalysis x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (let
               x5 = Curry_Analysis.nd_C_baseAnalysisName x1 x2000 x3250 x3500
                in (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (nd_OP__case_1 x1 x3 x4 x5 x2 (Curry_Analysis.nd_C_isCombinedAnalysis x1 x2001 x3250 x3500) x2002 x3250 x3500)))))))))

d_OP_prepareCombinedAnalysis_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_prepareCombinedAnalysis_dot___hash_lambda8 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_runAnalysisWithWorkersNoLoad x1 x2) x3250 x3500) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3250 x3500) x3250 x3500

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_1 x1 x3 x4 x5 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP__case_0 x1 x3 x4 x5 x2 (Curry_Analysis.d_C_isSimpleAnalysis x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x3 x4 x5 x2 x1002 x3250 x3500) (d_OP__case_1 x1 x3 x4 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x3 x4 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x3 x4 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_1 x1 x3 x4 x5 x2 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_0 x1 x3 x4 x5 x2 (Curry_Analysis.nd_C_isSimpleAnalysis x1 x2000 x3250 x3500) x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x3 x4 x5 x2 x1002 x3000 x3250 x3500) (nd_OP__case_1 x1 x3 x4 x5 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x3 x4 x5 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x3 x4 x5 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_0 x1 x3 x4 x5 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x2 x3250 x3500) (d_OP_prepareCombinedAnalysis_dot___hash_lambda8 x5 x4 x2) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_runAnalysisWithWorkersNoLoad x5 x4) x3250 x3500) x3 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x3 x4 x5 x2 x1002 x3250 x3500) (d_OP__case_0 x1 x3 x4 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x3 x4 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x3 x4 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Analysis.C_Analysis t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_0 x1 x3 x4 x5 x2 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CurryFiles.d_C_getImports x2 x3250 x3500) (wrapDX id (d_OP_prepareCombinedAnalysis_dot___hash_lambda8 x5 x4 x2)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO_ (wrapDX id (d_C_runAnalysisWithWorkersNoLoad x5 x4)) x2000 x3250 x3500) x3 x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x3 x4 x5 x2 x1002 x3000 x3250 x3500) (nd_OP__case_0 x1 x3 x4 x5 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x3 x4 x5 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x3 x4 x5 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x3 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_LoadAnalysis.d_C_loadCompleteAnalysis x1 x3 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Left) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.C_Left (Curry_GenericProgInfo.d_C_emptyProgInfo x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x3 x1 x1002 x3250 x3500) (d_OP__case_2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Either (Curry_GenericProgInfo.C_ProgInfo t0) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_2 x3 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) x3 x3250 x3500) x3250 x3500) (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_LoadAnalysis.nd_C_loadCompleteAnalysis x1 x3 x2000 x3250 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id Curry_Prelude.C_Left)) x2001 x3250 x3500) x2002 x3250 x3500))))))) x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_C_return (Curry_Prelude.C_Left (Curry_GenericProgInfo.nd_C_emptyProgInfo x2000 x3250 x3500)) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_2 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x5 x4 x1 x3 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))))))))))))) x3250 x3500) (Curry_ServerFunctions.d_C_masterLoop x2 Curry_Prelude.OP_List x1 x3 x4 Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_analyzeLocally x1 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x5 x4 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_3 x5 x4 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x5 x4 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x5 x4 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_Analysis t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x6 x5 x3 x2 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 1#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_length x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_prepareCombinedAnalysis x1 x5 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x6 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Configuration.d_C_numberOfWorkers x3250 x3500) (d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda5 x2 x3 x5 x6) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x6 x5 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_4 x6 x5 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x6 x5 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x6 x5 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Analysis.C_Analysis t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_4 x6 x5 x3 x2 x1 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     Curry_Prelude.C_False -> let
          x2004 = x3000
           in (seq x2004 (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 1#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show (Curry_Prelude.d_C_length x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (let
               x2002 = leftSupply x2004
               x2003 = rightSupply x2004
                in (seq x2002 (seq x2003 (Curry_Prelude.d_OP_gt_gt (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (nd_C_prepareCombinedAnalysis x1 x5 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x6 x2000 x3250 x3500) x3 x2001 x3250 x3500)))) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Configuration.d_C_numberOfWorkers x3250 x3500) (wrapDX id (d_OP_analyzeMain_dot___hash_lambda4_dot___hash_lambda5 x2 x3 x5 x6)) x2003 x3250 x3500) x3250 x3500)))) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x6 x5 x3 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_4 x6 x5 x3 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x6 x5 x3 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x6 x5 x3 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> C_RegisteredAnalysis -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
d_OP__case_6 x1 x4 x3 x3250 x3500 = case x3 of
     (C_RegAna x5 x6 x7 x8 x9) -> d_OP__case_5 x5 x1 x4 x3 (Curry_Prelude.d_OP_eq_eq x1 x5 x3250 x3500) x3250 x3500
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x4 x1002 x3250 x3500) (d_OP__case_6 x1 x4 x1003 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x4 z x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> C_RegisteredAnalysis -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
nd_OP__case_6 x1 x4 x3 x3000 x3250 x3500 = case x3 of
     (HO_C_RegAna x5 x6 x7 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x5 x1 x4 x3 (Curry_Prelude.d_OP_eq_eq x1 x5 x3250 x3500) x2000 x3250 x3500))
     (Choice_C_RegisteredAnalysis x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_6 x1 x4 x1003 x3000 x3250 x3500)
     (Choices_C_RegisteredAnalysis x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x4 z x3000 x3250 x3500) x1002
     (Guard_C_RegisteredAnalysis x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Fail_C_RegisteredAnalysis x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> C_RegisteredAnalysis -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
d_OP__case_5 x5 x1 x4 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x3
     Curry_Prelude.C_False -> d_C_lookupRegAna x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x1 x4 x3 x1002 x3250 x3500) (d_OP__case_5 x5 x1 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 x1 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x1 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_RegisteredAnalysis -> C_RegisteredAnalysis -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe C_RegisteredAnalysis
nd_OP__case_5 x5 x1 x4 x3 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just x3
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_C_lookupRegAna x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x5 x1 x4 x3 x1002 x3000 x3250 x3500) (nd_OP__case_5 x5 x1 x4 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x5 x1 x4 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x5 x1 x4 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
