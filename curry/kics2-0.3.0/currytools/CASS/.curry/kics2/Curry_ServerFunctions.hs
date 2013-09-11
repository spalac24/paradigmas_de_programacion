{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ServerFunctions (C_WorkerMessage (..), d_C_debugMessage, d_C_masterLoop, d_C_deleteIndex) where

import Basics
import qualified Curry_AnalysisDependencies
import qualified Curry_Configuration
import qualified Curry_IO
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_FlatCurry
import qualified Curry_FlatCurryGoodies
import qualified Curry_Socket
import qualified Curry_System
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_Maybe
import qualified Curry_List
import qualified Curry_Time
import qualified Curry_Analysis
import qualified Curry_GenericProgInfo
import qualified Curry_XML
data C_WorkerMessage
     = C_Task (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_ChangePath (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_StopWorker
     | Choice_C_WorkerMessage Cover ID C_WorkerMessage C_WorkerMessage
     | Choices_C_WorkerMessage Cover ID ([C_WorkerMessage])
     | Fail_C_WorkerMessage Cover FailInfo
     | Guard_C_WorkerMessage Cover Constraints C_WorkerMessage

instance Show C_WorkerMessage where
  showsPrec d (Choice_C_WorkerMessage cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_WorkerMessage cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_WorkerMessage cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_WorkerMessage cd info) = showChar '!'
  showsPrec _ (C_Task x1 x2) = (showString "(Task") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))
  showsPrec _ (C_ChangePath x1) = (showString "(ChangePath") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ C_StopWorker = showString "StopWorker"


instance Read C_WorkerMessage where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Task x1 x2,r2) | (_,r0) <- readQualified "ServerFunctions" "Task" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s) ++ ((readParen (d > 10) (\r -> [ (C_ChangePath x1,r1) | (_,r0) <- readQualified "ServerFunctions" "ChangePath" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen False (\r -> [ (C_StopWorker,r0) | (_,r0) <- readQualified "ServerFunctions" "StopWorker" r]) s))


instance NonDet C_WorkerMessage where
  choiceCons = Choice_C_WorkerMessage
  choicesCons = Choices_C_WorkerMessage
  failCons = Fail_C_WorkerMessage
  guardCons = Guard_C_WorkerMessage
  try (Choice_C_WorkerMessage cd i x y) = tryChoice cd i x y
  try (Choices_C_WorkerMessage cd i xs) = tryChoices cd i xs
  try (Fail_C_WorkerMessage cd info) = Fail cd info
  try (Guard_C_WorkerMessage cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_WorkerMessage cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_WorkerMessage cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_WorkerMessage cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_WorkerMessage cd i _) = error ("ServerFunctions.WorkerMessage.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_WorkerMessage cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_WorkerMessage cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_WorkerMessage where
  generate s c = Choices_C_WorkerMessage c (freeID [2,1,0] s) [(C_Task (generate (leftSupply s) c) (generate (rightSupply s) c)),(C_ChangePath (generate (leftSupply s) c)),C_StopWorker]


instance NormalForm C_WorkerMessage where
  ($!!) cont (C_Task x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Task y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (C_ChangePath x1) d cs = (((\y1 d cs -> cont (C_ChangePath y1) d cs) $!! x1) d) cs
  ($!!) cont C_StopWorker d cs = cont C_StopWorker d cs
  ($!!) cont (Choice_C_WorkerMessage cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_WorkerMessage cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_WorkerMessage cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_WorkerMessage cd info) _ _ = failCons cd info
  ($##) cont (C_Task x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Task y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (C_ChangePath x1) d cs = (((\y1 d cs -> cont (C_ChangePath y1) d cs) $## x1) d) cs
  ($##) cont C_StopWorker d cs = cont C_StopWorker d cs
  ($##) cont (Choice_C_WorkerMessage cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_WorkerMessage cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_WorkerMessage cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_WorkerMessage cd info) _ _ = failCons cd info
  searchNF search cont (C_Task x1 x2) = search (\y1 -> search (\y2 -> cont (C_Task y1 y2)) x2) x1
  searchNF search cont (C_ChangePath x1) = search (\y1 -> cont (C_ChangePath y1)) x1
  searchNF _ cont C_StopWorker = cont C_StopWorker
  searchNF _ _ x = error ("ServerFunctions.WorkerMessage.searchNF: no constructor: " ++ (show x))


instance Unifiable C_WorkerMessage where
  (=.=) (C_Task x1 x2) (C_Task y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) (C_ChangePath x1) (C_ChangePath y1) d cs = ((x1 =:= y1) d) cs
  (=.=) C_StopWorker C_StopWorker d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_Task x1 x2) (C_Task y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) (C_ChangePath x1) (C_ChangePath y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) C_StopWorker C_StopWorker d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_Task x3 x4) = ((i :=: (ChooseN 0 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind cd i (C_ChangePath x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i C_StopWorker = ((i :=: (ChooseN 2 0)):(concat []))
  bind d i (Choice_C_WorkerMessage cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_WorkerMessage cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_WorkerMessage cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_WorkerMessage cd i _) = error ("ServerFunctions.WorkerMessage.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_WorkerMessage cd info) = [(Unsolvable info)]
  bind d i (Guard_C_WorkerMessage cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_Task x3 x4) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind cd i (C_ChangePath x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i C_StopWorker = [(i :=: (ChooseN 2 0))]
  lazyBind d i (Choice_C_WorkerMessage cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_WorkerMessage cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_WorkerMessage cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_WorkerMessage cd i _) = error ("ServerFunctions.WorkerMessage.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_WorkerMessage cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_WorkerMessage cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_WorkerMessage where
  (=?=) (Choice_C_WorkerMessage cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_WorkerMessage cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_WorkerMessage cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_WorkerMessage cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_WorkerMessage cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_WorkerMessage cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_WorkerMessage cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_WorkerMessage cd info) _ _ = failCons cd info
  (=?=) (C_Task x1 x2) (C_Task y1 y2) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) (C_ChangePath x1) (C_ChangePath y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) C_StopWorker C_StopWorker d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_WorkerMessage cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_WorkerMessage cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_WorkerMessage cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_WorkerMessage cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_WorkerMessage cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_WorkerMessage cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_WorkerMessage cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_WorkerMessage cd info) _ _ = failCons cd info
  (<?=) (C_Task x1 x2) (C_Task y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) (C_Task _ _) (C_ChangePath _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Task _ _) C_StopWorker _ _ = Curry_Prelude.C_True
  (<?=) (C_ChangePath x1) (C_ChangePath y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_ChangePath _) C_StopWorker _ _ = Curry_Prelude.C_True
  (<?=) C_StopWorker C_StopWorker d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_debugMessage :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_debugMessage x1 x2 x3250 x3500 = Curry_Configuration.d_C_debugMessageLevel x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))) x2 x3250 x3500) x3250 x3500

d_C_masterLoop :: Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_masterLoop x1 x2 x3 x4 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.OP_List -> d_OP__case_10 x4 x3 x2 x1 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x13 x14) -> d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 x1 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_masterLoop x1 x2 x3 x4 x5 x1002 x3250 x3500) (d_C_masterLoop x1 x2 x3 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_masterLoop x1 x2 x3 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_masterLoop x1 x2 x3 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_masterLoop_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda1 x1 x2 x3 x4 x3250 x3500 = d_OP__case_6 x4 x3 x1 x2 (Curry_Prelude.d_OP_slash_eq x4 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))) x3 x3250 x3500) x3250 x3500) (let
     x4 = Curry_ReadShowTerm.d_C_readQTerm x3 x3250 x3500
     x5 = d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 x4 x3250 x3500
     x6 = d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 x4 x3250 x3500
      in (d_OP__case_5 x2 x6 x5 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x1 x5 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x6 x2 x3250 x3500) x3250 x3500) x3250 x3500)) x3250 x3500

d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 :: C_WorkerMessage -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 x1 x3250 x3500 = case x1 of
     (C_Task x2 x3) -> x2
     (Choice_C_WorkerMessage x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 x1002 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 x1003 x3250 x3500)
     (Choices_C_WorkerMessage x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 z x3250 x3500) x1002
     (Guard_C_WorkerMessage x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP2_hash_ananame2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_WorkerMessage x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 :: C_WorkerMessage -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 x1 x3250 x3500 = case x1 of
     (C_Task x2 x3) -> x3
     (Choice_C_WorkerMessage x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 x1002 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 x1003 x3250 x3500)
     (Choices_C_WorkerMessage x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 z x3250 x3500) x1002
     (Guard_C_WorkerMessage x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_moduleName2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_WorkerMessage x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_masterLoop_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda3 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 (Curry_Prelude.d_OP_lt x7 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))) x9 x3250 x3500) x3250 x3500) (let
     x10 = Curry_ReadShowTerm.d_C_readQTerm x9 x3250 x3500
     x11 = d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 x10 x3250 x3500
     x12 = d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 x10 x3250 x3500
      in (d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 (Curry_Prelude.d_OP_eq_eq x1 x11 x3250 x3500) x3250 x3500)) x3250 x3500

d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 :: C_WorkerMessage -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 x1 x3250 x3500 = case x1 of
     (C_Task x2 x3) -> x2
     (Choice_C_WorkerMessage x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 x1002 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 x1003 x3250 x3500)
     (Choices_C_WorkerMessage x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 z x3250 x3500) x1002
     (Guard_C_WorkerMessage x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP5_hash_ananame2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_WorkerMessage x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 :: C_WorkerMessage -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 x1 x3250 x3500 = case x1 of
     (C_Task x2 x3) -> x3
     (Choice_C_WorkerMessage x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 x1002 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 x1003 x3250 x3500)
     (Choices_C_WorkerMessage x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 z x3250 x3500) x1002
     (Guard_C_WorkerMessage x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4_dot___hash_selFP6_hash_moduleName2 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_WorkerMessage x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_masterLoop_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda5 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 (Curry_Prelude.d_OP_lt x7 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x7 x8 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))) x8 x3250 x3500) x3250 x3500) (let
     x9 = Curry_ReadShowTerm.d_C_readQTerm x8 x3250 x3500
     x10 = d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule x9 x3250 x3500
     x11 = Curry_ReadShowTerm.d_C_showQTerm (C_Task x1 x5) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x3 x11 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hFlush x3 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x11 x3250 x3500) x3250 x3500) (let
          x12 = Curry_AnalysisDependencies.d_C_reduceDependencies x6 (Curry_Prelude.OP_Cons x10 Curry_Prelude.OP_List) x3250 x3500
           in (d_C_masterLoop Curry_Prelude.OP_List x2 x1 x4 x12 x7 x3250 x3500)) x3250 x3500) x3250 x3500) x3250 x3500)) x3250 x3500

d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule :: C_WorkerMessage -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule x1 x3250 x3500 = case x1 of
     (C_Task x2 x3) -> x3
     (Choice_C_WorkerMessage x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule x1002 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule x1003 x3250 x3500)
     (Choices_C_WorkerMessage x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule z x3250 x3500) x1002
     (Guard_C_WorkerMessage x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6_dot___hash_selFP8_hash_finishedmodule x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_WorkerMessage x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deleteIndex :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_deleteIndex x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_1 x1 x4 x3 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteIndex x1 x1002 x3250 x3500) (d_C_deleteIndex x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteIndex x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteIndex x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_1 x1 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_0 x4 x1 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x4 x3 x1002 x3250 x3500) (d_OP__case_1 x1 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_0 x4 x1 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x3 (d_C_deleteIndex (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x4 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x4 x1 x3 x1002 x3250 x3500) (d_OP__case_0 x4 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x4 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x4 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500
     Curry_Prelude.C_False -> let
          x8 = Curry_Prelude.d_OP_bang_bang x2 x7 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x8 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x8 x3 x4 x5 x6) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x7 x2 x6 x5 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x13 = Curry_AnalysisDependencies.d_C_reduceDependencies x7 (Curry_Prelude.OP_Cons x12 Curry_Prelude.OP_List) x3250 x3500
          x14 = d_C_deleteIndex x5 x2 x3250 x3500
           in (d_C_masterLoop (Curry_Prelude.OP_Cons x3 x4) x14 x1 x6 x13 x8 x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 x1002 x3250 x3500) (d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x11 x1 x2 x5 x12 x7 x8 x6 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500
     Curry_Prelude.C_False -> let
          x8 = Curry_Prelude.d_OP_bang_bang x2 x7 x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x8 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x8 x3 x7 x4 x5 x6) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 x1002 x3250 x3500) (d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x7 x2 x6 x5 x4 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_5 x2 x6 x5 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x6 x5 x1 x1002 x3250 x3500) (d_OP__case_5 x2 x6 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x6 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x6 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_6 x4 x3 x1 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x2 x3250 x3500) (d_OP_masterLoop_dot___hash_lambda1_dot___hash_lambda2 x1 x3) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x4 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_6 x4 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x4 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x4 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x15 x16) -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (let
          x17 = Curry_ReadShowTerm.d_C_showQTerm (C_Task x3 x13) x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x15 x17 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hFlush x15 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x17 x3250 x3500) x3250 x3500) (d_C_masterLoop x16 (Curry_Prelude.OP_Cons x15 x2) x3 x4 x5 x14 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)) x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_debugMessage (Curry_Prelude.C_Int 2#)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x6 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hWaitForInputs x2 (Curry_Configuration.d_C_waitTime x3250 x3500) x3250 x3500) (d_OP_masterLoop_dot___hash_lambda5 x3 x2 x4 x13 x5 x14) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 x1002 x3250 x3500) (d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x14 x5 x13 x4 x2 x3 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_10 x4 x3 x2 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_9 x4 x3 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x9 x10) -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 3#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))) (Curry_ReadShowTerm.d_C_showQTerm x5 x3250 x3500) x3250 x3500) x3250 x3500) (let
          x11 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not Curry_Prelude.d_C_null x3250 x3500) Curry_Prelude.d_C_snd x3250 x3500) x5 x3250 x3500
          x12 = Curry_Prelude.d_C_map Curry_Prelude.d_C_fst (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_null Curry_Prelude.d_C_snd x3250 x3500) x5 x3250 x3500) x3250 x3500
           in (d_OP__case_8 x12 x11 x4 x3 x2 x1 (Curry_Prelude.d_C_null x12 x3250 x3500) x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_10 x4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_8 x12 x11 x4 x3 x2 x1 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hWaitForInputs x2 (Curry_Configuration.d_C_waitTime x3250 x3500) x3250 x3500) (d_OP_masterLoop_dot___hash_lambda3 x3 x2 x1 x4 x11 x12) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_masterLoop x1 x2 x3 x4 x11 x12 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x12 x11 x4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_8 x12 x11 x4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x12 x11 x4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x12 x11 x4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_9 x4 x3 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))))) x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> Curry_Prelude.d_OP_gt_gt (d_C_debugMessage (Curry_Prelude.C_Int 2#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hWaitForInputs (Curry_Prelude.OP_Cons x7 x8) (Curry_Configuration.d_C_waitTime x3250 x3500) x3250 x3500) (d_OP_masterLoop_dot___hash_lambda1 x3 x7 x4) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x4 x3 x1002 x3250 x3500) (d_OP__case_9 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
