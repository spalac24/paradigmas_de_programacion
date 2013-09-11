{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Profile (C_ProcessInfo (..), d_C_showMemInfo, d_C_printMemInfo, d_C_profileTime, d_C_profileTimeNF, d_C_profileSpace, d_C_profileSpaceNF, d_C_getProcessInfos, d_C_garbageCollectorOff, d_C_garbageCollectorOn, d_C_garbageCollect) where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
import System.Mem (performGC)
import System.CPUTime
import qualified Curry_Prelude as CP


data C_ProcessInfo
     = C_RunTime
     | C_ElapsedTime
     | C_Memory
     | C_Code
     | C_Stack
     | C_Heap
     | C_Choices
     | C_GarbageCollections
     | Choice_C_ProcessInfo Cover ID C_ProcessInfo C_ProcessInfo
     | Choices_C_ProcessInfo Cover ID ([C_ProcessInfo])
     | Fail_C_ProcessInfo Cover FailInfo
     | Guard_C_ProcessInfo Cover Constraints C_ProcessInfo

instance Show C_ProcessInfo where
  showsPrec d (Choice_C_ProcessInfo cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ProcessInfo cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ProcessInfo cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ProcessInfo cd info) = showChar '!'
  showsPrec _ C_RunTime = showString "RunTime"
  showsPrec _ C_ElapsedTime = showString "ElapsedTime"
  showsPrec _ C_Memory = showString "Memory"
  showsPrec _ C_Code = showString "Code"
  showsPrec _ C_Stack = showString "Stack"
  showsPrec _ C_Heap = showString "Heap"
  showsPrec _ C_Choices = showString "Choices"
  showsPrec _ C_GarbageCollections = showString "GarbageCollections"


instance Read C_ProcessInfo where
  readsPrec _ s = (readParen False (\r -> [ (C_RunTime,r0) | (_,r0) <- readQualified "Profile" "RunTime" r]) s) ++ ((readParen False (\r -> [ (C_ElapsedTime,r0) | (_,r0) <- readQualified "Profile" "ElapsedTime" r]) s) ++ ((readParen False (\r -> [ (C_Memory,r0) | (_,r0) <- readQualified "Profile" "Memory" r]) s) ++ ((readParen False (\r -> [ (C_Code,r0) | (_,r0) <- readQualified "Profile" "Code" r]) s) ++ ((readParen False (\r -> [ (C_Stack,r0) | (_,r0) <- readQualified "Profile" "Stack" r]) s) ++ ((readParen False (\r -> [ (C_Heap,r0) | (_,r0) <- readQualified "Profile" "Heap" r]) s) ++ ((readParen False (\r -> [ (C_Choices,r0) | (_,r0) <- readQualified "Profile" "Choices" r]) s) ++ (readParen False (\r -> [ (C_GarbageCollections,r0) | (_,r0) <- readQualified "Profile" "GarbageCollections" r]) s)))))))


instance NonDet C_ProcessInfo where
  choiceCons = Choice_C_ProcessInfo
  choicesCons = Choices_C_ProcessInfo
  failCons = Fail_C_ProcessInfo
  guardCons = Guard_C_ProcessInfo
  try (Choice_C_ProcessInfo cd i x y) = tryChoice cd i x y
  try (Choices_C_ProcessInfo cd i xs) = tryChoices cd i xs
  try (Fail_C_ProcessInfo cd info) = Fail cd info
  try (Guard_C_ProcessInfo cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ProcessInfo cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ProcessInfo cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ProcessInfo cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ProcessInfo cd i _) = error ("Profile.ProcessInfo.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ProcessInfo cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ProcessInfo cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ProcessInfo where
  generate s c = Choices_C_ProcessInfo c (freeID [0,0,0,0,0,0,0,0] s) [C_RunTime,C_ElapsedTime,C_Memory,C_Code,C_Stack,C_Heap,C_Choices,C_GarbageCollections]


instance NormalForm C_ProcessInfo where
  ($!!) cont C_RunTime d cs = cont C_RunTime d cs
  ($!!) cont C_ElapsedTime d cs = cont C_ElapsedTime d cs
  ($!!) cont C_Memory d cs = cont C_Memory d cs
  ($!!) cont C_Code d cs = cont C_Code d cs
  ($!!) cont C_Stack d cs = cont C_Stack d cs
  ($!!) cont C_Heap d cs = cont C_Heap d cs
  ($!!) cont C_Choices d cs = cont C_Choices d cs
  ($!!) cont C_GarbageCollections d cs = cont C_GarbageCollections d cs
  ($!!) cont (Choice_C_ProcessInfo cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ProcessInfo cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ProcessInfo cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ProcessInfo cd info) _ _ = failCons cd info
  ($##) cont C_RunTime d cs = cont C_RunTime d cs
  ($##) cont C_ElapsedTime d cs = cont C_ElapsedTime d cs
  ($##) cont C_Memory d cs = cont C_Memory d cs
  ($##) cont C_Code d cs = cont C_Code d cs
  ($##) cont C_Stack d cs = cont C_Stack d cs
  ($##) cont C_Heap d cs = cont C_Heap d cs
  ($##) cont C_Choices d cs = cont C_Choices d cs
  ($##) cont C_GarbageCollections d cs = cont C_GarbageCollections d cs
  ($##) cont (Choice_C_ProcessInfo cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ProcessInfo cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ProcessInfo cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ProcessInfo cd info) _ _ = failCons cd info
  searchNF _ cont C_RunTime = cont C_RunTime
  searchNF _ cont C_ElapsedTime = cont C_ElapsedTime
  searchNF _ cont C_Memory = cont C_Memory
  searchNF _ cont C_Code = cont C_Code
  searchNF _ cont C_Stack = cont C_Stack
  searchNF _ cont C_Heap = cont C_Heap
  searchNF _ cont C_Choices = cont C_Choices
  searchNF _ cont C_GarbageCollections = cont C_GarbageCollections
  searchNF _ _ x = error ("Profile.ProcessInfo.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ProcessInfo where
  (=.=) C_RunTime C_RunTime d cs = C_Success
  (=.=) C_ElapsedTime C_ElapsedTime d cs = C_Success
  (=.=) C_Memory C_Memory d cs = C_Success
  (=.=) C_Code C_Code d cs = C_Success
  (=.=) C_Stack C_Stack d cs = C_Success
  (=.=) C_Heap C_Heap d cs = C_Success
  (=.=) C_Choices C_Choices d cs = C_Success
  (=.=) C_GarbageCollections C_GarbageCollections d cs = C_Success
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) C_RunTime C_RunTime d cs = C_Success
  (=.<=) C_ElapsedTime C_ElapsedTime d cs = C_Success
  (=.<=) C_Memory C_Memory d cs = C_Success
  (=.<=) C_Code C_Code d cs = C_Success
  (=.<=) C_Stack C_Stack d cs = C_Success
  (=.<=) C_Heap C_Heap d cs = C_Success
  (=.<=) C_Choices C_Choices d cs = C_Success
  (=.<=) C_GarbageCollections C_GarbageCollections d cs = C_Success
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i C_RunTime = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i C_ElapsedTime = ((i :=: (ChooseN 1 0)):(concat []))
  bind cd i C_Memory = ((i :=: (ChooseN 2 0)):(concat []))
  bind cd i C_Code = ((i :=: (ChooseN 3 0)):(concat []))
  bind cd i C_Stack = ((i :=: (ChooseN 4 0)):(concat []))
  bind cd i C_Heap = ((i :=: (ChooseN 5 0)):(concat []))
  bind cd i C_Choices = ((i :=: (ChooseN 6 0)):(concat []))
  bind cd i C_GarbageCollections = ((i :=: (ChooseN 7 0)):(concat []))
  bind d i (Choice_C_ProcessInfo cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ProcessInfo cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ProcessInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ProcessInfo cd i _) = error ("Profile.ProcessInfo.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ProcessInfo cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ProcessInfo cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_RunTime = [(i :=: (ChooseN 0 0))]
  lazyBind cd i C_ElapsedTime = [(i :=: (ChooseN 1 0))]
  lazyBind cd i C_Memory = [(i :=: (ChooseN 2 0))]
  lazyBind cd i C_Code = [(i :=: (ChooseN 3 0))]
  lazyBind cd i C_Stack = [(i :=: (ChooseN 4 0))]
  lazyBind cd i C_Heap = [(i :=: (ChooseN 5 0))]
  lazyBind cd i C_Choices = [(i :=: (ChooseN 6 0))]
  lazyBind cd i C_GarbageCollections = [(i :=: (ChooseN 7 0))]
  lazyBind d i (Choice_C_ProcessInfo cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ProcessInfo cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ProcessInfo cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ProcessInfo cd i _) = error ("Profile.ProcessInfo.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ProcessInfo cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ProcessInfo cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ProcessInfo where
  (=?=) (Choice_C_ProcessInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ProcessInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ProcessInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ProcessInfo cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ProcessInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ProcessInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ProcessInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ProcessInfo cd info) _ _ = failCons cd info
  (=?=) C_RunTime C_RunTime d cs = Curry_Prelude.C_True
  (=?=) C_ElapsedTime C_ElapsedTime d cs = Curry_Prelude.C_True
  (=?=) C_Memory C_Memory d cs = Curry_Prelude.C_True
  (=?=) C_Code C_Code d cs = Curry_Prelude.C_True
  (=?=) C_Stack C_Stack d cs = Curry_Prelude.C_True
  (=?=) C_Heap C_Heap d cs = Curry_Prelude.C_True
  (=?=) C_Choices C_Choices d cs = Curry_Prelude.C_True
  (=?=) C_GarbageCollections C_GarbageCollections d cs = Curry_Prelude.C_True
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_ProcessInfo cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ProcessInfo cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ProcessInfo cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ProcessInfo cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ProcessInfo cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ProcessInfo cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ProcessInfo cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ProcessInfo cd info) _ _ = failCons cd info
  (<?=) C_RunTime C_RunTime d cs = Curry_Prelude.C_True
  (<?=) C_RunTime C_ElapsedTime _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_Memory _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_Code _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_Stack _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_Heap _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_RunTime C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_ElapsedTime d cs = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_Memory _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_Code _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_Stack _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_Heap _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_ElapsedTime C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_Memory C_Memory d cs = Curry_Prelude.C_True
  (<?=) C_Memory C_Code _ _ = Curry_Prelude.C_True
  (<?=) C_Memory C_Stack _ _ = Curry_Prelude.C_True
  (<?=) C_Memory C_Heap _ _ = Curry_Prelude.C_True
  (<?=) C_Memory C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_Memory C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_Code C_Code d cs = Curry_Prelude.C_True
  (<?=) C_Code C_Stack _ _ = Curry_Prelude.C_True
  (<?=) C_Code C_Heap _ _ = Curry_Prelude.C_True
  (<?=) C_Code C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_Code C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_Stack C_Stack d cs = Curry_Prelude.C_True
  (<?=) C_Stack C_Heap _ _ = Curry_Prelude.C_True
  (<?=) C_Stack C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_Stack C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_Heap C_Heap d cs = Curry_Prelude.C_True
  (<?=) C_Heap C_Choices _ _ = Curry_Prelude.C_True
  (<?=) C_Heap C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_Choices C_Choices d cs = Curry_Prelude.C_True
  (<?=) C_Choices C_GarbageCollections _ _ = Curry_Prelude.C_True
  (<?=) C_GarbageCollections C_GarbageCollections d cs = Curry_Prelude.C_True
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_showMemInfo :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showMemInfo x1 x3250 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_concat (Curry_Prelude.d_OP_dollar (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_OP_showMemInfo_dot_formatItem_dot_2 x1 C_Memory (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP_showMemInfo_dot_formatItem_dot_2 x1 C_Code (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP_showMemInfo_dot_formatItem_dot_2 x1 C_Stack (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP_showMemInfo_dot_formatItem_dot_2 x1 C_Choices (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))) x3250 x3500) (d_OP_showMemInfo_dot_formatItem_dot_2 x1 C_Heap (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_showMemInfo_dot_showBytes_dot_2 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showMemInfo_dot_showBytes_dot_2 x1 x3250 x3500 = d_OP__case_0 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 10000#) x3250 x3500) x3250 x3500

d_OP_showMemInfo_dot_formatItem_dot_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> C_ProcessInfo -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showMemInfo_dot_formatItem_dot_2 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_maybe Curry_Prelude.OP_List (d_OP_showMemInfo_dot_formatItem_dot_2_dot___hash_lambda1 x3) (Curry_Prelude.d_C_lookup x2 x1 x3250 x3500) x3250 x3500

d_OP_showMemInfo_dot_formatItem_dot_2_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_showMemInfo_dot_formatItem_dot_2_dot___hash_lambda1 x1 x2 x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.d_OP_plus_plus x1 (d_OP_showMemInfo_dot_showBytes_dot_2 x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List

d_C_printMemInfo :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_printMemInfo x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProcessInfos x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_putStrLn d_C_showMemInfo x3250 x3500) x3250 x3500

d_C_profileTime :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_profileTime x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_garbageCollect x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_getProcessInfos x3250 x3500) (d_OP_profileTime_dot___hash_lambda2 x1) x3250 x3500) x3250 x3500

d_OP_profileTime_dot___hash_lambda2 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileTime_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x1 (d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3 x2) x3250 x3500

d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProcessInfos x3250 x3500) (d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x2) x3250 x3500

d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileTime_dot___hash_lambda2_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_RunTime x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_ElapsedTime x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (d_C_showInfoDiff x1 x3 C_GarbageCollections x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_profileTimeNF :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_profileTimeNF x1 x3250 x3500 = d_C_profileTime (Curry_Prelude.d_C_seq (Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_id x1 x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500

d_C_profileSpace :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_C_profileSpace x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_garbageCollect x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_garbageCollectorOff x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_getProcessInfos x3250 x3500) (d_OP_profileSpace_dot___hash_lambda5 x1) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_profileSpace_dot___hash_lambda5 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_IO t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileSpace_dot___hash_lambda5 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x1 (d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6 x2) x3250 x3500

d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getProcessInfos x3250 x3500) (d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2) x3250 x3500

d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_IO t0
d_OP_profileSpace_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (d_C_garbageCollectorOn x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_RunTime x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_ElapsedTime x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (d_C_showInfoDiff x1 x3 C_GarbageCollections x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_Heap x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (d_C_showInfoDiff x1 x3 C_Stack x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_profileSpaceNF :: Curry_Prelude.Curry t0 => t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_profileSpaceNF x1 x3250 x3500 = d_C_profileSpace (Curry_Prelude.d_C_seq (Curry_Prelude.d_OP_dollar_bang_bang Curry_Prelude.d_C_id x1 x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500

d_C_showInfoDiff :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int) -> t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showInfoDiff x1 x2 x3 x3250 x3500 = Curry_Prelude.d_C_show (Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_maybe (Curry_Prelude.C_Int 0#) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x3 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_maybe (Curry_Prelude.C_Int 0#) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x3 x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP__case_0 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_show x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show (Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 1000#) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3250 x3500) (d_OP__case_0 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getProcessInfos :: Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int))
d_C_getProcessInfos x3250 x3500 = external_d_C_getProcessInfos x3250 x3500

d_C_garbageCollectorOff :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_garbageCollectorOff x3250 x3500 = external_d_C_garbageCollectorOff x3250 x3500

d_C_garbageCollectorOn :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_garbageCollectorOn x3250 x3500 = external_d_C_garbageCollectorOn x3250 x3500

d_C_garbageCollect :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_garbageCollect x3250 x3500 = external_d_C_garbageCollect x3250 x3500
external_d_C_getProcessInfos :: Cover -> ConstStore -> 
   CP.C_IO (CP.OP_List (CP.OP_Tuple2 C_ProcessInfo CP.C_Int))
external_d_C_getProcessInfos _ _ = fromIO $ do
  t <- getCPUTime
  return (CP.OP_Cons (CP.OP_Tuple2 C_RunTime (toCurry (t `div` (10^9))))
                     CP.OP_List)

external_d_C_garbageCollectorOff :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOff _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollectorOn :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollectorOn _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollect :: Cover -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_garbageCollect _ _ = toCurry performGC

