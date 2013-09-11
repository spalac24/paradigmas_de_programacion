{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Curry_Time (C_CalendarTime (..), C_ClockTime, d_C_ctYear, d_C_ctMonth, d_C_ctDay, d_C_ctHour, d_C_ctMin, d_C_ctSec, d_C_ctTZ, d_C_getLocalTime, d_C_clockTimeToInt, d_C_toCalendarTime, d_C_toUTCTime, d_C_toClockTime, d_C_calendarTimeToString, d_C_toDayString, d_C_toTimeString, d_C_addSeconds, d_C_addMinutes, d_C_addHours, d_C_addDays, d_C_addMonths, d_C_addYears, d_C_daysOfMonth, d_C_validDate, d_C_compareDate, nd_C_compareDate, d_C_compareCalendarTime, d_C_compareClockTime, d_C_getClockTime) where

import Basics
import qualified Curry_Prelude
import qualified System.Time as T
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Cal
import qualified Curry_Prelude as CP


data C_ClockTime
     = C_CTime Curry_Prelude.C_Int
     | Choice_C_ClockTime Cover ID C_ClockTime C_ClockTime
     | Choices_C_ClockTime Cover ID ([C_ClockTime])
     | Fail_C_ClockTime Cover FailInfo
     | Guard_C_ClockTime Cover Constraints C_ClockTime

instance Show C_ClockTime where
  showsPrec d (Choice_C_ClockTime cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_ClockTime cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_ClockTime cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_ClockTime cd info) = showChar '!'
  showsPrec _ (C_CTime x1) = (showString "(CTime") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_ClockTime where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CTime x1,r1) | (_,r0) <- readQualified "Time" "CTime" r, (x1,r1) <- readsPrec 11 r0]) s


instance NonDet C_ClockTime where
  choiceCons = Choice_C_ClockTime
  choicesCons = Choices_C_ClockTime
  failCons = Fail_C_ClockTime
  guardCons = Guard_C_ClockTime
  try (Choice_C_ClockTime cd i x y) = tryChoice cd i x y
  try (Choices_C_ClockTime cd i xs) = tryChoices cd i xs
  try (Fail_C_ClockTime cd info) = Fail cd info
  try (Guard_C_ClockTime cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_ClockTime cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_ClockTime cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_ClockTime cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_ClockTime cd i _) = error ("Time.ClockTime.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_ClockTime cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_ClockTime cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_ClockTime where
  generate s c = Choices_C_ClockTime c (freeID [1] s) [(C_CTime (generate (leftSupply s) c))]


instance NormalForm C_ClockTime where
  ($!!) cont (C_CTime x1) d cs = (((\y1 d cs -> cont (C_CTime y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_ClockTime cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_ClockTime cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_ClockTime cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_ClockTime cd info) _ _ = failCons cd info
  ($##) cont (C_CTime x1) d cs = (((\y1 d cs -> cont (C_CTime y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_ClockTime cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_ClockTime cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_ClockTime cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_ClockTime cd info) _ _ = failCons cd info
  searchNF search cont (C_CTime x1) = search (\y1 -> cont (C_CTime y1)) x1
  searchNF _ _ x = error ("Time.ClockTime.searchNF: no constructor: " ++ (show x))


instance Unifiable C_ClockTime where
  (=.=) (C_CTime x1) (C_CTime y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CTime x1) (C_CTime y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CTime x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_ClockTime cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_ClockTime cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_ClockTime cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_ClockTime cd i _) = error ("Time.ClockTime.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_ClockTime cd info) = [(Unsolvable info)]
  bind d i (Guard_C_ClockTime cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CTime x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_ClockTime cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_ClockTime cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_ClockTime cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_ClockTime cd i _) = error ("Time.ClockTime.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_ClockTime cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_ClockTime cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_ClockTime where
  (=?=) (Choice_C_ClockTime cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_ClockTime cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_ClockTime cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_ClockTime cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_ClockTime cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_ClockTime cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_ClockTime cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_ClockTime cd info) _ _ = failCons cd info
  (=?=) (C_CTime x1) (C_CTime y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (<?=) (Choice_C_ClockTime cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_ClockTime cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_ClockTime cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_ClockTime cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_ClockTime cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_ClockTime cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_ClockTime cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_ClockTime cd info) _ _ = failCons cd info
  (<?=) (C_CTime x1) (C_CTime y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs


data C_CalendarTime
     = C_CalendarTime Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int Curry_Prelude.C_Int
     | Choice_C_CalendarTime Cover ID C_CalendarTime C_CalendarTime
     | Choices_C_CalendarTime Cover ID ([C_CalendarTime])
     | Fail_C_CalendarTime Cover FailInfo
     | Guard_C_CalendarTime Cover Constraints C_CalendarTime

instance Show C_CalendarTime where
  showsPrec d (Choice_C_CalendarTime cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_CalendarTime cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_CalendarTime cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_CalendarTime cd info) = showChar '!'
  showsPrec _ (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) = (showString "(CalendarTime") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . ((showChar ' ') . ((shows x5) . ((showChar ' ') . ((shows x6) . ((showChar ' ') . ((shows x7) . (showChar ')')))))))))))))))


instance Read C_CalendarTime where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_CalendarTime x1 x2 x3 x4 x5 x6 x7,r7) | (_,r0) <- readQualified "Time" "CalendarTime" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3, (x5,r5) <- readsPrec 11 r4, (x6,r6) <- readsPrec 11 r5, (x7,r7) <- readsPrec 11 r6]) s


instance NonDet C_CalendarTime where
  choiceCons = Choice_C_CalendarTime
  choicesCons = Choices_C_CalendarTime
  failCons = Fail_C_CalendarTime
  guardCons = Guard_C_CalendarTime
  try (Choice_C_CalendarTime cd i x y) = tryChoice cd i x y
  try (Choices_C_CalendarTime cd i xs) = tryChoices cd i xs
  try (Fail_C_CalendarTime cd info) = Fail cd info
  try (Guard_C_CalendarTime cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_CalendarTime cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_CalendarTime cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_CalendarTime cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_CalendarTime cd i _) = error ("Time.CalendarTime.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_CalendarTime cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_CalendarTime cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_CalendarTime where
  generate s c = Choices_C_CalendarTime c (freeID [7] s) [(C_CalendarTime (generate (leftSupply (leftSupply (leftSupply s))) c) (generate (rightSupply (leftSupply (leftSupply s))) c) (generate (leftSupply (rightSupply (leftSupply s))) c) (generate (rightSupply (rightSupply (leftSupply s))) c) (generate (leftSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (leftSupply (rightSupply s))) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm C_CalendarTime where
  ($!!) cont (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> cont (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs) $!! x7) d) cs) $!! x6) d) cs) $!! x5) d) cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_CalendarTime cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_CalendarTime cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_CalendarTime cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_CalendarTime cd info) _ _ = failCons cd info
  ($##) cont (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> (((\y5 d cs -> (((\y6 d cs -> (((\y7 d cs -> cont (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs) $## x7) d) cs) $## x6) d) cs) $## x5) d) cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_CalendarTime cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_CalendarTime cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_CalendarTime cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_CalendarTime cd info) _ _ = failCons cd info
  searchNF search cont (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> search (\y5 -> search (\y6 -> search (\y7 -> cont (C_CalendarTime y1 y2 y3 y4 y5 y6 y7)) x7) x6) x5) x4) x3) x2) x1
  searchNF _ _ x = error ("Time.CalendarTime.searchNF: no constructor: " ++ (show x))


instance Unifiable C_CalendarTime where
  (=.=) (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & ((((((x4 =:= y4) d) cs) & ((((((x5 =:= y5) d) cs) & ((((((x6 =:= y6) d) cs) & (((x7 =:= y7) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & ((((((x4 =:<= y4) d) cs) & ((((((x5 =:<= y5) d) cs) & ((((((x6 =:<= y6) d) cs) & (((x7 =:<= y7) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CalendarTime x3 x4 x5 x6 x7 x8 x9) = ((i :=: (ChooseN 0 7)):(concat [(bind cd (leftID (leftID (leftID i))) x3),(bind cd (rightID (leftID (leftID i))) x4),(bind cd (leftID (rightID (leftID i))) x5),(bind cd (rightID (rightID (leftID i))) x6),(bind cd (leftID (leftID (rightID i))) x7),(bind cd (rightID (leftID (rightID i))) x8),(bind cd (rightID (rightID i)) x9)]))
  bind d i (Choice_C_CalendarTime cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_CalendarTime cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_CalendarTime cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_CalendarTime cd i _) = error ("Time.CalendarTime.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_CalendarTime cd info) = [(Unsolvable info)]
  bind d i (Guard_C_CalendarTime cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CalendarTime x3 x4 x5 x6 x7 x8 x9) = [(i :=: (ChooseN 0 7)),((leftID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (leftID i))) x3))),((rightID (leftID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (leftID i))) x4))),((leftID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (leftID (rightID (leftID i))) x5))),((rightID (rightID (leftID i))) :=: (LazyBind (lazyBind cd (rightID (rightID (leftID i))) x6))),((leftID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (leftID (leftID (rightID i))) x7))),((rightID (leftID (rightID i))) :=: (LazyBind (lazyBind cd (rightID (leftID (rightID i))) x8))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x9)))]
  lazyBind d i (Choice_C_CalendarTime cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_CalendarTime cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_CalendarTime cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_CalendarTime cd i _) = error ("Time.CalendarTime.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_CalendarTime cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_CalendarTime cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_CalendarTime where
  (=?=) (Choice_C_CalendarTime cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_CalendarTime cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_CalendarTime cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_CalendarTime cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_CalendarTime cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_CalendarTime cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_CalendarTime cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_CalendarTime cd info) _ _ = failCons cd info
  (=?=) (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x6 Curry_Prelude.=?= y6) d) cs) (((x7 Curry_Prelude.=?= y7) d) cs) d cs) d cs) d cs) d cs) d cs) d cs
  (<?=) (Choice_C_CalendarTime cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_CalendarTime cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_CalendarTime cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_CalendarTime cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_CalendarTime cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_CalendarTime cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_CalendarTime cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_CalendarTime cd info) _ _ = failCons cd info
  (<?=) (C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (C_CalendarTime y1 y2 y3 y4 y5 y6 y7) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x4 y4 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x4 Curry_Prelude.=?= y4) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x5 y5 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x5 Curry_Prelude.=?= y5) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x6 y6 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x6 Curry_Prelude.=?= y6) d) cs) (((x7 Curry_Prelude.<?= y7) d) cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_ctYear :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctYear x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x2
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctYear x1002 x3250 x3500) (d_C_ctYear x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctYear z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctYear x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctMonth :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctMonth x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x3
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctMonth x1002 x3250 x3500) (d_C_ctMonth x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctMonth z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctMonth x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctDay :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctDay x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x4
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctDay x1002 x3250 x3500) (d_C_ctDay x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctDay z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctDay x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctHour :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctHour x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x5
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctHour x1002 x3250 x3500) (d_C_ctHour x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctHour z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctHour x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctMin :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctMin x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x6
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctMin x1002 x3250 x3500) (d_C_ctMin x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctMin z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctMin x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctSec :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctSec x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x7
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctSec x1002 x3250 x3500) (d_C_ctSec x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctSec z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctSec x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_ctTZ :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_ctTZ x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x8
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_ctTZ x1002 x3250 x3500) (d_C_ctTZ x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_ctTZ z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_ctTZ x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getLocalTime :: Cover -> ConstStore -> Curry_Prelude.C_IO C_CalendarTime
d_C_getLocalTime x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getClockTime x3250 x3500) d_OP_getLocalTime_dot___hash_lambda1 x3250 x3500

d_OP_getLocalTime_dot___hash_lambda1 :: C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CalendarTime
d_OP_getLocalTime_dot___hash_lambda1 x1 x3250 x3500 = d_C_toCalendarTime x1 x3250 x3500

d_C_clockTimeToInt :: C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_clockTimeToInt x1 x3250 x3500 = case x1 of
     (C_CTime x2) -> x2
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_clockTimeToInt x1002 x3250 x3500) (d_C_clockTimeToInt x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_clockTimeToInt z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_clockTimeToInt x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toCalendarTime :: C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CalendarTime
d_C_toCalendarTime x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_toCalendarTime x1 x3250 x3500

d_C_toUTCTime :: C_ClockTime -> Cover -> ConstStore -> C_CalendarTime
d_C_toUTCTime x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_toUTCTime x1 x3250 x3500

d_C_toClockTime :: C_CalendarTime -> Cover -> ConstStore -> C_ClockTime
d_C_toClockTime x1 x3250 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_toClockTime x1 x3250 x3500

d_C_calendarTimeToString :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_calendarTimeToString x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> let
          x9 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List)))))))))))
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_bang_bang x9 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_C_toTimeString x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_calendarTimeToString x1002 x3250 x3500) (d_C_calendarTimeToString x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_calendarTimeToString z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_calendarTimeToString x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toDayString :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_toDayString x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> let
          x9 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'J'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))))) Curry_Prelude.OP_List)))))))))))
           in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_OP_bang_bang x9 (Curry_Prelude.d_OP_minus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x4 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toDayString x1002 x3250 x3500) (d_C_toDayString x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toDayString z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toDayString x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toTimeString :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_toTimeString x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> Curry_Prelude.d_OP_plus_plus (d_OP_toTimeString_dot_digit2_dot_89 x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (d_OP_toTimeString_dot_digit2_dot_89 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List) (d_OP_toTimeString_dot_digit2_dot_89 x7 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toTimeString x1002 x3250 x3500) (d_C_toTimeString x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toTimeString z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toTimeString x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_toTimeString_dot_digit2_dot_89 :: Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_toTimeString_dot_digit2_dot_89 x1 x3250 x3500 = d_OP__case_8 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 10#) x3250 x3500) x3250 x3500

d_C_addSeconds :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addSeconds x1 x2 x3250 x3500 = case x2 of
     (C_CTime x3) -> C_CTime (Curry_Prelude.d_OP_plus x3 x1 x3250 x3500)
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_addSeconds x1 x1002 x3250 x3500) (d_C_addSeconds x1 x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_addSeconds x1 z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_addSeconds x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addMinutes :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addMinutes x1 x2 x3250 x3500 = case x2 of
     (C_CTime x3) -> C_CTime (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.d_OP_star x1 (Curry_Prelude.C_Int 60#) x3250 x3500) x3250 x3500)
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_addMinutes x1 x1002 x3250 x3500) (d_C_addMinutes x1 x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_addMinutes x1 z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_addMinutes x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addHours :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addHours x1 x2 x3250 x3500 = case x2 of
     (C_CTime x3) -> C_CTime (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.d_OP_star x1 (Curry_Prelude.C_Int 3600#) x3250 x3500) x3250 x3500)
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_addHours x1 x1002 x3250 x3500) (d_C_addHours x1 x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_addHours x1 z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_addHours x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addDays :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addDays x1 x2 x3250 x3500 = case x2 of
     (C_CTime x3) -> C_CTime (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.d_OP_star x1 (Curry_Prelude.C_Int 86400#) x3250 x3500) x3250 x3500)
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_addDays x1 x1002 x3250 x3500) (d_C_addDays x1 x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_addDays x1 z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_addDays x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addMonths :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addMonths x1 x2 x3250 x3500 = let
     x3 = d_C_toUTCTime x2 x3250 x3500
     x4 = d_OP_addMonths_dot___hash_selFP2_hash_y x3 x3250 x3500
     x5 = d_OP_addMonths_dot___hash_selFP3_hash_mo x3 x3250 x3500
     x6 = d_OP_addMonths_dot___hash_selFP4_hash_d x3 x3250 x3500
     x7 = d_OP_addMonths_dot___hash_selFP5_hash_h x3 x3250 x3500
     x8 = d_OP_addMonths_dot___hash_selFP6_hash_mi x3 x3250 x3500
     x9 = d_OP_addMonths_dot___hash_selFP7_hash_s x3 x3250 x3500
     x10 = d_OP_addMonths_dot___hash_selFP8_hash_tz x3 x3250 x3500
     x11 = Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_mod (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x1 x3250 x3500) (Curry_Prelude.C_Int 12#) x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500
      in (d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 (Curry_Prelude.d_OP_gt x11 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500)

d_OP_addMonths_dot___hash_selFP2_hash_y :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP2_hash_y x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x2
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP2_hash_y x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP2_hash_y x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP2_hash_y z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP2_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP3_hash_mo :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP3_hash_mo x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x3
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP3_hash_mo x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP3_hash_mo x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP3_hash_mo z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP3_hash_mo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP4_hash_d :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP4_hash_d x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x4
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP4_hash_d x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP4_hash_d x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP4_hash_d z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP4_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP5_hash_h :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP5_hash_h x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x5
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP5_hash_h x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP5_hash_h x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP5_hash_h z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP5_hash_h x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP6_hash_mi :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP6_hash_mi x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x6
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP6_hash_mi x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP6_hash_mi x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP6_hash_mi z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP6_hash_mi x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP7_hash_s :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP7_hash_s x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x7
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP7_hash_s x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP7_hash_s x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP7_hash_s z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP7_hash_s x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addMonths_dot___hash_selFP8_hash_tz :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addMonths_dot___hash_selFP8_hash_tz x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x8
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addMonths_dot___hash_selFP8_hash_tz x1002 x3250 x3500) (d_OP_addMonths_dot___hash_selFP8_hash_tz x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addMonths_dot___hash_selFP8_hash_tz z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addMonths_dot___hash_selFP8_hash_tz x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_addYears :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> C_ClockTime
d_C_addYears x1 x2 x3250 x3500 = d_OP__case_6 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500

d_OP_addYears_dot___hash_selFP10_hash_y :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP10_hash_y x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x2
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP10_hash_y x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP10_hash_y x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP10_hash_y z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP10_hash_y x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP11_hash_mo :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP11_hash_mo x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x3
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP11_hash_mo x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP11_hash_mo x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP11_hash_mo z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP11_hash_mo x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP12_hash_d :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP12_hash_d x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x4
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP12_hash_d x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP12_hash_d x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP12_hash_d z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP12_hash_d x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP13_hash_h :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP13_hash_h x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x5
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP13_hash_h x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP13_hash_h x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP13_hash_h z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP13_hash_h x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP14_hash_mi :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP14_hash_mi x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x6
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP14_hash_mi x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP14_hash_mi x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP14_hash_mi z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP14_hash_mi x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP15_hash_s :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP15_hash_s x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x7
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP15_hash_s x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP15_hash_s x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP15_hash_s z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP15_hash_s x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_addYears_dot___hash_selFP16_hash_tz :: C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP_addYears_dot___hash_selFP16_hash_tz x1 x3250 x3500 = case x1 of
     (C_CalendarTime x2 x3 x4 x5 x6 x7 x8) -> x8
     (Choice_C_CalendarTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_addYears_dot___hash_selFP16_hash_tz x1002 x3250 x3500) (d_OP_addYears_dot___hash_selFP16_hash_tz x1003 x3250 x3500)
     (Choices_C_CalendarTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_addYears_dot___hash_selFP16_hash_tz z x3250 x3500) x1002
     (Guard_C_CalendarTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_addYears_dot___hash_selFP16_hash_tz x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_CalendarTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_daysOfMonth :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_daysOfMonth x1 x2 x3250 x3500 = d_OP__case_5 x1 x2 (Curry_Prelude.d_OP_slash_eq x1 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500

d_C_validDate :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_validDate x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_lt x2 (Curry_Prelude.C_Int 13#) x3250 x3500) (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt x3 (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_lt_eq x3 (d_C_daysOfMonth x2 x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_C_compareDate :: Cover -> ConstStore -> C_CalendarTime -> Cover -> ConstStore -> C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_compareDate x3250 x3500 = acceptCs id d_C_compareCalendarTime

nd_C_compareDate :: IDSupply -> Cover -> ConstStore -> Func C_CalendarTime (Func C_CalendarTime Curry_Prelude.C_Ordering)
nd_C_compareDate x3000 x3250 x3500 = wrapDX (wrapDX id) (acceptCs id d_C_compareCalendarTime)

d_C_compareCalendarTime :: C_CalendarTime -> C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_compareCalendarTime x1 x2 x3250 x3500 = d_C_compareClockTime (d_C_toClockTime x1 x3250 x3500) (d_C_toClockTime x2 x3250 x3500) x3250 x3500

d_C_compareClockTime :: C_ClockTime -> C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_C_compareClockTime x1 x2 x3250 x3500 = case x1 of
     (C_CTime x3) -> d_OP__case_3 x3 x2 x3250 x3500
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_compareClockTime x1002 x2 x3250 x3500) (d_C_compareClockTime x1003 x2 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_compareClockTime z x2 x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_compareClockTime x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Int -> C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_3 x3 x2 x3250 x3500 = case x2 of
     (C_CTime x4) -> d_OP__case_2 x4 x3 (Curry_Prelude.d_OP_lt x3 x4 x3250 x3500) x3250 x3500
     (Choice_C_ClockTime x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x1002 x3250 x3500) (d_OP__case_3 x3 x1003 x3250 x3500)
     (Choices_C_ClockTime x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 z x3250 x3500) x1002
     (Guard_C_ClockTime x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_ClockTime x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_2 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_LT
     Curry_Prelude.C_False -> d_OP__case_1 x4 x3 (Curry_Prelude.d_OP_gt x3 x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x3 x1002 x3250 x3500) (d_OP__case_2 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_1 x4 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.C_GT
     Curry_Prelude.C_False -> d_OP__case_0 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x3 x1002 x3250 x3500) (d_OP__case_1 x4 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Ordering
d_OP__case_0 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_EQ
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_5 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_bang_bang (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 28#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 30#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 30#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 30#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 30#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 31#) Curry_Prelude.OP_List)))))))))))) (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_4 x2 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 4#) x3250 x3500) (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 100#) x3250 x3500) (Curry_Prelude.C_Int 0#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_mod x2 (Curry_Prelude.C_Int 400#) x3250 x3500) (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x1002 x3250 x3500) (d_OP__case_5 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_OP__case_4 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 29#
     Curry_Prelude.C_False -> Curry_Prelude.C_Int 28#
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3250 x3500) (d_OP__case_4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Int -> C_ClockTime -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ClockTime
d_OP__case_6 x1 x2 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x3 = d_C_toUTCTime x2 x3250 x3500
          x4 = d_OP_addYears_dot___hash_selFP10_hash_y x3 x3250 x3500
          x5 = d_OP_addYears_dot___hash_selFP11_hash_mo x3 x3250 x3500
          x6 = d_OP_addYears_dot___hash_selFP12_hash_d x3 x3250 x3500
          x7 = d_OP_addYears_dot___hash_selFP13_hash_h x3 x3250 x3500
          x8 = d_OP_addYears_dot___hash_selFP14_hash_mi x3 x3250 x3500
          x9 = d_OP_addYears_dot___hash_selFP15_hash_s x3 x3250 x3500
          x10 = d_OP_addYears_dot___hash_selFP16_hash_tz x3 x3250 x3500
           in (d_C_toClockTime (C_CalendarTime (Curry_Prelude.d_OP_plus x4 x1 x3250 x3500) x5 x6 x7 x8 x9 x10) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x1002 x3250 x3500) (d_OP__case_6 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_ClockTime
d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_C_addYears (Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x1 x3250 x3500) (Curry_Prelude.C_Int 12#) x3250 x3500) (d_C_toClockTime (C_CalendarTime x4 x11 x6 x7 x8 x9 x10) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_addYears (Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x1 x3250 x3500) (Curry_Prelude.C_Int 12#) x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) (d_C_toClockTime (C_CalendarTime x4 (Curry_Prelude.d_OP_plus x11 (Curry_Prelude.C_Int 12#) x3250 x3500) x6 x7 x8 x9 x10) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 x1002 x3250 x3500) (d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x11 x10 x9 x8 x7 x6 x4 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_8 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '0'#) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3250 x3500) x1 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_show x1 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3250 x3500) (d_OP__case_8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_getClockTime :: Cover -> ConstStore -> Curry_Prelude.C_IO C_ClockTime
d_C_getClockTime x3250 x3500 = external_d_C_getClockTime x3250 x3500

d_C_prim_toCalendarTime :: C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO C_CalendarTime
d_C_prim_toCalendarTime x1 x3250 x3500 = external_d_C_prim_toCalendarTime x1 x3250 x3500

d_C_prim_toUTCTime :: C_ClockTime -> Cover -> ConstStore -> C_CalendarTime
d_C_prim_toUTCTime x1 x3250 x3500 = external_d_C_prim_toUTCTime x1 x3250 x3500

d_C_prim_toClockTime :: C_CalendarTime -> Cover -> ConstStore -> C_ClockTime
d_C_prim_toClockTime x1 x3250 x3500 = external_d_C_prim_toClockTime x1 x3250 x3500
instance ConvertCurryHaskell C_ClockTime T.ClockTime where
  fromCurry (C_CTime i) = T.TOD (fromCurry i) 0
  toCurry   (T.TOD i _) = C_CTime (toCurry i)

instance ConvertCurryHaskell C_CalendarTime T.CalendarTime where
  fromCurry (C_CalendarTime y m d h min s tz) =
    T.CalendarTime  (fromCurry y)
                    (toEnum (fromCurry m - 1))
                    (fromCurry d)
                    (fromCurry h)
                    (fromCurry min)
                    (fromCurry s)
                    0 undefined undefined undefined
                    (fromCurry tz)
                    undefined

  toCurry (T.CalendarTime y m d h min s _ _ _ _ tz _) =
    C_CalendarTime  (toCurry y)
                    (toCurry (fromEnum m + 1))
                    (toCurry d)
                    (toCurry h)
                    (toCurry min)
                    (toCurry s)
                    (toCurry tz)

instance ConvertCurryHaskell C_ClockTime Clock.UTCTime where
  fromCurry ct = let (T.CalendarTime y m d h min s _ _ _ _ tz _) 
                        = T.toUTCTime (fromCurry ct)
                 in  fromIntegral tz
                     `Clock.addUTCTime`
                     Clock.UTCTime (Cal.fromGregorian (toInteger y) (fromEnum m + 1) d)
                                  (Clock.secondsToDiffTime (toInteger ((h * 60 + min) * 60 + s)))
                    
  toCurry (Clock.UTCTime day diff) = 
   let (y,m,d) = Cal.toGregorian day in
      toCurry (T.addToClockTime  
                  (T.TimeDiff 0 0 0 0 0 (round (toRational diff)) 0)
                  (T.toClockTime (T.CalendarTime (fromIntegral y) 
                                                 (toEnum (m - 1)) 
                                                 d 0 0 0 0 undefined 
                                                 undefined undefined 0 undefined)))
     

external_d_C_getClockTime :: Cover -> ConstStore -> CP.C_IO C_ClockTime
external_d_C_getClockTime _ _ = toCurry T.getClockTime

external_d_C_prim_toCalendarTime :: C_ClockTime -> Cover -> ConstStore -> CP.C_IO C_CalendarTime
external_d_C_prim_toCalendarTime ct _ _ = toCurry T.toCalendarTime ct

external_d_C_prim_toUTCTime :: C_ClockTime -> Cover -> ConstStore -> C_CalendarTime
external_d_C_prim_toUTCTime ct _ _ = toCurry T.toUTCTime ct

external_d_C_prim_toClockTime :: C_CalendarTime -> Cover -> ConstStore -> C_ClockTime
external_d_C_prim_toClockTime ct _ _ = toCurry T.toClockTime ct

