{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_GhciComm (C_GhciComm, d_C_initGhciComm, d_C_stopGhciComm, d_C_restartGhciComm, d_C_evalMainCmd, d_C_evalCustomCmd) where

import Basics
import qualified Curry_IO
import qualified Curry_IOExts
import qualified Curry_Prelude
import qualified Curry_Time
import qualified Curry_Utils
data C_GhciComm
     = C_GhciComm (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle Curry_Prelude.C_Bool
     | Choice_C_GhciComm Cover ID C_GhciComm C_GhciComm
     | Choices_C_GhciComm Cover ID ([C_GhciComm])
     | Fail_C_GhciComm Cover FailInfo
     | Guard_C_GhciComm Cover Constraints C_GhciComm

instance Show C_GhciComm where
  showsPrec d (Choice_C_GhciComm cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_GhciComm cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_GhciComm cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_GhciComm cd info) = showChar '!'
  showsPrec _ (C_GhciComm x1 x2 x3) = (showString "(GhciComm") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . (showChar ')')))))))


instance Read C_GhciComm where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_GhciComm x1 x2 x3,r3) | (_,r0) <- readQualified "GhciComm" "GhciComm" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2]) s


instance NonDet C_GhciComm where
  choiceCons = Choice_C_GhciComm
  choicesCons = Choices_C_GhciComm
  failCons = Fail_C_GhciComm
  guardCons = Guard_C_GhciComm
  try (Choice_C_GhciComm cd i x y) = tryChoice cd i x y
  try (Choices_C_GhciComm cd i xs) = tryChoices cd i xs
  try (Fail_C_GhciComm cd info) = Fail cd info
  try (Guard_C_GhciComm cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_GhciComm cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_GhciComm cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_GhciComm cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_GhciComm cd i _) = error ("GhciComm.GhciComm.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_GhciComm cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_GhciComm cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_GhciComm where
  generate s c = Choices_C_GhciComm c (freeID [3] s) [(C_GhciComm (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (rightSupply s) c))]


instance NormalForm C_GhciComm where
  ($!!) cont (C_GhciComm x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_GhciComm y1 y2 y3) d cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_GhciComm cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_GhciComm cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_GhciComm cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_GhciComm cd info) _ _ = failCons cd info
  ($##) cont (C_GhciComm x1 x2 x3) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> cont (C_GhciComm y1 y2 y3) d cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_GhciComm cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_GhciComm cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_GhciComm cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_GhciComm cd info) _ _ = failCons cd info
  searchNF search cont (C_GhciComm x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_GhciComm y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("GhciComm.GhciComm.searchNF: no constructor: " ++ (show x))


instance Unifiable C_GhciComm where
  (=.=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & (((x3 =:= y3) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & (((x3 =:<= y3) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_GhciComm x3 x4 x5) = ((i :=: (ChooseN 0 3)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (rightID i) x5)]))
  bind d i (Choice_C_GhciComm cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_GhciComm cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_GhciComm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_GhciComm cd i _) = error ("GhciComm.GhciComm.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_GhciComm cd info) = [(Unsolvable info)]
  bind d i (Guard_C_GhciComm cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_GhciComm x3 x4 x5) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x5)))]
  lazyBind d i (Choice_C_GhciComm cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_GhciComm cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_GhciComm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_GhciComm cd i _) = error ("GhciComm.GhciComm.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_GhciComm cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_GhciComm cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_GhciComm where
  (=?=) (Choice_C_GhciComm cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_GhciComm cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_GhciComm cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_GhciComm cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_GhciComm cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_GhciComm cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_GhciComm cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_GhciComm cd info) _ _ = failCons cd info
  (=?=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.=?= y3) d) cs) d cs) d cs
  (<?=) (Choice_C_GhciComm cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_GhciComm cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_GhciComm cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_GhciComm cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_GhciComm cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_GhciComm cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_GhciComm cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_GhciComm cd info) _ _ = failCons cd info
  (<?=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (((x3 Curry_Prelude.<?= y3) d) cs) d cs) d cs) d cs) d cs


d_C_initGhciComm :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_C_initGhciComm x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_connectToCommand x1 x3250 x3500) (d_OP_initGhciComm_dot___hash_lambda1 x1 x2) x3250 x3500

d_OP_initGhciComm_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_OP_initGhciComm_dot___hash_lambda1 x1 x2 x3 x3250 x3500 = let
     x4 = C_GhciComm x1 x3 x2
      in (Curry_Prelude.d_OP_gt_gt (d_C_evalCustomCmd x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))) x3250 x3500) (Curry_Prelude.d_C_return x4 x3250 x3500) x3250 x3500)

d_C_stopGhciComm :: C_GhciComm -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_stopGhciComm x1 x3250 x3500 = case x1 of
     (C_GhciComm x2 x3 x4) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Utils.d_C_when x4 (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x3 x3250 x3500) Curry_Prelude.d_C_putStrLn x3250 x3500) x3250 x3500) (Curry_IO.d_C_hClose x3 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_stopGhciComm x1002 x3250 x3500) (d_C_stopGhciComm x1003 x3250 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_stopGhciComm z x3250 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_stopGhciComm x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_restartGhciComm :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_C_restartGhciComm x1 x2 x3 x3250 x3500 = case x1 of
     (C_GhciComm x4 x5 x6) -> d_OP__case_2 x4 x2 x3 x1 (Curry_Prelude.d_OP_eq_eq x2 x4 x3250 x3500) x3250 x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_restartGhciComm x1002 x2 x3 x3250 x3500) (d_C_restartGhciComm x1003 x2 x3 x3250 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_restartGhciComm z x2 x3 x3250 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_restartGhciComm x1002 x2 x3 x3250) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_evalMainCmd :: C_GhciComm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_evalMainCmd x1 x2 x3250 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (d_OP__case_0 x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_evalCustomCmd x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))) x3250 x3500) (Curry_Utils.d_C_when x2 (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3250 x3500) Curry_Prelude.d_C_putStrLn x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_evalMainCmd x1002 x2 x3250 x3500) (d_C_evalMainCmd x1003 x2 x3250 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_evalMainCmd z x2 x3250 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_evalMainCmd x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_evalCustomCmd :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_evalCustomCmd x1 x2 x3250 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Time.d_C_getLocalTime x3250 x3500) (d_OP_evalCustomCmd_dot___hash_lambda3 x2 x1 x4) x3250 x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_evalCustomCmd x1002 x2 x3250 x3500) (d_C_evalCustomCmd x1003 x2 x3250 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_evalCustomCmd z x2 x3250 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_evalCustomCmd x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x1 x3250 x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 x1 x2) x3250 x3500

d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_unless (Curry_Prelude.d_OP_eq_eq x3 x2 x3250 x3500)) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn x3 x3250 x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x1 x2 x3250 x3500) x3250 x3500) x3250 x3500

d_OP_evalCustomCmd_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_GhciComm -> Curry_IO.C_Handle -> Curry_Time.C_CalendarTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot___hash_lambda3 x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Time.d_C_calendarTimeToString x4 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_hPutStrLnGhci x2) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x3 x5 x3250 x3500) x3250 x3500)

d_C_hPutStrLnGhci :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStrLnGhci x1 x2 x3250 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_when x5) (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x2 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x4 x2 x3250 x3500) (Curry_IO.d_C_hFlush x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hPutStrLnGhci x1002 x2 x3250 x3500) (d_C_hPutStrLnGhci x1003 x2 x3250 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hPutStrLnGhci z x2 x3250 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hPutStrLnGhci x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> C_GhciComm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_OP__case_2 x4 x2 x3 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3250 x3500) (Curry_Prelude.d_C_return x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_1 x3 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x2 x3 x1 x1002 x3250 x3500) (d_OP__case_2 x4 x2 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x2 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x2 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_GhciComm -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_OP__case_1 x3 x2 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_stopGhciComm x1 x3250 x3500) (d_C_initGhciComm x2 x3 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_1 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
