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
  generate s = Choices_C_GhciComm defCover (freeID [3] s) [(C_GhciComm (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (rightSupply s)))]


instance NormalForm C_GhciComm where
  ($!!) cont (C_GhciComm x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_GhciComm y1 y2 y3) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_GhciComm cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_GhciComm cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_GhciComm cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_GhciComm cd info) _ = failCons cd info
  ($##) cont (C_GhciComm x1 x2 x3) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> cont (C_GhciComm y1 y2 y3) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_GhciComm cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_GhciComm cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_GhciComm cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_GhciComm cd info) _ = failCons cd info
  searchNF search cont (C_GhciComm x1 x2 x3) = search (\y1 -> search (\y2 -> search (\y3 -> cont (C_GhciComm y1 y2 y3)) x3) x2) x1
  searchNF _ _ x = error ("GhciComm.GhciComm.searchNF: no constructor: " ++ (show x))


instance Unifiable C_GhciComm where
  (=.=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((x3 =:= y3) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((x3 =:<= y3) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_GhciComm x2 x3 x4) = ((i :=: (ChooseN 0 3)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (rightID i) x4)]))
  bind i (Choice_C_GhciComm cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_GhciComm cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_GhciComm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_GhciComm cd i _) = error ("GhciComm.GhciComm.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_GhciComm cd info) = [(Unsolvable info)]
  bind i (Guard_C_GhciComm cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_GhciComm x2 x3 x4) = [(i :=: (ChooseN 0 3)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x4)))]
  lazyBind i (Choice_C_GhciComm cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_GhciComm cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_GhciComm cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_GhciComm cd i _) = error ("GhciComm.GhciComm.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_GhciComm cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_GhciComm cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_GhciComm where
  (=?=) (Choice_C_GhciComm cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_GhciComm cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_GhciComm cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_GhciComm cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_GhciComm cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_GhciComm cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_GhciComm cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_GhciComm cd info) _ = failCons cd info
  (=?=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.=?= y3) cs) cs) cs
  (<?=) (Choice_C_GhciComm cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_GhciComm cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_GhciComm cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_GhciComm cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_GhciComm cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_GhciComm cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_GhciComm cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_GhciComm cd info) _ = failCons cd info
  (<?=) (C_GhciComm x1 x2 x3) (C_GhciComm y1 y2 y3) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) ((x3 Curry_Prelude.<?= y3) cs) cs) cs) cs) cs


instance Coverable C_GhciComm where
  cover (C_GhciComm x1 x2 x3) = C_GhciComm (cover x1) (cover x2) (cover x3)
  cover (Choice_C_GhciComm cd i x y) = Choice_C_GhciComm (incCover cd) i (cover x) (cover y)
  cover (Choices_C_GhciComm cd i xs) = Choices_C_GhciComm (incCover cd) i (map cover xs)
  cover (Fail_C_GhciComm cd info) = Fail_C_GhciComm (incCover cd) info
  cover (Guard_C_GhciComm cd c e) = Guard_C_GhciComm (incCover cd) c (cover e)


d_C_initGhciComm :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_C_initGhciComm x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_connectToCommand x1 x3500) (d_OP_initGhciComm_dot___hash_lambda1 x1 x2) x3500

d_OP_initGhciComm_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_OP_initGhciComm_dot___hash_lambda1 x1 x2 x3 x3500 = let
     x4 = C_GhciComm x1 x3 x2
      in (Curry_Prelude.d_OP_gt_gt (d_C_evalCustomCmd x4 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))) x3500) (Curry_Prelude.d_C_return x4 x3500) x3500)

d_C_stopGhciComm :: C_GhciComm -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_stopGhciComm x1 x3500 = case x1 of
     (C_GhciComm x2 x3 x4) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Utils.d_C_when x4 (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x3 x3500) Curry_Prelude.d_C_putStrLn x3500) x3500) (Curry_IO.d_C_hClose x3 x3500) x3500) x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_stopGhciComm x1002 x3500) (d_C_stopGhciComm x1003 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_stopGhciComm z x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_stopGhciComm x1002) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_restartGhciComm :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO C_GhciComm
d_C_restartGhciComm x1 x2 x3 x3500 = case x1 of
     (C_GhciComm x4 x5 x6) -> d_OP__case_2 x1 x2 x3 x4 (Curry_Prelude.d_OP_eq_eq x2 x4 x3500) x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_restartGhciComm x1002 x2 x3 x3500) (d_C_restartGhciComm x1003 x2 x3 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_restartGhciComm z x2 x3 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_restartGhciComm x1002 x2 x3) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_evalMainCmd :: C_GhciComm -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_evalMainCmd x1 x2 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (d_OP__case_0 x2 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (d_C_evalCustomCmd x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))))))))))))) x3500) (Curry_Utils.d_C_when x2 (Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3500) Curry_Prelude.d_C_putStrLn x3500) x3500) x3500) x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_evalMainCmd x1002 x2 x3500) (d_C_evalMainCmd x1003 x2 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_evalMainCmd z x2 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_evalMainCmd x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_evalCustomCmd :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_evalCustomCmd x1 x2 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Time.d_C_getLocalTime x3500) (d_OP_evalCustomCmd_dot___hash_lambda3 x2 x1 x4) x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_evalCustomCmd x1002 x2 x3500) (d_C_evalCustomCmd x1003 x2 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_evalCustomCmd z x2 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_evalCustomCmd x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x1 x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 x1 x2) x3500

d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 :: Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20_dot___hash_lambda2 x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_unless (Curry_Prelude.d_OP_eq_eq x3 x2 x3500)) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn x3 x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x1 x2 x3500) x3500) x3500

d_OP_evalCustomCmd_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> C_GhciComm -> Curry_IO.C_Handle -> Curry_Time.C_CalendarTime -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_evalCustomCmd_dot___hash_lambda3 x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Time.d_C_calendarTimeToString x4 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_hPutStrLnGhci x2) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500) x3500) x3500) x3500) (d_OP_evalCustomCmd_dot_hPrintLinesBefore_dot_20 x3 x5 x3500) x3500)

d_C_hPutStrLnGhci :: C_GhciComm -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_hPutStrLnGhci x1 x2 x3500 = case x1 of
     (C_GhciComm x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_when x5) (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))) x2 x3500) x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x4 x2 x3500) (Curry_IO.d_C_hFlush x4 x3500) x3500) x3500
     (Choice_C_GhciComm x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hPutStrLnGhci x1002 x2 x3500) (d_C_hPutStrLnGhci x1003 x2 x3500)
     (Choices_C_GhciComm x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hPutStrLnGhci z x2 x3500) x1002
     (Guard_C_GhciComm x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hPutStrLnGhci x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_GhciComm x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3500) (d_OP__case_0 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1002 x3000 x3500) (nd_OP__case_0 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) (Curry_Prelude.d_C_return x1 x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x3 x4 x1002 x3500) (d_OP__case_2 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_hPutStrLnGhci x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x3500) (Curry_Prelude.d_C_return x1 x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 x2 x3 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_stopGhciComm x1 x3500) (d_C_initGhciComm x2 x3 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x2 x3 x1002 x3500) (d_OP__case_1 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (d_C_stopGhciComm x1 x3500) (d_C_initGhciComm x2 x3 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_1 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
