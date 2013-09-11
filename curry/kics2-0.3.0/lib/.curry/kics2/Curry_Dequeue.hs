{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Dequeue (C_Queue, d_C_empty, d_C_isEmpty, d_C_deqHead, d_C_deqLast, d_C_cons, d_C_deqTail, d_C_snoc, d_C_deqInit, d_C_deqReverse, d_C_listToDeq, d_C_deqToList, d_C_deqLength, d_C_rotate, d_C_matchHead, d_C_matchLast) where

import Basics
import qualified Curry_Prelude
data C_Queue t0
     = C_S Curry_Prelude.C_Int (Curry_Prelude.OP_List t0) Curry_Prelude.C_Int (Curry_Prelude.OP_List t0)
     | Choice_C_Queue Cover ID (C_Queue t0) (C_Queue t0)
     | Choices_C_Queue Cover ID ([C_Queue t0])
     | Fail_C_Queue Cover FailInfo
     | Guard_C_Queue Cover Constraints (C_Queue t0)

instance Show t0 => Show (C_Queue t0) where
  showsPrec d (Choice_C_Queue cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Queue cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Queue cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Queue cd info) = showChar '!'
  showsPrec _ (C_S x1 x2 x3 x4) = (showString "(S") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . ((showChar ' ') . ((shows x3) . ((showChar ' ') . ((shows x4) . (showChar ')')))))))))


instance Read t0 => Read (C_Queue t0) where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_S x1 x2 x3 x4,r4) | (_,r0) <- readQualified "Dequeue" "S" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1, (x3,r3) <- readsPrec 11 r2, (x4,r4) <- readsPrec 11 r3]) s


instance NonDet (C_Queue t0) where
  choiceCons = Choice_C_Queue
  choicesCons = Choices_C_Queue
  failCons = Fail_C_Queue
  guardCons = Guard_C_Queue
  try (Choice_C_Queue cd i x y) = tryChoice cd i x y
  try (Choices_C_Queue cd i xs) = tryChoices cd i xs
  try (Fail_C_Queue cd info) = Fail cd info
  try (Guard_C_Queue cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Queue cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Queue cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Queue cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Queue cd i _) = error ("Dequeue.Queue.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Queue cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Queue cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Queue t0) where
  generate s c = Choices_C_Queue c (freeID [4] s) [(C_S (generate (leftSupply (leftSupply s)) c) (generate (rightSupply (leftSupply s)) c) (generate (leftSupply (rightSupply s)) c) (generate (rightSupply (rightSupply s)) c))]


instance NormalForm t0 => NormalForm (C_Queue t0) where
  ($!!) cont (C_S x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_S y1 y2 y3 y4) d cs) $!! x4) d) cs) $!! x3) d) cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Queue cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Queue cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Queue cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Queue cd info) _ _ = failCons cd info
  ($##) cont (C_S x1 x2 x3 x4) d cs = (((\y1 d cs -> (((\y2 d cs -> (((\y3 d cs -> (((\y4 d cs -> cont (C_S y1 y2 y3 y4) d cs) $## x4) d) cs) $## x3) d) cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Queue cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Queue cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Queue cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Queue cd info) _ _ = failCons cd info
  searchNF search cont (C_S x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_S y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("Dequeue.Queue.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Queue t0) where
  (=.=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) d cs = (((((x1 =:= y1) d) cs) & ((((((x2 =:= y2) d) cs) & ((((((x3 =:= y3) d) cs) & (((x4 =:= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) d cs = (((((x1 =:<= y1) d) cs) & ((((((x2 =:<= y2) d) cs) & ((((((x3 =:<= y3) d) cs) & (((x4 =:<= y4) d) cs)) d) cs)) d) cs)) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_S x3 x4 x5 x6) = ((i :=: (ChooseN 0 4)):(concat [(bind cd (leftID (leftID i)) x3),(bind cd (rightID (leftID i)) x4),(bind cd (leftID (rightID i)) x5),(bind cd (rightID (rightID i)) x6)]))
  bind d i (Choice_C_Queue cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Queue cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Queue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Queue cd i _) = error ("Dequeue.Queue.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Queue cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Queue cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_S x3 x4 x5 x6) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind cd (leftID (leftID i)) x3))),((rightID (leftID i)) :=: (LazyBind (lazyBind cd (rightID (leftID i)) x4))),((leftID (rightID i)) :=: (LazyBind (lazyBind cd (leftID (rightID i)) x5))),((rightID (rightID i)) :=: (LazyBind (lazyBind cd (rightID (rightID i)) x6)))]
  lazyBind d i (Choice_C_Queue cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Queue cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Queue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Queue cd i _) = error ("Dequeue.Queue.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Queue cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Queue cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Queue t0) where
  (=?=) (Choice_C_Queue cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Queue cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Queue cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Queue cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Queue cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Queue cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Queue cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Queue cd info) _ _ = failCons cd info
  (=?=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.=?= y4) d) cs) d cs) d cs) d cs
  (<?=) (Choice_C_Queue cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Queue cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Queue cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Queue cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Queue cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Queue cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Queue cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Queue cd info) _ _ = failCons cd info
  (<?=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x1 Curry_Prelude.=?= y1) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x2 Curry_Prelude.=?= y2) d) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 d cs) (Curry_Prelude.d_OP_ampersand_ampersand (((x3 Curry_Prelude.=?= y3) d) cs) (((x4 Curry_Prelude.<?= y4) d) cs) d cs) d cs) d cs) d cs) d cs) d cs


d_C_empty :: Curry_Prelude.Curry t0 => Cover -> ConstStore -> C_Queue t0
d_C_empty x3250 x3500 = C_S (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List

d_C_isEmpty :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmpty x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_OP_plus x2 x4 x3250 x3500) (Curry_Prelude.C_Int 0#) x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isEmpty x1002 x3250 x3500) (d_C_isEmpty x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isEmpty z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isEmpty x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqHead :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> t0
d_C_deqHead x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_C_head (d_OP__case_11 x2 x3 x5 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqHead x1002 x3250 x3500) (d_C_deqHead x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqHead z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqHead x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqLast :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> t0
d_C_deqLast x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_C_head (d_OP__case_10 x4 x5 x3 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqLast x1002 x3250 x3500) (d_C_deqLast x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqLast z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqLast x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_cons :: Curry_Prelude.Curry t0 => t0 -> C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_cons x1 x2 x3250 x3500 = case x2 of
     (C_S x3 x4 x5 x6) -> d_C_check (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.OP_Cons x1 x4) x5 x6 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cons x1 x1002 x3250 x3500) (d_C_cons x1 x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cons x1 z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cons x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqTail :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_deqTail x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_9 x2 x5 x4 x3 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqTail x1002 x3250 x3500) (d_C_deqTail x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqTail z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqTail x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_snoc :: Curry_Prelude.Curry t0 => t0 -> C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_snoc x1 x2 x3250 x3500 = case x2 of
     (C_S x3 x4 x5 x6) -> d_C_deqReverse (d_C_check (Curry_Prelude.d_OP_plus x5 (Curry_Prelude.C_Int 1#) x3250 x3500) (Curry_Prelude.OP_Cons x1 x6) x3 x4 x3250 x3500) x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_snoc x1 x1002 x3250 x3500) (d_C_snoc x1 x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_snoc x1 z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_snoc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqInit :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_deqInit x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_8 x4 x3 x2 x5 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqInit x1002 x3250 x3500) (d_C_deqInit x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqInit z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqInit x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqReverse :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_deqReverse x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> C_S x4 x5 x2 x3
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqReverse x1002 x3250 x3500) (d_C_deqReverse x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqReverse z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqReverse x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_check :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Queue t0
d_C_check x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_OP_plus x1 x3 x3250 x3500
     x6 = Curry_Prelude.d_C_div x5 (Curry_Prelude.C_Int 2#) x3250 x3500
     x7 = Curry_Prelude.d_OP_minus x5 x6 x3250 x3500
     x8 = Curry_Prelude.d_C_splitAt x6 x2 x3250 x3500
     x9 = d_OP_check_dot___hash_selFP2_hash_f' x8 x3250 x3500
     x10 = d_OP_check_dot___hash_selFP3_hash_rf' x8 x3250 x3500
     x11 = Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x10 x3250 x3500) x3250 x3500
      in (d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 (Curry_Prelude.d_OP_lt_eq x1 (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 3#) x3 x3250 x3500) (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500)

d_OP_check_dot___hash_selFP2_hash_f' :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_check_dot___hash_selFP2_hash_f' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_check_dot___hash_selFP2_hash_f' x1002 x3250 x3500) (d_OP_check_dot___hash_selFP2_hash_f' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_check_dot___hash_selFP2_hash_f' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_check_dot___hash_selFP2_hash_f' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_check_dot___hash_selFP3_hash_rf' :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_List t0) -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_check_dot___hash_selFP3_hash_rf' x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_check_dot___hash_selFP3_hash_rf' x1002 x3250 x3500) (d_OP_check_dot___hash_selFP3_hash_rf' x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_check_dot___hash_selFP3_hash_rf' z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_check_dot___hash_selFP3_hash_rf' x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_listToDeq :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Queue t0
d_C_listToDeq x1 x3250 x3500 = d_C_check (Curry_Prelude.d_C_length x1 x3250 x3500) x1 (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List x3250 x3500

d_C_deqToList :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_deqToList x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x5 x3250 x3500) x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqToList x1002 x3250 x3500) (d_C_deqToList x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqToList z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqToList x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_deqLength :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> Curry_Prelude.C_Int
d_C_deqLength x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_plus x2 x4 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqLength x1002 x3250 x3500) (d_C_deqLength x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqLength z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqLength x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_rotate :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> C_Queue t0
d_C_rotate x1 x3250 x3500 = d_C_snoc (d_C_deqHead x1 x3250 x3500) (d_C_deqTail x1 x3250 x3500) x3250 x3500

d_C_matchHead :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_C_matchHead x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_5 x2 x5 x4 x3 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_matchHead x1002 x3250 x3500) (d_C_matchHead x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_matchHead z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_matchHead x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_matchLast :: Curry_Prelude.Curry t0 => C_Queue t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_C_matchLast x1 x3250 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_2 x4 x3 x2 x5 x3250 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_matchLast x1002 x3250 x3500) (d_C_matchLast x1003 x3250 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_matchLast z x3250 x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_matchLast x1002 x3250) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_2 x4 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_1 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3250 x3500) x9 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x3 x2 x1002 x3250 x3500) (d_OP__case_2 x4 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_0 x6 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3250 x3500) (d_OP__case_1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_0 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x1002 x3250 x3500) (d_OP__case_0 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_5 x2 x5 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_4 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x9 x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x5 x4 x1002 x3250 x3500) (d_OP__case_5 x2 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_3 x6 x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3250 x3500) (d_OP__case_4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_OP__case_3 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x6 x1002 x3250 x3500) (d_OP__case_3 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Queue t0
d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x1 x2 x3 x4
     Curry_Prelude.C_False -> d_OP__case_6 x11 x7 x9 x6 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 x1002 x3250 x3500) (d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x3 x1 x11 x7 x9 x6 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> C_Queue t0
d_OP__case_6 x11 x7 x9 x6 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x6 x9 x7 x11
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x11 x7 x9 x6 x1002 x3250 x3500) (d_OP__case_6 x11 x7 x9 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x11 x7 x9 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x11 x7 x9 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Queue t0
d_OP__case_8 x4 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_C_empty x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3250 x3500) x7 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x4 x3 x2 x1002 x3250 x3500) (d_OP__case_8 x4 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x4 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x4 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> C_Queue t0
d_OP__case_9 x2 x5 x4 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_C_empty x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3250 x3500) x7 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x2 x5 x4 x1002 x3250 x3500) (d_OP__case_9 x2 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x2 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x2 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_10 x4 x5 x3 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x4 x5 x3 x1002 x3250 x3500) (d_OP__case_10 x4 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x4 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x4 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP__case_11 x2 x3 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x3 x5 x1002 x3250 x3500) (d_OP__case_11 x2 x3 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 x3 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x3 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
