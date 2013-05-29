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
  generate s = Choices_C_Queue defCover (freeID [4] s) [(C_S (generate (leftSupply (leftSupply s))) (generate (rightSupply (leftSupply s))) (generate (leftSupply (rightSupply s))) (generate (rightSupply (rightSupply s))))]


instance NormalForm t0 => NormalForm (C_Queue t0) where
  ($!!) cont (C_S x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_S y1 y2 y3 y4) cs) $!! x4) cs) $!! x3) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Queue cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Queue cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Queue cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Queue cd info) _ = failCons cd info
  ($##) cont (C_S x1 x2 x3 x4) cs = ((\y1 cs -> ((\y2 cs -> ((\y3 cs -> ((\y4 cs -> cont (C_S y1 y2 y3 y4) cs) $## x4) cs) $## x3) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Queue cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Queue cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Queue cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Queue cd info) _ = failCons cd info
  searchNF search cont (C_S x1 x2 x3 x4) = search (\y1 -> search (\y2 -> search (\y3 -> search (\y4 -> cont (C_S y1 y2 y3 y4)) x4) x3) x2) x1
  searchNF _ _ x = error ("Dequeue.Queue.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Queue t0) where
  (=.=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) cs = (((x1 =:= y1) cs) & ((((x2 =:= y2) cs) & ((((x3 =:= y3) cs) & ((x4 =:= y4) cs)) cs)) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) cs = (((x1 =:<= y1) cs) & ((((x2 =:<= y2) cs) & ((((x3 =:<= y3) cs) & ((x4 =:<= y4) cs)) cs)) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_S x2 x3 x4 x5) = ((i :=: (ChooseN 0 4)):(concat [(bind (leftID (leftID i)) x2),(bind (rightID (leftID i)) x3),(bind (leftID (rightID i)) x4),(bind (rightID (rightID i)) x5)]))
  bind i (Choice_C_Queue cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Queue cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Queue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Queue cd i _) = error ("Dequeue.Queue.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Queue cd info) = [(Unsolvable info)]
  bind i (Guard_C_Queue cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_S x2 x3 x4 x5) = [(i :=: (ChooseN 0 4)),((leftID (leftID i)) :=: (LazyBind (lazyBind (leftID (leftID i)) x2))),((rightID (leftID i)) :=: (LazyBind (lazyBind (rightID (leftID i)) x3))),((leftID (rightID i)) :=: (LazyBind (lazyBind (leftID (rightID i)) x4))),((rightID (rightID i)) :=: (LazyBind (lazyBind (rightID (rightID i)) x5)))]
  lazyBind i (Choice_C_Queue cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Queue cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Queue cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Queue cd i _) = error ("Dequeue.Queue.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Queue cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Queue cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Queue t0) where
  (=?=) (Choice_C_Queue cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Queue cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Queue cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Queue cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Queue cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Queue cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Queue cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Queue cd info) _ = failCons cd info
  (=?=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.=?= y4) cs) cs) cs) cs
  (<?=) (Choice_C_Queue cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Queue cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Queue cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Queue cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Queue cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Queue cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Queue cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Queue cd info) _ = failCons cd info
  (<?=) (C_S x1 x2 x3 x4) (C_S y1 y2 y3 y4) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x2 y2 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x2 Curry_Prelude.=?= y2) cs) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x3 y3 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x3 Curry_Prelude.=?= y3) cs) ((x4 Curry_Prelude.<?= y4) cs) cs) cs) cs) cs) cs) cs


instance Coverable t0 => Coverable (C_Queue t0) where
  cover (C_S x1 x2 x3 x4) = C_S (cover x1) (cover x2) (cover x3) (cover x4)
  cover (Choice_C_Queue cd i x y) = Choice_C_Queue (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Queue cd i xs) = Choices_C_Queue (incCover cd) i (map cover xs)
  cover (Fail_C_Queue cd info) = Fail_C_Queue (incCover cd) info
  cover (Guard_C_Queue cd c e) = Guard_C_Queue (incCover cd) c (cover e)


d_C_empty :: Curry_Prelude.Curry t0 => ConstStore -> C_Queue t0
d_C_empty x3500 = C_S (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List

d_C_isEmpty :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> Curry_Prelude.C_Bool
d_C_isEmpty x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_OP_plus x2 x4 x3500) (Curry_Prelude.C_Int 0#) x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isEmpty x1002 x3500) (d_C_isEmpty x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isEmpty z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isEmpty x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqHead :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> t0
d_C_deqHead x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_C_head (d_OP__case_11 x2 x3 x5 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500) x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqHead x1002 x3500) (d_C_deqHead x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqHead z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqHead x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqLast :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> t0
d_C_deqLast x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_C_head (d_OP__case_10 x3 x4 x5 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Int 0#) x3500) x3500) x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqLast x1002 x3500) (d_C_deqLast x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqLast z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqLast x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_cons :: Curry_Prelude.Curry t0 => t0 -> C_Queue t0 -> ConstStore -> C_Queue t0
d_C_cons x1 x2 x3500 = case x2 of
     (C_S x3 x4 x5 x6) -> d_C_check (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.OP_Cons x1 x4) x5 x6 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_cons x1 x1002 x3500) (d_C_cons x1 x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_cons x1 z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_cons x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqTail :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> C_Queue t0
d_C_deqTail x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_9 x2 x4 x5 x3 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqTail x1002 x3500) (d_C_deqTail x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqTail z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqTail x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_snoc :: Curry_Prelude.Curry t0 => t0 -> C_Queue t0 -> ConstStore -> C_Queue t0
d_C_snoc x1 x2 x3500 = case x2 of
     (C_S x3 x4 x5 x6) -> d_C_deqReverse (d_C_check (Curry_Prelude.d_OP_plus x5 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.OP_Cons x1 x6) x3 x4 x3500) x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_snoc x1 x1002 x3500) (d_C_snoc x1 x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_snoc x1 z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_snoc x1 x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqInit :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> C_Queue t0
d_C_deqInit x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_8 x2 x3 x4 x5 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqInit x1002 x3500) (d_C_deqInit x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqInit z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqInit x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqReverse :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> C_Queue t0
d_C_deqReverse x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> C_S x4 x5 x2 x3
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqReverse x1002 x3500) (d_C_deqReverse x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqReverse z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqReverse x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_check :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List t0 -> ConstStore -> C_Queue t0
d_C_check x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.d_OP_plus x1 x3 x3500
     x6 = Curry_Prelude.d_C_div x5 (Curry_Prelude.C_Int 2#) x3500
     x7 = Curry_Prelude.d_OP_minus x5 x6 x3500
     x8 = Curry_Prelude.d_C_splitAt x6 x2 x3500
     x9 = d_OP_check_dot___hash_selFP2_hash_f' x8 x3500
     x10 = d_OP_check_dot___hash_selFP3_hash_rf' x8 x3500
     x11 = Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x10 x3500) x3500
      in (d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 (Curry_Prelude.d_OP_lt_eq x1 (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star (Curry_Prelude.C_Int 3#) x3 x3500) (Curry_Prelude.C_Int 1#) x3500) x3500) x3500)

d_OP_check_dot___hash_selFP2_hash_f' :: Curry_Prelude.Curry t44 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t44) (Curry_Prelude.OP_List t44) -> ConstStore -> Curry_Prelude.OP_List t44
d_OP_check_dot___hash_selFP2_hash_f' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_check_dot___hash_selFP2_hash_f' x1002 x3500) (d_OP_check_dot___hash_selFP2_hash_f' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_check_dot___hash_selFP2_hash_f' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_check_dot___hash_selFP2_hash_f' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_check_dot___hash_selFP3_hash_rf' :: Curry_Prelude.Curry t44 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t44) (Curry_Prelude.OP_List t44) -> ConstStore -> Curry_Prelude.OP_List t44
d_OP_check_dot___hash_selFP3_hash_rf' x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_check_dot___hash_selFP3_hash_rf' x1002 x3500) (d_OP_check_dot___hash_selFP3_hash_rf' x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_check_dot___hash_selFP3_hash_rf' z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_check_dot___hash_selFP3_hash_rf' x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_listToDeq :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> C_Queue t0
d_C_listToDeq x1 x3500 = d_C_check (Curry_Prelude.d_C_length x1 x3500) x1 (Curry_Prelude.C_Int 0#) Curry_Prelude.OP_List x3500

d_C_deqToList :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> Curry_Prelude.OP_List t0
d_C_deqToList x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x5 x3500) x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqToList x1002 x3500) (d_C_deqToList x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqToList z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqToList x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deqLength :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> Curry_Prelude.C_Int
d_C_deqLength x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> Curry_Prelude.d_OP_plus x2 x4 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deqLength x1002 x3500) (d_C_deqLength x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deqLength z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deqLength x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_rotate :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> C_Queue t0
d_C_rotate x1 x3500 = d_C_snoc (d_C_deqHead x1 x3500) (d_C_deqTail x1 x3500) x3500

d_C_matchHead :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_C_matchHead x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_5 x2 x4 x5 x3 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_matchHead x1002 x3500) (d_C_matchHead x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_matchHead z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_matchHead x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_matchLast :: Curry_Prelude.Curry t0 => C_Queue t0 -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 (C_Queue t0))
d_C_matchLast x1 x3500 = case x1 of
     (C_S x2 x3 x4 x5) -> d_OP__case_2 x2 x3 x4 x5 x3500
     (Choice_C_Queue x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_matchLast x1002 x3500) (d_C_matchLast x1003 x3500)
     (Choices_C_Queue x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_matchLast z x3500) x1002
     (Guard_C_Queue x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_matchLast x1002) $! (addCs x1001 x3500))
     (Fail_C_Queue x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_1 x3 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3500) x9 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x3 x4 x1002 x3500) (d_OP__case_2 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x3 x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3500) x9 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_2 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_0 x6 x7 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3500) (d_OP__case_1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1002 x3000 x3500) (nd_OP__case_1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x6 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x1002 x3500) (d_OP__case_0 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x6 x1002 x3000 x3500) (nd_OP__case_0 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x4 x5 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_OP__case_4 x5 x3500
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x4 x5 x1002 x3500) (d_OP__case_5 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x4 x5 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x5 x2000 x3500))
     (Curry_Prelude.OP_Cons x8 x9) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x8 (d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x9 x3500) x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_5 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_3 x6 x7 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3500) (d_OP__case_4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1002 x3000 x3500) (nd_OP__case_4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x6 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x6 x1002 x3500) (d_OP__case_3 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 (d_C_empty x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x6 x1002 x3000 x3500) (nd_OP__case_3 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x1 x2 x3 x4
     Curry_Prelude.C_False -> d_OP__case_6 x6 x7 x9 x11 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1002 x3500) (d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x1 x2 x3 x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x6 x7 x9 x11 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x4 x6 x7 x9 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x6 x7 x9 x11 x12 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x6 x9 x7 x11
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x6 x7 x9 x11 x1002 x3500) (d_OP__case_6 x6 x7 x9 x11 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x6 x7 x9 x11 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x6 x7 x9 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x6 x7 x9 x11 x12 x3000 x3500 = case x12 of
     Curry_Prelude.C_True -> C_S x6 x9 x7 x11
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x6 x7 x9 x11 x1002 x3000 x3500) (nd_OP__case_6 x6 x7 x9 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x6 x7 x9 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x6 x7 x9 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_C_empty x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3500) x7 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x3 x4 x1002 x3500) (d_OP__case_8 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_C_empty x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_check x2 x3 (Curry_Prelude.d_OP_minus x4 (Curry_Prelude.C_Int 1#) x3500) x7 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_8 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x2 x4 x5 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_C_empty x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x7 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x2 x4 x5 x1002 x3500) (d_OP__case_9 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x2 x4 x5 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> d_C_empty x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_C_deqReverse (d_C_check x4 x5 (Curry_Prelude.d_OP_minus x2 (Curry_Prelude.C_Int 1#) x3500) x7 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_9 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x3 x4 x5 x1002 x3500) (d_OP__case_10 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> x5
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_10 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x2 x3 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x3 x5 x1002 x3500) (d_OP__case_11 x2 x3 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 x3 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x3 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x2 x3 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x5
     Curry_Prelude.C_False -> x3
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x2 x3 x5 x1002 x3000 x3500) (nd_OP__case_11 x2 x3 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x2 x3 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x2 x3 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
