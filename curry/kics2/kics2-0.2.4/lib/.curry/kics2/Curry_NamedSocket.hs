{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_NamedSocket (C_Socket, d_C_listenOn, d_C_socketAccept, d_C_waitForSocketAccept, nd_C_waitForSocketAccept, d_C_sClose, d_C_socketName, d_C_connectToSocketRepeat, d_C_connectToSocketWait, d_C_connectToSocket) where

import Basics
import qualified Curry_CPNS
import qualified Curry_IO
import qualified Curry_Prelude
import qualified Curry_Socket
import qualified Curry_System
data C_Socket
     = C_NamedSocket (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Socket.C_Socket
     | Choice_C_Socket Cover ID C_Socket C_Socket
     | Choices_C_Socket Cover ID ([C_Socket])
     | Fail_C_Socket Cover FailInfo
     | Guard_C_Socket Cover Constraints C_Socket

instance Show C_Socket where
  showsPrec d (Choice_C_Socket cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Socket cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Socket cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Socket cd info) = showChar '!'
  showsPrec _ (C_NamedSocket x1 x2) = (showString "(NamedSocket") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read C_Socket where
  readsPrec d s = readParen (d > 10) (\r -> [ (C_NamedSocket x1 x2,r2) | (_,r0) <- readQualified "NamedSocket" "NamedSocket" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s


instance NonDet C_Socket where
  choiceCons = Choice_C_Socket
  choicesCons = Choices_C_Socket
  failCons = Fail_C_Socket
  guardCons = Guard_C_Socket
  try (Choice_C_Socket cd i x y) = tryChoice cd i x y
  try (Choices_C_Socket cd i xs) = tryChoices cd i xs
  try (Fail_C_Socket cd info) = Fail cd info
  try (Guard_C_Socket cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Socket cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Socket cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Socket cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Socket cd i _) = error ("NamedSocket.Socket.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Socket cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Socket cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_Socket where
  generate s = Choices_C_Socket defCover (freeID [2] s) [(C_NamedSocket (generate (leftSupply s)) (generate (rightSupply s)))]


instance NormalForm C_Socket where
  ($!!) cont (C_NamedSocket x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_NamedSocket y1 y2) cs) $!! x2) cs) $!! x1) cs
  ($!!) cont (Choice_C_Socket cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_Socket cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_Socket cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_Socket cd info) _ = failCons cd info
  ($##) cont (C_NamedSocket x1 x2) cs = ((\y1 cs -> ((\y2 cs -> cont (C_NamedSocket y1 y2) cs) $## x2) cs) $## x1) cs
  ($##) cont (Choice_C_Socket cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_Socket cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_Socket cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_Socket cd info) _ = failCons cd info
  searchNF search cont (C_NamedSocket x1 x2) = search (\y1 -> search (\y2 -> cont (C_NamedSocket y1 y2)) x2) x1
  searchNF _ _ x = error ("NamedSocket.Socket.searchNF: no constructor: " ++ (show x))


instance Unifiable C_Socket where
  (=.=) (C_NamedSocket x1 x2) (C_NamedSocket y1 y2) cs = (((x1 =:= y1) cs) & ((x2 =:= y2) cs)) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_NamedSocket x1 x2) (C_NamedSocket y1 y2) cs = (((x1 =:<= y1) cs) & ((x2 =:<= y2) cs)) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_NamedSocket x2 x3) = ((i :=: (ChooseN 0 2)):(concat [(bind (leftID i) x2),(bind (rightID i) x3)]))
  bind i (Choice_C_Socket cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_Socket cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_Socket cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Socket cd i _) = error ("NamedSocket.Socket.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Socket cd info) = [(Unsolvable info)]
  bind i (Guard_C_Socket cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_NamedSocket x2 x3) = [(i :=: (ChooseN 0 2)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2))),((rightID i) :=: (LazyBind (lazyBind (rightID i) x3)))]
  lazyBind i (Choice_C_Socket cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_Socket cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_Socket cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Socket cd i _) = error ("NamedSocket.Socket.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Socket cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_Socket cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_Socket where
  (=?=) (Choice_C_Socket cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Socket cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Socket cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_Socket cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_Socket cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Socket cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Socket cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_Socket cd info) _ = failCons cd info
  (=?=) (C_NamedSocket x1 x2) (C_NamedSocket y1 y2) cs = Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.=?= y2) cs) cs
  (<?=) (Choice_C_Socket cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Socket cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Socket cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_Socket cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_Socket cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Socket cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Socket cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_Socket cd info) _ = failCons cd info
  (<?=) (C_NamedSocket x1 x2) (C_NamedSocket y1 y2) cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 cs) (Curry_Prelude.d_OP_ampersand_ampersand ((x1 Curry_Prelude.=?= y1) cs) ((x2 Curry_Prelude.<?= y2) cs) cs) cs


instance Coverable C_Socket where
  cover (C_NamedSocket x1 x2) = C_NamedSocket (cover x1) (cover x2)
  cover (Choice_C_Socket cd i x y) = Choice_C_Socket (incCover cd) i (cover x) (cover y)
  cover (Choices_C_Socket cd i xs) = Choices_C_Socket (incCover cd) i (map cover xs)
  cover (Fail_C_Socket cd info) = Fail_C_Socket (incCover cd) info
  cover (Guard_C_Socket cd c e) = Guard_C_Socket (incCover cd) c (cover e)


d_C_listenOn :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO C_Socket
d_C_listenOn x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Socket.d_C_listenOnFresh x3500) (d_OP_listenOn_dot___hash_lambda1 x1) x3500

d_OP_listenOn_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Socket.C_Socket -> ConstStore -> Curry_Prelude.C_IO C_Socket
d_OP_listenOn_dot___hash_lambda1 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.d_OP_gt_gt (Curry_CPNS.d_C_registerPort x1 x3 (Curry_Prelude.C_Int 0#) x3500) (Curry_Prelude.d_C_return (C_NamedSocket x1 x4) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_listenOn_dot___hash_lambda1 x1 x1002 x3500) (d_OP_listenOn_dot___hash_lambda1 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_listenOn_dot___hash_lambda1 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_listenOn_dot___hash_lambda1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_socketAccept :: C_Socket -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)
d_C_socketAccept x1 x3500 = case x1 of
     (C_NamedSocket x2 x3) -> Curry_Socket.d_C_socketAccept x3 x3500
     (Choice_C_Socket x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_socketAccept x1002 x3500) (d_C_socketAccept x1003 x3500)
     (Choices_C_Socket x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_socketAccept z x3500) x1002
     (Guard_C_Socket x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_socketAccept x1002) $! (addCs x1001 x3500))
     (Fail_C_Socket x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_waitForSocketAccept :: C_Socket -> ConstStore -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle))
d_C_waitForSocketAccept x1 x3500 = case x1 of
     (C_NamedSocket x2 x3) -> Curry_Socket.d_C_waitForSocketAccept x3
     (Choice_C_Socket x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_waitForSocketAccept x1002 x3500) (d_C_waitForSocketAccept x1003 x3500)
     (Choices_C_Socket x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_waitForSocketAccept z x3500) x1002
     (Guard_C_Socket x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_waitForSocketAccept x1002) $! (addCs x1001 x3500))
     (Fail_C_Socket x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_waitForSocketAccept :: C_Socket -> IDSupply -> ConstStore -> Func Curry_Prelude.C_Int (Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_IO.C_Handle)))
nd_C_waitForSocketAccept x1 x3000 x3500 = case x1 of
     (C_NamedSocket x2 x3) -> wrapDX id (Curry_Socket.d_C_waitForSocketAccept x3)
     (Choice_C_Socket x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_waitForSocketAccept x1002 x3000 x3500) (nd_C_waitForSocketAccept x1003 x3000 x3500)
     (Choices_C_Socket x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_waitForSocketAccept z x3000 x3500) x1002
     (Guard_C_Socket x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_waitForSocketAccept x1002 x3000) $! (addCs x1001 x3500))
     (Fail_C_Socket x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_sClose :: C_Socket -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sClose x1 x3500 = case x1 of
     (C_NamedSocket x2 x3) -> Curry_Prelude.d_OP_gt_gt (Curry_Socket.d_C_sClose x3 x3500) (Curry_CPNS.d_C_unregisterPort x2 x3500) x3500
     (Choice_C_Socket x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_sClose x1002 x3500) (d_C_sClose x1003 x3500)
     (Choices_C_Socket x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_sClose z x3500) x1002
     (Guard_C_Socket x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_sClose x1002) $! (addCs x1001 x3500))
     (Fail_C_Socket x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_socketName :: C_Socket -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_socketName x1 x3500 = case x1 of
     (C_NamedSocket x2 x3) -> x2
     (Choice_C_Socket x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_socketName x1002 x3500) (d_C_socketName x1003 x3500)
     (Choices_C_Socket x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_socketName z x3500) x1002
     (Guard_C_Socket x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_socketName x1002) $! (addCs x1001 x3500))
     (Fail_C_Socket x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_connectToSocketRepeat :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> Curry_Prelude.C_IO t0 -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_IO.C_Handle)
d_C_connectToSocketRepeat x1 x2 x3 x4 x3500 = let
     x5 = d_OP__case_7 x1 x2 x3 x4 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Int 0#) x3500) x3500
     x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '@'#)) x3500) x4 x3500
     x7 = d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name x6 x3500
     x8 = d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost x6 x3500
     x9 = d_OP__case_6 x8 (Curry_Prelude.d_OP_eq_eq x8 Curry_Prelude.OP_List x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_CPNS.d_C_cpnsAlive x1 x9 x3500) (d_OP_connectToSocketRepeat_dot___hash_lambda2 x9 x7 x5) x3500)

d_OP_connectToSocketRepeat_dot_ms2s_dot_17 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_connectToSocketRepeat_dot_ms2s_dot_17 x1 x3500 = let
     x2 = Curry_Prelude.d_C_div x1 (Curry_Prelude.C_Int 1000#) x3500
      in (d_OP__case_5 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 0#) x3500) x3500)

d_OP_connectToSocketRepeat_dot_decr_dot_17 :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_connectToSocketRepeat_dot_decr_dot_17 x1 x3500 = d_OP__case_4 x1 (Curry_Prelude.d_OP_lt x1 (Curry_Prelude.C_Int 0#) x3500) x3500

d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name x1002 x3500) (d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocketRepeat_dot___hash_selFP2_hash_name x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost x1002 x3500) (d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocketRepeat_dot___hash_selFP3_hash_atHost x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_connectToSocketRepeat_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_IO.C_Handle) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_IO.C_Handle)
d_OP_connectToSocketRepeat_dot___hash_lambda2 x1 x2 x3 x4 x3500 = d_OP__case_3 x1 x2 x3 x4 (Curry_Prelude.d_C_not x4 x3500) x3500

d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_IO.C_Handle) -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_IO.C_Handle)
d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_2 x1 x2 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Int 0#) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x1002 x3500) (d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_connectToSocketWait :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_connectToSocketWait x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_connectToSocketRepeat (Curry_Prelude.C_Int 1000#) (Curry_Prelude.d_C_done x3500) (Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 1#) x3500) x1 x3500) d_OP_connectToSocketWait_dot___hash_lambda4 x3500

d_OP_connectToSocketWait_dot___hash_lambda4 :: Curry_Prelude.C_Maybe Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_OP_connectToSocketWait_dot___hash_lambda4 x1 x3500 = case x1 of
     (Curry_Prelude.C_Just x2) -> Curry_Prelude.d_C_return x2 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocketWait_dot___hash_lambda4 x1002 x3500) (d_OP_connectToSocketWait_dot___hash_lambda4 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocketWait_dot___hash_lambda4 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocketWait_dot___hash_lambda4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_connectToSocket :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_C_connectToSocket x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char '@'#)) x3500) x1 x3500
     x3 = d_OP_connectToSocket_dot___hash_selFP5_hash_name x2 x3500
     x4 = d_OP_connectToSocket_dot___hash_selFP6_hash_atHost x2 x3500
     x5 = d_OP__case_1 x4 (Curry_Prelude.d_OP_eq_eq x4 Curry_Prelude.OP_List x3500) x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_CPNS.d_C_getPortInfo x3 x5 x3500) (d_OP_connectToSocket_dot___hash_lambda5 x5 x3) x3500)

d_OP_connectToSocket_dot___hash_selFP5_hash_name :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_connectToSocket_dot___hash_selFP5_hash_name x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocket_dot___hash_selFP5_hash_name x1002 x3500) (d_OP_connectToSocket_dot___hash_selFP5_hash_name x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocket_dot___hash_selFP5_hash_name z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocket_dot___hash_selFP5_hash_name x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_connectToSocket_dot___hash_selFP6_hash_atHost :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_connectToSocket_dot___hash_selFP6_hash_atHost x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocket_dot___hash_selFP6_hash_atHost x1002 x3500) (d_OP_connectToSocket_dot___hash_selFP6_hash_atHost x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocket_dot___hash_selFP6_hash_atHost z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocket_dot___hash_selFP6_hash_atHost x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_connectToSocket_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_IO.C_Handle
d_OP_connectToSocket_dot___hash_lambda5 x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> Curry_Prelude.d_OP_gt_gt (d_OP__case_0 x1 x2 x4 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Int 0#) x3500) x3500) (Curry_Socket.d_C_connectToSocket x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_connectToSocket_dot___hash_lambda5 x1 x2 x1002 x3500) (d_OP_connectToSocket_dot___hash_lambda5 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_connectToSocket_dot___hash_lambda5 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_connectToSocket_dot___hash_lambda5 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))) x3500) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x4 x1002 x3500) (d_OP__case_0 x1 x2 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))) x3500) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_tail x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x1002 x3500) (d_OP__case_1 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_tail x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x4 x1002 x3000 x3500) (nd_OP__case_1 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Socket.d_C_connectToSocket x1 x4 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Just) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x4 x1002 x3500) (d_OP__case_2 x1 x2 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Socket.d_C_connectToSocket x1 x4 x3500) (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_return) (wrapDX id (acceptCs id Curry_Prelude.C_Just)) x2000 x3500) x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x4 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_CPNS.d_C_getPortInfo x2 x1 x3500) (d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x4 x1002 x3500) (d_OP__case_3 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_CPNS.d_C_getPortInfo x2 x1 x3500) (wrapDX id (d_OP_connectToSocketRepeat_dot___hash_lambda2_dot___hash_lambda3 x1 x3)) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3500) (d_OP__case_4 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_minus x1 (Curry_Prelude.C_Int 1#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x1002 x3000 x3500) (nd_OP__case_4 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x2 x1002 x3500) (d_OP__case_5 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Int 1#
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x2 x1002 x3000 x3500) (nd_OP__case_5 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x8 x9 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_tail x8 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x8 x1002 x3500) (d_OP__case_6 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x8 x9 x3000 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_tail x8 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x8 x1002 x3000 x3500) (nd_OP__case_6 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt x2 (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_sleep (d_OP_connectToSocketRepeat_dot_ms2s_dot_17 x1 x3500) x3500) (d_C_connectToSocketRepeat x1 x2 (d_OP_connectToSocketRepeat_dot_decr_dot_17 x3 x3500) x4 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x4 x1002 x3500) (d_OP__case_7 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt x2 (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_sleep (d_OP_connectToSocketRepeat_dot_ms2s_dot_17 x1 x3500) x3500) (d_C_connectToSocketRepeat x1 x2 (d_OP_connectToSocketRepeat_dot_decr_dot_17 x3 x3500) x4 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
