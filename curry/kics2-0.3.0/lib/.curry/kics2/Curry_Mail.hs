{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Mail (C_MailOption (..), d_C_sendMail, nd_C_sendMail, d_C_sendMailWithOptions) where

import Basics
import qualified Curry_IO
import qualified Curry_IOExts
import qualified Curry_List
import qualified Curry_Prelude
data C_MailOption
     = C_CC (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_BCC (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | C_TO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
     | Choice_C_MailOption Cover ID C_MailOption C_MailOption
     | Choices_C_MailOption Cover ID ([C_MailOption])
     | Fail_C_MailOption Cover FailInfo
     | Guard_C_MailOption Cover Constraints C_MailOption

instance Show C_MailOption where
  showsPrec d (Choice_C_MailOption cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_MailOption cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_MailOption cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_MailOption cd info) = showChar '!'
  showsPrec _ (C_CC x1) = (showString "(CC") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_BCC x1) = (showString "(BCC") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec _ (C_TO x1) = (showString "(TO") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_MailOption where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_CC x1,r1) | (_,r0) <- readQualified "Mail" "CC" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen (d > 10) (\r -> [ (C_BCC x1,r1) | (_,r0) <- readQualified "Mail" "BCC" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_TO x1,r1) | (_,r0) <- readQualified "Mail" "TO" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_MailOption where
  choiceCons = Choice_C_MailOption
  choicesCons = Choices_C_MailOption
  failCons = Fail_C_MailOption
  guardCons = Guard_C_MailOption
  try (Choice_C_MailOption cd i x y) = tryChoice cd i x y
  try (Choices_C_MailOption cd i xs) = tryChoices cd i xs
  try (Fail_C_MailOption cd info) = Fail cd info
  try (Guard_C_MailOption cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_MailOption cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_MailOption cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_MailOption cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_MailOption cd i _) = error ("Mail.MailOption.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_MailOption cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_MailOption cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable C_MailOption where
  generate s c = Choices_C_MailOption c (freeID [1,1,1] s) [(C_CC (generate (leftSupply s) c)),(C_BCC (generate (leftSupply s) c)),(C_TO (generate (leftSupply s) c))]


instance NormalForm C_MailOption where
  ($!!) cont (C_CC x1) d cs = (((\y1 d cs -> cont (C_CC y1) d cs) $!! x1) d) cs
  ($!!) cont (C_BCC x1) d cs = (((\y1 d cs -> cont (C_BCC y1) d cs) $!! x1) d) cs
  ($!!) cont (C_TO x1) d cs = (((\y1 d cs -> cont (C_TO y1) d cs) $!! x1) d) cs
  ($!!) cont (Choice_C_MailOption cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_MailOption cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_MailOption cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_MailOption cd info) _ _ = failCons cd info
  ($##) cont (C_CC x1) d cs = (((\y1 d cs -> cont (C_CC y1) d cs) $## x1) d) cs
  ($##) cont (C_BCC x1) d cs = (((\y1 d cs -> cont (C_BCC y1) d cs) $## x1) d) cs
  ($##) cont (C_TO x1) d cs = (((\y1 d cs -> cont (C_TO y1) d cs) $## x1) d) cs
  ($##) cont (Choice_C_MailOption cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_MailOption cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_MailOption cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_MailOption cd info) _ _ = failCons cd info
  searchNF search cont (C_CC x1) = search (\y1 -> cont (C_CC y1)) x1
  searchNF search cont (C_BCC x1) = search (\y1 -> cont (C_BCC y1)) x1
  searchNF search cont (C_TO x1) = search (\y1 -> cont (C_TO y1)) x1
  searchNF _ _ x = error ("Mail.MailOption.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MailOption where
  (=.=) (C_CC x1) (C_CC y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_BCC x1) (C_BCC y1) d cs = ((x1 =:= y1) d) cs
  (=.=) (C_TO x1) (C_TO y1) d cs = ((x1 =:= y1) d) cs
  (=.=) _ _ d _ = Fail_C_Success d defFailInfo
  (=.<=) (C_CC x1) (C_CC y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_BCC x1) (C_BCC y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) (C_TO x1) (C_TO y1) d cs = ((x1 =:<= y1) d) cs
  (=.<=) _ _ d _ = Fail_C_Success d defFailInfo
  bind cd i (C_CC x3) = ((i :=: (ChooseN 0 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_BCC x3) = ((i :=: (ChooseN 1 1)):(concat [(bind cd (leftID i) x3)]))
  bind cd i (C_TO x3) = ((i :=: (ChooseN 2 1)):(concat [(bind cd (leftID i) x3)]))
  bind d i (Choice_C_MailOption cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_MailOption cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_MailOption cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_MailOption cd i _) = error ("Mail.MailOption.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_MailOption cd info) = [(Unsolvable info)]
  bind d i (Guard_C_MailOption cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i (C_CC x3) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_BCC x3) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind cd i (C_TO x3) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3)))]
  lazyBind d i (Choice_C_MailOption cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_MailOption cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_MailOption cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_MailOption cd i _) = error ("Mail.MailOption.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_MailOption cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_MailOption cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry C_MailOption where
  (=?=) (Choice_C_MailOption cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_MailOption cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_MailOption cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_MailOption cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_MailOption cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_MailOption cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_MailOption cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_MailOption cd info) _ _ = failCons cd info
  (=?=) (C_CC x1) (C_CC y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_BCC x1) (C_BCC y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) (C_TO x1) (C_TO y1) d cs = ((x1 Curry_Prelude.=?= y1) d) cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MailOption cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_MailOption cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_MailOption cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_MailOption cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_MailOption cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_MailOption cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_MailOption cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_MailOption cd info) _ _ = failCons cd info
  (<?=) (C_CC x1) (C_CC y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_CC _) (C_BCC _) _ _ = Curry_Prelude.C_True
  (<?=) (C_CC _) (C_TO _) _ _ = Curry_Prelude.C_True
  (<?=) (C_BCC x1) (C_BCC y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) (C_BCC _) (C_TO _) _ _ = Curry_Prelude.C_True
  (<?=) (C_TO x1) (C_TO y1) d cs = ((x1 Curry_Prelude.<?= y1) d) cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_sendMail :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sendMail x1 x2 x3 x3250 x3500 = d_C_sendMailWithOptions x1 x3 (Curry_Prelude.OP_Cons (C_TO x2) Curry_Prelude.OP_List)

nd_C_sendMail :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_sendMail x1 x2 x3 x3000 x3250 x3500 = wrapDX id (d_C_sendMailWithOptions x1 x3 (Curry_Prelude.OP_Cons (C_TO x2) Curry_Prelude.OP_List))

d_C_sendMailWithOptions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MailOption -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sendMailWithOptions x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda3) Curry_Prelude.OP_List x3 x3250 x3500) x3250 x3500) x3250 x3500
     x6 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda7) Curry_Prelude.OP_List x3 x3250 x3500) x3250 x3500) x3250 x3500
     x7 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda11) Curry_Prelude.OP_List x3 x3250 x3500) x3250 x3500) x3250 x3500
      in (d_C_execMailCmd (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_OP__case_1 x7 (Curry_Prelude.d_C_null x7 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_0 x6 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500) x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x4 x3250 x3500)

d_OP_sendMailWithOptions_dot___hash_lambda3 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda3 x1 x2 x3250 x3500 = case x1 of
     (C_TO x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_CC x4) -> x2
     (C_BCC x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda3 x1002 x2 x3250 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda3 x1003 x2 x3250 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda3 z x2 x3250 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda3 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_sendMailWithOptions_dot___hash_lambda7 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda7 x1 x2 x3250 x3500 = case x1 of
     (C_CC x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_BCC x4) -> x2
     (C_TO x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda7 x1002 x2 x3250 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda7 x1003 x2 x3250 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda7 z x2 x3250 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda7 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_sendMailWithOptions_dot___hash_lambda11 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda11 x1 x2 x3250 x3500 = case x1 of
     (C_BCC x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_CC x4) -> x2
     (C_TO x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda11 x1002 x2 x3250 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda11 x1003 x2 x3250 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda11 z x2 x3250 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda11 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_execMailCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_execMailCmd x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_execCmd x1 x3250 x3500) (d_OP_execMailCmd_dot___hash_lambda13 x2) x3250 x3500

d_OP_execMailCmd_dot_isUnixChar_dot_12 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_execMailCmd_dot_isUnixChar_dot_12 x1 x3250 x3500 = Curry_Prelude.d_OP_slash_eq x1 (Curry_Prelude.C_Char '\r'#) x3250 x3500

d_OP_execMailCmd_dot___hash_lambda13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_execMailCmd_dot___hash_lambda13 x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple3 x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x3 (Curry_Prelude.d_C_filter d_OP_execMailCmd_dot_isUnixChar_dot_12 x1 x3250 x3500) x3250 x3500) (Curry_IO.d_C_hClose x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_execMailCmd_dot___hash_lambda13 x1 x1002 x3250 x3500) (d_OP_execMailCmd_dot___hash_lambda13 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_execMailCmd_dot___hash_lambda13 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_execMailCmd_dot___hash_lambda13 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_0 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x1002 x3250 x3500) (d_OP__case_0 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_1 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x7 x1002 x3250 x3500) (d_OP__case_1 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
