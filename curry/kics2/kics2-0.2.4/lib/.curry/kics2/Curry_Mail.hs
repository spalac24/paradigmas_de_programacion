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
  generate s = Choices_C_MailOption defCover (freeID [1,1,1] s) [(C_CC (generate (leftSupply s))),(C_BCC (generate (leftSupply s))),(C_TO (generate (leftSupply s)))]


instance NormalForm C_MailOption where
  ($!!) cont (C_CC x1) cs = ((\y1 cs -> cont (C_CC y1) cs) $!! x1) cs
  ($!!) cont (C_BCC x1) cs = ((\y1 cs -> cont (C_BCC y1) cs) $!! x1) cs
  ($!!) cont (C_TO x1) cs = ((\y1 cs -> cont (C_TO y1) cs) $!! x1) cs
  ($!!) cont (Choice_C_MailOption cd i x y) cs = nfChoice cont cd i x y cs
  ($!!) cont (Choices_C_MailOption cd i xs) cs = nfChoices cont cd i xs cs
  ($!!) cont (Guard_C_MailOption cd c e) cs = guardCons cd c ((cont $!! e) (addCs c cs))
  ($!!) _ (Fail_C_MailOption cd info) _ = failCons cd info
  ($##) cont (C_CC x1) cs = ((\y1 cs -> cont (C_CC y1) cs) $## x1) cs
  ($##) cont (C_BCC x1) cs = ((\y1 cs -> cont (C_BCC y1) cs) $## x1) cs
  ($##) cont (C_TO x1) cs = ((\y1 cs -> cont (C_TO y1) cs) $## x1) cs
  ($##) cont (Choice_C_MailOption cd i x y) cs = gnfChoice cont cd i x y cs
  ($##) cont (Choices_C_MailOption cd i xs) cs = gnfChoices cont cd i xs cs
  ($##) cont (Guard_C_MailOption cd c e) cs = guardCons cd c ((cont $## e) (addCs c cs))
  ($##) _ (Fail_C_MailOption cd info) _ = failCons cd info
  searchNF search cont (C_CC x1) = search (\y1 -> cont (C_CC y1)) x1
  searchNF search cont (C_BCC x1) = search (\y1 -> cont (C_BCC y1)) x1
  searchNF search cont (C_TO x1) = search (\y1 -> cont (C_TO y1)) x1
  searchNF _ _ x = error ("Mail.MailOption.searchNF: no constructor: " ++ (show x))


instance Unifiable C_MailOption where
  (=.=) (C_CC x1) (C_CC y1) cs = (x1 =:= y1) cs
  (=.=) (C_BCC x1) (C_BCC y1) cs = (x1 =:= y1) cs
  (=.=) (C_TO x1) (C_TO y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) (C_CC x1) (C_CC y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_BCC x1) (C_BCC y1) cs = (x1 =:<= y1) cs
  (=.<=) (C_TO x1) (C_TO y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i (C_CC x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_BCC x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_TO x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_MailOption cd j x y) = [(ConstraintChoice cd j (bind i x) (bind i y))]
  bind i (Choices_C_MailOption cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_MailOption cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_MailOption cd i _) = error ("Mail.MailOption.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_MailOption cd info) = [(Unsolvable info)]
  bind i (Guard_C_MailOption cd c e) = (getConstrList c) ++ (bind i e)
  lazyBind i (C_CC x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_BCC x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_TO x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_MailOption cd j x y) = [(ConstraintChoice cd j (lazyBind i x) (lazyBind i y))]
  lazyBind i (Choices_C_MailOption cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_MailOption cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_MailOption cd i _) = error ("Mail.MailOption.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_MailOption cd info) = [(Unsolvable info)]
  lazyBind i (Guard_C_MailOption cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind i e)))]


instance Curry_Prelude.Curry C_MailOption where
  (=?=) (Choice_C_MailOption cd i x y) z cs = narrow cd i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_MailOption cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_MailOption cd c e) y cs = guardCons cd c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) (Fail_C_MailOption cd info) _ _ = failCons cd info
  (=?=) z (Choice_C_MailOption cd i x y) cs = narrow cd i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_MailOption cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_MailOption cd c e) cs = guardCons cd c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ (Fail_C_MailOption cd info) _ = failCons cd info
  (=?=) (C_CC x1) (C_CC y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_BCC x1) (C_BCC y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (C_TO x1) (C_TO y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_C_MailOption cd i x y) z cs = narrow cd i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_MailOption cd i xs) y cs = narrows cs cd i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_MailOption cd c e) y cs = guardCons cd c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) (Fail_C_MailOption cd info) _ _ = failCons cd info
  (<?=) z (Choice_C_MailOption cd i x y) cs = narrow cd i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_MailOption cd i xs) cs = narrows cs cd i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_MailOption cd c e) cs = guardCons cd c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ (Fail_C_MailOption cd info) _ = failCons cd info
  (<?=) (C_CC x1) (C_CC y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_CC _) (C_BCC _) _ = Curry_Prelude.C_True
  (<?=) (C_CC _) (C_TO _) _ = Curry_Prelude.C_True
  (<?=) (C_BCC x1) (C_BCC y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (C_BCC _) (C_TO _) _ = Curry_Prelude.C_True
  (<?=) (C_TO x1) (C_TO y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False


instance Coverable C_MailOption where
  cover (C_CC x1) = C_CC (cover x1)
  cover (C_BCC x1) = C_BCC (cover x1)
  cover (C_TO x1) = C_TO (cover x1)
  cover (Choice_C_MailOption cd i x y) = Choice_C_MailOption (incCover cd) i (cover x) (cover y)
  cover (Choices_C_MailOption cd i xs) = Choices_C_MailOption (incCover cd) i (map cover xs)
  cover (Fail_C_MailOption cd info) = Fail_C_MailOption (incCover cd) info
  cover (Guard_C_MailOption cd c e) = Guard_C_MailOption (incCover cd) c (cover e)


d_C_sendMail :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sendMail x1 x2 x3 x3500 = d_C_sendMailWithOptions x1 x3 (Curry_Prelude.OP_Cons (C_TO x2) Curry_Prelude.OP_List)

nd_C_sendMail :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO Curry_Prelude.OP_Unit)
nd_C_sendMail x1 x2 x3 x3000 x3500 = wrapDX id (d_C_sendMailWithOptions x1 x3 (Curry_Prelude.OP_Cons (C_TO x2) Curry_Prelude.OP_List))

d_C_sendMailWithOptions :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List C_MailOption -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sendMailWithOptions x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda3) Curry_Prelude.OP_List x3 x3500) x3500) x3500
     x6 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda7) Curry_Prelude.OP_List x3 x3500) x3500) x3500
     x7 = Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_foldr (acceptCs id d_OP_sendMailWithOptions_dot___hash_lambda11) Curry_Prelude.OP_List x3 x3500) x3500) x3500
      in (d_C_execMailCmd (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (d_OP__case_1 x7 (Curry_Prelude.d_C_null x7 x3500) x3500) (Curry_Prelude.d_OP_plus_plus (d_OP__case_0 x6 (Curry_Prelude.d_C_null x6 x3500) x3500) x5 x3500) x3500) x3500) x3500) x3500) x3500) x3500) x4 x3500)

d_OP_sendMailWithOptions_dot___hash_lambda3 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda3 x1 x2 x3500 = case x1 of
     (C_TO x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_CC x4) -> x2
     (C_BCC x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda3 x1002 x2 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda3 x1003 x2 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda3 z x2 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda3 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_sendMailWithOptions_dot___hash_lambda7 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda7 x1 x2 x3500 = case x1 of
     (C_CC x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_BCC x4) -> x2
     (C_TO x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda7 x1002 x2 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda7 x1003 x2 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda7 z x2 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda7 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_sendMailWithOptions_dot___hash_lambda11 :: C_MailOption -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_sendMailWithOptions_dot___hash_lambda11 x1 x2 x3500 = case x1 of
     (C_BCC x3) -> Curry_Prelude.OP_Cons x3 x2
     (C_CC x4) -> x2
     (C_TO x5) -> x2
     (Choice_C_MailOption x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sendMailWithOptions_dot___hash_lambda11 x1002 x2 x3500) (d_OP_sendMailWithOptions_dot___hash_lambda11 x1003 x2 x3500)
     (Choices_C_MailOption x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sendMailWithOptions_dot___hash_lambda11 z x2 x3500) x1002
     (Guard_C_MailOption x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sendMailWithOptions_dot___hash_lambda11 x1002 x2) $! (addCs x1001 x3500))
     (Fail_C_MailOption x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_execMailCmd :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_execMailCmd x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IOExts.d_C_execCmd x1 x3500) (d_OP_execMailCmd_dot___hash_lambda13 x2) x3500

d_OP_execMailCmd_dot_isUnixChar_dot_12 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_execMailCmd_dot_isUnixChar_dot_12 x1 x3500 = Curry_Prelude.d_OP_slash_eq x1 (Curry_Prelude.C_Char '\r'#) x3500

d_OP_execMailCmd_dot___hash_lambda13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_execMailCmd_dot___hash_lambda13 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple3 x3 x4 x5) -> Curry_Prelude.d_OP_gt_gt (Curry_IO.d_C_hPutStrLn x3 (Curry_Prelude.d_C_filter d_OP_execMailCmd_dot_isUnixChar_dot_12 x1 x3500) x3500) (Curry_IO.d_C_hClose x3 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_execMailCmd_dot___hash_lambda13 x1 x1002 x3500) (d_OP_execMailCmd_dot___hash_lambda13 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_execMailCmd_dot___hash_lambda13 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_execMailCmd_dot___hash_lambda13 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x1002 x3500) (d_OP__case_0 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x6 x1002 x3000 x3500) (nd_OP__case_0 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x7 x1002 x3500) (d_OP__case_1 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x7 x1002 x3000 x3500) (nd_OP__case_1 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
