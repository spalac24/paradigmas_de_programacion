{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Read (d_C_readNat, d_C_readInt, d_C_readHex) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
d_C_readNat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_C_readNat x1 x3500 = d_OP_readNat_dot_readNatPrefix_dot_2 (Curry_Prelude.d_C_dropWhile d_OP_readNat_dot___hash_lambda1 x1 x3500) (Curry_Prelude.C_Int 0#) x3500

d_OP_readNat_dot_readNatPrefix_dot_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_readNat_dot_readNatPrefix_dot_2 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_C_ord x3 x3500
           in (d_OP__case_4 x2 x4 x5 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt_eq x5 (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3500) x3500) (Curry_Prelude.d_OP_lt_eq x5 (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '9'#) x3500) x3500) x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readNat_dot_readNatPrefix_dot_2 x1002 x2 x3500) (d_OP_readNat_dot_readNatPrefix_dot_2 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readNat_dot_readNatPrefix_dot_2 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readNat_dot_readNatPrefix_dot_2 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readNat_dot___hash_lambda1 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readNat_dot___hash_lambda1 x1 x3500 = Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3500

d_C_readInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_C_readInt x1 x3500 = d_OP_readInt_dot_readIntPrefix_dot_13 (Curry_Prelude.d_C_dropWhile d_OP_readInt_dot___hash_lambda2 x1 x3500) x3500

d_OP_readInt_dot_readIntPrefix_dot_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_OP_readInt_dot_readIntPrefix_dot_13 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_3 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '-'#) x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readInt_dot_readIntPrefix_dot_13 x1002 x3500) (d_OP_readInt_dot_readIntPrefix_dot_13 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readInt_dot_readIntPrefix_dot_13 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readInt_dot_readIntPrefix_dot_13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readInt_dot___hash_lambda2 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readInt_dot___hash_lambda2 x1 x3500 = Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3500

d_C_readHex :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_C_readHex x1 x3500 = d_OP_readHex_dot_readHexPrefix_dot_21 (Curry_Prelude.d_C_dropWhile d_OP_readHex_dot___hash_lambda3 x1 x3500) (Curry_Prelude.C_Int 0#) x3500

d_OP_readHex_dot_hex2int_dot_21 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_OP_readHex_dot_hex2int_dot_21 x1 x3500 = d_OP__case_2 x1 (Curry_Char.d_C_isDigit x1 x3500) x3500

d_OP_readHex_dot_readHexPrefix_dot_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_OP_readHex_dot_readHexPrefix_dot_21 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = d_OP_readHex_dot_hex2int_dot_21 x3 x3500
           in (d_OP__case_0 x2 x4 x5 (Curry_Prelude.d_OP_gt_eq x5 (Curry_Prelude.C_Int 0#) x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readHex_dot_readHexPrefix_dot_21 x1002 x2 x3500) (d_OP_readHex_dot_readHexPrefix_dot_21 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readHex_dot_readHexPrefix_dot_21 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readHex_dot_readHexPrefix_dot_21 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_readHex_dot___hash_lambda3 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readHex_dot___hash_lambda3 x1 x3500 = Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3500

d_OP__case_0 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_readHex_dot_readHexPrefix_dot_21 x4 (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x2 (Curry_Prelude.C_Int 16#) x3500) x5 x3500) x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x4 x5 x1002 x3500) (d_OP__case_0 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_readHex_dot_readHexPrefix_dot_21 x4 (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x2 (Curry_Prelude.C_Int 16#) x3500) x5 x3500) x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_0 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3500) x3500
     Curry_Prelude.C_False -> d_OP__case_1 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt_eq (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3500) x3500) (Curry_Prelude.d_OP_lt_eq (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'F'#) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_gt_eq (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3500) x3500) (Curry_Prelude.d_OP_lt_eq (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'F'#) x3500) x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3500) x3500) (Curry_Prelude.C_Int 10#) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 1#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3500) (d_OP__case_1 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_minus (Curry_Prelude.d_C_ord x1 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char 'A'#) x3500) x3500) (Curry_Prelude.C_Int 10#) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_negate (Curry_Prelude.C_Int 1#) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x1002 x3000 x3500) (nd_OP__case_1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_negate (d_C_readNat x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_readNat (Curry_Prelude.OP_Cons x2 x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x3 x1002 x3500) (d_OP__case_3 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_negate (d_C_readNat x3 x3500) x3500
     Curry_Prelude.C_False -> d_C_readNat (Curry_Prelude.OP_Cons x2 x3) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x2 x3 x1002 x3000 x3500) (nd_OP__case_3 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_readNat_dot_readNatPrefix_dot_2 x4 (Curry_Prelude.d_OP_minus (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x2 (Curry_Prelude.C_Int 10#) x3500) x5 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3500) x3500) x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x4 x5 x1002 x3500) (d_OP__case_4 x2 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> d_OP_readNat_dot_readNatPrefix_dot_2 x4 (Curry_Prelude.d_OP_minus (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x2 (Curry_Prelude.C_Int 10#) x3500) x5 x3500) (Curry_Prelude.d_C_ord (Curry_Prelude.C_Char '0'#) x3500) x3500) x3500
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x2 x4 x5 x1002 x3000 x3500) (nd_OP__case_4 x2 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x2 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x2 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
