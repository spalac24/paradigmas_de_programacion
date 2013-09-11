{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ReadNumeric (d_C_readInt, d_C_readNat, d_C_readHex, d_C_readOct) where

import Basics
import qualified Curry_Char
import qualified Curry_Prelude
d_C_readInt :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readInt x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x1 x3250 x3500
      in (d_OP__case_7 x2 x3250 x3500)

d_OP_readInt_dot___hash_lambda2 :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_readInt_dot___hash_lambda2 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_negate x2 x3250 x3500) x3)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readInt_dot___hash_lambda2 x1002 x3250 x3500) (d_OP_readInt_dot___hash_lambda2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readInt_dot___hash_lambda2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readInt_dot___hash_lambda2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readNat :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readNat x1 x3250 x3500 = d_C_readNumPrefix (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (Curry_Prelude.C_Char ' '#)) x1 x3250 x3500) Curry_Prelude.C_Nothing (Curry_Prelude.C_Int 10#) Curry_Char.d_C_isDigit Curry_Char.d_C_digitToInt x3250 x3500

d_C_readHex :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readHex x1 x3250 x3500 = d_C_readNumPrefix (Curry_Prelude.d_C_dropWhile d_OP_readHex_dot___hash_lambda3 x1 x3250 x3500) Curry_Prelude.C_Nothing (Curry_Prelude.C_Int 16#) Curry_Char.d_C_isHexDigit Curry_Char.d_C_digitToInt x3250 x3500

d_OP_readHex_dot___hash_lambda3 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readHex_dot___hash_lambda3 x1 x3250 x3500 = Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3250 x3500

d_C_readOct :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readOct x1 x3250 x3500 = d_C_readNumPrefix (Curry_Prelude.d_C_dropWhile d_OP_readOct_dot___hash_lambda4 x1 x3250 x3500) Curry_Prelude.C_Nothing (Curry_Prelude.C_Int 8#) Curry_Char.d_C_isOctDigit Curry_Char.d_C_digitToInt x3250 x3500

d_OP_readOct_dot___hash_lambda4 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readOct_dot___hash_lambda4 x1 x3250 x3500 = Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char ' '#) x3250 x3500

d_C_readNumPrefix :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readNumPrefix x1 x2 x3 x4 x5 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_5 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> d_OP__case_4 x7 x4 x5 x3 x8 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_readNumPrefix x1002 x2 x3 x4 x5 x3250 x3500) (d_C_readNumPrefix x1003 x2 x3 x4 x5 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_readNumPrefix z x2 x3 x4 x5 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_readNumPrefix x1002 x2 x3 x4 x5 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_readNumPrefix :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Char Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Char Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_readNumPrefix x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_5 x2 x3250 x3500
     (Curry_Prelude.OP_Cons x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x7 x4 x5 x3 x8 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_readNumPrefix x1002 x2 x3 x4 x5 x3000 x3250 x3500) (nd_C_readNumPrefix x1003 x2 x3 x4 x5 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_readNumPrefix z x2 x3 x4 x5 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_readNumPrefix x1002 x2 x3 x4 x5 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Char -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_4 x7 x4 x5 x3 x8 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.C_Just x9) -> d_OP__case_3 x7 x4 x8 x9 x5 x3 (Curry_Prelude.d_C_apply x4 x7 x3250 x3500) x3250 x3500
     Curry_Prelude.C_Nothing -> d_OP__case_1 x7 x4 x5 x3 x8 (Curry_Prelude.d_C_apply x4 x7 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x7 x4 x5 x3 x8 x1002 x3250 x3500) (d_OP__case_4 x7 x4 x5 x3 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x7 x4 x5 x3 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x7 x4 x5 x3 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_4 :: Curry_Prelude.C_Char -> Func Curry_Prelude.C_Char Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Char Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_4 x7 x4 x5 x3 x8 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.C_Just x9) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_3 x7 x4 x8 x9 x5 x3 (Curry_Prelude.nd_C_apply x4 x7 x2000 x3250 x3500) x2001 x3250 x3500)))))
     Curry_Prelude.C_Nothing -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_1 x7 x4 x5 x3 x8 (Curry_Prelude.nd_C_apply x4 x7 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x7 x4 x5 x3 x8 x1002 x3000 x3250 x3500) (nd_OP__case_4 x7 x4 x5 x3 x8 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x7 x4 x5 x3 x8 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x7 x4 x5 x3 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_1 x7 x4 x5 x3 x8 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_readNumPrefix x8 (Curry_Prelude.C_Just (Curry_Prelude.d_C_apply x5 x7 x3250 x3500)) x3 x4 x5 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_0 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x7 x4 x5 x3 x8 x1002 x3250 x3500) (d_OP__case_1 x7 x4 x5 x3 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x7 x4 x5 x3 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x7 x4 x5 x3 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_1 :: Curry_Prelude.C_Char -> Func Curry_Prelude.C_Char Curry_Prelude.C_Bool -> Func Curry_Prelude.C_Char Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_1 x7 x4 x5 x3 x8 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_readNumPrefix x8 (Curry_Prelude.C_Just (Curry_Prelude.nd_C_apply x5 x7 x2000 x3250 x3500)) x3 x4 x5 x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> d_OP__case_0 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x7 x4 x5 x3 x8 x1002 x3000 x3250 x3500) (nd_OP__case_1 x7 x4 x5 x3 x8 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x7 x4 x5 x3 x8 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x7 x4 x5 x3 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_0 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3250 x3500) (d_OP__case_0 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Char -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> (Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Int) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_3 x7 x4 x8 x9 x5 x3 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> d_C_readNumPrefix x8 (Curry_Prelude.C_Just (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x3 x9 x3250 x3500) (Curry_Prelude.d_C_apply x5 x7 x3250 x3500) x3250 x3500)) x3 x4 x5 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_2 x8 x7 x9 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x7 x4 x8 x9 x5 x3 x1002 x3250 x3500) (d_OP__case_3 x7 x4 x8 x9 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x7 x4 x8 x9 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x7 x4 x8 x9 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_3 :: Curry_Prelude.C_Char -> Func Curry_Prelude.C_Char Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Func Curry_Prelude.C_Char Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_OP__case_3 x7 x4 x8 x9 x5 x3 x10 x3000 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_C_readNumPrefix x8 (Curry_Prelude.C_Just (Curry_Prelude.d_OP_plus (Curry_Prelude.d_OP_star x3 x9 x3250 x3500) (Curry_Prelude.nd_C_apply x5 x7 x2000 x3250 x3500) x3250 x3500)) x3 x4 x5 x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> d_OP__case_2 x8 x7 x9 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x7 x4 x8 x9 x5 x3 x1002 x3000 x3250 x3500) (nd_OP__case_3 x7 x4 x8 x9 x5 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x7 x4 x8 x9 x5 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x7 x4 x8 x9 x5 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_2 x8 x7 x9 x10 x3250 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x9 (Curry_Prelude.OP_Cons x7 x8))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x8 x7 x9 x1002 x3250 x3500) (d_OP__case_2 x8 x7 x9 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x8 x7 x9 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x8 x7 x9 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Maybe Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_5 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.C_Nothing
     (Curry_Prelude.C_Just x6) -> Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x6 Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3250 x3500) (d_OP__case_5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_7 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_6 x5 x2 x4 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_6 x5 x2 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_maybe Curry_Prelude.C_Nothing d_OP_readInt_dot___hash_lambda2 (d_C_readNat x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_C_readNat x2 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x5 x2 x4 x1002 x3250 x3500) (d_OP__case_6 x5 x2 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x5 x2 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x5 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
