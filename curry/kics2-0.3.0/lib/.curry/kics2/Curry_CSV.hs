{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CSV (d_C_writeCSVFile, d_C_showCSV, d_C_readCSVFile, nd_C_readCSVFile, d_C_readCSVFileWithDelims, d_C_readCSV, nd_C_readCSV, d_C_readCSVWithDelims) where

import Basics
import qualified Curry_List
import qualified Curry_Prelude
d_C_writeCSVFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeCSVFile x1 x2 x3250 x3500 = Curry_Prelude.d_C_writeFile x1 (d_C_showCSV x2 x3250 x3500) x3250 x3500

d_C_showCSV :: Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCSV x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_C_showCSVLine x3250 x3500) x1 x3250 x3500

d_C_showCSVLine :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_showCSVLine x1 x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_map d_OP_showCSVLine_dot_convert_dot_6 x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List) x3250 x3500

d_OP_showCSVLine_dot_convert_dot_6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCSVLine_dot_convert_dot_6 x1 x3250 x3500 = d_OP__case_8 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda1 x3250 x3500) x1 x3250 x3500) x3250 x3500

d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda1 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda1 x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))) x3250 x3500

d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda2 :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda2 x1 x3250 x3500 = d_OP__case_7 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500

d_C_readCSVFile :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_readCSVFile x3250 x3500 = d_C_readCSVFileWithDelims (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)

nd_C_readCSVFile :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))))
nd_C_readCSVFile x3000 x3250 x3500 = wrapDX id (d_C_readCSVFileWithDelims (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List))

d_C_readCSVFileWithDelims :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_readCSVFileWithDelims x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x2 x3250 x3500) (d_OP_readCSVFileWithDelims_dot___hash_lambda3 x1) x3250 x3500

d_OP_readCSVFileWithDelims_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_readCSVFileWithDelims_dot___hash_lambda3 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (d_C_readCSVWithDelims x1 x2 x3250 x3500) x3250 x3500

d_C_readCSV :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readCSV x3250 x3500 = d_C_readCSVWithDelims (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)

nd_C_readCSV :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_readCSV x3000 x3250 x3500 = wrapDX id (d_C_readCSVWithDelims (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List))

d_C_readCSVWithDelims :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_readCSVWithDelims x1 x2 x3250 x3500 = Curry_Prelude.d_C_map (d_C_components x1) (Curry_Prelude.d_C_lines x2 x3250 x3500) x3250 x3500

d_C_components :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_components x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_C_error (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'V'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) x3250 x3500
           in (d_OP__case_6 x3 x4 x1 x5 (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_components x1 x1002 x3250 x3500) (d_C_components x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_components x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_components x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot_breakString_dot_24 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_components_dot_breakString_dot_24 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_4 x4 x2 x1 x5 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot_breakString_dot_24 x1 x2 x1002 x3250 x3500) (d_OP_components_dot_breakString_dot_24 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot_breakString_dot_24 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot_breakString_dot_24 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b x1002 x3250 x3500) (d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x3
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs x1002 x3250 x3500) (d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b x1002 x3250 x3500) (d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> x3
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs x1002 x3250 x3500) (d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot___hash_selFP8_hash_e :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_components_dot___hash_selFP8_hash_e x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot___hash_selFP8_hash_e x1002 x3250 x3500) (d_OP_components_dot___hash_selFP8_hash_e x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot___hash_selFP8_hash_e z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot___hash_selFP8_hash_e x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_components_dot___hash_selFP9_hash_s :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_components_dot___hash_selFP9_hash_s x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_components_dot___hash_selFP9_hash_s x1002 x3250 x3500) (d_OP_components_dot___hash_selFP9_hash_s x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_components_dot___hash_selFP9_hash_s z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_components_dot___hash_selFP9_hash_s x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_4 x4 x2 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.OP_List -> d_OP__case_3 x4 x1 (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_2 x6 x4 x2 x7 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x6 (Curry_Prelude.C_Char '"'#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x4 x2 x1 x1002 x3250 x3500) (d_OP__case_4 x4 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x4 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x4 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_2 x6 x4 x2 x7 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x8 = d_OP_components_dot_breakString_dot_24 x1 x2 x7 x3250 x3500
          x9 = d_OP_components_dot_breakString_dot_24_dot___hash_selFP2_hash_b x8 x3250 x3500
          x10 = d_OP_components_dot_breakString_dot_24_dot___hash_selFP3_hash_bs x8 x3250 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 x9) x10)
     Curry_Prelude.C_False -> d_OP__case_1 x2 x6 x4 x7 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 (Curry_Prelude.C_Char '"'#) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x6 x3250 x3500) x2 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x6 x4 x2 x7 x1 x1002 x3250 x3500) (d_OP__case_2 x6 x4 x2 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x6 x4 x2 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x6 x4 x2 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_1 x2 x6 x4 x7 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List (d_C_components x2 x7 x3250 x3500)
     Curry_Prelude.C_False -> d_OP__case_0 x7 x6 x2 x1 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x6 x4 x7 x1 x1002 x3250 x3500) (d_OP__case_1 x2 x6 x4 x7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x6 x4 x7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x6 x4 x7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_0 x7 x6 x2 x1 x4 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x11 = d_OP_components_dot_breakString_dot_24 x1 x2 (Curry_Prelude.OP_Cons x6 x7) x3250 x3500
          x12 = d_OP_components_dot_breakString_dot_24_dot___hash_selFP5_hash_b x11 x3250 x3500
          x13 = d_OP_components_dot_breakString_dot_24_dot___hash_selFP6_hash_bs x11 x3250 x3500
           in (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons x4 x12) x13)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x7 x6 x2 x1 x4 x1002 x3250 x3500) (d_OP__case_0 x7 x6 x2 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x7 x6 x2 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x7 x6 x2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_3 x4 x1 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons Curry_Prelude.OP_List Curry_Prelude.OP_List
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x4 x1 x1002 x3250 x3500) (d_OP__case_3 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_6 x3 x4 x1 x5 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP_components_dot_breakString_dot_24 x5 x1 x4 x3250 x3500
     Curry_Prelude.C_False -> let
          x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem x1) x3250 x3500) (Curry_Prelude.OP_Cons x3 x4) x3250 x3500
          x7 = d_OP_components_dot___hash_selFP8_hash_e x6 x3250 x3500
          x8 = d_OP_components_dot___hash_selFP9_hash_s x6 x3250 x3500
           in (Curry_Prelude.OP_Cons x7 (d_OP__case_5 x8 x1 (Curry_Prelude.d_C_null x8 x3250 x3500) x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x3 x4 x1 x5 x1002 x3250 x3500) (d_OP__case_6 x3 x4 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x3 x4 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x3 x4 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_5 x8 x1 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_C_components x1 (Curry_Prelude.d_C_tail x8 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x8 x1 x1002 x3250 x3500) (d_OP__case_5 x8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_7 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x1002 x3250 x3500) (d_OP__case_7 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_8 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_showCSVLine_dot_convert_dot_6_dot___hash_lambda2 x3250 x3500) x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3250 x3500)
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x1002 x3250 x3500) (d_OP__case_8 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
