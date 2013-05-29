{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FileGoodies (d_C_separatorChar, d_C_pathSeparatorChar, d_C_suffixSeparatorChar, d_C_isAbsolute, d_C_dirName, d_C_baseName, d_C_splitDirectoryBaseName, d_C_stripSuffix, nd_C_stripSuffix, d_C_fileSuffix, nd_C_fileSuffix, d_C_splitBaseName, d_C_splitPath, d_C_lookupFileInPath, d_C_getFileInPath) where

import Basics
import qualified Curry_Directory
import qualified Curry_List
import qualified Curry_Prelude
d_C_separatorChar :: ConstStore -> Curry_Prelude.C_Char
d_C_separatorChar x3500 = Curry_Prelude.C_Char '/'#

d_C_pathSeparatorChar :: ConstStore -> Curry_Prelude.C_Char
d_C_pathSeparatorChar x3500 = Curry_Prelude.C_Char ':'#

d_C_suffixSeparatorChar :: ConstStore -> Curry_Prelude.C_Char
d_C_suffixSeparatorChar x3500 = Curry_Prelude.C_Char '.'#

d_C_isAbsolute :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isAbsolute x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_eq_eq x2 (d_C_separatorChar x3500) x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isAbsolute x1002 x3500) (d_C_isAbsolute x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isAbsolute z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isAbsolute x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dirName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_dirName x1 x3500 = Curry_Prelude.d_C_fst (d_C_splitDirectoryBaseName x1 x3500) x3500

d_C_baseName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_baseName x1 x3500 = Curry_Prelude.d_C_snd (d_C_splitDirectoryBaseName x1 x3500) x3500

d_C_splitDirectoryBaseName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_splitDirectoryBaseName x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (d_C_separatorChar x3500)) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x1 x3500) x3500
     x3 = d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase x2 x3500
     x4 = d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir x2 x3500
      in (d_OP__case_3 x3 x4 (Curry_Prelude.d_C_null x4 x3500) x3500)

d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase x1002 x3500) (d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitDirectoryBaseName_dot___hash_selFP2_hash_rbase x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir x1002 x3500) (d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitDirectoryBaseName_dot___hash_selFP3_hash_rdir x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_stripSuffix :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_stripSuffix x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_fst d_C_splitBaseName x3500

nd_C_stripSuffix :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_stripSuffix x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_fst) (wrapDX id d_C_splitBaseName) x2000 x3500))

d_C_fileSuffix :: ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fileSuffix x3500 = Curry_Prelude.d_OP_dot Curry_Prelude.d_C_snd d_C_splitBaseName x3500

nd_C_fileSuffix :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_fileSuffix x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_snd) (wrapDX id d_C_splitBaseName) x2000 x3500))

d_C_splitBaseName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_splitBaseName x1 x3500 = let
     x2 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (d_C_suffixSeparatorChar x3500)) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x1 x3500) x3500
     x3 = d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix x2 x3500
     x4 = d_OP_splitBaseName_dot___hash_selFP6_hash_rbase x2 x3500
      in (d_OP__case_2 x1 x3 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_null x4 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (d_C_separatorChar x3500) x3500) x3 x3500) x3500) x3500)

d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix x1002 x3500) (d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitBaseName_dot___hash_selFP5_hash_rsuffix x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitBaseName_dot___hash_selFP6_hash_rbase :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitBaseName_dot___hash_selFP6_hash_rbase x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitBaseName_dot___hash_selFP6_hash_rbase x1002 x3500) (d_OP_splitBaseName_dot___hash_selFP6_hash_rbase x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitBaseName_dot___hash_selFP6_hash_rbase z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitBaseName_dot___hash_selFP6_hash_rbase x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_splitPath x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_break (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_eq_eq) (d_C_pathSeparatorChar x3500)) x3500) (Curry_Prelude.OP_Cons x2 x3) x3500
          x5 = d_OP_splitPath_dot___hash_selFP8_hash_ys x4 x3500
          x6 = d_OP_splitPath_dot___hash_selFP9_hash_zs x4 x3500
           in (d_OP__case_1 x5 x6 (Curry_Prelude.d_C_null x6 x3500) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_splitPath x1002 x3500) (d_C_splitPath x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_splitPath z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_splitPath x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitPath_dot___hash_selFP8_hash_ys :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitPath_dot___hash_selFP8_hash_ys x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitPath_dot___hash_selFP8_hash_ys x1002 x3500) (d_OP_splitPath_dot___hash_selFP8_hash_ys x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitPath_dot___hash_selFP8_hash_ys z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitPath_dot___hash_selFP8_hash_ys x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitPath_dot___hash_selFP9_hash_zs :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_splitPath_dot___hash_selFP9_hash_zs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitPath_dot___hash_selFP9_hash_zs x1002 x3500) (d_OP_splitPath_dot___hash_selFP9_hash_zs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitPath_dot___hash_selFP9_hash_zs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitPath_dot___hash_selFP9_hash_zs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lookupFileInPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_lookupFileInPath x1 x2 x3 x3500 = d_OP__case_0 x1 x2 x3 (d_C_isAbsolute x1 x3500) x3500

d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x2 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = Curry_Prelude.d_OP_plus_plus x1 x3 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x5 x3500) (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x5 x4) x3500)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x1002 x3500) (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just x2) x3500
     Curry_Prelude.C_False -> d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x2 x3 x1002 x3500) (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35_dot___hash_lambda2 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3500
     (Curry_Prelude.OP_Cons x4 x5) -> Curry_Prelude.d_OP_gt_gt_eq (d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.OP_Cons (d_C_separatorChar x3500) x1) x3500) x2 x3500) (d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35_dot___hash_lambda1 x5 x1 x2) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x1002 x3500) (d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35_dot___hash_lambda1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35_dot___hash_lambda1 x1 x2 x3 x4 x3500 = Curry_Prelude.d_C_maybe (d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x2 x3 x1 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Just) x3500) x4 x3500

d_C_getFileInPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getFileInPath x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_lookupFileInPath x1 x2 x3 x3500) (d_OP_getFileInPath_dot___hash_lambda3 x1 x3) x3500

d_OP_getFileInPath_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getFileInPath_dot___hash_lambda3 x1 x2 x3 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))))))) (Curry_Prelude.d_C_concat (Curry_List.d_C_intersperse (Curry_Prelude.OP_Cons (d_C_pathSeparatorChar x3500) Curry_Prelude.OP_List) x2 x3500) x3500) x3500) x3500) x3500) x3500) Curry_Prelude.d_C_return x3 x3500

d_OP__case_0 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x2 x3500
     Curry_Prelude.C_False -> d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x2 x3 x1002 x3500) (d_OP__case_0 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP_lookupFileInPath_dot_lookupFirstFileWithSuffix_dot_35 x1 x2 x3500
     Curry_Prelude.C_False -> d_OP_lookupFileInPath_dot_lookupFirstFile_dot_35 x1 x2 x3 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_0 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x5 (d_C_splitPath (Curry_Prelude.d_C_tail x6 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x5 x6 x1002 x3500) (d_OP__case_1 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x5 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x5 (d_C_splitPath (Curry_Prelude.d_C_tail x6 x3500) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x5 x6 x1002 x3000 x3500) (nd_OP__case_1 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_tail x4 x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x3 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x3 x4 x1002 x3500) (d_OP__case_2 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (Curry_Prelude.OP_Tuple2 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) (Curry_Prelude.d_C_tail x4 x3500) x2001 x3500)))) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2003 x3500) x3 x2004 x3500)))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_2 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x3 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_tail x4 x3500) x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) x3 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x4 x1002 x3500) (d_OP__case_3 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) x3 x2001 x3500))))))
     Curry_Prelude.C_False -> let
          x2006 = x3000
           in (seq x2006 (let
               x2002 = leftSupply x2006
               x2005 = rightSupply x2006
                in (seq x2002 (seq x2005 (Curry_Prelude.OP_Tuple2 (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) (Curry_Prelude.d_C_tail x4 x3500) x2001 x3500)))) (let
                    x2004 = leftSupply x2005
                    x2003 = rightSupply x2005
                     in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2003 x3500) x3 x2004 x3500)))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x3 x4 x1002 x3000 x3500) (nd_OP__case_3 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
