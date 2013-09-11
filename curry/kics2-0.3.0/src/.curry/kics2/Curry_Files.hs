{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Files (d_C_withComponents, nd_C_withComponents, d_C_withDirectory, nd_C_withDirectory, d_C_withBaseName, nd_C_withBaseName, d_C_withExtension, nd_C_withExtension, d_C_createDirectoryIfMissing, d_C_writeFileInDir, d_C_writeQTermFileInDir, d_C_removeFileIfExists) where

import Basics
import qualified Curry_Directory
import qualified Curry_FilePath
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_Utils
d_C_withComponents :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_withComponents x1 x2 x3 x4 x3250 x3500 = let
     x5 = Curry_FilePath.d_C_splitFileName x4 x3250 x3500
     x6 = d_OP_withComponents_dot___hash_selFP5_hash_path x5 x3250 x3500
     x7 = d_OP_withComponents_dot___hash_selFP6_hash_bassfx x5 x3250 x3500
     x8 = Curry_FilePath.d_C_splitExtension x7 x3250 x3500
     x9 = d_OP_withComponents_dot___hash_selFP3_hash_base x8 x3250 x3500
     x10 = d_OP_withComponents_dot___hash_selFP4_hash_suffix x8 x3250 x3500
      in (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.d_C_apply x1 x6 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_dot_gt x3250 x3500) (Curry_Prelude.d_C_apply x2 x9 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply x3 x10 x3250 x3500) x3250 x3500) x3250 x3500)

nd_C_withComponents :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_withComponents x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2015 = x3000
      in (seq x2015 (let
          x5 = Curry_FilePath.d_C_splitFileName x4 x3250 x3500
          x6 = d_OP_withComponents_dot___hash_selFP5_hash_path x5 x3250 x3500
          x7 = d_OP_withComponents_dot___hash_selFP6_hash_bassfx x5 x3250 x3500
          x8 = Curry_FilePath.d_C_splitExtension x7 x3250 x3500
          x9 = d_OP_withComponents_dot___hash_selFP3_hash_base x8 x3250 x3500
          x10 = d_OP_withComponents_dot___hash_selFP4_hash_suffix x8 x3250 x3500
           in (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2003 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2003 (seq x2012 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_FilePath.nd_OP_lt_slash_gt x2000 x3250 x3500) (Curry_Prelude.nd_C_apply x1 x6 x2001 x3250 x3500) x2002 x3250 x3500))))))) (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2008 = leftSupply x2013
                              x2010 = rightSupply x2013
                               in (seq x2008 (seq x2010 (Curry_Prelude.nd_C_apply (let
                                   x2007 = leftSupply x2008
                                   x2009 = rightSupply x2008
                                    in (seq x2007 (seq x2009 (let
                                        x2005 = leftSupply x2009
                                        x2006 = rightSupply x2009
                                         in (seq x2005 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_FilePath.nd_OP_lt_dot_gt x2005 x3250 x3500) (Curry_Prelude.nd_C_apply x2 x9 x2006 x3250 x3500) x2007 x3250 x3500))))))) (Curry_Prelude.nd_C_apply x3 x10 x2010 x3250 x3500) x2011 x3250 x3500))))))) x2014 x3250 x3500)))))))))

d_OP_withComponents_dot___hash_selFP5_hash_path :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_withComponents_dot___hash_selFP5_hash_path x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_withComponents_dot___hash_selFP5_hash_path x1002 x3250 x3500) (d_OP_withComponents_dot___hash_selFP5_hash_path x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_withComponents_dot___hash_selFP5_hash_path z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_withComponents_dot___hash_selFP5_hash_path x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_withComponents_dot___hash_selFP6_hash_bassfx :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_withComponents_dot___hash_selFP6_hash_bassfx x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_withComponents_dot___hash_selFP6_hash_bassfx x1002 x3250 x3500) (d_OP_withComponents_dot___hash_selFP6_hash_bassfx x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_withComponents_dot___hash_selFP6_hash_bassfx z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_withComponents_dot___hash_selFP6_hash_bassfx x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_withComponents_dot___hash_selFP3_hash_base :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_withComponents_dot___hash_selFP3_hash_base x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_withComponents_dot___hash_selFP3_hash_base x1002 x3250 x3500) (d_OP_withComponents_dot___hash_selFP3_hash_base x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_withComponents_dot___hash_selFP3_hash_base z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_withComponents_dot___hash_selFP3_hash_base x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_withComponents_dot___hash_selFP4_hash_suffix :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_withComponents_dot___hash_selFP4_hash_suffix x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_withComponents_dot___hash_selFP4_hash_suffix x1002 x3250 x3500) (d_OP_withComponents_dot___hash_selFP4_hash_suffix x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_withComponents_dot___hash_selFP4_hash_suffix z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_withComponents_dot___hash_selFP4_hash_suffix x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_withDirectory :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_withDirectory x1 x2 x3250 x3500 = d_C_withComponents x1 Curry_Prelude.d_C_id Curry_Prelude.d_C_id x2 x3250 x3500

nd_C_withDirectory :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_withDirectory x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_withComponents x1 (wrapDX id Curry_Prelude.d_C_id) (wrapDX id Curry_Prelude.d_C_id) x2 x2000 x3250 x3500))

d_C_withBaseName :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_withBaseName x1 x2 x3250 x3500 = d_C_withComponents Curry_Prelude.d_C_id x1 Curry_Prelude.d_C_id x2 x3250 x3500

nd_C_withBaseName :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_withBaseName x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_withComponents (wrapDX id Curry_Prelude.d_C_id) x1 (wrapDX id Curry_Prelude.d_C_id) x2 x2000 x3250 x3500))

d_C_withExtension :: (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_withExtension x1 x2 x3250 x3500 = d_C_withComponents Curry_Prelude.d_C_id Curry_Prelude.d_C_id x1 x2 x3250 x3500

nd_C_withExtension :: Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
nd_C_withExtension x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_withComponents (wrapDX id Curry_Prelude.d_C_id) (wrapDX id Curry_Prelude.d_C_id) x1 x2 x2000 x3250 x3500))

d_C_createDirectoryIfMissing :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDirectoryIfMissing x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_OP_dollar (Curry_List.d_C_scanl1 (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500)) (Curry_Prelude.d_OP_dollar Curry_FilePath.d_C_splitDirectories x2 x3250 x3500) x3250 x3500
      in (d_OP__case_1 x3 x1 x3250 x3500)

d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesDirectoryExist x2 x3250 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_12_dot___hash_lambda1 x2 x3) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x1002 x3250 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_createDirectoryIfMissing_dot_createDirs_dot_12_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDirectoryIfMissing_dot_createDirs_dot_12_dot___hash_lambda1 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_unless x3) (Curry_Directory.d_C_createDirectory x1 x3250 x3500) x3250 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x2 x3250 x3500) x3250 x3500

d_C_writeFileInDir :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeFileInDir x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_createDirectoryIfMissing Curry_Prelude.C_True) (Curry_FilePath.d_C_takeDirectory x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_writeFile x1 x2 x3250 x3500) x3250 x3500

d_C_writeQTermFileInDir :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List Curry_Prelude.C_Char -> t0 -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeQTermFileInDir x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (d_C_createDirectoryIfMissing Curry_Prelude.C_True) (Curry_FilePath.d_C_takeDirectory x1 x3250 x3500) x3250 x3500) (Curry_ReadShowTerm.d_C_writeQTermFile x1 x2 x3250 x3500) x3250 x3500

d_C_removeFileIfExists :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_removeFileIfExists x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3250 x3500) (d_OP_removeFileIfExists_dot___hash_lambda2 x1) x3250 x3500

d_OP_removeFileIfExists_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_removeFileIfExists_dot___hash_lambda2 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_when x2) (Curry_Directory.d_C_removeFile x1 x3250 x3500) x3250 x3500

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_1 x3 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_0 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x3 x1002 x3250 x3500) (d_OP__case_1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_0 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_12 (Curry_Prelude.OP_Cons (Curry_List.d_C_last x3 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x1002 x3250 x3500) (d_OP__case_0 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
