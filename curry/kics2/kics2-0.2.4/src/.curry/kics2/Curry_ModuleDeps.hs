{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_ModuleDeps (C_ModuleIdent, C_Source, d_C_deps) where

import Basics
import qualified Curry_CompilerOpts
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FilePath
import qualified Curry_FiniteMap
import qualified Curry_FlatCurry
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_SCC
import qualified Curry_Time
import qualified Curry_Utils
import qualified Curry_Files
type C_ModuleIdent = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_FilePath = Curry_Prelude.OP_List Curry_Prelude.C_Char

type C_Source = Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog

type C_SourceEnv = Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))

d_C_deps :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_deps x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_sourceDeps x1 (Curry_Prelude.d_OP_dollar (Curry_FilePath.d_C_dropExtension x3500) (Curry_Prelude.d_C_apply (Curry_FilePath.d_C_takeBaseName x3500) x2 x3500) x3500) x2 (Curry_FiniteMap.d_C_emptyFM (acceptCs id Curry_Prelude.d_OP_lt) x3500) x3500) (d_OP_deps_dot___hash_lambda1 x2) x3500

d_OP_deps_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_deps_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_isFlatCurryValid x1 x3500) (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2 x2) x3500

nd_OP_deps_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_deps_dot___hash_lambda1 x1 x2 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_isFlatCurryValid x1 x3500) (wrapNX id (nd_OP_deps_dot___hash_lambda1_dot___hash_lambda2 x2)) x2000 x3500))

d_OP_deps_dot___hash_lambda1_dot___hash_lambda2 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_OP_deps_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3500 = let
     x3 = d_C_filterMissing x1 x3500
     x4 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x3 x3500
     x5 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x3 x3500
     x6 = Curry_Prelude.d_C_apply (d_C_flattenDeps x3500) x4 x3500
     x7 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x6 x3500
     x8 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x6 x3500
      in (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x7 (d_OP__case_9 x5 x8 x2 x3500)) x3500)

nd_OP_deps_dot___hash_lambda1_dot___hash_lambda2 :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_Prelude.C_Bool -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_OP_deps_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2000 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2000 (seq x2006 (let
               x2003 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2003 (seq x2004 (let
                    x3 = nd_C_filterMissing x1 x2000 x3500
                    x4 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x3 x3500
                    x5 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x3 x3500
                    x6 = let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_flattenDeps x2001 x3500) x4 x2002 x3500)))
                    x7 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x6 x3500
                    x8 = d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x6 x3500
                     in (Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple2 x7 (nd_OP__case_9 x5 x8 x2 x2004 x3500)) x3500)))))))))

d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))
d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x1002 x3500) (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP5_hash_mods1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x1002 x3500) (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP6_hash_errs1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))
d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x1002 x3500) (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP3_hash_mods2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x1002 x3500) (d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_deps_dot___hash_lambda1_dot___hash_lambda2_dot___hash_selFP4_hash_errs2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_isFlatCurryValid :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_isFlatCurryValid x1 x3500 = let
     x2 = Curry_FlatCurry.d_C_flatCurryFileName x1 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3500) (d_OP_isFlatCurryValid_dot___hash_lambda3 x2 x1) x3500)

d_OP_isFlatCurryValid_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isFlatCurryValid_dot___hash_lambda3 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3500) (d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4 x3 x1 x2) x3500

d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x3500 = d_OP__case_8 x1 x2 x3 x4 (Curry_Prelude.d_OP_ampersand_ampersand x1 x4 x3500) x3500

d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3500) (d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x2) x3500

d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_OP_gt_eq x2 x1 x3500) x3500

d_C_moduleDeps :: Curry_CompilerOpts.C_Options -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_C_moduleDeps x1 x2 x3 x3500 = d_OP__case_7 x1 x2 x3 (Curry_FiniteMap.d_C_lookupFM x2 x3 x3500) x3500

nd_C_moduleDeps :: Curry_CompilerOpts.C_Options -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
nd_C_moduleDeps x1 x2 x3 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_7 x1 x2 x3 (Curry_FiniteMap.nd_C_lookupFM x2 x3 x2000 x3500) x2001 x3500)))))

d_OP_moduleDeps_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x4 x3500 = case x4 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_FiniteMap.d_C_addToFM x2 x1 Curry_Prelude.C_Nothing x3500) x3500
     (Curry_Prelude.C_Just x5) -> d_C_sourceDeps x3 x1 x5 x2 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1002 x3500) (d_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_moduleDeps_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
nd_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x4 x3000 x3500 = case x4 of
     Curry_Prelude.C_Nothing -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id Curry_Prelude.d_C_return) (Curry_FiniteMap.nd_C_addToFM x2 x1 Curry_Prelude.C_Nothing x2000 x3500) x2001 x3500)))))
     (Curry_Prelude.C_Just x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_sourceDeps x3 x1 x5 x2 x2000 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1002 x3000 x3500) (nd_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_moduleDeps_dot___hash_lambda8 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lookupModule :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_lookupModule x1 x2 x3500 = let
     x3 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optImportPaths x1 x3500)
      in (Curry_FileGoodies.d_C_lookupFileInPath x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) (Curry_Prelude.d_C_map Curry_FilePath.d_C_dropTrailingPathSeparator x3 x3500) x3500)

d_C_sourceDeps :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_C_sourceDeps x1 x2 x3 x4 x3500 = let
     x5 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optImportPaths x1 x3500)
     x6 = Curry_Prelude.d_OP_lt (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optVerbosity x1 x3500) Curry_CompilerOpts.C_VerbFrontend x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (Curry_FlatCurry.d_C_readFlatCurryWithParseOptions (Curry_Prelude.d_C_apply (Curry_FilePath.d_C_dropExtension x3500) x3 x3500)) (Curry_Prelude.d_OP_dollar (Curry_Distribution.d_C_setFullPath x5) (Curry_Distribution.d_C_setQuiet x6 (Curry_Distribution.d_C_defaultParams x3500) x3500) x3500) x3500) (d_OP_sourceDeps_dot___hash_lambda10 x3 x2 x4 x1) x3500)

nd_C_sourceDeps :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
nd_C_sourceDeps x1 x2 x3 x4 x3000 x3500 = let
     x2008 = x3000
      in (seq x2008 (let
          x5 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optImportPaths x1 x3500)
          x6 = Curry_Prelude.d_OP_lt (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optVerbosity x1 x3500) Curry_CompilerOpts.C_VerbFrontend x3500
           in (let
               x2007 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2007 (seq x2005 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2002 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_FlatCurry.d_C_readFlatCurryWithParseOptions (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_FilePath.nd_C_dropExtension x2000 x3500) x3 x2001 x3500)))))) (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Distribution.d_C_setFullPath x5)) (Curry_Distribution.d_C_setQuiet x6 (Curry_Distribution.d_C_defaultParams x3500) x3500) x2003 x3500) x2004 x3500))))))) (wrapNX id (nd_OP_sourceDeps_dot___hash_lambda10 x3 x2 x4 x1)) x2007 x3500))))))

d_OP_sourceDeps_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x5 x3500 = case x5 of
     (Curry_FlatCurry.C_Prog x6 x7 x8 x9 x10) -> Curry_Utils.d_C_foldIO (acceptCs id (d_C_moduleDeps x4)) (Curry_FiniteMap.d_C_addToFM x3 x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x1 x5)) x3500) x7 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1002 x3500) (d_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_sourceDeps_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_FlatCurry.C_Prog -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
nd_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     (Curry_FlatCurry.C_Prog x6 x7 x8 x9 x10) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Utils.nd_C_foldIO (wrapDX (wrapNX id) (acceptCs id (nd_C_moduleDeps x4))) (Curry_FiniteMap.nd_C_addToFM x3 x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 x1 x5)) x2000 x3500) x7 x2001 x3500)))))
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_sourceDeps_dot___hash_lambda10 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_filterMissing :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_filterMissing x1 x3500 = let
     x2 = Curry_Prelude.d_OP_dollar (Curry_List.d_C_partition (Curry_Prelude.d_OP_dot Curry_Maybe.d_C_isJust Curry_Prelude.d_C_snd x3500)) (Curry_FiniteMap.d_C_fmToList x1 x3500) x3500
     x3 = d_OP_filterMissing_dot___hash_selFP8_hash_present x2 x3500
     x4 = d_OP_filterMissing_dot___hash_selFP9_hash_missing x2 x3500
     x5 = Curry_Prelude.d_C_map d_OP_filterMissing_dot___hash_lambda11 x4 x3500
      in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_map (Curry_Utils.d_C_mapSnd Curry_Maybe.d_C_fromJust) x3 x3500) x5)

nd_C_filterMissing :: Curry_FiniteMap.C_FM (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_filterMissing x1 x3000 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2003 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2003 (seq x2008 (let
               x2005 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2005 (seq x2006 (let
                    x2 = let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_List.nd_C_partition (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Maybe.d_C_isJust) (wrapDX id Curry_Prelude.d_C_snd) x2000 x3500))) (Curry_FiniteMap.nd_C_fmToList x1 x2001 x3500) x2002 x3500))))))
                    x3 = d_OP_filterMissing_dot___hash_selFP8_hash_present x2 x3500
                    x4 = d_OP_filterMissing_dot___hash_selFP9_hash_missing x2 x3500
                    x5 = Curry_Prelude.nd_C_map (wrapDX id d_OP_filterMissing_dot___hash_lambda11) x4 x2005 x3500
                     in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_map (wrapNX id (Curry_Utils.nd_C_mapSnd (wrapDX id Curry_Maybe.d_C_fromJust))) x3 x2006 x3500) x5)))))))))

d_OP_filterMissing_dot___hash_selFP8_hash_present :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_OP_filterMissing_dot___hash_selFP8_hash_present x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_filterMissing_dot___hash_selFP8_hash_present x1002 x3500) (d_OP_filterMissing_dot___hash_selFP8_hash_present x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_filterMissing_dot___hash_selFP8_hash_present z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_filterMissing_dot___hash_selFP8_hash_present x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_filterMissing_dot___hash_selFP9_hash_missing :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)))
d_OP_filterMissing_dot___hash_selFP9_hash_missing x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_filterMissing_dot___hash_selFP9_hash_missing x1002 x3500) (d_OP_filterMissing_dot___hash_selFP9_hash_missing x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_filterMissing_dot___hash_selFP9_hash_missing z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_filterMissing_dot___hash_selFP9_hash_missing x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_filterMissing_dot___hash_lambda11 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_filterMissing_dot___hash_lambda11 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_filterMissing_dot___hash_lambda11 x1002 x3500) (d_OP_filterMissing_dot___hash_lambda11 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_filterMissing_dot___hash_lambda11 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_filterMissing_dot___hash_lambda11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_flattenDeps :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_flattenDeps x3500 = let
     x1 = Curry_SCC.d_C_scc d_OP_flattenDeps_dot_modules_dot_48 d_OP_flattenDeps_dot_imports_dot_48 x3500
     x2 = Curry_Prelude.d_C_foldr (acceptCs id d_OP_flattenDeps_dot_checkdep_dot_47) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List)
      in (Curry_Prelude.d_OP_dot x2 x1 x3500)

nd_C_flattenDeps :: IDSupply -> ConstStore -> Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
nd_C_flattenDeps x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2000 = leftSupply x2002
          x2001 = rightSupply x2002
           in (seq x2000 (seq x2001 (let
               x1 = Curry_SCC.nd_C_scc (wrapDX id d_OP_flattenDeps_dot_modules_dot_48) (wrapDX id d_OP_flattenDeps_dot_imports_dot_48) x2000 x3500
               x2 = wrapNX id (Curry_Prelude.nd_C_foldr (wrapDX (wrapDX id) (acceptCs id d_OP_flattenDeps_dot_checkdep_dot_47)) (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List))
                in (Curry_Prelude.nd_OP_dot x2 x1 x2001 x3500))))))

d_OP_flattenDeps_dot_modules_dot_48 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => Curry_Prelude.OP_Tuple2 t0 t1 -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_flattenDeps_dot_modules_dot_48 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_modules_dot_48 x1002 x3500) (d_OP_flattenDeps_dot_modules_dot_48 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_modules_dot_48 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_modules_dot_48 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_imports_dot_48 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_Tuple2 t0 (Curry_Prelude.OP_Tuple2 t1 Curry_FlatCurry.C_Prog) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_flattenDeps_dot_imports_dot_48 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_6 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_imports_dot_48 x1002 x3500) (d_OP_flattenDeps_dot_imports_dot_48 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_imports_dot_48 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_imports_dot_48 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_cyclicError_dot_47 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_flattenDeps_dot_cyclicError_dot_47 x1 x3500 = let
     x2 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 x1 x3500
     x3 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits x2 x3500
     x4 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last x2 x3500
      in (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_List.d_C_intercalate (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))) x4 x3500) x3500) x3500)

d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) t0
d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_4 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 x1002 x3500) (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs :: Curry_Prelude.Curry t122 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t122) t122 -> ConstStore -> Curry_Prelude.OP_List t122
d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x1002 x3500) (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z :: Curry_Prelude.Curry t122 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t122) t122 -> ConstStore -> t122
d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x1002 x3500) (d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits x1002 x3500) (d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP14_hash_inits x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last x1002 x3500) (d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_cyclicError_dot_47_dot___hash_selFP15_hash_last x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_flattenDeps_dot_checkdep_dot_47 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) t0)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_flattenDeps_dot_checkdep_dot_47 x1 x2 x3500 = case x1 of
     Curry_Prelude.OP_List -> d_OP__case_3 x2 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_2 x1 x2 x5 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_flattenDeps_dot_checkdep_dot_47 x1002 x2 x3500) (d_OP_flattenDeps_dot_checkdep_dot_47 x1003 x2 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_flattenDeps_dot_checkdep_dot_47 z x2 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_flattenDeps_dot_checkdep_dot_47 x1002 x2) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x5 x6 x3500 = case x6 of
     Curry_Prelude.OP_List -> d_OP__case_1 x5 x2 x3500
     (Curry_Prelude.OP_Cons x9 x10) -> d_OP__case_0 x1 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x2 x5 x1002 x3500) (d_OP__case_2 x1 x2 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 x2 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x5 x2 x2000 x3500))
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x1 x2 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_2 x1 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> Curry_Prelude.OP_Tuple2 x11 (Curry_Prelude.OP_Cons (d_OP_flattenDeps_dot_cyclicError_dot_47 (Curry_Prelude.d_C_map Curry_Prelude.d_C_fst x1 x3500) x3500) x12)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1 x1002 x3500) (d_OP__case_0 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.OP_Tuple2 x11 (Curry_Prelude.OP_Cons (d_OP_flattenDeps_dot_cyclicError_dot_47 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_fst) x1 x2000 x3500) x3500) x12)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1 x1002 x3000 x3500) (nd_OP__case_0 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x5 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x5 x7) x8
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x5 x1002 x3500) (d_OP__case_1 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x5 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x5 x7) x8
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x5 x1002 x3000 x3500) (nd_OP__case_1 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1002 x3500) (d_OP__case_3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> Curry_Prelude.OP_Tuple2 x3 x4
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1002 x3000 x3500) (nd_OP__case_3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x2
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 (Curry_Prelude.OP_Cons x4 x5) x3500
          x7 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x6 x3500
          x8 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x6 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x7) x8)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x2 x1002 x3500) (d_OP__case_4 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List x2
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x6 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71 (Curry_Prelude.OP_Cons x4 x5) x3500
          x7 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP12_hash_xs x6 x3500
          x8 = d_OP_flattenDeps_dot_cyclicError_dot_47_dot_splitLast_dot_71_dot___hash_selFP13_hash_z x6 x3500
           in (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons x2 x7) x8)
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x2 x1002 x3000 x3500) (nd_OP__case_4 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_5 x5 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1002 x3500) (d_OP__case_6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x5 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1002 x3000 x3500) (nd_OP__case_6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x5 x3500 = case x5 of
     (Curry_FlatCurry.C_Prog x6 x7 x8 x9 x10) -> x7
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1002 x3500) (d_OP__case_5 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x5 x3000 x3500 = case x5 of
     (Curry_FlatCurry.C_Prog x6 x7 x8 x9 x10) -> x7
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1002 x3000 x3500) (nd_OP__case_5 x1003 x3000 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 z x3000 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x3 x5 x3500 = case x5 of
     (Curry_Prelude.C_Just x4) -> Curry_Prelude.d_C_return x2 x3500
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_OP_gt_gt_eq (d_C_lookupModule x1 x3 x3500) (d_OP_moduleDeps_dot___hash_lambda8 x3 x2 x1) x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x3 x1002 x3500) (d_OP__case_7 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x3 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.C_Just x4) -> Curry_Prelude.d_C_return x2 x3500
     Curry_Prelude.C_Nothing -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_lookupModule x1 x3 x3500) (wrapNX id (nd_OP_moduleDeps_dot___hash_lambda8 x3 x2 x1)) x2000 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x3 x3500) (d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x2) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x3 x4 x1002 x3500) (d_OP__case_8 x1 x2 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x3 x3500) (wrapDX id (d_OP_isFlatCurryValid_dot___hash_lambda3_dot___hash_lambda4_dot___hash_lambda5 x2)) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x3 x4 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x5 x8 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 x8 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x5 x8 x1002 x3500) (d_OP__case_9 x5 x8 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x5 x8 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x5 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x5 x8 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus x5 x8 x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x5 x8 x1002 x3000 x3500) (nd_OP__case_9 x5 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x5 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x5 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
