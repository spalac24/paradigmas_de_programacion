{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Compile (d_C_main, d_C_build, d_C_locateCurryFile, d_C_makeModule, nd_C_makeModule, d_C_storeAnalysis, nd_C_storeAnalysis, d_C_loadAnalysis, nd_C_loadAnalysis, d_C_compileModule, nd_C_compileModule, d_C_extractFuncInfos, d_C_patchCurryTypeClassIntoPrelude, d_C_compMessage, d_C_filterPrelude, d_C_integrateExternals, d_C_lookupExternals, d_C_splitExternals, d_C_dump, d_C_rename) where

import Basics
import qualified Curry_AbstractHaskell
import qualified Curry_AbstractHaskellGoodies
import qualified Curry_AbstractHaskellPrinter
import qualified Curry_Base
import qualified Curry_Char
import qualified Curry_CompilerOpts
import qualified Curry_Directory
import qualified Curry_EliminateCond
import qualified Curry_FileGoodies
import qualified Curry_FilePath
import qualified Curry_Files
import qualified Curry_FiniteMap
import qualified Curry_FlatCurry
import qualified Curry_FlatCurry2AbstractHaskell
import qualified Curry_FlatCurryGoodies
import qualified Curry_LiftCase
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Message
import qualified Curry_ModuleDeps
import qualified Curry_Names
import qualified Curry_Prelude
import qualified Curry_ReadShowTerm
import qualified Curry_SimpleMake
import qualified Curry_TransFunctions
import qualified Curry_TransTypes
import qualified Curry_Utils
d_C_main :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_main x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_CompilerOpts.d_C_compilerOpts x3500) d_OP_main_dot___hash_lambda1 x3500

d_OP_main_dot___hash_lambda1 :: Curry_Prelude.OP_Tuple2 Curry_CompilerOpts.C_Options (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_main_dot___hash_lambda1 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_build x2) x3500) x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_main_dot___hash_lambda1 x1002 x3500) (d_OP_main_dot___hash_lambda1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_main_dot___hash_lambda1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_main_dot___hash_lambda1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_build :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_build x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_locateCurryFile x2 x3500) (d_OP_build_dot___hash_lambda2 x2 x1) x3500

d_OP_build_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CompilerOpts.C_Options -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_build_dot___hash_lambda2 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_OP_dollar Curry_Message.d_C_putErrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))) x1 x3500) x3500
     (Curry_Prelude.C_Just x4) -> let
          x5 = Curry_TransFunctions.d_OP___hash_updR_at_State_dot_compOptions (Curry_TransFunctions.d_C_defaultState x3500) x2 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_ModuleDeps.d_C_deps x2 x4 x3500) (d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x5) x3500)
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_build_dot___hash_lambda2 x1 x2 x1002 x3500) (d_OP_build_dot___hash_lambda2 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_build_dot___hash_lambda2 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_build_dot___hash_lambda2 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_build_dot___hash_lambda2_dot___hash_lambda4 :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_14 x1 x3 x4 (Curry_Prelude.d_C_null x4 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1002 x3500) (d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog))) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x1 x3 x4 (Curry_Prelude.d_C_null x4 x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1002 x3000 x3500) (nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_build_dot___hash_lambda2_dot___hash_lambda4 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_locateCurryFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_locateCurryFile x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x1 x3500) (d_OP_locateCurryFile_dot___hash_lambda5 x1) x3500

d_OP_locateCurryFile_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_locateCurryFile_dot___hash_lambda5 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return (Curry_Prelude.C_Just x1) x3500
     Curry_Prelude.C_False -> Curry_FileGoodies.d_C_lookupFileInPath x1 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List) Curry_Prelude.OP_List) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_locateCurryFile_dot___hash_lambda5 x1 x1002 x3500) (d_OP_locateCurryFile_dot___hash_lambda5 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_locateCurryFile_dot___hash_lambda5 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_locateCurryFile_dot___hash_lambda5 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_makeModule :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_C_makeModule x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_13 x1 x2 x3 x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_makeModule x1 x2 x1002 x3500) (d_C_makeModule x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_makeModule x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_makeModule x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_makeModule :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_C_makeModule x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x1 x2 x3 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_makeModule x1 x2 x1002 x3000 x3500) (nd_C_makeModule x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_makeModule x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_makeModule x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeModule_dot___hash_selFP2_hash_imps :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_makeModule_dot___hash_selFP2_hash_imps x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x3
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeModule_dot___hash_selFP2_hash_imps x1002 x3500) (d_OP_makeModule_dot___hash_selFP2_hash_imps x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeModule_dot___hash_selFP2_hash_imps z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeModule_dot___hash_selFP2_hash_imps x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_makeModule_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_makeModule_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x3500 = let
     x7 = Curry_Prelude.OP_Cons x2 (d_OP__case_9 x1 x6 x3500)
     x8 = Curry_Prelude.d_C_map (d_OP_makeModule_dot___hash_lambda6_dot___hash_lambda7 x4 x5) x3 x3500
      in (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_OP_plus_plus x7 x8 x3500) x3500)

d_OP_makeModule_dot___hash_lambda6_dot___hash_lambda7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) -> Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_makeModule_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar (Curry_Names.d_C_destFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x2 x3500) x3500) (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_fst (Curry_Prelude.d_OP_dollar Curry_Maybe.d_C_fromJust (Curry_Prelude.d_C_lookup x3 x1 x3500) x3500) x3500) x3500

d_OP_makeModule_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_CompilerOpts.C_Options -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_OP_makeModule_dot___hash_lambda8 x1 x2 x3 x4 x5 x6 x3500 = Curry_SimpleMake.d_C_smake (Curry_Prelude.d_C_apply (Curry_Names.d_C_destFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x4 x3500) x3500) x1 x3500) x6 (d_C_compileModule x3 x5 x2 x3500) (d_C_loadAnalysis x3 x5 x2 x3500) x3500

nd_OP_makeModule_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_CompilerOpts.C_Options -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_OP_makeModule_dot___hash_lambda8 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2002 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2002 (seq x2006 (let
               x2003 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2003 (seq x2004 (Curry_SimpleMake.d_C_smake (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_destFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x4 x3500) x2000 x3500) x1 x2001 x3500)))) x6 (nd_C_compileModule x3 x5 x2 x2003 x3500) (nd_C_loadAnalysis x3 x5 x2 x2004 x3500) x3500))))))))

d_C_storeAnalysis :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple4 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_storeAnalysis x1 x2 x3 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x4 x5 x6 x7) -> let
          x8 = Curry_TransFunctions.d_OP___hash_selR_at_State_dot_compOptions x1 x3500
          x9 = Curry_Prelude.d_C_apply (Curry_Names.d_C_analysisFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x8 x3500) x3500) x3 x3500
          x10 = Curry_FiniteMap.d_C_showFM x5 x3500
          x11 = Curry_FiniteMap.d_C_showFM x6 x3500
          x12 = Curry_FiniteMap.d_C_showFM x7 x3500
          x13 = Curry_FiniteMap.d_C_showFM x4 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showDetail x8) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))) x9 x3500) x3500) (Curry_Files.d_C_writeQTermFileInDir x9 (Curry_Prelude.OP_Tuple4 x10 x11 x12 x13) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_storeAnalysis x1 x1002 x3 x3500) (d_C_storeAnalysis x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_storeAnalysis x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_storeAnalysis x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_storeAnalysis :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple4 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_storeAnalysis x1 x2 x3 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x4 x5 x6 x7) -> let
          x2009 = x3000
           in (seq x2009 (let
               x2010 = leftSupply x2009
               x2012 = rightSupply x2009
                in (seq x2010 (seq x2012 (let
                    x2000 = leftSupply x2010
                    x2011 = rightSupply x2010
                     in (seq x2000 (seq x2011 (let
                         x2003 = leftSupply x2011
                         x2004 = rightSupply x2011
                          in (seq x2003 (seq x2004 (let
                              x2013 = leftSupply x2012
                              x2014 = rightSupply x2012
                               in (seq x2013 (seq x2014 (let
                                   x2005 = leftSupply x2013
                                   x2006 = rightSupply x2013
                                    in (seq x2005 (seq x2006 (let
                                        x2007 = leftSupply x2014
                                        x2008 = rightSupply x2014
                                         in (seq x2007 (seq x2008 (let
                                             x8 = Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_compOptions x1 x2000 x3500
                                             x9 = let
                                                  x2002 = leftSupply x2003
                                                  x2001 = rightSupply x2003
                                                   in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_analysisFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x8 x3500) x2001 x3500) x3 x2002 x3500)))
                                             x10 = Curry_FiniteMap.nd_C_showFM x5 x2004 x3500
                                             x11 = Curry_FiniteMap.nd_C_showFM x6 x2005 x3500
                                             x12 = Curry_FiniteMap.nd_C_showFM x7 x2006 x3500
                                             x13 = Curry_FiniteMap.nd_C_showFM x4 x2007 x3500
                                              in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showDetail x8)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))) x9 x3500) x2008 x3500) (Curry_Files.d_C_writeQTermFileInDir x9 (Curry_Prelude.OP_Tuple4 x10 x11 x12 x13) x3500) x3500)))))))))))))))))))))
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_storeAnalysis x1 x1002 x3 x3000 x3500) (nd_C_storeAnalysis x1 x1003 x3 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_storeAnalysis x1 z x3 x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_storeAnalysis x1 x1002 x3 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_loadAnalysis :: Curry_Prelude.C_Int -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_C_loadAnalysis x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_8 x1 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_loadAnalysis x1 x2 x1002 x3500) (d_C_loadAnalysis x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_loadAnalysis x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_loadAnalysis x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_loadAnalysis :: Curry_Prelude.C_Int -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_C_loadAnalysis x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_8 x1 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_loadAnalysis x1 x2 x1002 x3000 x3500) (nd_C_loadAnalysis x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_loadAnalysis x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_loadAnalysis x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_loadAnalysis_dot___hash_lambda9 :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_OP_loadAnalysis_dot___hash_lambda9 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> Curry_Prelude.d_C_return (Curry_TransFunctions.d_OP___hash_updR_at_State_dot_typeMap (Curry_TransFunctions.d_OP___hash_updR_at_State_dot_hoResultCons (Curry_TransFunctions.d_OP___hash_updR_at_State_dot_hoResultFun (Curry_TransFunctions.d_OP___hash_updR_at_State_dot_ndResult x1 (Curry_FiniteMap.d_C_plusFM (Curry_TransFunctions.d_OP___hash_selR_at_State_dot_ndResult x1 x3500) (Curry_FiniteMap.d_C_readFM (acceptCs id Curry_Prelude.d_OP_lt) x3 x3500) x3500) x3500) (Curry_FiniteMap.d_C_plusFM (Curry_TransFunctions.d_OP___hash_selR_at_State_dot_hoResultFun x1 x3500) (Curry_FiniteMap.d_C_readFM (acceptCs id Curry_Prelude.d_OP_lt) x4 x3500) x3500) x3500) (Curry_FiniteMap.d_C_plusFM (Curry_TransFunctions.d_OP___hash_selR_at_State_dot_hoResultCons x1 x3500) (Curry_FiniteMap.d_C_readFM (acceptCs id Curry_Prelude.d_OP_lt) x5 x3500) x3500) x3500) (Curry_FiniteMap.d_C_plusFM (Curry_TransFunctions.d_OP___hash_selR_at_State_dot_typeMap x1 x3500) (Curry_FiniteMap.d_C_readFM (acceptCs id Curry_Prelude.d_OP_lt) x6 x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_loadAnalysis_dot___hash_lambda9 x1 x1002 x3500) (d_OP_loadAnalysis_dot___hash_lambda9 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_loadAnalysis_dot___hash_lambda9 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_loadAnalysis_dot___hash_lambda9 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_loadAnalysis_dot___hash_lambda9 :: Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple4 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_OP_loadAnalysis_dot___hash_lambda9 x1 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple4 x3 x4 x5 x6) -> let
          x2029 = x3000
           in (seq x2029 (Curry_Prelude.d_C_return (let
               x2028 = leftSupply x2029
               x2030 = rightSupply x2029
                in (seq x2028 (seq x2030 (let
                    x2021 = leftSupply x2030
                    x2026 = rightSupply x2030
                     in (seq x2021 (seq x2026 (Curry_TransFunctions.nd_OP___hash_updR_at_State_dot_typeMap (let
                         x2020 = leftSupply x2021
                         x2022 = rightSupply x2021
                          in (seq x2020 (seq x2022 (let
                              x2013 = leftSupply x2022
                              x2018 = rightSupply x2022
                               in (seq x2013 (seq x2018 (Curry_TransFunctions.nd_OP___hash_updR_at_State_dot_hoResultCons (let
                                   x2012 = leftSupply x2013
                                   x2014 = rightSupply x2013
                                    in (seq x2012 (seq x2014 (let
                                        x2006 = leftSupply x2014
                                        x2010 = rightSupply x2014
                                         in (seq x2006 (seq x2010 (Curry_TransFunctions.nd_OP___hash_updR_at_State_dot_hoResultFun (let
                                             x2005 = leftSupply x2006
                                             x2003 = rightSupply x2006
                                              in (seq x2005 (seq x2003 (Curry_TransFunctions.nd_OP___hash_updR_at_State_dot_ndResult x1 (let
                                                  x2002 = leftSupply x2003
                                                  x2004 = rightSupply x2003
                                                   in (seq x2002 (seq x2004 (let
                                                       x2000 = leftSupply x2004
                                                       x2001 = rightSupply x2004
                                                        in (seq x2000 (seq x2001 (Curry_FiniteMap.nd_C_plusFM (Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_ndResult x1 x2000 x3500) (Curry_FiniteMap.nd_C_readFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x3 x2001 x3500) x2002 x3500))))))) x2005 x3500)))) (let
                                             x2009 = leftSupply x2010
                                             x2011 = rightSupply x2010
                                              in (seq x2009 (seq x2011 (let
                                                  x2007 = leftSupply x2011
                                                  x2008 = rightSupply x2011
                                                   in (seq x2007 (seq x2008 (Curry_FiniteMap.nd_C_plusFM (Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_hoResultFun x1 x2007 x3500) (Curry_FiniteMap.nd_C_readFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x4 x2008 x3500) x2009 x3500))))))) x2012 x3500))))))) (let
                                   x2017 = leftSupply x2018
                                   x2019 = rightSupply x2018
                                    in (seq x2017 (seq x2019 (let
                                        x2015 = leftSupply x2019
                                        x2016 = rightSupply x2019
                                         in (seq x2015 (seq x2016 (Curry_FiniteMap.nd_C_plusFM (Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_hoResultCons x1 x2015 x3500) (Curry_FiniteMap.nd_C_readFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x5 x2016 x3500) x2017 x3500))))))) x2020 x3500))))))) (let
                         x2025 = leftSupply x2026
                         x2027 = rightSupply x2026
                          in (seq x2025 (seq x2027 (let
                              x2023 = leftSupply x2027
                              x2024 = rightSupply x2027
                               in (seq x2023 (seq x2024 (Curry_FiniteMap.nd_C_plusFM (Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_typeMap x1 x2023 x3500) (Curry_FiniteMap.nd_C_readFM (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_lt)) x6 x2024 x3500) x2025 x3500))))))) x2028 x3500))))))) x3500))
     (Curry_Prelude.Choice_OP_Tuple4 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_loadAnalysis_dot___hash_lambda9 x1 x1002 x3000 x3500) (nd_OP_loadAnalysis_dot___hash_lambda9 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple4 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_loadAnalysis_dot___hash_lambda9 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple4 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_loadAnalysis_dot___hash_lambda9 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple4 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_compileModule :: Curry_Prelude.C_Int -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_C_compileModule x1 x2 x3 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_6 x1 x2 x5 x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_compileModule x1 x2 x1002 x3500) (d_C_compileModule x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_compileModule x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_compileModule x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_compileModule :: Curry_Prelude.C_Int -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_FlatCurry.C_Prog)) Curry_Prelude.C_Int -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_C_compileModule x1 x2 x3 x3000 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x1 x2 x5 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_compileModule x1 x2 x1002 x3000 x3500) (nd_C_compileModule x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_compileModule x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_compileModule x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot_fcyFile_dot_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_compileModule_dot_fcyFile_dot_52 x1 x3500 = Curry_Files.d_C_withExtension (Curry_Prelude.d_C_const (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))) x1 x3500

d_OP_compileModule_dot_ahsFile_dot_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_compileModule_dot_ahsFile_dot_52 x1 x3500 = Curry_Files.d_C_withExtension (Curry_Prelude.d_C_const (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))) x1 x3500

d_OP_compileModule_dot___hash_selFP10_hash_renamed :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FlatCurry.C_Prog
d_OP_compileModule_dot___hash_selFP10_hash_renamed x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x1
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_selFP10_hash_renamed x1002 x3500) (d_OP_compileModule_dot___hash_selFP10_hash_renamed x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_selFP10_hash_renamed z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_selFP10_hash_renamed x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_selFP11_hash_ts :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP_compileModule_dot___hash_selFP11_hash_ts x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x4
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_selFP11_hash_ts x1002 x3500) (d_OP_compileModule_dot___hash_selFP11_hash_ts x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_selFP11_hash_ts z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_selFP11_hash_ts x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Prog (Curry_Prelude.OP_Tuple4 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass))) Curry_TransFunctions.C_State -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x10 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3500) (d_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP_compileModule_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 Curry_FlatCurry.C_Prog (Curry_Prelude.OP_Tuple4 (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_NDClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass) (Curry_FiniteMap.C_FM (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Base.C_HOClass))) Curry_TransFunctions.C_State -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000 x3500) (nd_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_compileModule_dot___hash_lambda10 x1 x2 x3 x4 x5 x6 x7 x8 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_AbstractHaskell.C_Prog
d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x1
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x1002 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x2
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x1002 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x3
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x1002 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl
d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x5
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x1002 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_OpDecl
d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> x6
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x1002 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Curry_CompilerOpts.C_Options -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
d_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showDetail x4) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x1 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Files.d_C_writeFileInDir x1 x6 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showDetail x4) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) x2 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Files.d_C_writeQTermFileInDir x2 (d_C_extractFuncInfos x3 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showDetail x4) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3500) (Curry_Prelude.d_C_return x5 x3500) x3500) x3500) x3500) x3500) x3500

nd_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Curry_CompilerOpts.C_Options -> Curry_TransFunctions.C_State -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_TransFunctions.C_State
nd_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x4 x5 x6 x3000 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2000 = leftSupply x2004
          x2003 = rightSupply x2004
           in (seq x2000 (seq x2003 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showDetail x4)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))) x1 x3500) x2000 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Files.d_C_writeFileInDir x1 x6 x3500) (let
               x2001 = leftSupply x2003
               x2002 = rightSupply x2003
                in (seq x2001 (seq x2002 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showDetail x4)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) x2 x3500) x2001 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Files.d_C_writeQTermFileInDir x2 (d_C_extractFuncInfos x3 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showDetail x4)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2002 x3500) (Curry_Prelude.d_C_return x5 x3500) x3500) x3500) x3500)))) x3500) x3500)))))

d_C_extractFuncInfos :: Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool)
d_C_extractFuncInfos x1 x3500 = Curry_Prelude.d_C_map d_OP_extractFuncInfos_dot___hash_lambda12 x1 x3500

d_OP_extractFuncInfos_dot_withIOResult_dot_91 :: Curry_AbstractHaskell.C_TypeExpr -> ConstStore -> Curry_Prelude.C_Bool
d_OP_extractFuncInfos_dot_withIOResult_dot_91 x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_TVar x2) -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_FuncType x3 x4) -> d_OP_extractFuncInfos_dot_withIOResult_dot_91 x4 x3500
     (Curry_AbstractHaskell.C_TCons x5 x6) -> Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) Curry_Prelude.OP_List))))) x3500
     (Curry_AbstractHaskell.Choice_C_TypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_extractFuncInfos_dot_withIOResult_dot_91 x1002 x3500) (d_OP_extractFuncInfos_dot_withIOResult_dot_91 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_extractFuncInfos_dot_withIOResult_dot_91 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_extractFuncInfos_dot_withIOResult_dot_91 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_extractFuncInfos_dot_isIO_dot_91 :: Curry_AbstractHaskell.C_TypeSig -> ConstStore -> Curry_Prelude.C_Bool
d_OP_extractFuncInfos_dot_isIO_dot_91 x1 x3500 = case x1 of
     Curry_AbstractHaskell.C_Untyped -> Curry_Prelude.C_False
     (Curry_AbstractHaskell.C_FType x2) -> d_OP_extractFuncInfos_dot_withIOResult_dot_91 x2 x3500
     (Curry_AbstractHaskell.C_CType x3 x4) -> d_OP_extractFuncInfos_dot_withIOResult_dot_91 x4 x3500
     (Curry_AbstractHaskell.Choice_C_TypeSig x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_extractFuncInfos_dot_isIO_dot_91 x1002 x3500) (d_OP_extractFuncInfos_dot_isIO_dot_91 x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_TypeSig x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_extractFuncInfos_dot_isIO_dot_91 z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_TypeSig x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_extractFuncInfos_dot_isIO_dot_91 x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_TypeSig x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_extractFuncInfos_dot___hash_lambda12 :: Curry_AbstractHaskell.C_FuncDecl -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool
d_OP_extractFuncInfos_dot___hash_lambda12 x1 x3500 = Curry_Prelude.OP_Tuple2 (Curry_AbstractHaskellGoodies.d_C_funcName x1 x3500) (d_OP_extractFuncInfos_dot_isIO_dot_91 (Curry_AbstractHaskellGoodies.d_C_typeOf x1 x3500) x3500)

d_C_patchCurryTypeClassIntoPrelude :: Curry_AbstractHaskell.C_Prog -> ConstStore -> Curry_AbstractHaskell.C_Prog
d_C_patchCurryTypeClassIntoPrelude x1 x3500 = case x1 of
     (Curry_AbstractHaskell.C_Prog x2 x3 x4 x5 x6) -> let
          x7 = Curry_AbstractHaskell.C_Type (Curry_Prelude.OP_Tuple2 (Curry_Names.d_C_curryPrelude x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_AbstractHaskell.C_Public Curry_Prelude.OP_List Curry_Prelude.OP_List
           in (d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Names.d_C_curryPrelude x3500) x3500) x3500)
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_patchCurryTypeClassIntoPrelude x1002 x3500) (d_C_patchCurryTypeClassIntoPrelude x1003 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_patchCurryTypeClassIntoPrelude z x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_patchCurryTypeClassIntoPrelude x1002) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_compMessage :: Curry_Prelude.C_Int -> Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_compMessage x1 x2 x3 x4 x5 x3500 = let
     x6 = Curry_Prelude.d_C_show x1 x3500
     x7 = Curry_Prelude.d_C_show x2 x3500
     x8 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_length x7 x3500
      in (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.d_OP_plus_plus (d_OP_compMessage_dot_fill_dot_113 x8 x6 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (Curry_FilePath.d_C_normalise x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_FilePath.d_C_normalise x5 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) x3500) x3500) x3500) x3500) x3500)) x3500) x3500) x3500) x3500))

d_OP_compMessage_dot_fill_dot_113 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_compMessage_dot_fill_dot_113 x1 x2 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_replicate (Curry_Prelude.d_OP_minus x1 (Curry_Prelude.d_C_length x2 x3500) x3500) (Curry_Prelude.C_Char ' '#) x3500) x2 x3500

d_C_filterPrelude :: Curry_CompilerOpts.C_Options -> Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_filterPrelude x1 x2 x3500 = case x2 of
     (Curry_FlatCurry.C_Prog x3 x4 x5 x6 x7) -> let
          x8 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem Curry_CompilerOpts.C_ExtNoImplicitPrelude x3500) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optExtensions x1 x3500) x3500
           in (d_OP__case_1 x2 x3 x4 x5 x6 x7 x8 x3500)
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_filterPrelude x1 x1002 x3500) (d_C_filterPrelude x1 x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_filterPrelude x1 z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_filterPrelude x1 x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_integrateExternals :: Curry_CompilerOpts.C_Options -> Curry_AbstractHaskell.C_Prog -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_integrateExternals x1 x2 x3 x3500 = case x2 of
     (Curry_AbstractHaskell.C_Prog x4 x5 x6 x7 x8) -> let
          x9 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'G'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))) x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (d_C_lookupExternals x1 (Curry_Prelude.d_C_apply (Curry_FilePath.d_C_dropExtension x3500) x3 x3500) x3500) (d_OP_integrateExternals_dot___hash_lambda13 x9 x7 x5 x4 x8 x6) x3500)
     (Curry_AbstractHaskell.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_integrateExternals x1 x1002 x3 x3500) (d_C_integrateExternals x1 x1003 x3 x3500)
     (Curry_AbstractHaskell.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_integrateExternals x1 z x3 x3500) x1002
     (Curry_AbstractHaskell.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_integrateExternals x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_AbstractHaskell.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_integrateExternals_dot___hash_lambda13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_FuncDecl -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_OpDecl -> Curry_Prelude.OP_List Curry_AbstractHaskell.C_TypeDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_integrateExternals_dot___hash_lambda13 x1 x2 x3 x4 x5 x6 x7 x3500 = let
     x8 = d_C_splitExternals x7 x3500
     x9 = d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas x8 x3500
     x10 = d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps x8 x3500
     x11 = d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls x8 x3500
      in (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_return (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_unlines (Curry_Prelude.d_C_filter (Curry_Utils.d_C_notNull x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons x1 x9) x3500) (Curry_Prelude.OP_Cons (Curry_AbstractHaskellPrinter.d_C_showModuleHeader x4 x6 x2 x3 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_unlines x10 x3500) (Curry_Prelude.OP_Cons (Curry_AbstractHaskellPrinter.d_C_showDecls x4 x5 x6 x2 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_unlines x11 x3500) Curry_Prelude.OP_List))))) x3500) x3500) x3500)

d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x2
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas x1002 x3500) (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP13_hash_pragmas x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x3
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps x1002 x3500) (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP14_hash_extimps x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls :: Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple3 x2 x3 x4) -> x4
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls x1002 x3500) (d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_integrateExternals_dot___hash_lambda13_dot___hash_selFP15_hash_extdecls x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_lookupExternals :: Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_lookupExternals x1 x2 x3500 = let
     x3 = Curry_Prelude.d_C_apply (Curry_Names.d_C_externalFile x3500) x2 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x3 x3500) (d_OP_lookupExternals_dot___hash_lambda14 x3 x1) x3500)

d_OP_lookupExternals_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CompilerOpts.C_Options -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_lookupExternals_dot___hash_lambda14 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))) x3500) (Curry_Prelude.d_C_readFile x1 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))))))))))))))) x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_lookupExternals_dot___hash_lambda14 x1 x2 x1002 x3500) (d_OP_lookupExternals_dot___hash_lambda14 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_lookupExternals_dot___hash_lambda14 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_lookupExternals_dot___hash_lambda14 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_splitExternals :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_splitExternals x1 x3500 = let
     x2 = Curry_Prelude.d_C_span d_OP_splitExternals_dot_isPragma_dot_133 (Curry_Prelude.d_C_lines x1 x3500) x3500
     x3 = d_OP_splitExternals_dot___hash_selFP20_hash_pragmas x2 x3500
     x4 = d_OP_splitExternals_dot___hash_selFP21_hash_rest x2 x3500
     x5 = Curry_Prelude.d_C_span d_OP_splitExternals_dot_isImport_dot_133 x4 x3500
     x6 = d_OP_splitExternals_dot___hash_selFP18_hash_imports x5 x3500
     x7 = d_OP_splitExternals_dot___hash_selFP19_hash_decls x5 x3500
      in (Curry_Prelude.OP_Tuple3 x3 x6 x7)

d_OP_splitExternals_dot_isPragma_dot_133 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitExternals_dot_isPragma_dot_133 x1 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3500) x1 x3500) (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) x1 x3500) x3500

d_OP_splitExternals_dot___hash_selFP20_hash_pragmas :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_splitExternals_dot___hash_selFP20_hash_pragmas x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitExternals_dot___hash_selFP20_hash_pragmas x1002 x3500) (d_OP_splitExternals_dot___hash_selFP20_hash_pragmas x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitExternals_dot___hash_selFP20_hash_pragmas z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitExternals_dot___hash_selFP20_hash_pragmas x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitExternals_dot___hash_selFP21_hash_rest :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_splitExternals_dot___hash_selFP21_hash_rest x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitExternals_dot___hash_selFP21_hash_rest x1002 x3500) (d_OP_splitExternals_dot___hash_selFP21_hash_rest x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitExternals_dot___hash_selFP21_hash_rest z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitExternals_dot___hash_selFP21_hash_rest x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitExternals_dot_isComment_dot_133 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitExternals_dot_isComment_dot_133 x1 x3500 = Curry_Prelude.d_OP_ampersand_ampersand (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x1 x3500) (Curry_Prelude.d_C_not (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))))))))))) x1 x3500) x3500) x3500

d_OP_splitExternals_dot_isImport_dot_133 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_OP_splitExternals_dot_isImport_dot_133 x1 x3500 = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3500) x1 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) x1 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_List.d_C_isPrefixOf (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List) x1 x3500) (d_OP_splitExternals_dot_isComment_dot_133 x1 x3500) x3500) x3500) x3500

d_OP_splitExternals_dot___hash_selFP18_hash_imports :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_splitExternals_dot___hash_selFP18_hash_imports x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitExternals_dot___hash_selFP18_hash_imports x1002 x3500) (d_OP_splitExternals_dot___hash_selFP18_hash_imports x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitExternals_dot___hash_selFP18_hash_imports z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitExternals_dot___hash_selFP18_hash_imports x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitExternals_dot___hash_selFP19_hash_decls :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_splitExternals_dot___hash_selFP19_hash_decls x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitExternals_dot___hash_selFP19_hash_decls x1002 x3500) (d_OP_splitExternals_dot___hash_selFP19_hash_decls x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitExternals_dot___hash_selFP19_hash_decls z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitExternals_dot___hash_selFP19_hash_decls x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_dump :: Curry_CompilerOpts.C_DumpFormat -> Curry_CompilerOpts.C_Options -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_dump x1 x2 x3 x4 x3500 = Curry_Prelude.d_OP_dollar (Curry_Utils.d_C_when (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optDump x2 x3500) x3500)) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showDetail x2) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) x3 x3500) x3500) (Curry_Files.d_C_writeFileInDir (Curry_Files.d_C_withDirectory (Curry_Prelude.d_C_flip (Curry_FilePath.d_OP_lt_slash_gt x3500) (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x2 x3500)) x3 x3500) x4 x3500) x3500) x3500

d_C_rename :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_FlatCurry.C_Prog
d_C_rename x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> let
          x7 = Curry_Prelude.d_C_apply (Curry_FlatCurryGoodies.d_C_updQNamesInProg Curry_Names.d_C_renameQName x3500) x1 x3500
          x8 = d_OP_rename_dot___hash_selFP23_hash_td x7 x3500
          x9 = d_OP_rename_dot___hash_selFP24_hash_fd x7 x3500
          x10 = d_OP_rename_dot___hash_selFP25_hash_od x7 x3500
           in (Curry_FlatCurry.C_Prog (Curry_Prelude.d_C_apply (Curry_Names.d_C_renameModule x3500) x2 x3500) (Curry_Prelude.d_C_map (Curry_Names.d_C_renameModule x3500) x3 x3500) x8 x9 x10)
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_rename x1002 x3500) (d_C_rename x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_rename z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_rename x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_rename_dot___hash_selFP23_hash_td :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl
d_OP_rename_dot___hash_selFP23_hash_td x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x4
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_rename_dot___hash_selFP23_hash_td x1002 x3500) (d_OP_rename_dot___hash_selFP23_hash_td x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_rename_dot___hash_selFP23_hash_td z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_rename_dot___hash_selFP23_hash_td x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_rename_dot___hash_selFP24_hash_fd :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl
d_OP_rename_dot___hash_selFP24_hash_fd x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x5
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_rename_dot___hash_selFP24_hash_fd x1002 x3500) (d_OP_rename_dot___hash_selFP24_hash_fd x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_rename_dot___hash_selFP24_hash_fd z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_rename_dot___hash_selFP24_hash_fd x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_rename_dot___hash_selFP25_hash_od :: Curry_FlatCurry.C_Prog -> ConstStore -> Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl
d_OP_rename_dot___hash_selFP25_hash_od x1 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x6
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_rename_dot___hash_selFP25_hash_od x1002 x3500) (d_OP_rename_dot___hash_selFP25_hash_od x1003 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_rename_dot___hash_selFP25_hash_od z x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_rename_dot___hash_selFP25_hash_od x1002) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_FlatCurry.C_Prog x3 (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Names.d_C_prelude x3500)) x4 x3500) x5 x6 x7
     Curry_Prelude.C_False -> d_OP__case_0 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_OP__case_1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_FlatCurry.C_Prog x3 (Curry_Prelude.nd_C_filter (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (Curry_Names.d_C_prelude x3500))) x4 x2000 x3500) x5 x6 x7))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x2 x1002 x3500) (d_OP__case_0 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x2 x1002 x3000 x3500) (nd_OP__case_0 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Prog x2 x3 (Curry_Prelude.OP_Cons x7 x4) x5 x6
     Curry_Prelude.C_False -> d_OP__case_2 x1 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1002 x3500) (d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_AbstractHaskell.C_Prog x2 x3 (Curry_Prelude.OP_Cons x7 x4) x5 x6
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x2 x3 x4 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x10 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> Curry_Prelude.d_OP_gt_gt (d_C_storeAnalysis x11 x13 x3 x3500) (let
          x14 = Curry_FlatCurry2AbstractHaskell.d_C_fcy2abs x12 x3500
          x15 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x14 x3500
          x16 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x14 x3500
          x17 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x14 x3500
          x18 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x14 x3500
          x19 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x14 x3500
           in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpFunDecls x6 x4 (Curry_Prelude.d_C_show x15 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) x3500) (let
               x20 = Curry_Prelude.d_C_apply (Curry_TransTypes.d_C_transTypes (Curry_TransFunctions.d_OP___hash_selR_at_State_dot_hoResultCons x11 x3500) x3500) x7 x3500
                in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpTypeDecls x6 x8 (Curry_Prelude.d_C_show x20 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500) (let
                    x21 = Curry_AbstractHaskell.C_Prog x16 (Curry_Prelude.d_OP_plus_plus (Curry_TransFunctions.d_C_defaultModules x3500) x17 x3500) x20 x18 x19
                    x22 = d_C_patchCurryTypeClassIntoPrelude x21 x3500
                     in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpAbstractHs x6 x1 (Curry_Prelude.d_C_show x22 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_integrateExternals x6 x22 x3 x3500) (d_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 x2 x5 x18 x6 x11) x3500) x3500) x3500)) x3500) x3500)) x3500) x3500)) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1002 x3500) (d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2000 = leftSupply x2008
               x2007 = rightSupply x2008
                in (seq x2000 (seq x2007 (Curry_Prelude.d_OP_gt_gt (nd_C_storeAnalysis x11 x13 x3 x2000 x3500) (let
                    x14 = Curry_FlatCurry2AbstractHaskell.d_C_fcy2abs x12 x3500
                    x15 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP5_hash_ahsFun x14 x3500
                    x16 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP6_hash_n x14 x3500
                    x17 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP7_hash_imps x14 x3500
                    x18 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP8_hash_funs x14 x3500
                    x19 = d_OP_compileModule_dot___hash_lambda10_dot___hash_selFP9_hash_ops x14 x3500
                     in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpFunDecls x6 x4 (Curry_Prelude.d_C_show x15 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))) x3500) (let
                         x2005 = leftSupply x2007
                         x2006 = rightSupply x2007
                          in (seq x2005 (seq x2006 (let
                              x20 = let
                                   x2004 = leftSupply x2005
                                   x2003 = rightSupply x2005
                                    in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_apply (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_TransTypes.nd_C_transTypes (Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_hoResultCons x11 x2001 x3500) x2002 x3500)))) x7 x2004 x3500)))
                               in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpTypeDecls x6 x8 (Curry_Prelude.d_C_show x20 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))) x3500) (let
                                   x21 = Curry_AbstractHaskell.C_Prog x16 (Curry_Prelude.d_OP_plus_plus (Curry_TransFunctions.d_C_defaultModules x3500) x17 x3500) x20 x18 x19
                                   x22 = d_C_patchCurryTypeClassIntoPrelude x21 x3500
                                    in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpAbstractHs x6 x1 (Curry_Prelude.d_C_show x22 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3500) (Curry_Prelude.nd_OP_gt_gt_eq (d_C_integrateExternals x6 x22 x3 x3500) (wrapNX id (nd_OP_compileModule_dot___hash_lambda10_dot___hash_lambda11 x2 x5 x18 x6 x11)) x2006 x3500) x3500) x3500)) x3500) x3500))))) x3500) x3500)) x3500)))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1002 x3000 x3500) (nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x2 x3 x4 x5 x6 x7 x8 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x1 x2 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_5 x1 x2 x5 x6 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x2 x5 x1002 x3500) (d_OP__case_6 x1 x2 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x2 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x1 x2 x5 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_6 x1 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x1 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x1 x2 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x10 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_fcyFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List))))) x6 x3500) x3500
          x11 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_fcyFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))) x6 x3500) x3500
          x12 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_fcyFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) x6 x3500) x3500
          x13 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_fcyFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) x6 x3500) x3500
          x14 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_ahsFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x6 x3500) x3500
          x15 = Curry_Prelude.d_OP_dollar d_OP_compileModule_dot_ahsFile_dot_52 (Curry_Files.d_C_withBaseName (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_plus_plus) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) x6 x3500) x3500
          x16 = d_OP_compileModule_dot_ahsFile_dot_52 x6 x3500
          x17 = Curry_TransFunctions.d_OP___hash_selR_at_State_dot_compOptions x2 x3500
          x18 = Curry_Prelude.d_C_apply (Curry_Names.d_C_destFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x17 x3500) x3500) x8 x3500
          x19 = Curry_Prelude.d_C_apply (Curry_Names.d_C_funcInfoFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x17 x3500) x3500) x8 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showStatus x17) (d_C_compMessage x5 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) x6 x3500) x8 x18 x3500) x3500) (let
               x20 = d_C_filterPrelude x17 x9 x3500
                in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpFlat x17 x10 (Curry_Prelude.d_C_show x20 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))) x3500) (let
                    x21 = Curry_LiftCase.d_C_liftCases Curry_Prelude.C_True x20 x3500
                     in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpLifted x17 x11 (Curry_Prelude.d_C_show x21 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))))))) x3500) (let
                         x22 = Curry_EliminateCond.d_C_eliminateCond x21 x3500
                          in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpEliminated x17 x12 (Curry_Prelude.d_C_show x22 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (let
                              x23 = d_C_rename x22 x3500
                              x24 = d_OP_compileModule_dot___hash_selFP10_hash_renamed x23 x3500
                              x25 = d_OP_compileModule_dot___hash_selFP11_hash_ts x23 x3500
                               in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpRenamed x17 x13 (Curry_Prelude.d_C_show x24 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_apply (Curry_TransFunctions.d_C_unM (Curry_TransFunctions.d_C_transProg x24 x3500) x3500) x2 x3500) (d_OP_compileModule_dot___hash_lambda10 x16 x18 x8 x14 x19 x17 x25 x15) x3500) x3500) x3500)) x3500) x3500)) x3500) x3500)) x3500) x3500)) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x1 x2 x5 x6 x1002 x3500) (d_OP__case_5 x1 x2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x1 x2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x1 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x1 x2 x5 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2034 = x3000
           in (seq x2034 (let
               x2035 = leftSupply x2034
               x2039 = rightSupply x2034
                in (seq x2035 (seq x2039 (let
                    x2036 = leftSupply x2035
                    x2037 = rightSupply x2035
                     in (seq x2036 (seq x2037 (let
                         x2002 = leftSupply x2036
                         x2005 = rightSupply x2036
                          in (seq x2002 (seq x2005 (let
                              x2008 = leftSupply x2037
                              x2038 = rightSupply x2037
                               in (seq x2008 (seq x2038 (let
                                   x2011 = leftSupply x2038
                                   x2014 = rightSupply x2038
                                    in (seq x2011 (seq x2014 (let
                                        x2040 = leftSupply x2039
                                        x2041 = rightSupply x2039
                                         in (seq x2040 (seq x2041 (let
                                             x2017 = leftSupply x2040
                                             x2018 = rightSupply x2040
                                              in (seq x2017 (seq x2018 (let
                                                  x2021 = leftSupply x2041
                                                  x2042 = rightSupply x2041
                                                   in (seq x2021 (seq x2042 (let
                                                       x2024 = leftSupply x2042
                                                       x2033 = rightSupply x2042
                                                        in (seq x2024 (seq x2033 (let
                                                            x10 = let
                                                                 x2001 = leftSupply x2002
                                                                 x2000 = rightSupply x2002
                                                                  in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_fcyFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))))) x6 x2000 x3500) x2001 x3500)))
                                                            x11 = let
                                                                 x2004 = leftSupply x2005
                                                                 x2003 = rightSupply x2005
                                                                  in (seq x2004 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_fcyFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))) x6 x2003 x3500) x2004 x3500)))
                                                            x12 = let
                                                                 x2007 = leftSupply x2008
                                                                 x2006 = rightSupply x2008
                                                                  in (seq x2007 (seq x2006 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_fcyFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))))))))) x6 x2006 x3500) x2007 x3500)))
                                                            x13 = let
                                                                 x2010 = leftSupply x2011
                                                                 x2009 = rightSupply x2011
                                                                  in (seq x2010 (seq x2009 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_fcyFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))) x6 x2009 x3500) x2010 x3500)))
                                                            x14 = let
                                                                 x2013 = leftSupply x2014
                                                                 x2012 = rightSupply x2014
                                                                  in (seq x2013 (seq x2012 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_ahsFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))) x6 x2012 x3500) x2013 x3500)))
                                                            x15 = let
                                                                 x2016 = leftSupply x2017
                                                                 x2015 = rightSupply x2017
                                                                  in (seq x2016 (seq x2015 (Curry_Prelude.nd_OP_dollar (wrapDX id d_OP_compileModule_dot_ahsFile_dot_52) (Curry_Files.nd_C_withBaseName (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_plus_plus)) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))))) x6 x2015 x3500) x2016 x3500)))
                                                            x16 = d_OP_compileModule_dot_ahsFile_dot_52 x6 x3500
                                                            x17 = Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_compOptions x2 x2018 x3500
                                                            x18 = let
                                                                 x2020 = leftSupply x2021
                                                                 x2019 = rightSupply x2021
                                                                  in (seq x2020 (seq x2019 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_destFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x17 x3500) x2019 x3500) x8 x2020 x3500)))
                                                            x19 = let
                                                                 x2023 = leftSupply x2024
                                                                 x2022 = rightSupply x2024
                                                                  in (seq x2023 (seq x2022 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_funcInfoFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x17 x3500) x2022 x3500) x8 x2023 x3500)))
                                                             in (let
                                                                 x2025 = leftSupply x2033
                                                                 x2032 = rightSupply x2033
                                                                  in (seq x2025 (seq x2032 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showStatus x17)) (d_C_compMessage x5 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) x6 x3500) x8 x18 x3500) x2025 x3500) (let
                                                                      x20 = d_C_filterPrelude x17 x9 x3500
                                                                       in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpFlat x17 x10 (Curry_Prelude.d_C_show x20 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))))) x3500) (let
                                                                           x21 = Curry_LiftCase.d_C_liftCases Curry_Prelude.C_True x20 x3500
                                                                            in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpLifted x17 x11 (Curry_Prelude.d_C_show x21 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List))))))))))))))))))))))) x3500) (let
                                                                                x22 = Curry_EliminateCond.d_C_eliminateCond x21 x3500
                                                                                 in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpEliminated x17 x12 (Curry_Prelude.d_C_show x22 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))) x3500) (let
                                                                                     x23 = d_C_rename x22 x3500
                                                                                     x24 = d_OP_compileModule_dot___hash_selFP10_hash_renamed x23 x3500
                                                                                     x25 = d_OP_compileModule_dot___hash_selFP11_hash_ts x23 x3500
                                                                                      in (Curry_Prelude.d_OP_gt_gt (d_C_dump Curry_CompilerOpts.C_DumpRenamed x17 x13 (Curry_Prelude.d_C_show x24 x3500) x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Message.d_C_showDetail x17 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))))))))))))) x3500) (let
                                                                                          x2031 = leftSupply x2032
                                                                                          x2030 = rightSupply x2032
                                                                                           in (seq x2031 (seq x2030 (Curry_Prelude.nd_OP_gt_gt_eq (let
                                                                                               x2029 = leftSupply x2030
                                                                                               x2028 = rightSupply x2030
                                                                                                in (seq x2029 (seq x2028 (Curry_Prelude.nd_C_apply (let
                                                                                                    x2027 = leftSupply x2028
                                                                                                    x2026 = rightSupply x2028
                                                                                                     in (seq x2027 (seq x2026 (Curry_TransFunctions.nd_C_unM (Curry_TransFunctions.nd_C_transProg x24 x2026 x3500) x2027 x3500)))) x2 x2029 x3500)))) (wrapNX id (nd_OP_compileModule_dot___hash_lambda10 x16 x18 x8 x14 x19 x17 x25 x15)) x2031 x3500)))) x3500) x3500)) x3500) x3500)) x3500) x3500)) x3500) x3500)) x3500)))))))))))))))))))))))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x1 x2 x5 x6 x1002 x3000 x3500) (nd_OP__case_5 x1 x2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x1 x2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x1 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x1 x2 x5 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_7 x1 x2 x5 x6 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x1 x2 x5 x1002 x3500) (d_OP__case_8 x1 x2 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x1 x2 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x1 x2 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x1 x2 x5 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x1 x2 x5 x6 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x1 x2 x5 x1002 x3000 x3500) (nd_OP__case_8 x1 x2 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x1 x2 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x1 x2 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x1 x2 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x10 = Curry_TransFunctions.d_OP___hash_selR_at_State_dot_compOptions x2 x3500
          x11 = Curry_Prelude.d_C_apply (Curry_Names.d_C_analysisFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x10 x3500) x3500) x8 x3500
           in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar (Curry_Message.d_C_showStatus x10) (d_C_compMessage x5 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) x6 x3500) x8 x11 x3500) x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_ReadShowTerm.d_C_readQTermFile x11 x3500) (d_OP_loadAnalysis_dot___hash_lambda9 x2) x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1 x2 x5 x6 x1002 x3500) (d_OP__case_7 x1 x2 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x1 x2 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1 x2 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x1 x2 x5 x6 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2000 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2000 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2006 = rightSupply x2008
                     in (seq x2003 (seq x2006 (let
                         x10 = Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_compOptions x2 x2000 x3500
                         x11 = let
                              x2002 = leftSupply x2003
                              x2001 = rightSupply x2003
                               in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_analysisFile (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optOutputSubdir x10 x3500) x2001 x3500) x8 x2002 x3500)))
                          in (let
                              x2004 = leftSupply x2006
                              x2005 = rightSupply x2006
                               in (seq x2004 (seq x2005 (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.nd_OP_dollar (wrapDX id (Curry_Message.d_C_showStatus x10)) (d_C_compMessage x5 x1 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'z'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))) x6 x3500) x8 x11 x3500) x2004 x3500) (Curry_Prelude.nd_OP_gt_gt_eq (Curry_ReadShowTerm.d_C_readQTermFile x11 x3500) (wrapNX id (nd_OP_loadAnalysis_dot___hash_lambda9 x2)) x2005 x3500) x3500))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x1 x2 x5 x6 x1002 x3000 x3500) (nd_OP__case_7 x1 x2 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x1 x2 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x1 x2 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x1 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x1 x1002 x3500) (d_OP__case_9 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x1 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x1 x1002 x3000 x3500) (nd_OP__case_9 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x1 x2 x3 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_12 x1 x2 x3 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x1 x2 x3 x1002 x3500) (d_OP__case_13 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x1 x2 x3 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x1 x2 x3 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_13 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x1 x2 x3 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x10 = Curry_Prelude.d_C_apply (Curry_Names.d_C_externalFile x3500) x8 x3500
          x11 = d_OP_makeModule_dot___hash_selFP2_hash_imps x9 x3500
          x12 = Curry_TransFunctions.d_OP___hash_selR_at_State_dot_compOptions x2 x3500
          x13 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x10 x3500) (d_OP_makeModule_dot___hash_lambda6 x10 x8 x11 x1 x12) x3500
          x14 = Curry_Prelude.d_C_length x1 x3500
           in (d_OP__case_11 x2 x3 x8 x12 x13 x14 (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optForce x12 x3500) x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x1 x2 x3 x1002 x3500) (d_OP__case_12 x1 x2 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x1 x2 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x1 x2 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x1 x2 x3 x7 x3000 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2007 = leftSupply x2006
               x2008 = rightSupply x2006
                in (seq x2007 (seq x2008 (let
                    x2002 = leftSupply x2007
                    x2003 = rightSupply x2007
                     in (seq x2002 (seq x2003 (let
                         x2004 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2004 (seq x2005 (let
                              x10 = let
                                   x2001 = leftSupply x2002
                                   x2000 = rightSupply x2002
                                    in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Names.nd_C_externalFile x2000 x3500) x8 x2001 x3500)))
                              x11 = d_OP_makeModule_dot___hash_selFP2_hash_imps x9 x3500
                              x12 = Curry_TransFunctions.nd_OP___hash_selR_at_State_dot_compOptions x2 x2003 x3500
                              x13 = Curry_Prelude.nd_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x10 x3500) (wrapDX id (d_OP_makeModule_dot___hash_lambda6 x10 x8 x11 x1 x12)) x2004 x3500
                              x14 = Curry_Prelude.d_C_length x1 x3500
                               in (nd_OP__case_11 x2 x3 x8 x12 x13 x14 (Curry_CompilerOpts.d_OP___hash_selR_at_Options_dot_optForce x12 x3500) x2005 x3500))))))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x1 x2 x3 x1002 x3000 x3500) (nd_OP__case_12 x1 x2 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x1 x2 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x1 x2 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x2 x3 x8 x12 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> d_C_compileModule x14 x2 x3 x3500
     Curry_Prelude.C_False -> d_OP__case_10 x2 x3 x8 x12 x13 x14 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x3 x8 x12 x13 x14 x1002 x3500) (d_OP__case_11 x2 x3 x8 x12 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 x3 x8 x12 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x3 x8 x12 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x2 x3 x8 x12 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_compileModule x14 x2 x3 x2000 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x2 x3 x8 x12 x13 x14 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x2 x3 x8 x12 x13 x14 x1002 x3000 x3500) (nd_OP__case_11 x2 x3 x8 x12 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x2 x3 x8 x12 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x2 x3 x8 x12 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x2 x3 x8 x12 x13 x14 x15 x3500 = case x15 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq x13 (d_OP_makeModule_dot___hash_lambda8 x8 x3 x14 x12 x2) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x2 x3 x8 x12 x13 x14 x1002 x3500) (d_OP__case_10 x2 x3 x8 x12 x13 x14 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x2 x3 x8 x12 x13 x14 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x2 x3 x8 x12 x13 x14 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x2 x3 x8 x12 x13 x14 x15 x3000 x3500 = case x15 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq x13 (wrapNX id (nd_OP_makeModule_dot___hash_lambda8 x8 x3 x14 x12 x2)) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x2 x3 x8 x12 x13 x14 x1002 x3000 x3500) (nd_OP__case_10 x2 x3 x8 x12 x13 x14 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x2 x3 x8 x12 x13 x14 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x2 x3 x8 x12 x13 x14 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x1 x3 x4 x5 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_Utils.d_C_foldIO (acceptCs id (d_C_makeModule x3)) x1 (Curry_Prelude.d_C_zip x3 (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3500) x3500) x3500) (Curry_Prelude.d_C_done x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ Curry_Message.d_C_putErrLn x3500) x4 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x3 x4 x1002 x3500) (d_OP__case_14 x1 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x1 x3 x4 x5 x3000 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_Utils.nd_C_foldIO (wrapDX (wrapNX id) (acceptCs id (nd_C_makeModule x3))) x1 (Curry_Prelude.d_C_zip x3 (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3500) x3500) x2000 x3500) (Curry_Prelude.d_C_done x3500) x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_mapIO_ (wrapDX id Curry_Message.d_C_putErrLn) x2000 x3500) x4 x2001 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x1 x3 x4 x1002 x3000 x3500) (nd_OP__case_14 x1 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x1 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x1 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
