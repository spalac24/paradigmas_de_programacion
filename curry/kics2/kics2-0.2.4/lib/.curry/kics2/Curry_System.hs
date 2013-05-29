{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE CPP, ForeignFunctionInterface, MultiParamTypeClasses #-}

module Curry_System (d_C_getEnviron, d_C_setEnviron, d_C_unsetEnviron, d_C_system, d_C_exitWith, d_C_sleep, d_C_isPosix, d_C_getCPUTime, d_C_getElapsedTime, d_C_getArgs, d_C_getHostname, d_C_getPID, d_C_getProgName, d_C_isWindows) where

import Basics
import qualified Curry_Global
import qualified Curry_Prelude
import qualified Curry_Prelude as CP

import Control.Exception as C (IOException, handle)
import Network.BSD (getHostName)
import System.Cmd
import System.CPUTime (getCPUTime)
import System.Environment
import System.Exit

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.Win32.Process
#else
import System.Posix.Process (getProcessID)
#endif


d_C_getEnviron :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getEnviron x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_environ x3500) x3500) (d_OP_getEnviron_dot___hash_lambda1 x1) x3500

d_OP_getEnviron_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_getEnviron_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_getEnviron x1 x3500) Curry_Prelude.d_C_return (Curry_Prelude.d_C_lookup x1 x2 x3500) x3500

d_C_environ :: ConstStore -> Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
d_C_environ x0 = global_C_environ

global_C_environ :: Curry_Global.C_Global (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)))
global_C_environ = Curry_Global.d_C_global (let
     x3500 = emptyCs
      in Curry_Prelude.OP_List) Curry_Global.C_Temporary emptyCs

d_C_setEnviron :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setEnviron x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_environ x3500) x3500) (d_OP_setEnviron_dot___hash_lambda2 x1 x2) x3500

d_OP_setEnviron_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_setEnviron_dot___hash_lambda2 x1 x2 x3 x3500 = Curry_Global.d_C_writeGlobal (d_C_environ x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x1 x2) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) x1) Curry_Prelude.d_C_fst x3500) x3 x3500)) x3500

d_C_unsetEnviron :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_unsetEnviron x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_environ x3500) x3500) (d_OP_unsetEnviron_dot___hash_lambda3 x1) x3500

d_OP_unsetEnviron_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_unsetEnviron_dot___hash_lambda3 x1 x2 x3500 = Curry_Global.d_C_writeGlobal (d_C_environ x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) x1) Curry_Prelude.d_C_fst x3500) x2 x3500) x3500

d_C_system :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_system x1 x3500 = let
     x2 = d_C_isWindows x3500
     x3 = d_OP__case_3 x1 x2 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Global.d_C_readGlobal (d_C_environ x3500) x3500) (d_OP_system_dot___hash_lambda4 x3 x2) x3500)

d_OP_system_dot_escapeWinSpecials_dot_13 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_system_dot_escapeWinSpecials_dot_13 x1 x3500 = d_OP__case_2 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) Curry_Prelude.OP_List))))) x3500) x3500

d_OP_system_dot_encodeShellSpecials_dot_13 :: Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_system_dot_encodeShellSpecials_dot_13 x1 x3500 = d_OP__case_1 x1 (Curry_Prelude.d_OP_eq_eq x1 (Curry_Prelude.C_Char '\''#) x3500) x3500

d_OP_system_dot_envToExport_dot_13 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP_system_dot_envToExport_dot_13 x1 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_0 x3 x4 x1 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_system_dot_envToExport_dot_13 x1 x1002 x3500) (d_OP_system_dot_envToExport_dot_13 x1 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_system_dot_envToExport_dot_13 x1 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_system_dot_envToExport_dot_13 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_system_dot___hash_lambda4 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_OP_system_dot___hash_lambda4 x1 x2 x3 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap (d_OP_system_dot_envToExport_dot_13 x2) x3500) x3 x3500) x1 x3500) x3500

d_C_exitWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO t0
d_C_exitWith x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_exitWith x1 x3500

d_C_sleep :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_sleep x1 x3500 = Curry_Prelude.d_OP_dollar_hash d_C_prim_sleep x1 x3500

d_C_isPosix :: ConstStore -> Curry_Prelude.C_Bool
d_C_isPosix x3500 = Curry_Prelude.d_C_not (d_C_isWindows x3500) x3500

d_OP__case_0 x3 x4 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_system_dot_escapeWinSpecials_dot_13 x3500) x4 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3500) x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_system_dot_encodeShellSpecials_dot_13 x3500) x4 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x3500) x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x4 x1002 x3500) (d_OP__case_0 x3 x4 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x4 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x4 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x3 x4 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_system_dot_escapeWinSpecials_dot_13) x2000 x3500) x4 x2001 x3500)))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x3500) x3500) x3500) x3500))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_concatMap (wrapDX id d_OP_system_dot_encodeShellSpecials_dot_13) x2000 x3500) x4 x2001 x3500)))) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\''#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) x3500) x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x3 x4 x1002 x3000 x3500) (nd_OP__case_0 x3 x4 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 x3 x4 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x3 x4 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_map Curry_Prelude.d_C_chr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 34#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 34#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) Curry_Prelude.OP_List))))) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3500) (d_OP__case_1 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_map (wrapDX id Curry_Prelude.d_C_chr) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 34#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 34#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Int 39#) Curry_Prelude.OP_List))))) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x1002 x3000 x3500) (nd_OP__case_1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x1002 x3500) (d_OP__case_3 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List) x3500)
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x1002 x3000 x3500) (nd_OP__case_3 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_getCPUTime :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_getCPUTime x3500 = external_d_C_getCPUTime x3500

d_C_getElapsedTime :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_getElapsedTime x3500 = external_d_C_getElapsedTime x3500

d_C_getArgs :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getArgs x3500 = external_d_C_getArgs x3500

d_C_prim_getEnviron :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_prim_getEnviron x1 x3500 = external_d_C_prim_getEnviron x1 x3500

d_C_getHostname :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getHostname x3500 = external_d_C_getHostname x3500

d_C_getPID :: ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_getPID x3500 = external_d_C_getPID x3500

d_C_getProgName :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getProgName x3500 = external_d_C_getProgName x3500

d_C_prim_system :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_prim_system x1 x3500 = external_d_C_prim_system x1 x3500

d_C_prim_exitWith :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO t0
d_C_prim_exitWith x1 x3500 = external_d_C_prim_exitWith x1 x3500

d_C_prim_sleep :: Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_sleep x1 x3500 = external_d_C_prim_sleep x1 x3500

d_C_isWindows :: ConstStore -> Curry_Prelude.C_Bool
d_C_isWindows x3500 = external_d_C_isWindows x3500
-- #endimport - do not remove this line!

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import stdcall unsafe "windows.h GetCurrentProcessId"
  getProcessID :: IO ProcessId
#endif

external_d_C_getCPUTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getCPUTime _ = toCurry (getCPUTime >>= return . (`div` (10 ^ 9)))

external_d_C_getElapsedTime :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getElapsedTime _ = toCurry (return 0 :: IO Int)

external_d_C_getArgs :: ConstStore -> CP.C_IO (CP.OP_List CP.C_String)
external_d_C_getArgs _ = toCurry getArgs

external_d_C_prim_getEnviron :: CP.C_String -> ConstStore -> CP.C_IO CP.C_String
external_d_C_prim_getEnviron str _ =
  toCurry (handle handleIOException . getEnv) str
  where
  handleIOException :: IOException -> IO String
  handleIOException _ = return ""

external_d_C_getHostname :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getHostname _ = toCurry getHostName

external_d_C_getPID :: ConstStore -> CP.C_IO CP.C_Int
external_d_C_getPID _ = toCurry $ do
  pid <- getProcessID
  return (fromIntegral pid :: Int)

external_d_C_getProgName :: ConstStore -> CP.C_IO CP.C_String
external_d_C_getProgName _ = toCurry getProgName

external_d_C_prim_system :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_system str _ = toCurry system str

instance ConvertCurryHaskell CP.C_Int ExitCode where
  toCurry ExitSuccess     = toCurry (0 :: Int)
  toCurry (ExitFailure i) = toCurry i

  fromCurry j = let i = fromCurry j :: Int
                in if i == 0 then ExitSuccess else ExitFailure i

external_d_C_prim_exitWith :: CP.Curry a => CP.C_Int -> ConstStore -> CP.C_IO a
external_d_C_prim_exitWith c _ = fromIO (exitWith (fromCurry c))

external_d_C_prim_sleep :: CP.C_Int -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_sleep x _ =
  toCurry (\i -> system ("sleep "++show (i :: Int)) >> return ()) x -- TODO

external_d_C_isWindows :: ConstStore -> CP.C_Bool
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
external_d_C_isWindows _ = CP.C_True
#else
external_d_C_isWindows _ = CP.C_False
#endif

