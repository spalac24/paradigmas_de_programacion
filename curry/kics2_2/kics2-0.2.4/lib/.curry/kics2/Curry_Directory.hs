{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_Directory (d_C_doesFileExist, d_C_doesDirectoryExist, d_C_fileSize, d_C_getModificationTime, d_C_setCurrentDirectory, d_C_getDirectoryContents, d_C_createDirectory, d_C_createDirectoryIfMissing, d_C_removeDirectory, d_C_renameDirectory, d_C_getHomeDirectory, d_C_getTemporaryDirectory, d_C_removeFile, d_C_renameFile, d_C_copyFile, d_C_getCurrentDirectory) where

import Basics
import qualified Curry_FilePath
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_System
import qualified Curry_Time
import System.Directory
import System.IO
import System.Time

import qualified Curry_Prelude as CP


d_C_doesFileExist :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_doesFileExist x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_doesFileExist x1 x3500

d_C_doesDirectoryExist :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_doesDirectoryExist x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_doesDirectoryExist x1 x3500

d_C_fileSize :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_fileSize x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_fileSize x1 x3500

d_C_getModificationTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Time.C_ClockTime
d_C_getModificationTime x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_getModificationTime x1 x3500

d_C_setCurrentDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_setCurrentDirectory x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_setCurrentDirectory x1 x3500

d_C_getDirectoryContents :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getDirectoryContents x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_getDirectoryContents x1 x3500

d_C_createDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDirectory x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_createDirectory x1 x3500

d_C_createDirectoryIfMissing :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDirectoryIfMissing x1 x2 x3500 = let
     x3 = Curry_Prelude.d_OP_dollar (Curry_List.d_C_scanl1 (Curry_FilePath.d_OP_lt_slash_gt x3500)) (Curry_Prelude.d_OP_dollar Curry_FilePath.d_C_splitDirectories x2 x3500) x3500
      in (d_OP__case_3 x3 x1 x3500)

d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_done x3500
     (Curry_Prelude.OP_Cons x2 x3) -> Curry_Prelude.d_OP_gt_gt_eq (d_C_doesDirectoryExist x2 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_16_dot___hash_lambda1 x2 x3) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x1002 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_createDirectoryIfMissing_dot_createDirs_dot_16_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDirectoryIfMissing_dot_createDirs_dot_16_dot___hash_lambda1 x1 x2 x3 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_2 x1 x3 x3500) (d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x2 x3500) x3500

d_C_removeDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_removeDirectory x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_removeDirectory x1 x3500

d_C_renameDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_renameDirectory x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_renameDirectory) x1 x3500) x2 x3500

d_C_getHomeDirectory :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getHomeDirectory x3500 = d_OP__case_1 (Curry_System.d_C_isWindows x3500) x3500

d_C_getTemporaryDirectory :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getTemporaryDirectory x3500 = d_OP__case_0 (Curry_System.d_C_isWindows x3500) x3500

d_C_removeFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_removeFile x1 x3500 = Curry_Prelude.d_OP_dollar_hash_hash d_C_prim_removeFile x1 x3500

d_C_renameFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_renameFile x1 x2 x3500 = Curry_Prelude.d_OP_dollar_hash_hash (Curry_Prelude.d_OP_dollar_hash_hash (acceptCs id d_C_prim_renameFile) x1 x3500) x2 x3500

d_C_copyFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_copyFile x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (Curry_Prelude.d_C_writeFile x2) x3500

d_OP__case_0 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) Curry_Prelude.OP_List))) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3500) (d_OP__case_0 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) Curry_Prelude.OP_List))) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) Curry_Prelude.OP_List)))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1002 x3000 x3500) (nd_OP__case_0 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List))))))))))) x3500
     Curry_Prelude.C_False -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1002 x3500) (d_OP__case_1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'S'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'L'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List))))))))))) x3500
     Curry_Prelude.C_False -> Curry_System.d_C_getEnviron (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) Curry_Prelude.OP_List)))) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1002 x3000 x3500) (nd_OP__case_1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> d_C_createDirectory x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3500
     Curry_Prelude.C_False -> d_C_createDirectory x1 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x3 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x3 x3500
     Curry_Prelude.C_False -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 (Curry_Prelude.OP_Cons (Curry_List.d_C_last x3 x3500) Curry_Prelude.OP_List) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x3 x1002 x3500) (d_OP__case_3 x3 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x3 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x3 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 x3 x3500
     Curry_Prelude.C_False -> d_OP_createDirectoryIfMissing_dot_createDirs_dot_16 (Curry_Prelude.OP_Cons (Curry_List.d_C_last x3 x3500) Curry_Prelude.OP_List) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x3 x1002 x3000 x3500) (nd_OP__case_3 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_prim_doesFileExist :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_doesFileExist x1 x3500 = external_d_C_prim_doesFileExist x1 x3500

d_C_prim_doesDirectoryExist :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_prim_doesDirectoryExist x1 x3500 = external_d_C_prim_doesDirectoryExist x1 x3500

d_C_prim_fileSize :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Int
d_C_prim_fileSize x1 x3500 = external_d_C_prim_fileSize x1 x3500

d_C_prim_getModificationTime :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Time.C_ClockTime
d_C_prim_getModificationTime x1 x3500 = external_d_C_prim_getModificationTime x1 x3500

d_C_getCurrentDirectory :: ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_getCurrentDirectory x3500 = external_d_C_getCurrentDirectory x3500

d_C_prim_setCurrentDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_setCurrentDirectory x1 x3500 = external_d_C_prim_setCurrentDirectory x1 x3500

d_C_prim_getDirectoryContents :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_prim_getDirectoryContents x1 x3500 = external_d_C_prim_getDirectoryContents x1 x3500

d_C_prim_createDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_createDirectory x1 x3500 = external_d_C_prim_createDirectory x1 x3500

d_C_prim_removeDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_removeDirectory x1 x3500 = external_d_C_prim_removeDirectory x1 x3500

d_C_prim_renameDirectory :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_renameDirectory x1 x2 x3500 = external_d_C_prim_renameDirectory x1 x2 x3500

d_C_prim_removeFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_removeFile x1 x3500 = external_d_C_prim_removeFile x1 x3500

d_C_prim_renameFile :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prim_renameFile x1 x2 x3500 = external_d_C_prim_renameFile x1 x2 x3500
external_d_C_prim_doesFileExist :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_doesFileExist s _ = toCurry doesFileExist s

external_d_C_prim_doesDirectoryExist :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Bool
external_d_C_prim_doesDirectoryExist s _ = toCurry doesDirectoryExist s

external_d_C_prim_fileSize :: CP.C_String -> ConstStore -> CP.C_IO CP.C_Int
external_d_C_prim_fileSize s _ = toCurry
  (\f -> do h <- openFile f ReadMode
            i <- hFileSize h
            hClose h
            return i
  ) s

external_d_C_prim_getModificationTime :: CP.C_String -> ConstStore
                                      -> CP.C_IO Curry_Time.C_ClockTime
external_d_C_prim_getModificationTime s _ = toCurry getModificationTime s

external_d_C_getCurrentDirectory :: ConstStore -> CP.C_IO (CP.C_String)
external_d_C_getCurrentDirectory _ = toCurry getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_setCurrentDirectory s _ = toCurry setCurrentDirectory s

external_d_C_prim_getDirectoryContents :: CP.C_String -> ConstStore
                                       -> CP.C_IO (CP.OP_List (CP.C_String))
external_d_C_prim_getDirectoryContents s _ = toCurry getDirectoryContents s

external_d_C_prim_createDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_createDirectory s _ = toCurry createDirectory s

external_d_C_prim_removeFile :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeFile s _ = toCurry removeFile s

external_d_C_prim_removeDirectory :: CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeDirectory s _ = toCurry removeDirectory s

external_d_C_prim_renameFile :: CP.C_String -> CP.C_String
                             -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameFile s1 s2 _ = toCurry renameFile s1 s2

external_d_C_prim_renameDirectory :: CP.C_String -> CP.C_String
                                  -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameDirectory s1 s2 _ = toCurry renameDirectory s1 s2

