{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_FlatCurryRead (d_C_readFlatCurryWithImports, d_C_readFlatCurryWithImportsInPath, d_C_readFlatCurryIntWithImports, d_C_readFlatCurryIntWithImportsInPath) where

import Basics
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurry
import qualified Curry_Prelude
import qualified Curry_Time
import qualified Curry_System
import qualified Curry_List
d_C_readFlatCurryWithImports :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_readFlatCurryWithImports x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile (Curry_FlatCurry.d_C_flatCurryFileName x1 x3250 x3500) x3250 x3500) (d_OP_readFlatCurryWithImports_dot___hash_lambda1 x1) x3250 x3500

d_OP_readFlatCurryWithImports_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryWithImports_dot___hash_lambda1 x1 x2 x3250 x3500 = d_C_readFlatCurryFileWithImports x2 (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3250 x3500

d_C_readFlatCurryWithImportsInPath :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_readFlatCurryWithImportsInPath x1 x2 x3250 x3500 = d_C_readFlatCurryFileWithImports x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List) x3250 x3500

d_C_readFlatCurryIntWithImports :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_readFlatCurryIntWithImports x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile (Curry_FlatCurry.d_C_flatCurryIntName x1 x3250 x3500) x3250 x3500) (d_OP_readFlatCurryIntWithImports_dot___hash_lambda2 x1) x3250 x3500

d_OP_readFlatCurryIntWithImports_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIntWithImports_dot___hash_lambda2 x1 x2 x3250 x3500 = d_C_readFlatCurryFileWithImports x2 (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)) x3250 x3500

d_C_readFlatCurryIntWithImportsInPath :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_readFlatCurryIntWithImportsInPath x1 x2 x3250 x3500 = d_C_readFlatCurryFileWithImports x1 x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)) x3250 x3500

d_C_readFlatCurryFileWithImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_readFlatCurryFileWithImports x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_tryReadFlatCurryFileWithImports x1 x2 x3 x3250 x3500) (d_OP_readFlatCurryFileWithImports_dot___hash_lambda3 x1 x2 x3) x3250 x3500) x3250 x3500

d_OP_readFlatCurryFileWithImports_dot___hash_lambda3 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryFileWithImports_dot___hash_lambda3 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_maybe (d_C_parseFlatCurryFileWithImports x1 x2 x3 x3250 x3500) Curry_Prelude.d_C_return x4 x3250 x3500

d_C_parseFlatCurryFileWithImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_C_parseFlatCurryFileWithImports x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Distribution.d_C_callFrontendWithParams Curry_Distribution.C_FCY (Curry_Distribution.d_C_setQuiet Curry_Prelude.C_True (Curry_Distribution.d_C_setFullPath x1 (Curry_Distribution.d_C_defaultParams x3250 x3500) x3250 x3500) x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))) x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x3 (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) Curry_Prelude.OP_List x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 x3 x4 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_2 x4 x5 x2 x6 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x5 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 x1002 x4 x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 x1003 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 z x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 x1002 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x5) x6 x3250 x3500

d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (Curry_FlatCurry.d_C_readFlatCurryFile x6 x3250 x3500) x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5) x3250 x3500

d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x2 x5 (Curry_Prelude.d_OP_plus_plus x4 (d_C_importsOf x6 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons x3 x1) x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x6) x3250 x3500

d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4_dot___hash_lambda5_dot___hash_lambda6_dot___hash_lambda7 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.OP_Cons x1 x2) x3250 x3500

d_C_tryReadFlatCurryFileWithImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_C_tryReadFlatCurryFileWithImports x1 x2 x3 x3250 x3500 = d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x3 (Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List) Curry_Prelude.OP_List x3250 x3500

d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 x3 x4 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_C_return (Curry_Prelude.C_Just Curry_Prelude.OP_List) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x5 x6) -> d_OP__case_1 x4 x5 x2 x6 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x5 x3250 x3500) x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 x1002 x4 x3250 x3500) (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 x1003 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 z x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 x1002 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500) (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x4 x5) x6 x3250 x3500

d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3 x4 x5 x6 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x2 x5 (Curry_Prelude.d_OP_plus_plus x4 (d_C_importsOf x6 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons x3 x1) x3250 x3500) (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 x6) x3250 x3500

d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3250 x3500 = Curry_Prelude.d_C_return (Curry_Prelude.d_C_maybe Curry_Prelude.C_Nothing (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1) x2 x3250 x3500) x3250 x3500

d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 :: Curry_FlatCurry.C_Prog -> Curry_Prelude.OP_List Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3250 x3500 = Curry_Prelude.C_Just (Curry_Prelude.OP_Cons x1 x2)

d_C_importsOf :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_importsOf x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> x3
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_importsOf x1002 x3250 x3500) (d_C_importsOf x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_importsOf z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_importsOf x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readFlatCurryIfPossible :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_C_readFlatCurryIfPossible x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath x2 (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x1 x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12 x1 x2 x3) x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath x2 x3 x1 x3250 x3500) d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13 x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15 x2 x3) x4 x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13 x1 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500) d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda13_dot___hash_lambda14 x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x1 x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Just) x3250 x3500) x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15 x1 x2 x3 x3250 x3500 = let
     x4 = Curry_FileGoodies.d_C_dirName x3 x3250 x3500
     x5 = Curry_Prelude.OP_Cons x4 (Curry_Prelude.OP_Cons (Curry_Distribution.d_C_addCurrySubdir x4 x3250 x3500) Curry_Prelude.OP_List)
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath x1 x2 x5 x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16 x3) x3250 x3500)

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16 x1 x2 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17 x1) x2 x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x2) x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x2 x1) x3250 x3500

d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 :: Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP_readFlatCurryIfPossible_dot___hash_lambda12_dot___hash_lambda15_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x3 x3250 x3500 = d_OP__case_0 x3 x1 x2 (Curry_Prelude.d_OP_gt x1 x3 x3250 x3500) x3250 x3500

d_OP__case_0 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe Curry_FlatCurry.C_Prog)
d_OP__case_0 x3 x1 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_Nothing x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStr (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (Curry_FlatCurry.d_C_readFlatCurryFile x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (acceptCs id Curry_Prelude.C_Just) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x3 x1 x2 x1002 x3250 x3500) (d_OP__case_0 x3 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x3 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x3 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog))
d_OP__case_1 x4 x5 x2 x6 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26 x1 x2 x6 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (d_C_readFlatCurryIfPossible x1 x5 x2 x3250 x3500) (d_OP_tryReadFlatCurryFileWithImports_dot_collectMods_dot_26_dot___hash_lambda8 x4 x1 x5 x6 x2) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x5 x2 x6 x1 x1002 x3250 x3500) (d_OP__case_1 x4 x5 x2 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x5 x2 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x5 x2 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_FlatCurry.C_Prog)
d_OP__case_2 x4 x5 x2 x6 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15 x1 x2 x6 x4 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath x5 x2 x1 x3250 x3500) (d_OP_parseFlatCurryFileWithImports_dot_collectMods_dot_15_dot___hash_lambda4 x4 x1 x5 x6 x2) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x4 x5 x2 x6 x1 x1002 x3250 x3500) (d_OP__case_2 x4 x5 x2 x6 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x4 x5 x2 x6 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x4 x5 x2 x6 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo