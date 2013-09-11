{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_CurryDoc (d_C_greeting, d_C_includeDir, d_C_main, d_C_processArgs, d_C_usageMessage, d_C_createDir, d_C_makeCompleteDoc, d_C_makeIndexPages, d_C_prepareDocDir, d_C_copyIncludeIfPresent, d_C_readAnaInfo, nd_C_readAnaInfo, d_C_makeDoc, d_C_makeDocWithComments, nd_C_makeDocWithComments, d_C_makeDocIfNecessary, d_C_getImports, d_C_copyOrMakeDoc, d_C_copyDocIfPossible, d_C_getDirName, d_C_readFlatCurryWithImports, d_C_findSourceFileInLoadPath, d_C_fileExtension, d_C_writeOutfile) where

import Basics
import qualified Curry_AnalysisServer
import qualified Curry_CurryDocCDoc
import qualified Curry_CurryDocConfig
import qualified Curry_CurryDocHtml
import qualified Curry_CurryDocParams
import qualified Curry_CurryDocRead
import qualified Curry_CurryDocTeX
import qualified Curry_Deterministic
import qualified Curry_Directory
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FilePath
import qualified Curry_FlatCurry
import qualified Curry_Indeterministic
import qualified Curry_Prelude
import qualified Curry_SolutionCompleteness
import qualified Curry_System
import qualified Curry_Time
import qualified Curry_TotallyDefined
import qualified Curry_List
d_C_greeting :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_greeting x3250 x3500 = Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) Curry_Prelude.OP_List)))))))))) (Curry_Prelude.d_OP_plus_plus (Curry_CurryDocConfig.d_C_currydocVersion x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'T'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\n'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500

d_C_includeDir :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_includeDir x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Distribution.d_C_installDir x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) x3250 x3500

d_C_main :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_main x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_System.d_C_getArgs x3250 x3500) d_OP_main_dot___hash_lambda1 x3250 x3500

d_OP_main_dot___hash_lambda1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_main_dot___hash_lambda1 x1 x3250 x3500 = d_C_processArgs (Curry_CurryDocParams.d_C_defaultCurryDocParams x3250 x3500) x1 x3250 x3500

d_C_processArgs :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_processArgs x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_111 x4 x1 x3 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_processArgs x1 x1002 x3250 x3500) (d_C_processArgs x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_processArgs x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_processArgs x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_usageMessage :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_usageMessage x3250 x3500 = Curry_Prelude.d_C_unlines (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'I'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'U'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) Curry_Prelude.OP_List)))) x3250 x3500

d_C_createDir :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_createDir x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesDirectoryExist x1 x3250 x3500) (d_OP_createDir_dot___hash_lambda3 x1) x3250 x3500

d_OP_createDir_dot___hash_lambda3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_createDir_dot___hash_lambda3 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'k'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_createDir_dot___hash_lambda3 x1 x1002 x3250 x3500) (d_OP_createDir_dot___hash_lambda3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_createDir_dot___hash_lambda3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_createDir_dot___hash_lambda3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_makeCompleteDoc :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_makeCompleteDoc x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (d_C_greeting x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_prepareDocDir (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) x3 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Distribution.d_C_callFrontend Curry_Distribution.C_FCY x4 x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_dollar (d_OP_makeCompleteDoc_dot_getProg_dot_39 x4) (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) x3250 x3500) (d_OP_makeCompleteDoc_dot___hash_lambda5 x3 x1 x4 x2) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_makeCompleteDoc_dot_getProg_dot_39 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocType -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_makeCompleteDoc_dot_getProg_dot_39 x1 x2 x3250 x3500 = case x2 of
     Curry_CurryDocParams.C_HtmlDoc -> d_C_readFlatCurryWithImports (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500
     Curry_CurryDocParams.C_TexDoc -> d_C_readFlatCurryWithImports (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) x3250 x3500
     Curry_CurryDocParams.C_CDoc -> Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurry x1 x3250 x3500) d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 x3250 x3500
     (Curry_CurryDocParams.Choice_C_DocType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompleteDoc_dot_getProg_dot_39 x1 x1002 x3250 x3500) (d_OP_makeCompleteDoc_dot_getProg_dot_39 x1 x1003 x3250 x3500)
     (Curry_CurryDocParams.Choices_C_DocType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompleteDoc_dot_getProg_dot_39 x1 z x3250 x3500) x1002
     (Curry_CurryDocParams.Guard_C_DocType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompleteDoc_dot_getProg_dot_39 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_CurryDocParams.Fail_C_DocType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 x4 x5 x6) x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 x1002 x3250 x3500) (d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompleteDoc_dot_getProg_dot_39_dot___hash_lambda4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_makeCompleteDoc_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeCompleteDoc_dot___hash_lambda5 x1 x2 x3 x4 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple3 x6 x7 x8) -> Curry_Prelude.d_OP_gt_gt (d_C_makeDocIfNecessary x2 x4 x1 x3 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_OP__case_12 x2 x6 x1 x7 x3 (Curry_CurryDocParams.d_C_withIndex x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeCompleteDoc_dot___hash_lambda5 x1 x2 x3 x4 x1002 x3250 x3500) (d_OP_makeCompleteDoc_dot___hash_lambda5 x1 x2 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeCompleteDoc_dot___hash_lambda5 x1 x2 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeCompleteDoc_dot___hash_lambda5 x1 x2 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_makeIndexPages :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_makeIndexPages x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (d_C_greeting x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_prepareDocDir Curry_CurryDocParams.C_HtmlDoc x1 x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_readFlatCurryWithImports x2 x3250 x3500) (d_OP_makeIndexPages_dot___hash_lambda6 x1 x2) x3250 x3500) x3250 x3500) x3250 x3500

d_OP_makeIndexPages_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeIndexPages_dot___hash_lambda6 x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple3 x4 x5 x6) -> Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_genMainIndexPage x1 x2 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_genFunctionIndexPage x1 x5 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_genConsIndexPage x1 x4 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'X'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))))))))))))))) x1 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeIndexPages_dot___hash_lambda6 x1 x2 x1002 x3250 x3500) (d_OP_makeIndexPages_dot___hash_lambda6 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeIndexPages_dot___hash_lambda6 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeIndexPages_dot___hash_lambda6 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_prepareDocDir :: Curry_CurryDocParams.C_DocType -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_prepareDocDir x1 x2 x3250 x3500 = case x1 of
     Curry_CurryDocParams.C_HtmlDoc -> Curry_Prelude.d_OP_gt_gt (d_C_createDir x2 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (d_C_copyIncludeIfPresent x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List))))))))))))))))))) x3250 x3500) (d_C_copyIncludeIfPresent x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_CurryDocParams.C_TexDoc -> Curry_Prelude.d_OP_gt_gt (d_C_createDir x2 x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_copyIncludeIfPresent x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_CurryDocParams.C_CDoc -> Curry_Prelude.d_OP_gt_gt (d_C_createDir x2 x3250 x3500) (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500
     (Curry_CurryDocParams.Choice_C_DocType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_prepareDocDir x1002 x2 x3250 x3500) (d_C_prepareDocDir x1003 x2 x3250 x3500)
     (Curry_CurryDocParams.Choices_C_DocType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_prepareDocDir z x2 x3250 x3500) x1002
     (Curry_CurryDocParams.Guard_C_DocType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_prepareDocDir x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_CurryDocParams.Fail_C_DocType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_copyIncludeIfPresent :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_copyIncludeIfPresent x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesDirectoryExist (d_C_includeDir x3250 x3500) x3250 x3500) (d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2) x3250 x3500

d_OP_copyIncludeIfPresent_dot___hash_lambda7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus (d_C_includeDir x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_done x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2 x1002 x3250 x3500) (d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_copyIncludeIfPresent_dot___hash_lambda7 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_readAnaInfo :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
d_C_readAnaInfo x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_AnalysisServer.d_C_initializeAnalysisSystem x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisServer.d_C_analyzeInterface (Curry_Deterministic.d_C_nondetAnalysis x3250 x3500) x1 x3250 x3500) d_OP_readAnaInfo_dot_stopIfError_dot_65 x3250 x3500) (d_OP_readAnaInfo_dot___hash_lambda9 x1) x3250 x3500) x3250 x3500

nd_C_readAnaInfo :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
nd_C_readAnaInfo x1 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (Curry_Prelude.d_OP_gt_gt (Curry_AnalysisServer.d_C_initializeAnalysisSystem x3250 x3500) (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_AnalysisServer.nd_C_analyzeInterface (Curry_Deterministic.nd_C_nondetAnalysis x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (wrapNX id nd_OP_readAnaInfo_dot_stopIfError_dot_65) x2003 x3250 x3500)))) (wrapNX id (nd_OP_readAnaInfo_dot___hash_lambda9 x1)) x2005 x3250 x3500)))) x3250 x3500))

d_OP_readAnaInfo_dot_stopIfError_dot_65 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.C_Either (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (t0 -> Cover -> ConstStore -> t1)
d_OP_readAnaInfo_dot_stopIfError_dot_65 x1 x3250 x3500 = case x1 of
     (Curry_Prelude.C_Right x2) -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.C_Left x3) -> Curry_Prelude.d_C_return (d_OP_readAnaInfo_dot_stopIfError_dot_65_dot___hash_lambda8 x3) x3250 x3500
     (Curry_Prelude.Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readAnaInfo_dot_stopIfError_dot_65 x1002 x3250 x3500) (d_OP_readAnaInfo_dot_stopIfError_dot_65 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readAnaInfo_dot_stopIfError_dot_65 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readAnaInfo_dot_stopIfError_dot_65 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_readAnaInfo_dot_stopIfError_dot_65 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.C_Either (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO (Func t0 t1)
nd_OP_readAnaInfo_dot_stopIfError_dot_65 x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.C_Right x2) -> Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.C_Left x3) -> Curry_Prelude.d_C_return (wrapDX id (d_OP_readAnaInfo_dot_stopIfError_dot_65_dot___hash_lambda8 x3)) x3250 x3500
     (Curry_Prelude.Choice_C_Either x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_readAnaInfo_dot_stopIfError_dot_65 x1002 x3000 x3250 x3500) (nd_OP_readAnaInfo_dot_stopIfError_dot_65 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Either x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_readAnaInfo_dot_stopIfError_dot_65 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Either x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_readAnaInfo_dot_stopIfError_dot_65 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Either x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readAnaInfo_dot_stopIfError_dot_65_dot___hash_lambda8 :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> t0 -> Cover -> ConstStore -> t1
d_OP_readAnaInfo_dot_stopIfError_dot_65_dot___hash_lambda8 x1 x2 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'N'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x2 x1 x3250 x3500) x3250 x3500

d_OP_readAnaInfo_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Deterministic.C_Deterministic) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
d_OP_readAnaInfo_dot___hash_lambda9 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisServer.d_C_analyzeInterface (Curry_TotallyDefined.d_C_patCompAnalysis x3250 x3500) x1 x3250 x3500) d_OP_readAnaInfo_dot_stopIfError_dot_65 x3250 x3500) (d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 x1 x2) x3250 x3500

nd_OP_readAnaInfo_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Deterministic.C_Deterministic -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
nd_OP_readAnaInfo_dot___hash_lambda9 x1 x2 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_AnalysisServer.nd_C_analyzeInterface (Curry_TotallyDefined.nd_C_patCompAnalysis x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (wrapNX id nd_OP_readAnaInfo_dot_stopIfError_dot_65) x2003 x3250 x3500)))) (wrapNX id (nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 x1 x2)) x2005 x3250 x3500)))))

d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Deterministic.C_Deterministic) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_TotallyDefined.C_Completeness) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisServer.d_C_analyzeInterface (Curry_Indeterministic.d_C_indetAnalysis x3250 x3500) x1 x3250 x3500) d_OP_readAnaInfo_dot_stopIfError_dot_65 x3250 x3500) (d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x3 x1 x2) x3250 x3500

nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Deterministic.C_Deterministic -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_TotallyDefined.C_Completeness -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10 x1 x2 x3 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_AnalysisServer.nd_C_analyzeInterface (Curry_Indeterministic.nd_C_indetAnalysis x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (wrapNX id nd_OP_readAnaInfo_dot_stopIfError_dot_65) x2003 x3250 x3500)))) (wrapNX id (nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x3 x1 x2)) x2005 x3250 x3500)))))

d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_TotallyDefined.C_Completeness) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Deterministic.C_Deterministic) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_OP_gt_gt_eq (Curry_AnalysisServer.d_C_analyzeInterface (Curry_SolutionCompleteness.d_C_solcompAnalysis x3250 x3500) x2 x3250 x3500) d_OP_readAnaInfo_dot_stopIfError_dot_65 x3250 x3500) (d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x4 x3) x3250 x3500

nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_TotallyDefined.C_Completeness -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Deterministic.C_Deterministic -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11 x1 x2 x3 x4 x3000 x3250 x3500 = let
     x2006 = x3000
      in (seq x2006 (let
          x2005 = leftSupply x2006
          x2004 = rightSupply x2006
           in (seq x2005 (seq x2004 (Curry_Prelude.nd_OP_gt_gt_eq (let
               x2003 = leftSupply x2004
               x2002 = rightSupply x2004
                in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (let
                    x2001 = leftSupply x2002
                    x2000 = rightSupply x2002
                     in (seq x2001 (seq x2000 (Curry_AnalysisServer.nd_C_analyzeInterface (Curry_SolutionCompleteness.nd_C_solcompAnalysis x2000 x3250 x3500) x2 x2001 x3250 x3500)))) (wrapNX id nd_OP_readAnaInfo_dot_stopIfError_dot_65) x2003 x3250 x3500)))) (wrapNX id (nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x4 x3)) x2005 x3250 x3500)))))

d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_TotallyDefined.C_Completeness) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Deterministic.C_Deterministic) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_return (Curry_CurryDocRead.C_AnaInfo (d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x3) x1 x2 x4) x3250 x3500

nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_TotallyDefined.C_Completeness -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Deterministic.C_Deterministic -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_CurryDocRead.C_AnaInfo
nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12 x1 x2 x3 x4 x3000 x3250 x3500 = Curry_Prelude.d_C_return (Curry_CurryDocRead.HO_C_AnaInfo (wrapNX id (nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x3)) x1 x2 x4) x3250 x3500

d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 :: (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Deterministic.C_Deterministic) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_apply x1 x2 x3250 x3500) Curry_Deterministic.C_NDet x3250 x3500

nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 :: Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Deterministic.C_Deterministic -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_Bool
nd_OP_readAnaInfo_dot___hash_lambda9_dot___hash_lambda10_dot___hash_lambda11_dot___hash_lambda12_dot___hash_lambda13 x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.nd_C_apply x1 x2 x2000 x3250 x3500) Curry_Deterministic.C_NDet x3250 x3500))

d_C_makeDoc :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_makeDoc x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (Curry_CurryDocRead.d_C_readComments (Curry_Prelude.d_OP_plus_plus x5 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (d_OP_makeDoc_dot___hash_lambda14 x3 x1 x4 x5 x2) x3250 x3500) x3250 x3500

d_OP_makeDoc_dot___hash_lambda14 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char))) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDoc_dot___hash_lambda14 x1 x2 x3 x4 x5 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'R'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt_eq (d_C_readAnaInfo x3 x3250 x3500) (d_OP_makeDoc_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x7 x8 x4 x5) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_makeDoc_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002 x3250 x3500) (d_OP_makeDoc_dot___hash_lambda14 x1 x2 x3 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_makeDoc_dot___hash_lambda14 x1 x2 x3 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_makeDoc_dot___hash_lambda14 x1 x2 x3 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_makeDoc_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_CurryDocRead.C_AnaInfo -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDoc_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_C_makeDocWithComments (Curry_CurryDocParams.d_C_docType x2 x3250 x3500) x2 x6 x1 x7 x5 x3 x4 x3250 x3500

nd_OP_makeDoc_dot___hash_lambda14_dot___hash_lambda15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_CurryDocRead.C_AnaInfo -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_makeDoc_dot___hash_lambda14_dot___hash_lambda15 x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_makeDocWithComments (Curry_CurryDocParams.d_C_docType x2 x3250 x3500) x2 x6 x1 x7 x5 x3 x4 x2000 x3250 x3500))

d_C_makeDocWithComments :: Curry_CurryDocParams.C_DocType -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_makeDocWithComments x1 x2 x3 x4 x5 x6 x7 x8 x3250 x3500 = case x1 of
     Curry_CurryDocParams.C_HtmlDoc -> Curry_Prelude.d_OP_gt_gt (d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocHtml.d_C_generateHtmlDocs x2 x5 x6 x7 x8 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_translateSource2ColoredHtml x4 x6 x3250 x3500) (d_C_writeOutfile (Curry_CurryDocParams.C_DocParams Curry_CurryDocParams.C_CDoc Curry_Prelude.C_False Curry_Prelude.C_False) Curry_Prelude.C_False x4 x6 (Curry_CurryDocCDoc.d_C_generateCDoc x6 x7 x8 x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_CurryDocParams.C_TexDoc -> d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocTeX.d_C_generateTexDocs x2 x5 x6 x7 x8 x3250 x3500) x3250 x3500
     Curry_CurryDocParams.C_CDoc -> d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocCDoc.d_C_generateCDoc x6 x7 x8 x5 x3250 x3500) x3250 x3500
     (Curry_CurryDocParams.Choice_C_DocType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_makeDocWithComments x1002 x2 x3 x4 x5 x6 x7 x8 x3250 x3500) (d_C_makeDocWithComments x1003 x2 x3 x4 x5 x6 x7 x8 x3250 x3500)
     (Curry_CurryDocParams.Choices_C_DocType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_makeDocWithComments z x2 x3 x4 x5 x6 x7 x8 x3250 x3500) x1002
     (Curry_CurryDocParams.Guard_C_DocType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_makeDocWithComments x1002 x2 x3 x4 x5 x6 x7 x8 x3250) $! (addCs x1001 x3500))
     (Curry_CurryDocParams.Fail_C_DocType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_makeDocWithComments :: Curry_CurryDocParams.C_DocType -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocRead.C_AnaInfo -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_CurryDocRead.C_SourceLine (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_makeDocWithComments x1 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500 = case x1 of
     Curry_CurryDocParams.C_HtmlDoc -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.d_OP_gt_gt (d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocHtml.nd_C_generateHtmlDocs x2 x5 x6 x7 x8 x2000 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_translateSource2ColoredHtml x4 x6 x3250 x3500) (d_C_writeOutfile (Curry_CurryDocParams.C_DocParams Curry_CurryDocParams.C_CDoc Curry_Prelude.C_False Curry_Prelude.C_False) Curry_Prelude.C_False x4 x6 (Curry_CurryDocCDoc.nd_C_generateCDoc x6 x7 x8 x5 x2001 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500)))))
     Curry_CurryDocParams.C_TexDoc -> let
          x2000 = x3000
           in (seq x2000 (d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocTeX.nd_C_generateTexDocs x2 x5 x6 x7 x8 x2000 x3250 x3500) x3250 x3500))
     Curry_CurryDocParams.C_CDoc -> let
          x2000 = x3000
           in (seq x2000 (d_C_writeOutfile x2 x3 x4 x6 (Curry_CurryDocCDoc.nd_C_generateCDoc x6 x7 x8 x5 x2000 x3250 x3500) x3250 x3500))
     (Curry_CurryDocParams.Choice_C_DocType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_makeDocWithComments x1002 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500) (nd_C_makeDocWithComments x1003 x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500)
     (Curry_CurryDocParams.Choices_C_DocType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_makeDocWithComments z x2 x3 x4 x5 x6 x7 x8 x3000 x3250 x3500) x1002
     (Curry_CurryDocParams.Guard_C_DocType x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_makeDocWithComments x1002 x2 x3 x4 x5 x6 x7 x8 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_CurryDocParams.Fail_C_DocType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_makeDocIfNecessary :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_makeDocIfNecessary x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x4 x3250 x3500) (d_OP_makeDocIfNecessary_dot___hash_lambda16 x3 x1 x4 x2) x3250 x3500

d_OP_makeDocIfNecessary_dot___hash_lambda16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDocIfNecessary_dot___hash_lambda16 x1 x2 x3 x4 x5 x3250 x3500 = let
     x6 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_apply (Curry_CurryDocRead.d_C_getLastName x3250 x3500) x5 x3250 x3500) (d_OP__case_11 x2 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x2 x3250 x3500) Curry_CurryDocParams.C_HtmlDoc x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x6 x3250 x3500) (d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17 x1 x6 x2 x3 x5 x4) x3250 x3500)

d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 (Curry_Prelude.d_C_not x7 x3250 x3500) x3250 x3500

d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x2 x3250 x3500) (d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x7 x1 x3 x4 x5 x6) x3250 x3500

d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 :: Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19 x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 (Curry_Prelude.d_OP_eq_eq (Curry_Time.d_C_compareClockTime x1 x7 x3250 x3500) Curry_Prelude.C_GT x3250 x3500) x3250 x3500

d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_makeDocIfNecessary x2 x3 x1) x3250 x3500) x4 x3250 x3500

d_C_getImports :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_getImports x1 x3250 x3500 = let
     x2 = Curry_FlatCurry.d_C_flatCurryIntName x1 x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x2 x3250 x3500) (d_OP_getImports_dot___hash_lambda21 x2 x1) x3250 x3500)

d_OP_getImports_dot___hash_lambda21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getImports_dot___hash_lambda21 x1 x2 x3 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_OP__case_7 x2 x1 x3 x3250 x3500) d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 x3250 x3500

d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 :: Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 x1 x3250 x3500 = case x1 of
     (Curry_FlatCurry.C_Prog x2 x3 x4 x5 x6) -> Curry_Prelude.d_C_return x3 x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 x1002 x3250 x3500) (d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_getImports_dot___hash_lambda21_dot___hash_lambda22 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_copyOrMakeDoc :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_copyOrMakeDoc x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_copyDocIfPossible x1 x3 x5 x3250 x3500) (d_OP_copyOrMakeDoc_dot___hash_lambda23 x3 x1 x4 x5 x2) x3250 x3500

d_OP_copyOrMakeDoc_dot___hash_lambda23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_copyOrMakeDoc_dot___hash_lambda23 x1 x2 x3 x4 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_done x3250 x3500
     Curry_Prelude.C_False -> d_C_makeDoc x2 x5 x1 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_copyOrMakeDoc_dot___hash_lambda23 x1 x2 x3 x4 x5 x1002 x3250 x3500) (d_OP_copyOrMakeDoc_dot___hash_lambda23 x1 x2 x3 x4 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_copyOrMakeDoc_dot___hash_lambda23 x1 x2 x3 x4 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_copyOrMakeDoc_dot___hash_lambda23 x1 x2 x3 x4 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_copyDocIfPossible :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_C_copyDocIfPossible x1 x2 x3 x3250 x3500 = d_OP__case_6 x1 x3 x2 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) Curry_CurryDocParams.C_TexDoc x3250 x3500) x3250 x3500

d_OP_copyDocIfPossible_dot___hash_lambda24 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_copyDocIfPossible_dot___hash_lambda24 x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_5 x5 x3 x2 x1 x4 (Curry_Prelude.d_C_not x5 x3250 x3500) x3250 x3500

d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime x1 x3250 x3500) (d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25_dot___hash_lambda26 x4 x1 x2 x3) x3250 x3500

d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25_dot___hash_lambda26 :: Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Time.C_ClockTime -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25_dot___hash_lambda26 x1 x2 x3 x4 x5 x3250 x3500 = d_OP__case_4 x5 x1 x3 x4 x2 (Curry_Prelude.d_OP_eq_eq (Curry_Time.d_C_compareClockTime x1 x5 x3250 x3500) Curry_Prelude.C_GT x3250 x3500) x3250 x3500

d_C_getDirName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_getDirName x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (Curry_Prelude.C_Char '/'#)) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x1 x3250 x3500) x3250 x3500
      in (d_OP__case_3 x2 (Curry_Prelude.d_OP_eq_eq x2 Curry_Prelude.OP_List x3250 x3500) x3250 x3500)

d_C_readFlatCurryWithImports :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_C_readFlatCurryWithImports x1 x3250 x3500 = d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x1 Curry_Prelude.OP_List x3250 x3500

d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x1 x2 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 Curry_Prelude.OP_List Curry_Prelude.OP_List Curry_Prelude.OP_List) x3250 x3500
     (Curry_Prelude.OP_Cons x3 x4) -> d_OP__case_2 x2 x3 x4 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x3 x3250 x3500) x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x1002 x2 x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x1003 x2 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 z x2 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FlatCurry.d_C_readFlatCurryFile x4 x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3) x3250 x3500

d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_FlatCurry.C_Prog -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_FlatCurry.C_Prog x5 x6 x7 x8 x9) -> Curry_Prelude.d_OP_gt_gt_eq (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 (Curry_Prelude.d_OP_plus_plus x3 x6 x3250 x3500) (Curry_Prelude.OP_Cons x2 x1) x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x8 x9 x7) x3250 x3500
     (Curry_FlatCurry.Choice_C_Prog x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x1002 x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x1003 x3250 x3500)
     (Curry_FlatCurry.Choices_C_Prog x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 z x3250 x3500) x1002
     (Curry_FlatCurry.Guard_C_Prog x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_FlatCurry.Fail_C_Prog x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 :: Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple3 x5 x6 x7) -> Curry_Prelude.d_C_return (Curry_Prelude.OP_Tuple3 (Curry_Prelude.d_OP_plus_plus x3 x5 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x1 x6 x3250 x3500) (Curry_Prelude.d_OP_plus_plus x2 x7 x3250 x3500)) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple3 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x1002 x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple3 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple3 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27_dot___hash_lambda28_dot___hash_lambda29 x1 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple3 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_findSourceFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_findSourceFileInLoadPath x1 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile x1 x3250 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda30 x1) x3250 x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda30 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda30 x1 x2 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath (Curry_FileGoodies.d_C_baseName x1 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x2 x3250 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda30_dot___hash_lambda31 x1) x3250 x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda30_dot___hash_lambda31 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda30_dot___hash_lambda31 x1 x2 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_return (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x3250 x3500) x2 x3250 x3500

d_C_fileExtension :: Curry_CurryDocParams.C_DocType -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_fileExtension x1 x3250 x3500 = case x1 of
     Curry_CurryDocParams.C_HtmlDoc -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))
     Curry_CurryDocParams.C_TexDoc -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))
     Curry_CurryDocParams.C_CDoc -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) Curry_Prelude.OP_List)))
     (Curry_CurryDocParams.Choice_C_DocType x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_fileExtension x1002 x3250 x3500) (d_C_fileExtension x1003 x3250 x3500)
     (Curry_CurryDocParams.Choices_C_DocType x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_fileExtension z x3250 x3500) x1002
     (Curry_CurryDocParams.Guard_C_DocType x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_fileExtension x1002 x3250) $! (addCs x1001 x3500))
     (Curry_CurryDocParams.Fail_C_DocType x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_writeOutfile :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_writeOutfile x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq x5 (d_OP_writeOutfile_dot___hash_lambda32 x3 x1 x4 x2) x3250 x3500

d_OP_writeOutfile_dot___hash_lambda32 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_writeOutfile_dot___hash_lambda32 x1 x2 x3 x4 x5 x3250 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_getImports x3 x3250 x3500) (d_OP_writeOutfile_dot___hash_lambda32_dot___hash_lambda33 x5 x1 x2 x3 x4) x3250 x3500

d_OP_writeOutfile_dot___hash_lambda32_dot___hash_lambda33 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_writeOutfile_dot___hash_lambda32_dot___hash_lambda33 x1 x2 x3 x4 x5 x6 x3250 x3500 = let
     x7 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_dot_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_CurryDocRead.d_C_getLastName x3250 x3500) x4 x3250 x3500) x3250 x3500) (d_C_fileExtension (Curry_CurryDocParams.d_C_docType x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'W'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List)))))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x7 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_writeFile x7 x1 x3250 x3500) (d_OP__case_0 x6 x2 x3 x5 x3250 x3500) x3250 x3500) x3250 x3500)

d_OP__case_0 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_0 x6 x2 x3 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_mapIO_ (d_C_makeDocIfNecessary x3 x5 x2) x3250 x3500) x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x6 x2 x3 x1002 x3250 x3500) (d_OP__case_0 x6 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x6 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x6 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP__case_2 x2 x3 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP_readFlatCurryWithImports_dot_collectMods_dot_121 x4 x2 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_1 x4 x3 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x2 x3 x4 x1002 x3250 x3500) (d_OP__case_2 x2 x3 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x2 x3 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x2 x3 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_Tuple3 (Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl) (Curry_Prelude.OP_List Curry_FlatCurry.C_OpDecl))
d_OP__case_1 x4 x3 x2 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_findFileInLoadPath (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_dot_gt x3250 x3500) x3 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500) (d_OP_readFlatCurryWithImports_dot_collectMods_dot_121_dot___hash_lambda27 x2 x3 x4) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x4 x3 x2 x1002 x3250 x3500) (d_OP__case_1 x4 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x4 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x4 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_3 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) (Curry_Prelude.d_C_tail x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x2 x1002 x3250 x3500) (d_OP__case_3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP__case_4 x5 x1 x3 x4 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))))))))))))) x2 x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus x2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) x3) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_System.d_C_system (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List))) (Curry_Prelude.d_OP_plus_plus x4 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))))))) x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_return Curry_Prelude.C_True x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x5 x1 x3 x4 x2 x1002 x3250 x3500) (d_OP__case_4 x5 x1 x3 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x5 x1 x3 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x5 x1 x3 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP__case_5 x5 x3 x2 x1 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime (Curry_FlatCurry.d_C_flatCurryFileName x4 x3250 x3500) x3250 x3500) (d_OP_copyDocIfPossible_dot___hash_lambda24_dot___hash_lambda25 x1 x2 x3) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x3 x2 x1 x4 x1002 x3250 x3500) (d_OP__case_5 x5 x3 x2 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 x3 x2 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x3 x2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.C_Bool
d_OP__case_6 x1 x3 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_return Curry_Prelude.C_False x3250 x3500
     Curry_Prelude.C_False -> let
          x4 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (d_C_getDirName x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_slash_gt x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) Curry_Prelude.OP_List)))) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_CurryDocRead.d_C_getLastName x3250 x3500) x3 x3250 x3500) x3250 x3500) x3250 x3500
          x5 = Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_FilePath.d_OP_lt_dot_gt x3250 x3500) x4 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))) x3250 x3500
           in (Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_doesFileExist x5 x3250 x3500) (d_OP_copyDocIfPossible_dot___hash_lambda24 x5 x2 x4 x3) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x1 x3 x2 x1002 x3250 x3500) (d_OP__case_6 x1 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x1 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x1 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_FlatCurry.C_Prog
d_OP__case_7 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_FlatCurry.d_C_readFlatCurryFile x1 x3250 x3500
     Curry_Prelude.C_False -> Curry_FlatCurry.d_C_readFlatCurryFile (Curry_FlatCurry.d_C_flatCurryFileName x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x2 x1 x1002 x3250 x3500) (d_OP__case_7 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Time.C_ClockTime -> Curry_Time.C_ClockTime -> Curry_Prelude.C_Bool -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_copyOrMakeDoc x3 x6 x2 x4 x5 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_8 x3 x2 x5 x6 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 x1002 x3250 x3500) (d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x7 x1 x6 x3 x2 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_8 x3 x2 x5 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt_eq (d_C_getImports x5 x3250 x3500) (d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18_dot___hash_lambda19_dot___hash_lambda20 x2 x3 x6) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x3 x2 x5 x1002 x3250 x3500) (d_OP__case_8 x3 x2 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x3 x2 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x3 x2 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_C_copyOrMakeDoc x3 x6 x1 x4 x5 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt_eq (Curry_Directory.d_C_getModificationTime (Curry_FlatCurry.d_C_flatCurryFileName x5 x3250 x3500) x3250 x3500) (d_OP_makeDocIfNecessary_dot___hash_lambda16_dot___hash_lambda17_dot___hash_lambda18 x1 x2 x3 x4 x5 x6) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 x1002 x3250 x3500) (d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x7 x6 x5 x4 x3 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_11 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List))))
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List)))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x2 x1002 x3250 x3500) (d_OP__case_11 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_FlatCurry.C_TypeDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_FlatCurry.C_FuncDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_12 x2 x6 x1 x7 x3 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_genMainIndexPage x1 (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3250 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_CurryDocHtml.d_C_genFunctionIndexPage x1 x7 x3250 x3500) (Curry_CurryDocHtml.d_C_genConsIndexPage x1 x6 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_done x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x2 x6 x1 x7 x3 x1002 x3250 x3500) (d_OP__case_12 x2 x6 x1 x7 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x2 x6 x1 x7 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x2 x6 x1 x7 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_111 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_111 x4 x1 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_110 x7 x4 x3 x1 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> d_OP__case_14 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x4 x1 x1002 x3250 x3500) (d_OP__case_111 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_14 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_C_makeCompleteDoc x1 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) Curry_CurryDocParams.C_HtmlDoc x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) Curry_Prelude.OP_List x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) Curry_Prelude.OP_List x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x149 x150) -> d_OP__case_13 x149 x1 x150 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x1 x1002 x3250 x3500) (d_OP__case_14 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_13 x149 x1 x150 x3250 x3500 = case x150 of
     Curry_Prelude.OP_List -> d_C_makeCompleteDoc x1 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) Curry_CurryDocParams.C_HtmlDoc x3250 x3500) Curry_Prelude.OP_List (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x149 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x151 x152) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x149 x1 x1002 x3250 x3500) (d_OP__case_13 x149 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x149 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x149 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_110 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_110 x7 x4 x3 x1 x6 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_109 x4 x1 x6 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_16 x3 x1 x4 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x7 x4 x3 x1 x6 x1002 x3250 x3500) (d_OP__case_110 x7 x4 x3 x1 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x7 x4 x3 x1 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x7 x4 x3 x1 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_16 x3 x1 x4 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> d_C_makeCompleteDoc x1 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) Curry_CurryDocParams.C_HtmlDoc x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'D'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'O'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List)))) (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x3 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x3 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x145 x146) -> d_OP__case_15 x145 x3 x1 x146 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x3 x1 x1002 x3250 x3500) (d_OP__case_16 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_15 x145 x3 x1 x146 x3250 x3500 = case x146 of
     Curry_Prelude.OP_List -> d_C_makeCompleteDoc x1 (Curry_Prelude.d_OP_eq_eq (Curry_CurryDocParams.d_C_docType x1 x3250 x3500) Curry_CurryDocParams.C_HtmlDoc x3250 x3500) x3 (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x145 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x147 x148) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x145 x3 x1 x1002 x3250 x3500) (d_OP__case_15 x145 x3 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x145 x3 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x145 x3 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_109 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_109 x4 x1 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_108 x10 x9 x4 x1 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '-'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x4 x1 x1002 x3250 x3500) (d_OP__case_109 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_108 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_108 x10 x9 x4 x1 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_107 x4 x1 x9 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x10 x9 x4 x1 x1002 x3250 x3500) (d_OP__case_108 x10 x9 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 x10 x9 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x10 x9 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_107 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_107 x4 x1 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_106 x13 x12 x4 x1 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x4 x1 x1002 x3250 x3500) (d_OP__case_107 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_106 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_106 x13 x12 x4 x1 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_105 x4 x1 x12 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_65 x13 x12 x4 x1 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'h'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x13 x12 x4 x1 x1002 x3250 x3500) (d_OP__case_106 x13 x12 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x13 x12 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x13 x12 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_65 x13 x12 x4 x1 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_64 x4 x1 x12 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_57 x13 x12 x4 x1 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 't'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x13 x12 x4 x1 x1002 x3250 x3500) (d_OP__case_65 x13 x12 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x13 x12 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x13 x12 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_57 x13 x12 x4 x1 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_56 x4 x1 x12 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_51 x13 x12 x4 x1 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x13 x12 x4 x1 x1002 x3250 x3500) (d_OP__case_57 x13 x12 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x13 x12 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x13 x12 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_51 x13 x12 x4 x1 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_50 x4 x1 x12 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_43 x13 x12 x4 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x13 x12 x4 x1 x1002 x3250 x3500) (d_OP__case_51 x13 x12 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x13 x12 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x13 x12 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_43 x13 x12 x4 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_42 x4 x12 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x13 x12 x4 x1002 x3250 x3500) (d_OP__case_43 x13 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x13 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x13 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_42 x4 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x105 x106) -> let
          x107 = x105
           in (d_OP__case_41 x107 x106 x4 (Curry_Prelude.d_OP_eq_eq x107 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x4 x1002 x3250 x3500) (d_OP__case_42 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_41 x107 x106 x4 x108 x3250 x3500 = case x108 of
     Curry_Prelude.C_True -> d_OP__case_40 x4 x106 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x107 x106 x4 x1002 x3250 x3500) (d_OP__case_41 x107 x106 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x107 x106 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x107 x106 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_40 x4 x106 x3250 x3500 = case x106 of
     (Curry_Prelude.OP_Cons x108 x109) -> let
          x110 = x108
           in (d_OP__case_39 x110 x109 x4 (Curry_Prelude.d_OP_eq_eq x110 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x4 x1002 x3250 x3500) (d_OP__case_40 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_39 x110 x109 x4 x111 x3250 x3500 = case x111 of
     Curry_Prelude.C_True -> d_OP__case_38 x4 x109 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x110 x109 x4 x1002 x3250 x3500) (d_OP__case_39 x110 x109 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x110 x109 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x110 x109 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_38 x4 x109 x3250 x3500 = case x109 of
     (Curry_Prelude.OP_Cons x111 x112) -> let
          x113 = x111
           in (d_OP__case_37 x113 x112 x4 (Curry_Prelude.d_OP_eq_eq x113 (Curry_Prelude.C_Char 'y'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x4 x1002 x3250 x3500) (d_OP__case_38 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_37 x113 x112 x4 x114 x3250 x3500 = case x114 of
     Curry_Prelude.C_True -> d_OP__case_36 x4 x112 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x113 x112 x4 x1002 x3250 x3500) (d_OP__case_37 x113 x112 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x113 x112 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x113 x112 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_36 x4 x112 x3250 x3500 = case x112 of
     (Curry_Prelude.OP_Cons x114 x115) -> let
          x116 = x114
           in (d_OP__case_35 x116 x115 x4 (Curry_Prelude.d_OP_eq_eq x116 (Curry_Prelude.C_Char 'i'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x4 x1002 x3250 x3500) (d_OP__case_36 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_35 x116 x115 x4 x117 x3250 x3500 = case x117 of
     Curry_Prelude.C_True -> d_OP__case_34 x4 x115 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x116 x115 x4 x1002 x3250 x3500) (d_OP__case_35 x116 x115 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x116 x115 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x116 x115 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_34 x4 x115 x3250 x3500 = case x115 of
     (Curry_Prelude.OP_Cons x117 x118) -> let
          x119 = x117
           in (d_OP__case_33 x119 x118 x4 (Curry_Prelude.d_OP_eq_eq x119 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x4 x1002 x3250 x3500) (d_OP__case_34 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_33 x119 x118 x4 x120 x3250 x3500 = case x120 of
     Curry_Prelude.C_True -> d_OP__case_32 x4 x118 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x119 x118 x4 x1002 x3250 x3500) (d_OP__case_33 x119 x118 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x119 x118 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x119 x118 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_32 x4 x118 x3250 x3500 = case x118 of
     (Curry_Prelude.OP_Cons x120 x121) -> let
          x122 = x120
           in (d_OP__case_31 x122 x121 x4 (Curry_Prelude.d_OP_eq_eq x122 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x4 x1002 x3250 x3500) (d_OP__case_32 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_31 x122 x121 x4 x123 x3250 x3500 = case x123 of
     Curry_Prelude.C_True -> d_OP__case_30 x4 x121 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x122 x121 x4 x1002 x3250 x3500) (d_OP__case_31 x122 x121 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 x122 x121 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x122 x121 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_30 x4 x121 x3250 x3500 = case x121 of
     (Curry_Prelude.OP_Cons x123 x124) -> let
          x125 = x123
           in (d_OP__case_29 x125 x124 x4 (Curry_Prelude.d_OP_eq_eq x125 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x4 x1002 x3250 x3500) (d_OP__case_30 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_29 x125 x124 x4 x126 x3250 x3500 = case x126 of
     Curry_Prelude.C_True -> d_OP__case_28 x4 x124 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x125 x124 x4 x1002 x3250 x3500) (d_OP__case_29 x125 x124 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x125 x124 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x125 x124 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_28 x4 x124 x3250 x3500 = case x124 of
     (Curry_Prelude.OP_Cons x126 x127) -> let
          x128 = x126
           in (d_OP__case_27 x128 x127 x4 (Curry_Prelude.d_OP_eq_eq x128 (Curry_Prelude.C_Char 'x'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x4 x1002 x3250 x3500) (d_OP__case_28 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_27 x128 x127 x4 x129 x3250 x3500 = case x129 of
     Curry_Prelude.C_True -> d_OP__case_26 x4 x127 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x128 x127 x4 x1002 x3250 x3500) (d_OP__case_27 x128 x127 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x128 x127 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x128 x127 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_26 x4 x127 x3250 x3500 = case x127 of
     (Curry_Prelude.OP_Cons x129 x130) -> let
          x131 = x129
           in (d_OP__case_25 x131 x130 x4 (Curry_Prelude.d_OP_eq_eq x131 (Curry_Prelude.C_Char 'h'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x4 x1002 x3250 x3500) (d_OP__case_26 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_25 x131 x130 x4 x132 x3250 x3500 = case x132 of
     Curry_Prelude.C_True -> d_OP__case_24 x4 x130 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x131 x130 x4 x1002 x3250 x3500) (d_OP__case_25 x131 x130 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x131 x130 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x131 x130 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_24 x4 x130 x3250 x3500 = case x130 of
     (Curry_Prelude.OP_Cons x132 x133) -> let
          x134 = x132
           in (d_OP__case_23 x134 x133 x4 (Curry_Prelude.d_OP_eq_eq x134 (Curry_Prelude.C_Char 't'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x4 x1002 x3250 x3500) (d_OP__case_24 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_23 x134 x133 x4 x135 x3250 x3500 = case x135 of
     Curry_Prelude.C_True -> d_OP__case_22 x4 x133 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x134 x133 x4 x1002 x3250 x3500) (d_OP__case_23 x134 x133 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x134 x133 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x134 x133 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_22 x4 x133 x3250 x3500 = case x133 of
     (Curry_Prelude.OP_Cons x135 x136) -> let
          x137 = x135
           in (d_OP__case_21 x137 x136 x4 (Curry_Prelude.d_OP_eq_eq x137 (Curry_Prelude.C_Char 'm'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x4 x1002 x3250 x3500) (d_OP__case_22 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_21 x137 x136 x4 x138 x3250 x3500 = case x138 of
     Curry_Prelude.C_True -> d_OP__case_20 x4 x136 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x137 x136 x4 x1002 x3250 x3500) (d_OP__case_21 x137 x136 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x137 x136 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x137 x136 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_20 x4 x136 x3250 x3500 = case x136 of
     (Curry_Prelude.OP_Cons x138 x139) -> let
          x140 = x138
           in (d_OP__case_19 x140 x139 x4 (Curry_Prelude.d_OP_eq_eq x140 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x4 x1002 x3250 x3500) (d_OP__case_20 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_19 x140 x139 x4 x141 x3250 x3500 = case x141 of
     Curry_Prelude.C_True -> d_OP__case_18 x4 x139 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x140 x139 x4 x1002 x3250 x3500) (d_OP__case_19 x140 x139 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x140 x139 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x140 x139 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_18 x4 x139 x3250 x3500 = case x139 of
     Curry_Prelude.OP_List -> d_OP__case_17 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x143 x144) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x4 x1002 x3250 x3500) (d_OP__case_18 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_17 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x141 x142) -> d_C_makeIndexPages x141 (Curry_Prelude.d_C_map (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x142 x3250 x3500) x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x1002 x3250 x3500) (d_OP__case_17 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_50 x4 x1 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x94 x95) -> let
          x96 = x94
           in (d_OP__case_49 x96 x95 x4 x1 (Curry_Prelude.d_OP_eq_eq x96 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x4 x1 x1002 x3250 x3500) (d_OP__case_50 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_49 x96 x95 x4 x1 x97 x3250 x3500 = case x97 of
     Curry_Prelude.C_True -> d_OP__case_48 x4 x1 x95 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x96 x95 x4 x1 x1002 x3250 x3500) (d_OP__case_49 x96 x95 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x96 x95 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x96 x95 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_48 x4 x1 x95 x3250 x3500 = case x95 of
     (Curry_Prelude.OP_Cons x97 x98) -> let
          x99 = x97
           in (d_OP__case_47 x99 x98 x4 x1 (Curry_Prelude.d_OP_eq_eq x99 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x4 x1 x1002 x3250 x3500) (d_OP__case_48 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_47 x99 x98 x4 x1 x100 x3250 x3500 = case x100 of
     Curry_Prelude.C_True -> d_OP__case_46 x4 x1 x98 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x99 x98 x4 x1 x1002 x3250 x3500) (d_OP__case_47 x99 x98 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x99 x98 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x99 x98 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_46 x4 x1 x98 x3250 x3500 = case x98 of
     (Curry_Prelude.OP_Cons x100 x101) -> let
          x102 = x100
           in (d_OP__case_45 x102 x101 x4 x1 (Curry_Prelude.d_OP_eq_eq x102 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x4 x1 x1002 x3250 x3500) (d_OP__case_46 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_45 x102 x101 x4 x1 x103 x3250 x3500 = case x103 of
     Curry_Prelude.C_True -> d_OP__case_44 x4 x1 x101 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x102 x101 x4 x1 x1002 x3250 x3500) (d_OP__case_45 x102 x101 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x102 x101 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x102 x101 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_44 x4 x1 x101 x3250 x3500 = case x101 of
     Curry_Prelude.OP_List -> d_C_processArgs (Curry_CurryDocParams.d_C_setDocType Curry_CurryDocParams.C_CDoc x1 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.OP_Cons x103 x104) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x4 x1 x1002 x3250 x3500) (d_OP__case_44 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_56 x4 x1 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x86 x87) -> let
          x88 = x86
           in (d_OP__case_55 x88 x87 x4 x1 (Curry_Prelude.d_OP_eq_eq x88 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x4 x1 x1002 x3250 x3500) (d_OP__case_56 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_55 x88 x87 x4 x1 x89 x3250 x3500 = case x89 of
     Curry_Prelude.C_True -> d_OP__case_54 x4 x1 x87 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x88 x87 x4 x1 x1002 x3250 x3500) (d_OP__case_55 x88 x87 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x88 x87 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x88 x87 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_54 x4 x1 x87 x3250 x3500 = case x87 of
     (Curry_Prelude.OP_Cons x89 x90) -> let
          x91 = x89
           in (d_OP__case_53 x91 x90 x4 x1 (Curry_Prelude.d_OP_eq_eq x91 (Curry_Prelude.C_Char 'x'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x4 x1 x1002 x3250 x3500) (d_OP__case_54 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_53 x91 x90 x4 x1 x92 x3250 x3500 = case x92 of
     Curry_Prelude.C_True -> d_OP__case_52 x4 x1 x90 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x91 x90 x4 x1 x1002 x3250 x3500) (d_OP__case_53 x91 x90 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x91 x90 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x91 x90 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_52 x4 x1 x90 x3250 x3500 = case x90 of
     Curry_Prelude.OP_List -> d_C_processArgs (Curry_CurryDocParams.d_C_setDocType Curry_CurryDocParams.C_TexDoc x1 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.OP_Cons x92 x93) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x4 x1 x1002 x3250 x3500) (d_OP__case_52 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_64 x4 x1 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x75 x76) -> let
          x77 = x75
           in (d_OP__case_63 x77 x76 x4 x1 (Curry_Prelude.d_OP_eq_eq x77 (Curry_Prelude.C_Char 't'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x4 x1 x1002 x3250 x3500) (d_OP__case_64 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_63 x77 x76 x4 x1 x78 x3250 x3500 = case x78 of
     Curry_Prelude.C_True -> d_OP__case_62 x4 x1 x76 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x77 x76 x4 x1 x1002 x3250 x3500) (d_OP__case_63 x77 x76 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x77 x76 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x77 x76 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_62 x4 x1 x76 x3250 x3500 = case x76 of
     (Curry_Prelude.OP_Cons x78 x79) -> let
          x80 = x78
           in (d_OP__case_61 x80 x79 x4 x1 (Curry_Prelude.d_OP_eq_eq x80 (Curry_Prelude.C_Char 'm'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x4 x1 x1002 x3250 x3500) (d_OP__case_62 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_61 x80 x79 x4 x1 x81 x3250 x3500 = case x81 of
     Curry_Prelude.C_True -> d_OP__case_60 x4 x1 x79 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x80 x79 x4 x1 x1002 x3250 x3500) (d_OP__case_61 x80 x79 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x80 x79 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x80 x79 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_60 x4 x1 x79 x3250 x3500 = case x79 of
     (Curry_Prelude.OP_Cons x81 x82) -> let
          x83 = x81
           in (d_OP__case_59 x83 x82 x4 x1 (Curry_Prelude.d_OP_eq_eq x83 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x4 x1 x1002 x3250 x3500) (d_OP__case_60 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_59 x83 x82 x4 x1 x84 x3250 x3500 = case x84 of
     Curry_Prelude.C_True -> d_OP__case_58 x4 x1 x82 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x83 x82 x4 x1 x1002 x3250 x3500) (d_OP__case_59 x83 x82 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x83 x82 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x83 x82 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_58 x4 x1 x82 x3250 x3500 = case x82 of
     Curry_Prelude.OP_List -> d_C_processArgs (Curry_CurryDocParams.d_C_setDocType Curry_CurryDocParams.C_HtmlDoc x1 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.OP_Cons x84 x85) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x4 x1 x1002 x3250 x3500) (d_OP__case_58 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_105 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_105 x4 x1 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x16 = x14
           in (d_OP__case_104 x16 x15 x4 x1 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x4 x1 x1002 x3250 x3500) (d_OP__case_105 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_104 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_104 x16 x15 x4 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> d_OP__case_103 x4 x1 x15 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x16 x15 x4 x1 x1002 x3250 x3500) (d_OP__case_104 x16 x15 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 x16 x15 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x16 x15 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_103 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_103 x4 x1 x15 x3250 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_102 x19 x18 x4 x1 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'm'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x4 x1 x1002 x3250 x3500) (d_OP__case_103 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_102 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_102 x19 x18 x4 x1 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_101 x4 x1 x18 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_86 x19 x18 x4 x1 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'i'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x19 x18 x4 x1 x1002 x3250 x3500) (d_OP__case_102 x19 x18 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 x19 x18 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x19 x18 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_86 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_86 x19 x18 x4 x1 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_85 x4 x1 x18 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x19 x18 x4 x1 x1002 x3250 x3500) (d_OP__case_86 x19 x18 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x19 x18 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x19 x18 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_85 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_85 x4 x1 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x43 x44) -> let
          x45 = x43
           in (d_OP__case_84 x45 x44 x4 x1 (Curry_Prelude.d_OP_eq_eq x45 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x4 x1 x1002 x3250 x3500) (d_OP__case_85 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_84 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_84 x45 x44 x4 x1 x46 x3250 x3500 = case x46 of
     Curry_Prelude.C_True -> d_OP__case_83 x4 x1 x44 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x45 x44 x4 x1 x1002 x3250 x3500) (d_OP__case_84 x45 x44 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x45 x44 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x45 x44 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_83 x4 x1 x44 x3250 x3500 = case x44 of
     (Curry_Prelude.OP_Cons x46 x47) -> let
          x48 = x46
           in (d_OP__case_82 x48 x47 x4 x1 (Curry_Prelude.d_OP_eq_eq x48 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x4 x1 x1002 x3250 x3500) (d_OP__case_83 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_82 x48 x47 x4 x1 x49 x3250 x3500 = case x49 of
     Curry_Prelude.C_True -> d_OP__case_81 x4 x1 x47 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x48 x47 x4 x1 x1002 x3250 x3500) (d_OP__case_82 x48 x47 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x48 x47 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x48 x47 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_81 x4 x1 x47 x3250 x3500 = case x47 of
     (Curry_Prelude.OP_Cons x49 x50) -> let
          x51 = x49
           in (d_OP__case_80 x51 x50 x4 x1 (Curry_Prelude.d_OP_eq_eq x51 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x4 x1 x1002 x3250 x3500) (d_OP__case_81 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_80 x51 x50 x4 x1 x52 x3250 x3500 = case x52 of
     Curry_Prelude.C_True -> d_OP__case_79 x4 x1 x50 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x51 x50 x4 x1 x1002 x3250 x3500) (d_OP__case_80 x51 x50 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x51 x50 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x51 x50 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_79 x4 x1 x50 x3250 x3500 = case x50 of
     (Curry_Prelude.OP_Cons x52 x53) -> let
          x54 = x52
           in (d_OP__case_78 x54 x53 x4 x1 (Curry_Prelude.d_OP_eq_eq x54 (Curry_Prelude.C_Char 'x'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x4 x1 x1002 x3250 x3500) (d_OP__case_79 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_78 x54 x53 x4 x1 x55 x3250 x3500 = case x55 of
     Curry_Prelude.C_True -> d_OP__case_77 x4 x1 x53 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x54 x53 x4 x1 x1002 x3250 x3500) (d_OP__case_78 x54 x53 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x54 x53 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x54 x53 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_77 x4 x1 x53 x3250 x3500 = case x53 of
     (Curry_Prelude.OP_Cons x55 x56) -> let
          x57 = x55
           in (d_OP__case_76 x57 x56 x4 x1 (Curry_Prelude.d_OP_eq_eq x57 (Curry_Prelude.C_Char 'h'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x4 x1 x1002 x3250 x3500) (d_OP__case_77 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_76 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_76 x57 x56 x4 x1 x58 x3250 x3500 = case x58 of
     Curry_Prelude.C_True -> d_OP__case_75 x4 x1 x56 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_76 x57 x56 x4 x1 x1002 x3250 x3500) (d_OP__case_76 x57 x56 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_76 x57 x56 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_76 x57 x56 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_75 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_75 x4 x1 x56 x3250 x3500 = case x56 of
     (Curry_Prelude.OP_Cons x58 x59) -> let
          x60 = x58
           in (d_OP__case_74 x60 x59 x4 x1 (Curry_Prelude.d_OP_eq_eq x60 (Curry_Prelude.C_Char 't'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_75 x4 x1 x1002 x3250 x3500) (d_OP__case_75 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_75 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_75 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_74 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_74 x60 x59 x4 x1 x61 x3250 x3500 = case x61 of
     Curry_Prelude.C_True -> d_OP__case_73 x4 x1 x59 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_74 x60 x59 x4 x1 x1002 x3250 x3500) (d_OP__case_74 x60 x59 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_74 x60 x59 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_74 x60 x59 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_73 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_73 x4 x1 x59 x3250 x3500 = case x59 of
     (Curry_Prelude.OP_Cons x61 x62) -> let
          x63 = x61
           in (d_OP__case_72 x63 x62 x4 x1 (Curry_Prelude.d_OP_eq_eq x63 (Curry_Prelude.C_Char 'm'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_73 x4 x1 x1002 x3250 x3500) (d_OP__case_73 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_73 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_73 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_72 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_72 x63 x62 x4 x1 x64 x3250 x3500 = case x64 of
     Curry_Prelude.C_True -> d_OP__case_71 x4 x1 x62 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_72 x63 x62 x4 x1 x1002 x3250 x3500) (d_OP__case_72 x63 x62 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_72 x63 x62 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_72 x63 x62 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_71 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_71 x4 x1 x62 x3250 x3500 = case x62 of
     (Curry_Prelude.OP_Cons x64 x65) -> let
          x66 = x64
           in (d_OP__case_70 x66 x65 x4 x1 (Curry_Prelude.d_OP_eq_eq x66 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_71 x4 x1 x1002 x3250 x3500) (d_OP__case_71 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_71 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_71 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_70 x66 x65 x4 x1 x67 x3250 x3500 = case x67 of
     Curry_Prelude.C_True -> d_OP__case_69 x4 x1 x65 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x66 x65 x4 x1 x1002 x3250 x3500) (d_OP__case_70 x66 x65 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x66 x65 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x66 x65 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_69 x4 x1 x65 x3250 x3500 = case x65 of
     Curry_Prelude.OP_List -> d_OP__case_68 x1 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x73 x74) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x4 x1 x1002 x3250 x3500) (d_OP__case_69 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_68 x1 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x67 x68) -> d_OP__case_67 x67 x1 x68 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x1 x1002 x3250 x3500) (d_OP__case_68 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_67 x67 x1 x68 x3250 x3500 = case x68 of
     (Curry_Prelude.OP_Cons x69 x70) -> d_OP__case_66 x69 x67 x1 x70 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x67 x1 x1002 x3250 x3500) (d_OP__case_67 x67 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x67 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x67 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_66 x69 x67 x1 x70 x3250 x3500 = case x70 of
     Curry_Prelude.OP_List -> d_C_makeCompleteDoc (Curry_CurryDocParams.d_C_setIndex Curry_Prelude.C_False (Curry_CurryDocParams.d_C_setDocType Curry_CurryDocParams.C_HtmlDoc x1 x3250 x3500) x3250 x3500) Curry_Prelude.C_True x67 (Curry_Prelude.d_C_apply (Curry_FileGoodies.d_C_stripSuffix x3250 x3500) x69 x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x71 x72) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x69 x67 x1 x1002 x3250 x3500) (d_OP__case_66 x69 x67 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 x69 x67 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x69 x67 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_101 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_101 x4 x1 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x22 = x20
           in (d_OP__case_100 x22 x21 x4 x1 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'a'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x4 x1 x1002 x3250 x3500) (d_OP__case_101 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_100 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_100 x22 x21 x4 x1 x23 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> d_OP__case_99 x4 x1 x21 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x22 x21 x4 x1 x1002 x3250 x3500) (d_OP__case_100 x22 x21 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 x22 x21 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x22 x21 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_99 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_99 x4 x1 x21 x3250 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x25 = x23
           in (d_OP__case_98 x25 x24 x4 x1 (Curry_Prelude.d_OP_eq_eq x25 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x4 x1 x1002 x3250 x3500) (d_OP__case_99 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_98 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_98 x25 x24 x4 x1 x26 x3250 x3500 = case x26 of
     Curry_Prelude.C_True -> d_OP__case_97 x4 x1 x24 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x25 x24 x4 x1 x1002 x3250 x3500) (d_OP__case_98 x25 x24 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x25 x24 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x25 x24 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_97 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_97 x4 x1 x24 x3250 x3500 = case x24 of
     (Curry_Prelude.OP_Cons x26 x27) -> let
          x28 = x26
           in (d_OP__case_96 x28 x27 x4 x1 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char 'k'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x4 x1 x1002 x3250 x3500) (d_OP__case_97 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_96 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_96 x28 x27 x4 x1 x29 x3250 x3500 = case x29 of
     Curry_Prelude.C_True -> d_OP__case_95 x4 x1 x27 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x28 x27 x4 x1 x1002 x3250 x3500) (d_OP__case_96 x28 x27 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x28 x27 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x28 x27 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_95 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_95 x4 x1 x27 x3250 x3500 = case x27 of
     (Curry_Prelude.OP_Cons x29 x30) -> let
          x31 = x29
           in (d_OP__case_94 x31 x30 x4 x1 (Curry_Prelude.d_OP_eq_eq x31 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x4 x1 x1002 x3250 x3500) (d_OP__case_95 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_94 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_94 x31 x30 x4 x1 x32 x3250 x3500 = case x32 of
     Curry_Prelude.C_True -> d_OP__case_93 x4 x1 x30 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x31 x30 x4 x1 x1002 x3250 x3500) (d_OP__case_94 x31 x30 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x31 x30 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x31 x30 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_93 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_93 x4 x1 x30 x3250 x3500 = case x30 of
     (Curry_Prelude.OP_Cons x32 x33) -> let
          x34 = x32
           in (d_OP__case_92 x34 x33 x4 x1 (Curry_Prelude.d_OP_eq_eq x34 (Curry_Prelude.C_Char 'o'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x4 x1 x1002 x3250 x3500) (d_OP__case_93 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_92 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_92 x34 x33 x4 x1 x35 x3250 x3500 = case x35 of
     Curry_Prelude.C_True -> d_OP__case_91 x4 x1 x33 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x34 x33 x4 x1 x1002 x3250 x3500) (d_OP__case_92 x34 x33 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x34 x33 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x34 x33 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_91 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_91 x4 x1 x33 x3250 x3500 = case x33 of
     (Curry_Prelude.OP_Cons x35 x36) -> let
          x37 = x35
           in (d_OP__case_90 x37 x36 x4 x1 (Curry_Prelude.d_OP_eq_eq x37 (Curry_Prelude.C_Char 'w'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x4 x1 x1002 x3250 x3500) (d_OP__case_91 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_90 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_90 x37 x36 x4 x1 x38 x3250 x3500 = case x38 of
     Curry_Prelude.C_True -> d_OP__case_89 x4 x1 x36 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x37 x36 x4 x1 x1002 x3250 x3500) (d_OP__case_90 x37 x36 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x37 x36 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x37 x36 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_89 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_89 x4 x1 x36 x3250 x3500 = case x36 of
     (Curry_Prelude.OP_Cons x38 x39) -> let
          x40 = x38
           in (d_OP__case_88 x40 x39 x4 x1 (Curry_Prelude.d_OP_eq_eq x40 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x4 x1 x1002 x3250 x3500) (d_OP__case_89 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_88 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_88 x40 x39 x4 x1 x41 x3250 x3500 = case x41 of
     Curry_Prelude.C_True -> d_OP__case_87 x4 x1 x39 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x40 x39 x4 x1 x1002 x3250 x3500) (d_OP__case_88 x40 x39 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x40 x39 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x40 x39 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_87 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_CurryDocParams.C_DocParams -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP__case_87 x4 x1 x39 x3250 x3500 = case x39 of
     Curry_Prelude.OP_List -> d_C_processArgs (Curry_CurryDocParams.d_C_setMarkDown Curry_Prelude.C_False x1 x3250 x3500) x4 x3250 x3500
     (Curry_Prelude.OP_Cons x41 x42) -> Curry_Prelude.d_C_putStrLn (d_C_usageMessage x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x4 x1 x1002 x3250 x3500) (d_OP__case_87 x4 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x4 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x4 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
