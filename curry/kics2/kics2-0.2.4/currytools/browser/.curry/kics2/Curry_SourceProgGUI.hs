{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_SourceProgGUI (d_C_findSourceFileInLoadPath, d_C_findFunDeclInProgText, d_C_findFirstDeclLine, nd_C_sourceProgGUI, nd_C_startGUI, nd_C_main, d_C_splitProgDefs, d_C_groupFuns, d_C_deleteAdjacentFuns, d_C_keywords, d_C_funDefOfLine, d_C_isCommentLine, d_C_m) where

import Basics
import qualified Curry_Char
import qualified Curry_Distribution
import qualified Curry_FileGoodies
import qualified Curry_FlatCurryShow
import qualified Curry_GUI
import qualified Curry_IO
import qualified Curry_List
import qualified Curry_Prelude
import qualified Curry_System
import qualified Curry_ReadNumeric
import qualified Curry_Maybe
d_C_findSourceFileInLoadPath :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_findSourceFileInLoadPath x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Distribution.d_C_getLoadPathForFile x1 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda1 x1) x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda1 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda1 x1 x2 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_FileGoodies.d_C_lookupFileInPath (Curry_FileGoodies.d_C_baseName x1 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)) x2 x3500) (d_OP_findSourceFileInLoadPath_dot___hash_lambda1_dot___hash_lambda2 x1) x3500

d_OP_findSourceFileInLoadPath_dot___hash_lambda1_dot___hash_lambda2 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_findSourceFileInLoadPath_dot___hash_lambda1_dot___hash_lambda2 x1 x2 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_error (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))))))))))))))))))))))) (Curry_Prelude.d_OP_plus_plus x1 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))))))))))) x3500) x3500) x3500) Curry_Prelude.d_C_return x2 x3500

d_C_findFunDeclInProgText :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Int
d_C_findFunDeclInProgText x1 x2 x3500 = d_C_findFirstDeclLine (Curry_FlatCurryShow.d_C_showCurryId x2 x3500) (Curry_Prelude.d_C_lines x1 x3500) (Curry_Prelude.C_Int 1#) x3500

d_C_findFirstDeclLine :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List (Curry_Prelude.OP_List t0) -> Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_Int
d_C_findFirstDeclLine x1 x2 x3 x3500 = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Int 0#
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_23 x1 x3 x4 x5 (Curry_List.d_C_isPrefixOf x1 x4 x3500) x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_findFirstDeclLine x1 x1002 x3 x3500) (d_C_findFirstDeclLine x1 x1003 x3 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_findFirstDeclLine x1 z x3 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_findFirstDeclLine x1 x1002 x3) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_sourceProgGUI :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> IDSupply -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_GUI.C_Widget (Curry_Prelude.OP_List (Func Curry_IO.C_Handle (Func Curry_GUI.C_GuiPort (Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_GUI.C_ReconfigureItem)))))
nd_C_sourceProgGUI x1 x2 x3000 x3500 = let
     x2011 = x3000
      in (seq x2011 (let
          x2007 = leftSupply x2011
          x2012 = rightSupply x2011
           in (seq x2007 (seq x2012 (let
               x2009 = leftSupply x2012
               x2010 = rightSupply x2012
                in (seq x2009 (seq x2010 (let
                    x3 = generate x2009
                    x4 = generate x2010
                     in (Curry_Prelude.OP_Tuple2 (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_col x2000 x3500) (let
                                   x2003 = leftSupply x2005
                                   x2004 = rightSupply x2005
                                    in (seq x2003 (seq x2004 (Curry_Prelude.OP_Cons (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_GUI.nd_C_row x2001 x3500) (Curry_Prelude.OP_Cons (Curry_GUI.C_Label (Curry_Prelude.OP_Cons (Curry_GUI.C_Text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'F'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List))))))))))))))))))) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Entry (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x4) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons Curry_GUI.C_FillX Curry_Prelude.OP_List)))) Curry_Prelude.OP_List)) x2002 x3500)))) (Curry_Prelude.OP_Cons (Curry_GUI.nd_C_TextEditScroll (Curry_Prelude.OP_Cons (Curry_GUI.C_WRef x3) (Curry_Prelude.OP_Cons (Curry_GUI.C_Text x1) (Curry_Prelude.OP_Cons (Curry_GUI.C_Background (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_GUI.C_Height (Curry_Prelude.C_Int 10#)) (Curry_Prelude.OP_Cons (Curry_GUI.C_Width (Curry_Prelude.C_Int 70#)) (Curry_Prelude.OP_Cons Curry_GUI.C_Fill Curry_Prelude.OP_List)))))) x2004 x3500) Curry_Prelude.OP_List))))) x2006 x3500))))))) (Curry_Prelude.OP_Cons (wrapDX (wrapNX id) (acceptCs id (nd_OP_sourceProgGUI_dot_extHandler_dot_17 x2 x3 x4))) Curry_Prelude.OP_List))))))))))

d_OP_sourceProgGUI_dot_extHandler_dot_17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_IO.C_Handle -> Curry_GUI.C_GuiPort -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_GUI.C_ReconfigureItem)
d_OP_sourceProgGUI_dot_extHandler_dot_17 x1 x2 x3 x4 x5 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3500) (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3 x5 x4 x1 x2 x3) x3500

nd_OP_sourceProgGUI_dot_extHandler_dot_17 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_IO.C_Handle -> Curry_GUI.C_GuiPort -> IDSupply -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List Curry_GUI.C_ReconfigureItem)
nd_OP_sourceProgGUI_dot_extHandler_dot_17 x1 x2 x3 x4 x5 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_IO.d_C_hGetLine x4 x3500) (wrapDX id (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3 x5 x4 x1 x2 x3)) x2000 x3500))

d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3 :: Curry_Prelude.Curry t142 => Curry_GUI.C_GuiPort -> Curry_IO.C_Handle -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO (Curry_Prelude.OP_List t142)
d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3 x1 x2 x3 x4 x5 x6 x3500 = Curry_Prelude.d_OP_gt_gt (d_OP__case_22 x1 x2 x3 x4 x5 x6 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x6 Curry_Prelude.OP_List x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x6 x3500) (Curry_Prelude.C_Char 'q'#) x3500) x3500) x3500) (Curry_Prelude.d_C_return Curry_Prelude.OP_List x3500) x3500

d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 :: Curry_GUI.C_GuiPort -> Curry_IO.C_Handle -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int)) -> Curry_GUI.C_WidgetRef -> Curry_GUI.C_WidgetRef -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x3 x3500) (Curry_Prelude.C_Char '+'#) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_C_startGUI :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_startGUI x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x1 x3500) (wrapNX id nd_OP_startGUI_dot___hash_lambda5) x2000 x3500))

nd_OP_startGUI_dot___hash_lambda5 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_startGUI_dot___hash_lambda5 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) (wrapNX id (nd_OP_startGUI_dot___hash_lambda5_dot___hash_lambda6 x1)) x2000 x3500))

nd_OP_startGUI_dot___hash_lambda5_dot___hash_lambda6 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_startGUI_dot___hash_lambda5_dot___hash_lambda6 x1 x2 x3000 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_GUI.nd_C_runHandlesControlledGUI (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'M'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))))))) x1 x3500) (nd_C_sourceProgGUI x2 (d_C_splitProgDefs x2 x3500) x2000 x3500) (Curry_Prelude.OP_Cons (Curry_IO.d_C_stdin x3500) Curry_Prelude.OP_List) x2001 x3500)))))

nd_C_main :: IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_main x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getArgs x3500) (wrapNX id nd_OP_main_dot___hash_lambda7) x2000 x3500))

nd_OP_main_dot___hash_lambda7 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_main_dot___hash_lambda7 x1 x3000 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_startGUI (Curry_Prelude.d_C_head x1 x3500) x2000 x3500))

d_C_splitProgDefs :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int))
d_C_splitProgDefs x1 x3500 = d_C_groupFuns (Curry_Prelude.d_C_dropWhile (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_null Curry_Prelude.d_C_fst x3500) (d_C_deleteAdjacentFuns (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_concatMap d_OP_splitProgDefs_dot___hash_lambda8 x3500) (Curry_Prelude.d_C_zip (Curry_Prelude.d_C_map d_C_funDefOfLine (Curry_Prelude.d_C_lines x1 x3500) x3500) (Curry_Prelude.d_C_enumFrom (Curry_Prelude.C_Int 1#) x3500) x3500) x3500) x3500) x3500) x3500

d_OP_splitProgDefs_dot___hash_lambda8 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Prelude.C_Int -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Int)
d_OP_splitProgDefs_dot___hash_lambda8 x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_C_maybe Curry_Prelude.OP_List (d_OP_splitProgDefs_dot___hash_lambda8_dot___hash_lambda9 x3) x2 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_splitProgDefs_dot___hash_lambda8 x1002 x3500) (d_OP_splitProgDefs_dot___hash_lambda8 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_splitProgDefs_dot___hash_lambda8 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_splitProgDefs_dot___hash_lambda8 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP_splitProgDefs_dot___hash_lambda8_dot___hash_lambda9 :: Curry_Prelude.C_Int -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) Curry_Prelude.C_Int)
d_OP_splitProgDefs_dot___hash_lambda8_dot___hash_lambda9 x1 x2 x3500 = d_OP__case_20 x1 x2 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem x2 x3500) (d_C_keywords x3500) x3500) x3500

d_C_groupFuns :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) Curry_Prelude.C_Int) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List t0) (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int Curry_Prelude.C_Int))
d_C_groupFuns x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_19 x3 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_groupFuns x1002 x3500) (d_C_groupFuns x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_groupFuns z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_groupFuns x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_deleteAdjacentFuns :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1) -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 t0 t1)
d_C_deleteAdjacentFuns x1 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_8 x2 x3 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_deleteAdjacentFuns x1002 x3500) (d_C_deleteAdjacentFuns x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_deleteAdjacentFuns z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_deleteAdjacentFuns x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_C_keywords :: ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_keywords x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))) Curry_Prelude.OP_List)))))

d_C_funDefOfLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_funDefOfLine x1 x3500 = d_OP__case_4 x1 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all Curry_Char.d_C_isSpace x3500) x1 x3500) x3500

d_C_isCommentLine :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_Bool
d_C_isCommentLine x1 x3500 = Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) (Curry_Prelude.d_C_dropWhile Curry_Char.d_C_isSpace x1 x3500) x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x3500

d_C_m :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_m x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (d_C_findSourceFileInLoadPath x1 x3500) d_OP_m_dot___hash_lambda10 x3500

d_OP_m_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_m_dot___hash_lambda10 x1 x3500 = Curry_Prelude.d_OP_gt_gt_eq (Curry_Prelude.d_C_readFile x1 x3500) d_OP_m_dot___hash_lambda10_dot___hash_lambda11 x3500

d_OP_m_dot___hash_lambda10_dot___hash_lambda11 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_OP_m_dot___hash_lambda10_dot___hash_lambda11 x1 x3500 = Curry_Prelude.d_OP_dollar Curry_Prelude.d_C_print (d_C_splitProgDefs x1 x3500) x3500

d_OP__case_4 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> d_OP__case_3 x1 (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x1 x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1 x1002 x3500) (d_OP__case_4 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_4 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_3 x1 (Curry_Char.d_C_isAlpha (Curry_Prelude.d_C_head x1 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_4 x1 x1002 x3000 x3500) (nd_OP__case_4 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_4 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_4 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_3 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head (Curry_Prelude.d_C_words x1 x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_2 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x1 x3500) (Curry_Prelude.C_Char '('#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x1 x1002 x3500) (d_OP__case_3 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_3 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_Prelude.d_C_head (Curry_Prelude.d_C_words x1 x3500) x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_2 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_head x1 x3500) (Curry_Prelude.C_Char '('#) x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_3 x1 x1002 x3000 x3500) (nd_OP__case_3 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_3 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_3 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_2 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_tail (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3500) (Curry_Prelude.d_C_head (Curry_Prelude.d_C_words (Curry_Prelude.d_C_tail x1 x3500) x3500) x3500) x3500) x3500) x3500)
     Curry_Prelude.C_False -> d_OP__case_1 x1 (d_C_isCommentLine x1 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x1 x1002 x3500) (d_OP__case_2 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_2 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2005 = x3000
           in (seq x2005 (Curry_Prelude.C_Just (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2000 x3500) (Curry_Prelude.d_C_tail (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_reverse x2001 x3500) (Curry_Prelude.d_C_head (Curry_Prelude.d_C_words (Curry_Prelude.d_C_tail x1 x3500) x3500) x3500) x2002 x3500)))) x3500) x2004 x3500)))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_1 x1 (d_C_isCommentLine x1 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_2 x1 x1002 x3000 x3500) (nd_OP__case_2 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_2 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_2 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_1 x1 x2 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_0 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x1 x1002 x3500) (d_OP__case_1 x1 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x1 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x1 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_1 x1 x2 x3000 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_0 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_1 x1 x1002 x3000 x3500) (nd_OP__case_1 x1 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_1 x1 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_1 x1 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_0 x1 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x1002 x3500) (d_OP__case_0 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_0 x1 x3000 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.C_Nothing
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_0 x1002 x3000 x3500) (nd_OP__case_0 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_0 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_0 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_8 x2 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> d_OP__case_7 x4 x5 x2 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x2 x1002 x3500) (d_OP__case_8 x2 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x2 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_8 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons x2 Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_7 x4 x5 x2 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_8 x2 x1002 x3000 x3500) (nd_OP__case_8 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_8 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_8 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_7 x4 x5 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> d_OP__case_6 x5 x6 x7 x4 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x4 x5 x1002 x3500) (d_OP__case_7 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_7 x4 x5 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_6 x5 x6 x7 x4 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_7 x4 x5 x1002 x3000 x3500) (nd_OP__case_7 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_7 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_7 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_6 x5 x6 x7 x4 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_5 x5 x6 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x6 x8 x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x5 x6 x7 x1002 x3500) (d_OP__case_6 x5 x6 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x5 x6 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x5 x6 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_6 x5 x6 x7 x4 x3000 x3500 = case x4 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_5 x5 x6 x7 x8 x9 (Curry_Prelude.d_OP_eq_eq x6 x8 x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_6 x5 x6 x7 x1002 x3000 x3500) (nd_OP__case_6 x5 x6 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_6 x5 x6 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_6 x5 x6 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_5 x5 x6 x7 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> d_C_deleteAdjacentFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) (d_C_deleteAdjacentFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x9) x5) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x5 x6 x7 x8 x9 x1002 x3500) (d_OP__case_5 x5 x6 x7 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x5 x6 x7 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x5 x6 x7 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_5 x5 x6 x7 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> d_C_deleteAdjacentFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) x5) x3500
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x6 x7) (d_C_deleteAdjacentFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x9) x5) x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_5 x5 x6 x7 x8 x9 x1002 x3000 x3500) (nd_OP__case_5 x5 x6 x7 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_5 x5 x6 x7 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_5 x5 x6 x7 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_19 x3 x2 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_18 x4 x5 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x3 x1002 x3500) (d_OP__case_19 x3 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x3 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x3 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_19 x3 x2 x3000 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_18 x4 x5 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_19 x3 x1002 x3000 x3500) (nd_OP__case_19 x3 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_19 x3 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_19 x3 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_18 x4 x5 x3 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x6 x7) -> d_OP__case_17 x4 x5 x7 x6 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x4 x5 x1002 x3500) (d_OP__case_18 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_18 x4 x5 x3 x3000 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_17 x4 x5 x7 x6 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_18 x4 x5 x1002 x3000 x3500) (nd_OP__case_18 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_18 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_18 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_17 x4 x5 x7 x6 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> d_OP__case_16 x4 x5 x8 x9 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x4 x5 x7 x1002 x3500) (d_OP__case_17 x4 x5 x7 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x4 x5 x7 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x4 x5 x7 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_17 x4 x5 x7 x6 x3000 x3500 = case x6 of
     (Curry_Prelude.OP_Tuple2 x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_16 x4 x5 x8 x9 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_17 x4 x5 x7 x1002 x3000 x3500) (nd_OP__case_17 x4 x5 x7 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_17 x4 x5 x7 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_17 x4 x5 x7 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_16 x4 x5 x8 x9 x7 x3500 = case x7 of
     Curry_Prelude.OP_List -> d_OP__case_15 x4 x5 x8 x9 (Curry_Prelude.d_OP_eq_eq x4 x8 x3500) x3500
     (Curry_Prelude.OP_Cons x10 x11) -> d_OP__case_13 x4 x5 x8 x9 x11 x10 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x4 x5 x8 x9 x1002 x3500) (d_OP__case_16 x4 x5 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x4 x5 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x4 x5 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_16 x4 x5 x8 x9 x7 x3000 x3500 = case x7 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_15 x4 x5 x8 x9 (Curry_Prelude.d_OP_eq_eq x4 x8 x3500) x2000 x3500))
     (Curry_Prelude.OP_Cons x10 x11) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_13 x4 x5 x8 x9 x11 x10 x2000 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_16 x4 x5 x8 x9 x1002 x3000 x3500) (nd_OP__case_16 x4 x5 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_16 x4 x5 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_16 x4 x5 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_13 x4 x5 x8 x9 x11 x10 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_null x8 x3500) (Curry_Prelude.d_OP_eq_eq x4 x12 x3500) x3500) x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x4 x5 x8 x9 x11 x1002 x3500) (d_OP__case_13 x4 x5 x8 x9 x11 x1003 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x4 x5 x8 x9 x11 z x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x4 x5 x8 x9 x11 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_13 x4 x5 x8 x9 x11 x10 x3000 x3500 = case x10 of
     (Curry_Prelude.OP_Tuple2 x12 x13) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_null x8 x3500) (Curry_Prelude.d_OP_eq_eq x4 x12 x3500) x3500) x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_13 x4 x5 x8 x9 x11 x1002 x3000 x3500) (nd_OP__case_13 x4 x5 x8 x9 x11 x1003 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_13 x4 x5 x8 x9 x11 z x3000 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_13 x4 x5 x8 x9 x11 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500
     Curry_Prelude.C_False -> d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_C_null x8 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1002 x3500) (d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_C_null x8 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_12 x4 x5 x8 x9 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3500))) (d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11) x3500)
     Curry_Prelude.C_False -> d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_OP_eq_eq x4 x8 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1002 x3500) (d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3500))) (d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11) x3500)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_OP_eq_eq x4 x8 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_11 x4 x5 x8 x9 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500
     Curry_Prelude.C_False -> d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1002 x3500) (d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 x5) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_10 x4 x5 x8 x9 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x14 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3500))) (d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x9) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1002 x3500) (d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x14 x3000 x3500 = case x14 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 (Curry_Prelude.d_OP_minus x9 (Curry_Prelude.C_Int 1#) x3500))) (d_C_groupFuns (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 x9) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x12 x13) x11)) x3500)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1002 x3000 x3500) (nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_9 x4 x5 x8 x9 x11 x12 x13 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_15 x4 x5 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x9)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> d_OP__case_14 x4 x5 x8 x9 (Curry_Prelude.d_C_null x8 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x4 x5 x8 x9 x1002 x3500) (d_OP__case_15 x4 x5 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x4 x5 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x4 x5 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_15 x4 x5 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x9)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_14 x4 x5 x8 x9 (Curry_Prelude.d_C_null x8 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_15 x4 x5 x8 x9 x1002 x3000 x3500) (nd_OP__case_15 x4 x5 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_15 x4 x5 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_15 x4 x5 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_14 x4 x5 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.OP_Tuple2 x9 x9)) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x4 x5 x8 x9 x1002 x3500) (d_OP__case_14 x4 x5 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x4 x5 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x4 x5 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_14 x4 x5 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x4 (Curry_Prelude.OP_Tuple2 x5 x5)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.OP_Tuple2 x9 x9)) Curry_Prelude.OP_List)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_14 x4 x5 x8 x9 x1002 x3000 x3500) (nd_OP__case_14 x4 x5 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_14 x4 x5 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_14 x4 x5 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_20 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 x1) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x1 x2 x1002 x3500) (d_OP__case_20 x1 x2 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x1 x2 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x1 x2 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_20 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_List
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 x2 x1) Curry_Prelude.OP_List
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_20 x1 x2 x1002 x3000 x3500) (nd_OP__case_20 x1 x2 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_20 x1 x2 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_20 x1 x2 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 (Curry_Prelude.d_C_tail x3 x3500) x1 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_addRegionStyle x5 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 0#)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus x9 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.C_Int 0#)) (Curry_GUI.C_Bg Curry_GUI.C_Yellow) x1 x3500) (Curry_GUI.d_C_seeText x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus x8 x9 x3500) (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#)) x1 x3500) x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_removeRegionStyle x5 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 0#)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus x9 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.C_Int 0#)) (Curry_GUI.C_Bg Curry_GUI.C_Yellow) x1 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 Curry_Prelude.OP_List x1 x3500) (Curry_Prelude.d_OP_gt_gt (d_OP_sourceProgGUI_dot_extHandler_dot_17 x4 x5 x6 x2 x1 x3500) (Curry_Prelude.d_C_done x3500) x3500) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1002 x3500) (d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 (Curry_Prelude.d_C_tail x3 x3500) x1 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_addRegionStyle x5 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 0#)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus x9 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.C_Int 0#)) (Curry_GUI.C_Bg Curry_GUI.C_Yellow) x1 x3500) (Curry_GUI.d_C_seeText x5 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_C_div (Curry_Prelude.d_OP_plus x8 x9 x3500) (Curry_Prelude.C_Int 2#) x3500) (Curry_Prelude.C_Int 0#)) x1 x3500) x3500) x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_removeRegionStyle x5 (Curry_Prelude.OP_Tuple2 x8 (Curry_Prelude.C_Int 0#)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.d_OP_plus x9 (Curry_Prelude.C_Int 1#) x3500) (Curry_Prelude.C_Int 0#)) (Curry_GUI.C_Bg Curry_GUI.C_Yellow) x1 x3500) (Curry_Prelude.d_OP_gt_gt (Curry_GUI.d_C_setValue x6 Curry_Prelude.OP_List x1 x3500) (Curry_Prelude.d_OP_gt_gt (nd_OP_sourceProgGUI_dot_extHandler_dot_17 x4 x5 x6 x2 x1 x2000 x3500) (Curry_Prelude.d_C_done x3500) x3500) x3500) x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1002 x3000 x3500) (nd_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_21 x1 x2 x3 x4 x5 x6 x8 x9 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_22 x1 x2 x3 x4 x5 x6 x7 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_GUI.d_C_exitGUI x1 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_maybe (Curry_Prelude.d_C_done x3500) (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x6 x3 x4 x5) (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_tail x6 x3500) x3 x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x1 x2 x3 x4 x5 x6 x1002 x3500) (d_OP__case_22 x1 x2 x3 x4 x5 x6 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x1 x2 x3 x4 x5 x6 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x1 x2 x3 x4 x5 x6 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_22 x1 x2 x3 x4 x5 x6 x7 x3000 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_GUI.d_C_exitGUI x1 x3500
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Prelude.nd_C_maybe (Curry_Prelude.d_C_done x3500) (wrapDX id (d_OP_sourceProgGUI_dot_extHandler_dot_17_dot___hash_lambda3_dot___hash_lambda4 x1 x2 x6 x3 x4 x5)) (Curry_Prelude.d_C_lookup (Curry_Prelude.d_C_tail x6 x3500) x3 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_22 x1 x2 x3 x4 x5 x6 x1002 x3000 x3500) (nd_OP__case_22 x1 x2 x3 x4 x5 x6 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_22 x1 x2 x3 x4 x5 x6 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_22 x1 x2 x3 x4 x5 x6 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

d_OP__case_23 x1 x3 x4 x5 x6 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_findFirstDeclLine x1 x5 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x1 x3 x4 x5 x1002 x3500) (d_OP__case_23 x1 x3 x4 x5 x1003 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x1 x3 x4 x5 z x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x1 x3 x4 x5 x1002) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo

nd_OP__case_23 x1 x3 x4 x5 x6 x3000 x3500 = case x6 of
     Curry_Prelude.C_True -> x3
     Curry_Prelude.C_False -> d_C_findFirstDeclLine x1 x5 (Curry_Prelude.d_OP_plus x3 (Curry_Prelude.C_Int 1#) x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_23 x1 x3 x4 x5 x1002 x3000 x3500) (nd_OP__case_23 x1 x3 x4 x5 x1003 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_23 x1 x3 x4 x5 z x3000 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_23 x1 x3 x4 x5 x1002 x3000) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons defCover defFailInfo
