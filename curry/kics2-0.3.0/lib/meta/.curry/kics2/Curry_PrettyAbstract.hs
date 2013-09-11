{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Curry_PrettyAbstract (d_C_preludePrecs, nd_C_prettyCProg, d_C_prettyCTypeExpr, nd_C_prettyCTypeExpr, d_C_prettyCTypes, nd_C_prettyCTypes, d_C_prettyCOps, nd_C_prettyCOps, nd_C_showCProg, nd_C_printCProg, nd_C_printUCProg, nd_C_cprogDoc, nd_C_cprogDocWithPrecedences, d_C_precs, nd_C_precs) where

import Basics
import qualified Curry_AbstractCurry
import qualified Curry_List
import qualified Curry_Maybe
import qualified Curry_Prelude
import qualified Curry_Pretty
import qualified Curry_System
import qualified Curry_Char
type C_Precs = Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int))

d_C_qualifiedNames :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_qualifiedNames x3250 x3500 = Curry_Prelude.C_True

d_C_debug :: Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_debug x3250 x3500 = Curry_Prelude.C_False

d_C_showPrecs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_showPrecs x1 x2 x3250 x3500 = d_OP__case_293 x2 x1 (d_C_debug x3250 x3500) x3250 x3500

nd_C_showPrecs :: (Curry_Prelude.Curry t0,Curry_Prelude.Curry t1) => t0 -> t1 -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_showPrecs x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_293 x2 x1 (d_C_debug x3250 x3500) x2000 x3250 x3500))

d_C_prelude :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prelude x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))

d_C_arrow :: Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_arrow x3250 x3500 = Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x3250 x3500

nd_C_arrow :: IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_arrow x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) x2000 x3250 x3500))

d_C_bar :: Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_bar x3250 x3500 = Curry_Pretty.d_C_char (Curry_Prelude.C_Char '|'#) x3250 x3500

nd_C_bar :: IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_bar x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Pretty.nd_C_char (Curry_Prelude.C_Char '|'#) x2000 x3250 x3500))

d_C_dcolon :: Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_dcolon x3250 x3500 = Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) x3250 x3500

nd_C_dcolon :: IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_dcolon x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) x2000 x3250 x3500))

d_C_preludePrecs :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int))
d_C_preludePrecs x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 9#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 9#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 7#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'v'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 7#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 7#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 6#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 6#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 5#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'E'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List)))))))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixOp (Curry_Prelude.C_Int 4#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 3#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 2#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 1#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 1#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'q'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List)))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 0#))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixrOp (Curry_Prelude.C_Int 5#))) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))

nd_C_prettyCProg :: Curry_Prelude.C_Int -> IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_prettyCProg x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_C_pretty x1)) (nd_C_cprogDoc x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_prettyCTypeExpr :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prettyCTypeExpr x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_pretty (Curry_Prelude.C_Int 78#)) (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#)) x3250 x3500

nd_C_prettyCTypeExpr :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CTypeExpr (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_prettyCTypeExpr x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_C_pretty (Curry_Prelude.C_Int 78#))) (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#))) x2000 x3250 x3500))

d_C_prettyCTypes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prettyCTypes x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_pretty (Curry_Prelude.C_Int 78#)) (d_C_typesDoc x1 x3250 x3500) x3250 x3500

nd_C_prettyCTypes :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_prettyCTypes x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_C_pretty (Curry_Prelude.C_Int 78#))) (nd_C_typesDoc x1 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_prettyCOps :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_prettyCOps x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_pretty (Curry_Prelude.C_Int 78#)) d_C_opsDoc x3250 x3500

nd_C_prettyCOps :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_prettyCOps x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_C_pretty (Curry_Prelude.C_Int 78#))) (wrapNX id nd_C_opsDoc) x2000 x3250 x3500))

nd_C_showCProg :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CurryProg (Curry_Prelude.OP_List Curry_Prelude.C_Char)
nd_C_showCProg x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_C_prettyCProg (Curry_Prelude.C_Int 78#) x2000 x3250 x3500))

nd_C_main :: IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_main x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x1 = Curry_Prelude.d_OP_gt_gt (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) Curry_Prelude.OP_List)))))))))))))))))))))))))))))))))))))))))) x3250 x3500) (Curry_Prelude.d_C_putStrLn (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'A'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'b'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'C'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'g'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) Curry_Prelude.OP_List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) x3250 x3500) x3250 x3500
           in (Curry_Prelude.nd_OP_gt_gt_eq (Curry_System.d_C_getArgs x3250 x3500) (wrapNX id (nd_OP_main_dot___hash_lambda1 x1)) x2000 x3250 x3500)))

d_OP_main_dot_checkMod'_dot_66 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_main_dot_checkMod'_dot_66 x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_291 x2 x3 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Char '/'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_main_dot_checkMod'_dot_66 x1002 x3250 x3500) (d_OP_main_dot_checkMod'_dot_66 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_main_dot_checkMod'_dot_66 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_main_dot_checkMod'_dot_66 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_main_dot_checkMod_dot_66 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP_main_dot_checkMod_dot_66 x1 x3250 x3500 = d_OP__case_289 x1 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 1#) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_OP_main_dot___hash_lambda1 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP_main_dot___hash_lambda1 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_287 x4 x1 x3 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_main_dot___hash_lambda1 x1 x1002 x3000 x3250 x3500) (nd_OP_main_dot___hash_lambda1 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_main_dot___hash_lambda1 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_main_dot___hash_lambda1 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_printCProg :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_printCProg x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AbstractCurry.d_C_readCurry x1 x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_putStrLn) (nd_C_showCProg x2000 x3250 x3500) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

nd_C_printUCProg :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_C_printUCProg x1 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_OP_gt_gt_eq (Curry_AbstractCurry.d_C_readUntypedCurry x1 x3250 x3500) (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id Curry_Prelude.d_C_putStrLn) (nd_C_showCProg x2000 x3250 x3500) x2001 x3250 x3500)))) x2003 x3250 x3500)))))

nd_C_cprogDoc :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CurryProg Curry_Pretty.C_Doc
nd_C_cprogDoc x3000 x3250 x3500 = wrapNX id (nd_C_cprogDocWithPrecedences (d_C_preludePrecs x3250 x3500))

nd_C_cprogDocWithPrecedences :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CurryProg -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_cprogDocWithPrecedences x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> let
          x2031 = x3000
           in (seq x2031 (let
               x2030 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2030 (seq x2032 (let
                    x2027 = leftSupply x2032
                    x2029 = rightSupply x2032
                     in (seq x2027 (seq x2029 (Curry_Prelude.nd_C_apply (let
                         x2026 = leftSupply x2027
                         x2028 = rightSupply x2027
                          in (seq x2026 (seq x2028 (let
                              x2000 = leftSupply x2028
                              x2024 = rightSupply x2028
                               in (seq x2000 (seq x2024 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) (let
                                   x2023 = leftSupply x2024
                                   x2025 = rightSupply x2024
                                    in (seq x2023 (seq x2025 (let
                                        x2016 = leftSupply x2025
                                        x2022 = rightSupply x2025
                                         in (seq x2016 (seq x2022 (nd_OP_lt_dollar_gt_gt (let
                                             x2015 = leftSupply x2016
                                             x2017 = rightSupply x2016
                                              in (seq x2015 (seq x2017 (let
                                                  x2010 = leftSupply x2017
                                                  x2014 = rightSupply x2017
                                                   in (seq x2010 (seq x2014 (nd_OP_lt_dollar_gt_gt (let
                                                       x2009 = leftSupply x2010
                                                       x2011 = rightSupply x2010
                                                        in (seq x2009 (seq x2011 (let
                                                            x2006 = leftSupply x2011
                                                            x2008 = rightSupply x2011
                                                             in (seq x2006 (seq x2008 (nd_OP_lt_dollar_gt_gt (let
                                                                 x2005 = leftSupply x2006
                                                                 x2007 = rightSupply x2006
                                                                  in (seq x2005 (seq x2007 (let
                                                                      x2003 = leftSupply x2007
                                                                      x2004 = rightSupply x2007
                                                                       in (seq x2003 (seq x2004 (nd_OP_lt_dollar_gt_gt (let
                                                                           x2002 = leftSupply x2003
                                                                           x2001 = rightSupply x2003
                                                                            in (seq x2002 (seq x2001 (nd_C_moduleHeaderDoc x3 x2 (nd_C_exportedNames x3 x2 x2001 x3250 x3500) x2002 x3250 x3500)))) (nd_C_impsDoc x4 x2004 x3250 x3500) x2005 x3250 x3500))))))) (nd_C_opsDoc x7 x2008 x3250 x3500) x2009 x3250 x3500))))))) (let
                                                       x2013 = leftSupply x2014
                                                       x2012 = rightSupply x2014
                                                        in (seq x2013 (seq x2012 (Curry_Prelude.nd_C_apply (nd_C_typesDoc x3 x2012 x3250 x3500) x5 x2013 x3250 x3500)))) x2015 x3250 x3500))))))) (let
                                             x2021 = leftSupply x2022
                                             x2020 = rightSupply x2022
                                              in (seq x2021 (seq x2020 (nd_C_funcsDoc (Curry_Prelude.d_OP_plus_plus (let
                                                  x2019 = leftSupply x2020
                                                  x2018 = rightSupply x2020
                                                   in (seq x2019 (seq x2018 (Curry_Prelude.nd_C_apply (nd_C_precs x2018 x3250 x3500) x7 x2019 x3250 x3500)))) x1 x3250 x3500) x3 x6 x2021 x3250 x3500)))) x2023 x3250 x3500))))))) x2026 x3250 x3500))))))) (Curry_Pretty.nd_C_empty x2029 x3250 x3500) x2030 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_cprogDocWithPrecedences x1 x1002 x3000 x3250 x3500) (nd_C_cprogDocWithPrecedences x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_cprogDocWithPrecedences x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_cprogDocWithPrecedences x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_precs :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int))
d_C_precs x3250 x3500 = Curry_Prelude.d_C_map d_OP_precs_dot___hash_lambda3

nd_C_precs :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl) (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)))
nd_C_precs x3000 x3250 x3500 = wrapNX id (Curry_Prelude.nd_C_map (wrapDX id d_OP_precs_dot___hash_lambda3))

d_OP_precs_dot___hash_lambda3 :: Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)
d_OP_precs_dot___hash_lambda3 x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_COp x2 x3 x4) -> Curry_Prelude.OP_Tuple2 x2 (Curry_Prelude.OP_Tuple2 x3 x4)
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_precs_dot___hash_lambda3 x1002 x3250 x3500) (d_OP_precs_dot___hash_lambda3 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_precs_dot___hash_lambda3 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_precs_dot___hash_lambda3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_lt_dollar_gt_gt :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_lt_dollar_gt_gt x1 x2 x3250 x3500 = d_OP__case_244 x1 x2 (Curry_Pretty.d_C_isEmpty x1 x3250 x3500) x3250 x3500

nd_OP_lt_dollar_gt_gt :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_lt_dollar_gt_gt x1 x2 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (nd_OP__case_244 x1 x2 (Curry_Pretty.nd_C_isEmpty x1 x2000 x3250 x3500) x2001 x3250 x3500)))))

d_C_def :: Curry_Pretty.C_Doc -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_def x1 x2 x3 x3250 x3500 = let
     x4 = d_OP__case_241 x2 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500
      in (Curry_Prelude.d_C_apply (d_C_block x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_dollar_gt x3250 x3500) (Curry_Pretty.d_OP_lt_gt x1 x4 x3250 x3500) x3250 x3500) x3 x3250 x3500) x3250 x3500)

nd_C_def :: Curry_Pretty.C_Doc -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_def x1 x2 x3 x3000 x3250 x3500 = let
     x2012 = x3000
      in (seq x2012 (let
          x2000 = leftSupply x2012
          x2010 = rightSupply x2012
           in (seq x2000 (seq x2010 (let
               x4 = nd_OP__case_241 x2 (Curry_Prelude.d_C_null x2 x3250 x3500) x2000 x3250 x3500
                in (let
                    x2009 = leftSupply x2010
                    x2011 = rightSupply x2010
                     in (seq x2009 (seq x2011 (let
                         x2001 = leftSupply x2011
                         x2008 = rightSupply x2011
                          in (seq x2001 (seq x2008 (Curry_Prelude.nd_C_apply (nd_C_block x2001 x3250 x3500) (let
                              x2007 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2007 (seq x2005 (Curry_Prelude.nd_C_apply (let
                                   x2004 = leftSupply x2005
                                   x2006 = rightSupply x2005
                                    in (seq x2004 (seq x2006 (let
                                        x2002 = leftSupply x2006
                                        x2003 = rightSupply x2006
                                         in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2002 x3250 x3500) (Curry_Pretty.nd_OP_lt_gt x1 x4 x2003 x3250 x3500) x2004 x3250 x3500))))))) x3 x2007 x3250 x3500)))) x2009 x3250 x3500))))))))))))

d_C_block :: Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_block x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Pretty.d_C_group (Curry_Pretty.d_C_hang (Curry_Prelude.C_Int 1#)) x3250 x3500

nd_C_block :: IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_C_block x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id Curry_Pretty.nd_C_group) (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) x2000 x3250 x3500))

d_C_app :: Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_app x1 x2 x3250 x3500 = d_OP__case_240 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500

nd_C_app :: Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_app x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_240 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x2000 x3250 x3500))

d_C_par :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_par x1 x3250 x3500 = d_OP__case_239 x1 (Curry_Maybe.d_C_isJust x1 x3250 x3500) x3250 x3500

nd_C_par :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_C_par x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_239 x1 (Curry_Maybe.d_C_isJust x1 x3250 x3500) x2000 x3250 x3500))

d_C_precFillEncloseSep :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_precFillEncloseSep x1 x2 x3 x4 x5 x6 x7 x3250 x3500 = let
     x8 = Curry_Maybe.d_C_fromJust x3 x3250 x3500
      in (d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 (Curry_Maybe.d_C_isNothing x3 x3250 x3500) x3250 x3500)

nd_C_precFillEncloseSep :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_precFillEncloseSep x1 x2 x3 x4 x5 x6 x7 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (let
          x8 = Curry_Maybe.d_C_fromJust x3 x3250 x3500
           in (nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 (Curry_Maybe.d_C_isNothing x3 x3250 x3500) x2000 x3250 x3500)))

d_OP_precFillEncloseSep_dot_pre_dot_107 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x3 x4 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> d_OP__case_236 x6 x5 x1 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1002 x3 x4 x3250 x3500) (d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1003 x3 x4 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_precFillEncloseSep_dot_pre_dot_107 x1 z x3 x4 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1002 x3 x4 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_precFillEncloseSep_dot_pre_dot_107 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x3 x4 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_236 x6 x5 x1 x4 x3 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1002 x3 x4 x3000 x3250 x3500) (nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1003 x3 x4 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 z x3 x4 x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x1002 x3 x4 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_layout :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_layout x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_align x3250 x3500) (Curry_Pretty.d_C_compose (acceptCs id (Curry_Pretty.d_C_combine (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_linesep x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x3250 x3500)))) x3250 x3500

nd_C_layout :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc
nd_C_layout x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (Curry_Pretty.nd_C_align x2000 x3250 x3500) (wrapNX id (Curry_Pretty.nd_C_compose (wrapDX (wrapNX id) (acceptCs id (Curry_Pretty.nd_C_combine (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_linesep x2001 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ';'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)) x2002 x3250 x3500))))))))) x2004 x3250 x3500))))))))

d_C_qname :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_qname x1 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_228 x2 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3250 x3500) (d_C_isTupleName x2 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_qname x1 x1002 x3250 x3500) (d_C_qname x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_qname x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_qname x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_qname :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_qname x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_228 x2 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3250 x3500) (d_C_isTupleName x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_qname x1 x1002 x3000 x3250 x3500) (nd_C_qname x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_qname x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_qname x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isTupleName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isTupleName x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x2 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_elem (Curry_Prelude.d_C_take (Curry_Prelude.C_Int 2#) x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ','#) Curry_Prelude.OP_List)) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isTupleName x1002 x3250 x3500) (d_C_isTupleName x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isTupleName z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isTupleName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isInfixName :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isInfixName x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_all (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (d_C_infixIDs x3250 x3500)) x3250 x3500) x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isInfixName x1002 x3250 x3500) (d_C_isInfixName x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isInfixName z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isInfixName x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_infixIDs :: Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_infixIDs x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '~'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '!'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '$'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '%'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '^'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '&'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '*'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '+'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '='#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '>'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '?'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '/'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ':'#) Curry_Prelude.OP_List)))))))))))))))))))

d_C_correctName :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_correctName x1 x3250 x3500 = let
     x2 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot Curry_Prelude.d_C_not (Curry_Prelude.d_C_flip Curry_Prelude.d_C_elem (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '#'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) Curry_Prelude.OP_List))) x3250 x3500) x1 x3250 x3500
      in (d_OP__case_223 x2 x3250 x3500)

d_C_varDoc :: Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_varDoc x3250 x3500 = Curry_Prelude.d_OP_dot Curry_Pretty.d_C_text Curry_Prelude.d_C_snd x3250 x3500

nd_C_varDoc :: IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) Curry_Pretty.C_Doc
nd_C_varDoc x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Prelude.nd_OP_dot (wrapNX id Curry_Pretty.nd_C_text) (wrapDX id Curry_Prelude.d_C_snd) x2000 x3250 x3500))

d_C_tvarDoc :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_tvarDoc x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> d_OP__case_221 x3 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x3 Curry_Prelude.OP_List x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.C_Char '_'#) (Curry_Prelude.d_C_head x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_tvarDoc x1002 x3250 x3500) (d_C_tvarDoc x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_tvarDoc z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_tvarDoc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_tvarDoc :: Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_tvarDoc x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_221 x3 x2 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x3 Curry_Prelude.OP_List x3250 x3500) (Curry_Prelude.d_OP_slash_eq (Curry_Prelude.C_Char '_'#) (Curry_Prelude.d_C_head x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_eq_eq x3 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_tvarDoc x1002 x3000 x3250 x3500) (nd_C_tvarDoc x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_tvarDoc z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_tvarDoc x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_litDoc :: Curry_AbstractCurry.C_CLiteral -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_litDoc x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CIntc x2) -> Curry_Pretty.d_C_int x2 x3250 x3500
     (Curry_AbstractCurry.C_CFloatc x3) -> Curry_Pretty.d_C_float x3 x3250 x3500
     (Curry_AbstractCurry.C_CCharc x4) -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_squotes x3250 x3500) (Curry_Pretty.d_C_text (d_C_quoteChar x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_litDoc x1002 x3250 x3500) (d_C_litDoc x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_litDoc z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_litDoc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_litDoc :: Curry_AbstractCurry.C_CLiteral -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_litDoc x1 x3000 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CIntc x2) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_int x2 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CFloatc x3) -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_float x3 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CCharc x4) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_squotes x2000 x3250 x3500) (Curry_Pretty.nd_C_text (d_C_quoteChar x4 x3250 x3500) x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_litDoc x1002 x3000 x3250 x3500) (nd_C_litDoc x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_litDoc z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_litDoc x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_quoteChar :: Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_C_quoteChar x1 x3250 x3500 = Curry_Prelude.d_C_maybe (Curry_Prelude.OP_Cons x1 Curry_Prelude.OP_List) Curry_Prelude.d_C_id (Curry_Prelude.d_C_lookup x1 (d_C_specialChars x3250 x3500) x3250 x3500) x3250 x3500

d_C_specialChars :: Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Char (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_C_specialChars x3250 x3500 = Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Char '\n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Char '\r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Char '\t'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) (Curry_Prelude.OP_Cons (Curry_Prelude.OP_Tuple2 (Curry_Prelude.C_Char '"'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '\\'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '"'#) Curry_Prelude.OP_List))) Curry_Prelude.OP_List))))

d_C_moduleHeaderDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_moduleHeaderDoc x1 x2 x3 x3250 x3500 = d_OP__case_218 x2 x1 x3 (d_C_hasPrivate x2 x3250 x3500) x3250 x3500

nd_C_moduleHeaderDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_moduleHeaderDoc x1 x2 x3 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_218 x2 x1 x3 (d_C_hasPrivate x2 x3250 x3500) x2000 x3250 x3500))

d_C_exportsDoc :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_exportsDoc x1 x3250 x3500 = Curry_Pretty.d_C_group (Curry_Pretty.d_C_nest (Curry_Prelude.C_Int 1#) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_dollar_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_dollar_gt x3250 x3500) (Curry_Pretty.d_C_lparen x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_align x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_fillSep x3250 x3500) (Curry_Pretty.d_C_punctuate (Curry_Pretty.d_C_comma x3250 x3500) x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_rparen x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_exportsDoc :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_exportsDoc x1 x3000 x3250 x3500 = let
     x2030 = x3000
      in (seq x2030 (let
          x2029 = leftSupply x2030
          x2028 = rightSupply x2030
           in (seq x2029 (seq x2028 (Curry_Pretty.nd_C_group (let
               x2027 = leftSupply x2028
               x2025 = rightSupply x2028
                in (seq x2027 (seq x2025 (Curry_Pretty.nd_C_nest (Curry_Prelude.C_Int 1#) (let
                    x2024 = leftSupply x2025
                    x2026 = rightSupply x2025
                     in (seq x2024 (seq x2026 (let
                         x2021 = leftSupply x2026
                         x2023 = rightSupply x2026
                          in (seq x2021 (seq x2023 (Curry_Prelude.nd_C_apply (let
                              x2020 = leftSupply x2021
                              x2022 = rightSupply x2021
                               in (seq x2020 (seq x2022 (let
                                   x2000 = leftSupply x2022
                                   x2018 = rightSupply x2022
                                    in (seq x2000 (seq x2018 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) (let
                                        x2017 = leftSupply x2018
                                        x2019 = rightSupply x2018
                                         in (seq x2017 (seq x2019 (let
                                             x2004 = leftSupply x2019
                                             x2015 = rightSupply x2019
                                              in (seq x2004 (seq x2015 (Curry_Prelude.nd_C_apply (let
                                                  x2003 = leftSupply x2004
                                                  x2005 = rightSupply x2004
                                                   in (seq x2003 (seq x2005 (let
                                                       x2001 = leftSupply x2005
                                                       x2002 = rightSupply x2005
                                                        in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2001 x3250 x3500) (Curry_Pretty.nd_C_lparen x2002 x3250 x3500) x2003 x3250 x3500))))))) (let
                                                  x2014 = leftSupply x2015
                                                  x2016 = rightSupply x2015
                                                   in (seq x2014 (seq x2016 (let
                                                       x2006 = leftSupply x2016
                                                       x2012 = rightSupply x2016
                                                        in (seq x2006 (seq x2012 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2006 x3250 x3500) (let
                                                            x2011 = leftSupply x2012
                                                            x2013 = rightSupply x2012
                                                             in (seq x2011 (seq x2013 (let
                                                                 x2007 = leftSupply x2013
                                                                 x2010 = rightSupply x2013
                                                                  in (seq x2007 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_fillSep x2007 x3250 x3500) (let
                                                                      x2009 = leftSupply x2010
                                                                      x2008 = rightSupply x2010
                                                                       in (seq x2009 (seq x2008 (Curry_Pretty.nd_C_punctuate (Curry_Pretty.nd_C_comma x2008 x3250 x3500) x1 x2009 x3250 x3500)))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))) x2017 x3250 x3500))))))) x2020 x3250 x3500))))))) (Curry_Pretty.nd_C_rparen x2023 x3250 x3500) x2024 x3250 x3500))))))) x2027 x3250 x3500)))) x2029 x3250 x3500)))))

d_C_hasPrivate :: Curry_AbstractCurry.C_CurryProg -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasPrivate x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CurryProg x2 x3 x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Private) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (d_C_typeCVisibility x3250 x3500) x4 x3250 x3500) (Curry_Prelude.d_C_map (d_C_funcCVisibility x3250 x3500) x5 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasPrivate x1002 x3250 x3500) (d_C_hasPrivate x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasPrivate z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasPrivate x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_exportedNames :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Pretty.C_Doc
d_C_exportedNames x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (d_OP_exportedNames_dot_typeExpDoc_dot_162 x1) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public) (d_C_typeCVisibility x3250 x3500) x3250 x3500) x5 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (d_C_qname x1) (d_C_funcName x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public) (d_C_funcCVisibility x3250 x3500) x3250 x3500) x6 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_exportedNames x1 x1002 x3250 x3500) (d_C_exportedNames x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_exportedNames x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_exportedNames x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_exportedNames :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CurryProg -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Pretty.C_Doc
nd_C_exportedNames x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> let
          x2018 = x3000
           in (seq x2018 (let
               x2006 = leftSupply x2018
               x2016 = rightSupply x2018
                in (seq x2006 (seq x2016 (Curry_Prelude.d_OP_plus_plus (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_exportedNames_dot_typeExpDoc_dot_162 x1)) (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_filter (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public)) (nd_C_typeCVisibility x2000 x3250 x3500) x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))) x2005 x3250 x3500)))) (let
                    x2015 = leftSupply x2016
                    x2017 = rightSupply x2016
                     in (seq x2015 (seq x2017 (let
                         x2009 = leftSupply x2017
                         x2014 = rightSupply x2017
                          in (seq x2009 (seq x2014 (Curry_Prelude.nd_C_map (let
                              x2008 = leftSupply x2009
                              x2007 = rightSupply x2009
                               in (seq x2008 (seq x2007 (Curry_Prelude.nd_OP_dot (wrapNX id (nd_C_qname x1)) (nd_C_funcName x2007 x3250 x3500) x2008 x3250 x3500)))) (let
                              x2013 = leftSupply x2014
                              x2012 = rightSupply x2014
                               in (seq x2013 (seq x2012 (Curry_Prelude.nd_C_filter (let
                                   x2011 = leftSupply x2012
                                   x2010 = rightSupply x2012
                                    in (seq x2011 (seq x2010 (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public)) (nd_C_funcCVisibility x2010 x3250 x3500) x2011 x3250 x3500)))) x6 x2013 x3250 x3500)))) x2015 x3250 x3500))))))) x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_exportedNames x1 x1002 x3000 x3250 x3500) (nd_C_exportedNames x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_exportedNames x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_exportedNames x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_exportedNames_dot_typeExpDoc_dot_162 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_exportedNames_dot_typeExpDoc_dot_162 x1 x2 x3250 x3500 = let
     x3 = Curry_Prelude.d_C_filter (Curry_Prelude.d_OP_dot (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public) (d_C_consCVisibility x3250 x3500) x3250 x3500) (d_C_trCType (acceptCs (acceptCs (acceptCs id)) d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda5) (acceptCs (acceptCs (acceptCs id)) d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda6) x2 x3250 x3500) x3250 x3500
      in (Curry_Pretty.d_OP_lt_gt (d_C_qname x1 (Curry_Prelude.d_C_apply (d_C_typeName x3250 x3500) x2 x3250 x3500) x3250 x3500) (d_OP__case_216 x3 (Curry_Prelude.d_C_null x3 x3250 x3500) x3250 x3500) x3250 x3500)

nd_OP_exportedNames_dot_typeExpDoc_dot_162 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CTypeDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_exportedNames_dot_typeExpDoc_dot_162 x1 x2 x3000 x3250 x3500 = let
     x2016 = x3000
      in (seq x2016 (let
          x2005 = leftSupply x2016
          x2014 = rightSupply x2016
           in (seq x2005 (seq x2014 (let
               x3 = let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2002 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_filter (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (wrapDX id (Curry_Prelude.d_OP_eq_eq Curry_AbstractCurry.C_Public)) (nd_C_consCVisibility x2000 x3250 x3500) x2001 x3250 x3500)))) (nd_C_trCType (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda5)) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda6)) x2 x2003 x3250 x3500) x2004 x3250 x3500))))))
                in (let
                    x2013 = leftSupply x2014
                    x2015 = rightSupply x2014
                     in (seq x2013 (seq x2015 (let
                         x2011 = leftSupply x2015
                         x2012 = rightSupply x2015
                          in (seq x2011 (seq x2012 (Curry_Pretty.nd_OP_lt_gt (let
                              x2010 = leftSupply x2011
                              x2009 = rightSupply x2011
                               in (seq x2010 (seq x2009 (nd_C_qname x1 (let
                                   x2008 = leftSupply x2009
                                   x2007 = rightSupply x2009
                                    in (seq x2008 (seq x2007 (Curry_Prelude.nd_C_apply (nd_C_typeName x2007 x3250 x3500) x2 x2008 x3250 x3500)))) x2010 x3250 x3500)))) (nd_OP__case_216 x3 (Curry_Prelude.d_C_null x3 x3250 x3500) x2012 x3250 x3500) x2013 x3250 x3500))))))))))))

d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda5 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl
d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda5 x1 x2 x3 x4 x3250 x3500 = x4

d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda6 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_OP_exportedNames_dot_typeExpDoc_dot_162_dot___hash_lambda6 x1 x2 x3 x4 x3250 x3500 = Curry_Prelude.OP_List

d_C_impsDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_impsDoc x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Pretty.d_C_vcat x3250 x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) Curry_Pretty.d_C_text x3250 x3500) (Curry_Prelude.d_C_filter (Curry_Prelude.d_C_flip (acceptCs id Curry_Prelude.d_OP_slash_eq) (d_C_prelude x3250 x3500)) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_impsDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_impsDoc x1 x3000 x3250 x3500 = let
     x2013 = x3000
      in (seq x2013 (let
          x2012 = leftSupply x2013
          x2014 = rightSupply x2013
           in (seq x2012 (seq x2014 (let
               x2000 = leftSupply x2014
               x2010 = rightSupply x2014
                in (seq x2000 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vcat x2000 x3250 x3500) (let
                    x2009 = leftSupply x2010
                    x2011 = rightSupply x2010
                     in (seq x2009 (seq x2011 (let
                         x2007 = leftSupply x2011
                         x2008 = rightSupply x2011
                          in (seq x2007 (seq x2008 (Curry_Prelude.nd_C_map (let
                              x2006 = leftSupply x2007
                              x2004 = rightSupply x2007
                               in (seq x2006 (seq x2004 (Curry_Prelude.nd_OP_dot (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List)))))) x2002 x3250 x3500) x2003 x3250 x3500))))))) (wrapNX id Curry_Pretty.nd_C_text) x2006 x3250 x3500)))) (Curry_Prelude.nd_C_filter (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_slash_eq)) (d_C_prelude x3250 x3500))) x1 x2008 x3250 x3500) x2009 x3250 x3500))))))) x2012 x3250 x3500))))))))

d_C_opsDoc :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_opsDoc x1 x3250 x3500 = Curry_Prelude.d_C_apply (Curry_Pretty.d_C_vcat x3250 x3500) (Curry_Prelude.d_C_map d_C_opLineDoc (Curry_List.d_C_groupBy (acceptCs id d_C_eqCOpDecl) x1 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_opsDoc :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_opsDoc x1 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vcat x2000 x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (wrapNX id nd_C_opLineDoc) (Curry_List.nd_C_groupBy (wrapDX (wrapDX id) (acceptCs id d_C_eqCOpDecl)) x1 x2001 x3250 x3500) x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_C_opLineDoc :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_opLineDoc x1 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> Curry_Pretty.d_C_empty x3250 x3500
     (Curry_Prelude.OP_Cons x2 x3) -> d_OP__case_215 x1 x2 x3250 x3500
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_opLineDoc x1002 x3250 x3500) (d_C_opLineDoc x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_opLineDoc z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_opLineDoc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_opLineDoc :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_opLineDoc x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x2 x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_215 x1 x2 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_opLineDoc x1002 x3000 x3250 x3500) (nd_C_opLineDoc x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_opLineDoc z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_opLineDoc x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_opLineDoc_dot_fixDoc_dot_185 :: Curry_AbstractCurry.C_CFixity -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_opLineDoc_dot_fixDoc_dot_185 x1 x3250 x3500 = case x1 of
     Curry_AbstractCurry.C_CInfixOp -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_AbstractCurry.C_CInfixlOp -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List) x3250 x3500
     Curry_AbstractCurry.C_CInfixrOp -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CFixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_opLineDoc_dot_fixDoc_dot_185 x1002 x3250 x3500) (d_OP_opLineDoc_dot_fixDoc_dot_185 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CFixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_opLineDoc_dot_fixDoc_dot_185 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_opLineDoc_dot_fixDoc_dot_185 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_opLineDoc_dot_fixDoc_dot_185 :: Curry_AbstractCurry.C_CFixity -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_opLineDoc_dot_fixDoc_dot_185 x1 x3000 x3250 x3500 = case x1 of
     Curry_AbstractCurry.C_CInfixOp -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_AbstractCurry.C_CInfixlOp -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List) x2000 x3250 x3500))
     Curry_AbstractCurry.C_CInfixrOp -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) Curry_Prelude.OP_List) x2000 x3250 x3500))
     (Curry_AbstractCurry.Choice_C_CFixity x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_opLineDoc_dot_fixDoc_dot_185 x1002 x3000 x3250 x3500) (nd_OP_opLineDoc_dot_fixDoc_dot_185 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CFixity x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_opLineDoc_dot_fixDoc_dot_185 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFixity x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_opLineDoc_dot_fixDoc_dot_185 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFixity x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_eqCOpDecl :: Curry_AbstractCurry.C_COpDecl -> Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_eqCOpDecl x1 x2 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_COp x3 x4 x5) -> d_OP__case_214 x5 x4 x2 x3250 x3500
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_eqCOpDecl x1002 x2 x3250 x3500) (d_C_eqCOpDecl x1003 x2 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_eqCOpDecl z x2 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_eqCOpDecl x1002 x2 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_opDoc :: Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_opDoc x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_COp x2 x3 x4) -> d_OP__case_213 x2 x3250 x3500
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_opDoc x1002 x3250 x3500) (d_C_opDoc x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_opDoc z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_opDoc x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_opDoc :: Curry_AbstractCurry.C_COpDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_opDoc x1 x3000 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_COp x2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_213 x2 x2000 x3250 x3500))
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_opDoc x1002 x3000 x3250 x3500) (nd_C_opDoc x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_opDoc z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_opDoc x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_typesDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_typesDoc x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_vcat x3250 x3500) (Curry_Prelude.d_C_map (d_C_typeDoc x1)) x3250 x3500

nd_C_typesDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) Curry_Pretty.C_Doc
nd_C_typesDoc x1 x3000 x3250 x3500 = let
     x2002 = x3000
      in (seq x2002 (let
          x2001 = leftSupply x2002
          x2000 = rightSupply x2002
           in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dot (Curry_Pretty.nd_C_vcat x2000 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_map (wrapNX id (nd_C_typeDoc x1)))) x2001 x3250 x3500)))))

d_C_typeDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_typeDoc x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CType x3 x4 x5 x6) -> d_C_def (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) (d_C_qname x1 x3 x3250 x3500) x3250 x3500) x5 (d_C_consDeclsDoc x1 x6 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.C_CTypeSyn x7 x8 x9 x10) -> d_C_def (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x3250 x3500) x3250 x3500) (d_C_qname x1 x7 x3250 x3500) x3250 x3500) x9 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_equals x3250 x3500) x3250 x3500) (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) x10 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_typeDoc x1 x1002 x3250 x3500) (d_C_typeDoc x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_typeDoc x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_typeDoc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_typeDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CTypeDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_typeDoc x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CType x3 x4 x5 x6) -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2007 = leftSupply x2012
                    x2009 = rightSupply x2012
                     in (seq x2007 (seq x2009 (nd_C_def (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2003 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                                   x2002 = leftSupply x2003
                                   x2004 = rightSupply x2003
                                    in (seq x2002 (seq x2004 (let
                                        x2000 = leftSupply x2004
                                        x2001 = rightSupply x2004
                                         in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) Curry_Prelude.OP_List)))) x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_qname x1 x3 x2005 x3250 x3500) x2006 x3250 x3500))))))) x5 (nd_C_consDeclsDoc x1 x6 x2009 x3250 x3500) x2010 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CTypeSyn x7 x8 x9 x10) -> let
          x2019 = x3000
           in (seq x2019 (let
               x2018 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2018 (seq x2020 (let
                    x2007 = leftSupply x2020
                    x2016 = rightSupply x2020
                     in (seq x2007 (seq x2016 (nd_C_def (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2003 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                                   x2002 = leftSupply x2003
                                   x2004 = rightSupply x2003
                                    in (seq x2002 (seq x2004 (let
                                        x2000 = leftSupply x2004
                                        x2001 = rightSupply x2004
                                         in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'y'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'p'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_qname x1 x7 x2005 x3250 x3500) x2006 x3250 x3500))))))) x9 (let
                         x2015 = leftSupply x2016
                         x2017 = rightSupply x2016
                          in (seq x2015 (seq x2017 (let
                              x2012 = leftSupply x2017
                              x2014 = rightSupply x2017
                               in (seq x2012 (seq x2014 (Curry_Prelude.nd_C_apply (let
                                   x2011 = leftSupply x2012
                                   x2013 = rightSupply x2012
                                    in (seq x2011 (seq x2013 (let
                                        x2009 = leftSupply x2013
                                        x2010 = rightSupply x2013
                                         in (seq x2009 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2009 x3250 x3500) (Curry_Pretty.nd_C_equals x2010 x3250 x3500) x2011 x3250 x3500))))))) (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) x10 x2014 x3250 x3500) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_typeDoc x1 x1002 x3000 x3250 x3500) (nd_C_typeDoc x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_typeDoc x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_typeDoc x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_consDeclsDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_consDeclsDoc x1 x2 x3250 x3500 = d_OP__case_211 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x3250 x3500

nd_C_consDeclsDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_consDeclsDoc x1 x2 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_211 x2 x1 (Curry_Prelude.d_C_null x2 x3250 x3500) x2000 x3250 x3500))

d_C_consDeclDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_consDeclDoc x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCons x3 x4 x5 x6) -> d_C_app (d_C_qname x1 x3 x3250 x3500) (Curry_Prelude.d_C_map (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 2#)) x6 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_consDeclDoc x1 x1002 x3250 x3500) (d_C_consDeclDoc x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_consDeclDoc x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_consDeclDoc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_consDeclDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CConsDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_consDeclDoc x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCons x3 x4 x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (nd_C_app (nd_C_qname x1 x3 x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 2#))) x6 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_consDeclDoc x1 x1002 x3000 x3250 x3500) (nd_C_consDeclDoc x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_consDeclDoc x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_consDeclDoc x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_typeExprDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_typeExprDoc x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CTVar x4) -> d_C_tvarDoc x4 x3250 x3500
     (Curry_AbstractCurry.C_CTCons x5 x6) -> d_OP__case_209 x6 x5 x1 x2 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.C_CFuncType x7 x8) -> Curry_Prelude.d_OP_dollar (d_OP__case_204 x2 (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (d_C_arrow x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_space x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#)) (d_C_argTypes x3 x3250 x3500) x3250 x3500) (Curry_Prelude.OP_Cons (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) (d_C_resultType x3 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_typeExprDoc x1 x2 x1002 x3250 x3500) (d_C_typeExprDoc x1 x2 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_typeExprDoc x1 x2 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_typeExprDoc x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_typeExprDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CTypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_typeExprDoc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CTVar x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_tvarDoc x4 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CTCons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_209 x6 x5 x1 x2 (Curry_Prelude.d_C_null x6 x3250 x3500) x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CFuncType x7 x8) -> let
          x2021 = x3000
           in (seq x2021 (let
               x2020 = leftSupply x2021
               x2022 = rightSupply x2021
                in (seq x2020 (seq x2022 (let
                    x2000 = leftSupply x2022
                    x2016 = rightSupply x2022
                     in (seq x2000 (seq x2016 (Curry_Prelude.nd_OP_dollar (nd_OP__case_204 x2 (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 0#) x3250 x3500) x2000 x3250 x3500) (let
                         x2017 = leftSupply x2016
                         x2018 = rightSupply x2016
                          in (seq x2017 (seq x2018 (let
                              x2015 = leftSupply x2017
                              x2001 = rightSupply x2017
                               in (seq x2015 (seq x2001 (let
                                   x2002 = leftSupply x2018
                                   x2019 = rightSupply x2018
                                    in (seq x2002 (seq x2019 (let
                                        x2010 = leftSupply x2019
                                        x2014 = rightSupply x2019
                                         in (seq x2010 (seq x2014 (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_empty x2001 x3250 x3500) (Curry_Pretty.nd_C_empty x2002 x3250 x3500) (let
                                             x2009 = leftSupply x2010
                                             x2011 = rightSupply x2010
                                              in (seq x2009 (seq x2011 (let
                                                  x2006 = leftSupply x2011
                                                  x2008 = rightSupply x2011
                                                   in (seq x2006 (seq x2008 (Curry_Pretty.nd_OP_lt_gt (let
                                                       x2005 = leftSupply x2006
                                                       x2007 = rightSupply x2006
                                                        in (seq x2005 (seq x2007 (let
                                                            x2003 = leftSupply x2007
                                                            x2004 = rightSupply x2007
                                                             in (seq x2003 (seq x2004 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2003 x3250 x3500) (nd_C_arrow x2004 x3250 x3500) x2005 x3250 x3500))))))) (Curry_Pretty.nd_C_space x2008 x3250 x3500) x2009 x3250 x3500))))))) (let
                                             x2012 = leftSupply x2014
                                             x2013 = rightSupply x2014
                                              in (seq x2012 (seq x2013 (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.nd_C_map (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#))) (d_C_argTypes x3 x3250 x3500) x2012 x3250 x3500) (Curry_Prelude.OP_Cons (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) (d_C_resultType x3 x3250 x3500) x2013 x3250 x3500) Curry_Prelude.OP_List) x3250 x3500)))) x2015 x3250 x3500))))))))))))) x2020 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_typeExprDoc x1 x2 x1002 x3000 x3250 x3500) (nd_C_typeExprDoc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_typeExprDoc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_typeExprDoc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_isUntyped :: Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_isUntyped x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTCons x2 x3) -> d_OP__case_203 x3 x2 x3250 x3500
     (Curry_AbstractCurry.C_CTVar x54) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x55 x56) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CRecordType x57 x58) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_isUntyped x1002 x3250 x3500) (d_C_isUntyped x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_isUntyped z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_isUntyped x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_funcsDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_funcsDoc x1 x2 x3 x3000 x3250 x3500 = let
     x2007 = x3000
      in (seq x2007 (let
          x2006 = leftSupply x2007
          x2008 = rightSupply x2007
           in (seq x2006 (seq x2008 (let
               x2000 = leftSupply x2008
               x2004 = rightSupply x2008
                in (seq x2000 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vcat x2000 x3250 x3500) (let
                    x2003 = leftSupply x2004
                    x2005 = rightSupply x2004
                     in (seq x2003 (seq x2005 (let
                         x2001 = leftSupply x2005
                         x2002 = rightSupply x2005
                          in (seq x2001 (seq x2002 (Curry_Pretty.nd_C_punctuate (Curry_Pretty.nd_C_line x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_funcDoc x1 x2)) x3 x2002 x3250 x3500) x2003 x3250 x3500))))))) x2006 x3250 x3500))))))))

nd_C_funcDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CFuncDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_funcDoc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CFunc x4 x5 x6 x7 x8) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Pretty.nd_OP_lt_gt (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (nd_OP__case_171 x7 (d_C_hasRec x7 x3250 x3500) x2000 x3250 x3500) (nd_OP__case_170 x7 x4 x2 (d_C_isUntyped x7 x3250 x3500) x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_rulesDoc x1 x2 x4 x8 x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CmtFunc x9 x10 x11 x12 x13 x14) -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2007 = leftSupply x2012
                    x2009 = rightSupply x2012
                     in (seq x2007 (seq x2009 (Curry_Prelude.nd_C_apply (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2004 = rightSupply x2008
                               in (seq x2000 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vsep x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id nd_OP_funcDoc_dot___hash_lambda8) (Curry_Prelude.d_C_lines x9 x3250 x3500) x2002 x3250 x3500) x2003 x3250 x3500))))))) x2006 x3250 x3500))))))) (nd_C_funcDoc x1 x2 (Curry_AbstractCurry.C_CFunc x10 x11 x12 x13 x14) x2009 x3250 x3500) x2010 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_funcDoc x1 x2 x1002 x3000 x3250 x3500) (nd_C_funcDoc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_funcDoc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_funcDoc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_funcDoc_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_funcDoc_dot___hash_lambda8 x1 x3250 x3500 = Curry_Pretty.d_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x1 x3250 x3500) x3250 x3500

nd_OP_funcDoc_dot___hash_lambda8 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_funcDoc_dot___hash_lambda8 x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ' '#) Curry_Prelude.OP_List)))) x1 x3250 x3500) x2000 x3250 x3500))

d_C_hasRec :: Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_hasRec x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_Prelude.C_False
     (Curry_AbstractCurry.C_CFuncType x3 x4) -> Curry_Prelude.d_OP_bar_bar (d_C_hasRec x3 x3250 x3500) (d_C_hasRec x4 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.C_CTCons x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_any d_C_hasRec x3250 x3500) x6 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_hasRec x1002 x3250 x3500) (d_C_hasRec x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_hasRec z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_hasRec x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_localDeclsDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CLocalDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_localDeclsDoc x1 x2 x3 x3000 x3250 x3500 = let
     x2011 = x3000
      in (seq x2011 (let
          x2010 = leftSupply x2011
          x2012 = rightSupply x2011
           in (seq x2010 (seq x2012 (let
               x2000 = leftSupply x2012
               x2008 = rightSupply x2012
                in (seq x2000 (seq x2008 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_align x2000 x3250 x3500) (let
                    x2007 = leftSupply x2008
                    x2009 = rightSupply x2008
                     in (seq x2007 (seq x2009 (let
                         x2001 = leftSupply x2009
                         x2005 = rightSupply x2009
                          in (seq x2001 (seq x2005 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_vsep x2001 x3250 x3500) (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2002 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2002 (seq x2003 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_punctuate (Curry_Pretty.nd_C_line x2002 x3250 x3500))) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_localDeclDoc x1 x2)) x3 x2003 x3250 x3500) x2004 x3250 x3500))))))) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))))

nd_C_localDeclDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CLocalDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_localDeclDoc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CLocalFunc x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_funcDoc x1 x2 x4 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CLocalPat x5 x6 x7) -> let
          x2024 = x3000
           in (seq x2024 (let
               x2023 = leftSupply x2024
               x2021 = rightSupply x2024
                in (seq x2023 (seq x2021 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                    x2020 = leftSupply x2021
                    x2022 = rightSupply x2021
                     in (seq x2020 (seq x2022 (let
                         x2017 = leftSupply x2022
                         x2019 = rightSupply x2022
                          in (seq x2017 (seq x2019 (Curry_Pretty.nd_OP_lt_gt (let
                              x2016 = leftSupply x2017
                              x2018 = rightSupply x2017
                               in (seq x2016 (seq x2018 (let
                                   x2011 = leftSupply x2018
                                   x2015 = rightSupply x2018
                                    in (seq x2011 (seq x2015 (Curry_Prelude.nd_C_apply (let
                                        x2010 = leftSupply x2011
                                        x2012 = rightSupply x2011
                                         in (seq x2010 (seq x2012 (let
                                             x2000 = leftSupply x2012
                                             x2008 = rightSupply x2012
                                              in (seq x2000 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                                  x2007 = leftSupply x2008
                                                  x2009 = rightSupply x2008
                                                   in (seq x2007 (seq x2009 (let
                                                       x2004 = leftSupply x2009
                                                       x2006 = rightSupply x2009
                                                        in (seq x2004 (seq x2006 (Curry_Prelude.nd_C_apply (let
                                                            x2003 = leftSupply x2004
                                                            x2005 = rightSupply x2004
                                                             in (seq x2003 (seq x2005 (let
                                                                 x2001 = leftSupply x2005
                                                                 x2002 = rightSupply x2005
                                                                  in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (nd_C_patternDoc x2 x5 x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Pretty.nd_C_equals x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))) (let
                                        x2014 = leftSupply x2015
                                        x2013 = rightSupply x2015
                                         in (seq x2014 (seq x2013 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2013 x3250 x3500) x1 Curry_Prelude.C_Nothing x2 x6 x2014 x3250 x3500)))) x2016 x3250 x3500))))))) (nd_OP__case_169 x7 x2 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x2019 x3250 x3500) x2020 x3250 x3500))))))) x2023 x3250 x3500)))))
     (Curry_AbstractCurry.C_CLocalVar x8) -> let
          x2010 = x3000
           in (seq x2010 (let
               x2009 = leftSupply x2010
               x2007 = rightSupply x2010
                in (seq x2009 (seq x2007 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                    x2006 = leftSupply x2007
                    x2008 = rightSupply x2007
                     in (seq x2006 (seq x2008 (let
                         x2003 = leftSupply x2008
                         x2005 = rightSupply x2008
                          in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                              x2002 = leftSupply x2003
                              x2004 = rightSupply x2003
                               in (seq x2002 (seq x2004 (let
                                   x2000 = leftSupply x2004
                                   x2001 = rightSupply x2004
                                    in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (nd_C_tvarDoc x8 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2005 x3250 x3500) x2006 x3250 x3500))))))) x2009 x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CLocalDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_localDeclDoc x1 x2 x1002 x3000 x3250 x3500) (nd_C_localDeclDoc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CLocalDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_localDeclDoc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLocalDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_localDeclDoc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLocalDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_funcTypeDeclDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_funcTypeDeclDoc x1 x2 x3 x3250 x3500 = d_C_def (d_C_qname x1 x2 x3250 x3500) Curry_Prelude.OP_List (d_C_funcTypeDoc x1 (d_C_argTypes x3 x3250 x3500) (d_C_resultType x3 x3250 x3500) x3250 x3500) x3250 x3500

nd_C_funcTypeDeclDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CTypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_funcTypeDeclDoc x1 x2 x3 x3000 x3250 x3500 = let
     x2003 = x3000
      in (seq x2003 (let
          x2002 = leftSupply x2003
          x2004 = rightSupply x2003
           in (seq x2002 (seq x2004 (let
               x2000 = leftSupply x2004
               x2001 = rightSupply x2004
                in (seq x2000 (seq x2001 (nd_C_def (nd_C_qname x1 x2 x2000 x3250 x3500) Curry_Prelude.OP_List (nd_C_funcTypeDoc x1 (d_C_argTypes x3 x3250 x3500) (d_C_resultType x3 x3250 x3500) x2001 x3250 x3500) x2002 x3250 x3500))))))))

d_C_funcTypeDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_funcTypeDoc x1 x2 x3 x3250 x3500 = Curry_Pretty.d_C_fillEncloseSep (d_C_dcolon x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (d_C_arrow x3250 x3500) x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500)) (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#)) x3250 x3500) x2 x3250 x3500) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500)) (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#)) x3250 x3500) (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) x3250 x3500

nd_C_funcTypeDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_AbstractCurry.C_CTypeExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_funcTypeDoc x1 x2 x3 x3000 x3250 x3500 = let
     x2019 = x3000
      in (seq x2019 (let
          x2020 = leftSupply x2019
          x2021 = rightSupply x2019
           in (seq x2020 (seq x2021 (let
               x2018 = leftSupply x2020
               x2000 = rightSupply x2020
                in (seq x2018 (seq x2000 (let
                    x2001 = leftSupply x2021
                    x2022 = rightSupply x2021
                     in (seq x2001 (seq x2022 (let
                         x2005 = leftSupply x2022
                         x2017 = rightSupply x2022
                          in (seq x2005 (seq x2017 (Curry_Pretty.nd_C_fillEncloseSep (nd_C_dcolon x2000 x3250 x3500) (Curry_Pretty.nd_C_empty x2001 x3250 x3500) (let
                              x2004 = leftSupply x2005
                              x2006 = rightSupply x2005
                               in (seq x2004 (seq x2006 (let
                                   x2002 = leftSupply x2006
                                   x2003 = rightSupply x2006
                                    in (seq x2002 (seq x2003 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2002 x3250 x3500) (nd_C_arrow x2003 x3250 x3500) x2004 x3250 x3500))))))) (let
                              x2011 = leftSupply x2017
                              x2016 = rightSupply x2017
                               in (seq x2011 (seq x2016 (Curry_Prelude.d_OP_plus_plus (let
                                   x2010 = leftSupply x2011
                                   x2009 = rightSupply x2011
                                    in (seq x2010 (seq x2009 (Curry_Prelude.nd_C_map (let
                                        x2008 = leftSupply x2009
                                        x2007 = rightSupply x2009
                                         in (seq x2008 (seq x2007 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2007 x3250 x3500))) (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#))) x2008 x3250 x3500)))) x2 x2010 x3250 x3500)))) (let
                                   x2015 = leftSupply x2016
                                   x2014 = rightSupply x2016
                                    in (seq x2015 (seq x2014 (Curry_Prelude.nd_C_map (let
                                        x2013 = leftSupply x2014
                                        x2012 = rightSupply x2014
                                         in (seq x2013 (seq x2012 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2012 x3250 x3500))) (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 1#))) x2013 x3250 x3500)))) (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x2015 x3250 x3500)))) x3250 x3500)))) x2018 x3250 x3500))))))))))))))

nd_C_rulesDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CRules -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_rulesDoc x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     (Curry_AbstractCurry.C_CRules x5 x6) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vsep x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_ruleDoc x1 x2 x3)) x6 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CExternal x7) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (nd_C_qname x2 x3 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) Curry_Prelude.OP_List)))))))) x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CRules x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_rulesDoc x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_rulesDoc x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CRules x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_rulesDoc x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRules x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_rulesDoc x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRules x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_ruleDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CRule -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_ruleDoc x1 x2 x3 x4 x3000 x3250 x3500 = case x4 of
     (Curry_AbstractCurry.C_CRule x5 x6 x7) -> let
          x2004 = x3000
           in (seq x2004 (let
               x2005 = leftSupply x2004
               x2006 = rightSupply x2004
                in (seq x2005 (seq x2006 (let
                    x2000 = leftSupply x2005
                    x2001 = rightSupply x2005
                     in (seq x2000 (seq x2001 (let
                         x2002 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2002 (seq x2003 (let
                              x8 = nd_OP__case_166 x5 x2 (Curry_Prelude.d_C_null x5 x3250 x3500) x2000 x3250 x3500
                              x9 = nd_OP__case_165 x5 x3 x8 x2 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isInfixName x3 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x5 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x2001 x3250 x3500
                              x10 = nd_OP__case_164 x7 x2 x1 (Curry_Prelude.d_C_null x7 x3250 x3500) x2002 x3250 x3500
                               in (nd_OP__case_168 x6 x10 x1 x2 x9 (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_fst (Curry_Prelude.d_C_head x6 x3250 x3500) x3250 x3500) (Curry_AbstractCurry.C_CSymbol (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) Curry_Prelude.OP_List))))))))) x3250 x3500) x2003 x3250 x3500))))))))))))
     (Curry_AbstractCurry.Choice_C_CRule x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_ruleDoc x1 x2 x3 x1002 x3000 x3250 x3500) (nd_C_ruleDoc x1 x2 x3 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CRule x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_ruleDoc x1 x2 x3 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CRule x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_ruleDoc x1 x2 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CRule x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_ruleDoc_dot_choiceDoc_dot_285 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_ruleDoc_dot_choiceDoc_dot_285 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> let
          x2031 = x3000
           in (seq x2031 (let
               x2030 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2030 (seq x2032 (let
                    x2025 = leftSupply x2032
                    x2029 = rightSupply x2032
                     in (seq x2025 (seq x2029 (Curry_Prelude.nd_C_apply (let
                         x2024 = leftSupply x2025
                         x2026 = rightSupply x2025
                          in (seq x2024 (seq x2026 (let
                              x2000 = leftSupply x2026
                              x2022 = rightSupply x2026
                               in (seq x2000 (seq x2022 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2021 = leftSupply x2022
                                   x2023 = rightSupply x2022
                                    in (seq x2021 (seq x2023 (let
                                        x2018 = leftSupply x2023
                                        x2020 = rightSupply x2023
                                         in (seq x2018 (seq x2020 (Curry_Prelude.nd_C_apply (let
                                             x2017 = leftSupply x2018
                                             x2019 = rightSupply x2018
                                              in (seq x2017 (seq x2019 (let
                                                  x2001 = leftSupply x2019
                                                  x2015 = rightSupply x2019
                                                   in (seq x2001 (seq x2015 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                                       x2014 = leftSupply x2015
                                                       x2016 = rightSupply x2015
                                                        in (seq x2014 (seq x2016 (let
                                                            x2005 = leftSupply x2016
                                                            x2012 = rightSupply x2016
                                                             in (seq x2005 (seq x2012 (Curry_Prelude.nd_C_apply (let
                                                                 x2004 = leftSupply x2005
                                                                 x2006 = rightSupply x2005
                                                                  in (seq x2004 (seq x2006 (let
                                                                      x2002 = leftSupply x2006
                                                                      x2003 = rightSupply x2006
                                                                       in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List) x2003 x3250 x3500) x2004 x3250 x3500))))))) (let
                                                                 x2011 = leftSupply x2012
                                                                 x2013 = rightSupply x2012
                                                                  in (seq x2011 (seq x2013 (let
                                                                      x2007 = leftSupply x2013
                                                                      x2010 = rightSupply x2013
                                                                       in (seq x2007 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2007 x3250 x3500) (let
                                                                           x2009 = leftSupply x2010
                                                                           x2008 = rightSupply x2010
                                                                            in (seq x2009 (seq x2008 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2008 x3250 x3500) x2 Curry_Prelude.C_Nothing x1 x4 x2009 x3250 x3500)))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))) x2017 x3250 x3500))))))) (Curry_Pretty.nd_C_equals x2020 x3250 x3500) x2021 x3250 x3500))))))) x2024 x3250 x3500))))))) (let
                         x2028 = leftSupply x2029
                         x2027 = rightSupply x2029
                          in (seq x2028 (seq x2027 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2027 x3250 x3500) x2 Curry_Prelude.C_Nothing x1 x5 x2028 x3250 x3500)))) x2030 x3250 x3500))))))))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_ruleDoc_dot_choiceDoc_dot_285 x1 x2 x1002 x3000 x3250 x3500) (nd_OP_ruleDoc_dot_choiceDoc_dot_285 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_ruleDoc_dot_choiceDoc_dot_285 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_ruleDoc_dot_choiceDoc_dot_285 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_expDoc :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_expDoc x1 x2 x3 x4 x5 x3000 x3250 x3500 = let
     x2004 = x3000
      in (seq x2004 (let
          x2003 = leftSupply x2004
          x2002 = rightSupply x2004
           in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_maybe (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_maybe (nd_C_expDoc2 x1 x2 x3 x4 x5 x2000 x3250 x3500) (wrapNX id (nd_OP_expDoc_dot___hash_lambda9 x4 x2)) (d_C_toList x5 x3250 x3500) x2001 x3250 x3500)))) (wrapNX id nd_OP_expDoc_dot___hash_lambda10) (d_C_toString x5 x3250 x3500) x2003 x3250 x3500)))))

nd_OP_expDoc_dot___hash_lambda9 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_expDoc_dot___hash_lambda9 x1 x2 x3 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_list x2000 x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2001 x3250 x3500) x2 Curry_Prelude.C_Nothing x1)) x3 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_OP_expDoc_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_expDoc_dot___hash_lambda10 x1 x3250 x3500 = d_OP__case_163 x1 (Curry_Prelude.d_C_null x1 x3250 x3500) x3250 x3500

nd_OP_expDoc_dot___hash_lambda10 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_expDoc_dot___hash_lambda10 x1 x3000 x3250 x3500 = let
     x2000 = x3000
      in (seq x2000 (nd_OP__case_163 x1 (Curry_Prelude.d_C_null x1 x3250 x3500) x2000 x3250 x3500))

d_C_toList :: Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_C_toList x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CSymbol x2) -> d_OP__case_162 x2 x3250 x3500
     (Curry_AbstractCurry.C_CApply x36 x37) -> d_OP__case_141 x37 x36 x3250 x3500
     (Curry_AbstractCurry.C_CVar x106) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x107) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x108 x109) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x110 x111) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x112) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x113 x114) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x115 x116) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x117) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x118 x119) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x120 x121) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toList x1002 x3250 x3500) (d_C_toList x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toList z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toList x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toString :: Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_toString x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CSymbol x2) -> d_OP__case_120 x2 x3250 x3500
     (Curry_AbstractCurry.C_CApply x36 x37) -> d_OP__case_99 x37 x36 x3250 x3500
     (Curry_AbstractCurry.C_CVar x128) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x129) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x130 x131) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x132 x133) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x134) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x135 x136) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x137 x138) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x139) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x140 x141) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x142 x143) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toString x1002 x3250 x3500) (d_C_toString x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toString z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toString x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_expDoc2 :: Curry_Prelude.C_Bool -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_expDoc2 x1 x2 x3 x4 x5 x3000 x3250 x3500 = case x5 of
     (Curry_AbstractCurry.C_CVar x6) -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Pretty.nd_OP_lt_gt (nd_C_showPrecs x6 x3 x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_varDoc x2001 x3250 x3500) x6 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CLit x7) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (nd_C_showPrecs x7 x3 x2000 x3250 x3500) (nd_C_litDoc x7 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CSymbol x8) -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (nd_C_showPrecs x8 x3 x2000 x3250 x3500) (nd_C_qname x4 x8 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CLetDecl x9 x10) -> let
          x2031 = x3000
           in (seq x2031 (let
               x2030 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2030 (seq x2032 (let
                    x2000 = leftSupply x2032
                    x2029 = rightSupply x2032
                     in (seq x2000 (seq x2029 (Curry_Prelude.nd_OP_dollar (nd_C_par x3 x2000 x3250 x3500) (let
                         x2028 = leftSupply x2029
                         x2026 = rightSupply x2029
                          in (seq x2028 (seq x2026 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                              x2025 = leftSupply x2026
                              x2027 = rightSupply x2026
                               in (seq x2025 (seq x2027 (let
                                   x2020 = leftSupply x2027
                                   x2024 = rightSupply x2027
                                    in (seq x2020 (seq x2024 (Curry_Prelude.nd_C_apply (let
                                        x2019 = leftSupply x2020
                                        x2021 = rightSupply x2020
                                         in (seq x2019 (seq x2021 (let
                                             x2001 = leftSupply x2021
                                             x2017 = rightSupply x2021
                                              in (seq x2001 (seq x2017 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                                  x2016 = leftSupply x2017
                                                  x2018 = rightSupply x2017
                                                   in (seq x2016 (seq x2018 (let
                                                       x2013 = leftSupply x2018
                                                       x2015 = rightSupply x2018
                                                        in (seq x2013 (seq x2015 (Curry_Prelude.nd_C_apply (let
                                                            x2012 = leftSupply x2013
                                                            x2014 = rightSupply x2013
                                                             in (seq x2012 (seq x2014 (let
                                                                 x2002 = leftSupply x2014
                                                                 x2010 = rightSupply x2014
                                                                  in (seq x2002 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2002 x3250 x3500) (let
                                                                      x2009 = leftSupply x2010
                                                                      x2011 = rightSupply x2010
                                                                       in (seq x2009 (seq x2011 (let
                                                                           x2006 = leftSupply x2011
                                                                           x2008 = rightSupply x2011
                                                                            in (seq x2006 (seq x2008 (Curry_Prelude.nd_C_apply (let
                                                                                x2005 = leftSupply x2006
                                                                                x2007 = rightSupply x2006
                                                                                 in (seq x2005 (seq x2007 (let
                                                                                     x2003 = leftSupply x2007
                                                                                     x2004 = rightSupply x2007
                                                                                      in (seq x2003 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2003 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) x2004 x3250 x3500) x2005 x3250 x3500))))))) (nd_C_localDeclsDoc x2 x4 x9 x2008 x3250 x3500) x2009 x3250 x3500))))))) x2012 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)) x2015 x3250 x3500) x2016 x3250 x3500))))))) x2019 x3250 x3500))))))) (let
                                        x2023 = leftSupply x2024
                                        x2022 = rightSupply x2024
                                         in (seq x2023 (seq x2022 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2022 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 x10 x2023 x3250 x3500)))) x2025 x3250 x3500))))))) x2028 x3250 x3500)))) x2030 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CCase x11 x12) -> let
          x2039 = x3000
           in (seq x2039 (let
               x2038 = leftSupply x2039
               x2040 = rightSupply x2039
                in (seq x2038 (seq x2040 (let
                    x2000 = leftSupply x2040
                    x2037 = rightSupply x2040
                     in (seq x2000 (seq x2037 (Curry_Prelude.nd_OP_dollar (nd_C_par x3 x2000 x3250 x3500) (let
                         x2036 = leftSupply x2037
                         x2034 = rightSupply x2037
                          in (seq x2036 (seq x2034 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                              x2033 = leftSupply x2034
                              x2035 = rightSupply x2034
                               in (seq x2033 (seq x2035 (let
                                   x2026 = leftSupply x2035
                                   x2031 = rightSupply x2035
                                    in (seq x2026 (seq x2031 (Curry_Prelude.nd_C_apply (let
                                        x2025 = leftSupply x2026
                                        x2027 = rightSupply x2026
                                         in (seq x2025 (seq x2027 (let
                                             x2001 = leftSupply x2027
                                             x2023 = rightSupply x2027
                                              in (seq x2001 (seq x2023 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2001 x3250 x3500) (let
                                                  x2022 = leftSupply x2023
                                                  x2024 = rightSupply x2023
                                                   in (seq x2022 (seq x2024 (let
                                                       x2019 = leftSupply x2024
                                                       x2021 = rightSupply x2024
                                                        in (seq x2019 (seq x2021 (Curry_Prelude.nd_C_apply (let
                                                            x2018 = leftSupply x2019
                                                            x2020 = rightSupply x2019
                                                             in (seq x2018 (seq x2020 (let
                                                                 x2002 = leftSupply x2020
                                                                 x2016 = rightSupply x2020
                                                                  in (seq x2002 (seq x2016 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (let
                                                                      x2015 = leftSupply x2016
                                                                      x2017 = rightSupply x2016
                                                                       in (seq x2015 (seq x2017 (let
                                                                           x2006 = leftSupply x2017
                                                                           x2013 = rightSupply x2017
                                                                            in (seq x2006 (seq x2013 (Curry_Prelude.nd_C_apply (let
                                                                                x2005 = leftSupply x2006
                                                                                x2007 = rightSupply x2006
                                                                                 in (seq x2005 (seq x2007 (let
                                                                                     x2003 = leftSupply x2007
                                                                                     x2004 = rightSupply x2007
                                                                                      in (seq x2003 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2003 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'c'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'a'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2004 x3250 x3500) x2005 x3250 x3500))))))) (let
                                                                                x2012 = leftSupply x2013
                                                                                x2014 = rightSupply x2013
                                                                                 in (seq x2012 (seq x2014 (let
                                                                                     x2008 = leftSupply x2014
                                                                                     x2011 = rightSupply x2014
                                                                                      in (seq x2008 (seq x2011 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2008 x3250 x3500) (let
                                                                                          x2010 = leftSupply x2011
                                                                                          x2009 = rightSupply x2011
                                                                                           in (seq x2010 (seq x2009 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2009 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 x11 x2010 x3250 x3500)))) x2012 x3250 x3500))))))) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List)) x2021 x3250 x3500) x2022 x3250 x3500))))))) x2025 x3250 x3500))))))) (let
                                        x2030 = leftSupply x2031
                                        x2032 = rightSupply x2031
                                         in (seq x2030 (seq x2032 (let
                                             x2028 = leftSupply x2032
                                             x2029 = rightSupply x2032
                                              in (seq x2028 (seq x2029 (Curry_Prelude.nd_C_apply (nd_C_layout x2028 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_branchDoc x2 x4)) x12 x2029 x3250 x3500) x2030 x3250 x3500))))))) x2033 x3250 x3500))))))) x2036 x3250 x3500)))) x2038 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CLambda x13 x14) -> let
          x2033 = x3000
           in (seq x2033 (let
               x2032 = leftSupply x2033
               x2034 = rightSupply x2033
                in (seq x2032 (seq x2034 (let
                    x2000 = leftSupply x2034
                    x2031 = rightSupply x2034
                     in (seq x2000 (seq x2031 (Curry_Prelude.nd_OP_dollar (nd_C_par x3 x2000 x3250 x3500) (let
                         x2030 = leftSupply x2031
                         x2028 = rightSupply x2031
                          in (seq x2030 (seq x2028 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                              x2027 = leftSupply x2028
                              x2029 = rightSupply x2028
                               in (seq x2027 (seq x2029 (let
                                   x2022 = leftSupply x2029
                                   x2026 = rightSupply x2029
                                    in (seq x2022 (seq x2026 (Curry_Prelude.nd_C_apply (let
                                        x2021 = leftSupply x2022
                                        x2023 = rightSupply x2022
                                         in (seq x2021 (seq x2023 (let
                                             x2001 = leftSupply x2023
                                             x2019 = rightSupply x2023
                                              in (seq x2001 (seq x2019 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                                  x2018 = leftSupply x2019
                                                  x2020 = rightSupply x2019
                                                   in (seq x2018 (seq x2020 (let
                                                       x2015 = leftSupply x2020
                                                       x2017 = rightSupply x2020
                                                        in (seq x2015 (seq x2017 (Curry_Prelude.nd_C_apply (let
                                                            x2014 = leftSupply x2015
                                                            x2016 = rightSupply x2015
                                                             in (seq x2014 (seq x2016 (let
                                                                 x2002 = leftSupply x2016
                                                                 x2012 = rightSupply x2016
                                                                  in (seq x2002 (seq x2012 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (let
                                                                      x2011 = leftSupply x2012
                                                                      x2013 = rightSupply x2012
                                                                       in (seq x2011 (seq x2013 (let
                                                                           x2006 = leftSupply x2013
                                                                           x2010 = rightSupply x2013
                                                                            in (seq x2006 (seq x2010 (Curry_Prelude.nd_C_apply (let
                                                                                x2005 = leftSupply x2006
                                                                                x2007 = rightSupply x2006
                                                                                 in (seq x2005 (seq x2007 (let
                                                                                     x2003 = leftSupply x2007
                                                                                     x2004 = rightSupply x2007
                                                                                      in (seq x2003 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2003 x3250 x3500) (Curry_Pretty.nd_C_backslash x2004 x3250 x3500) x2005 x3250 x3500))))))) (let
                                                                                x2009 = leftSupply x2010
                                                                                x2008 = rightSupply x2010
                                                                                 in (seq x2009 (seq x2008 (Curry_Prelude.nd_C_apply (nd_C_patternsDoc x4 x2008 x3250 x3500) x13 x2009 x3250 x3500)))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))) (nd_C_arrow x2017 x3250 x3500) x2018 x3250 x3500))))))) x2021 x3250 x3500))))))) (let
                                        x2025 = leftSupply x2026
                                        x2024 = rightSupply x2026
                                         in (seq x2025 (seq x2024 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2024 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 x14 x2025 x3250 x3500)))) x2027 x3250 x3500))))))) x2030 x3250 x3500)))) x2032 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CApply x15 x16) -> let
          x2066 = x3000
           in (seq x2066 (let
               x2067 = leftSupply x2066
               x2069 = rightSupply x2066
                in (seq x2067 (seq x2069 (let
                    x2057 = leftSupply x2067
                    x2068 = rightSupply x2067
                     in (seq x2057 (seq x2068 (let
                         x2059 = leftSupply x2068
                         x2060 = rightSupply x2068
                          in (seq x2059 (seq x2060 (let
                              x2061 = leftSupply x2069
                              x2070 = rightSupply x2069
                               in (seq x2061 (seq x2070 (let
                                   x2062 = leftSupply x2070
                                   x2065 = rightSupply x2070
                                    in (seq x2062 (seq x2065 (let
                                        x17 = d_OP_expDoc2_dot_args_dot_347 (Curry_AbstractCurry.C_CApply x15 x16) x3250 x3500
                                        x18 = let
                                             x2056 = leftSupply x2057
                                             x2058 = rightSupply x2057
                                              in (seq x2056 (seq x2058 (let
                                                  x2047 = leftSupply x2058
                                                  x2054 = rightSupply x2058
                                                   in (seq x2047 (seq x2054 (Curry_Prelude.nd_C_apply (let
                                                       x2046 = leftSupply x2047
                                                       x2048 = rightSupply x2047
                                                        in (seq x2046 (seq x2048 (let
                                                            x2000 = leftSupply x2048
                                                            x2044 = rightSupply x2048
                                                             in (seq x2000 (seq x2044 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                                                 x2043 = leftSupply x2044
                                                                 x2045 = rightSupply x2044
                                                                  in (seq x2043 (seq x2045 (let
                                                                      x2040 = leftSupply x2045
                                                                      x2042 = rightSupply x2045
                                                                       in (seq x2040 (seq x2042 (Curry_Prelude.nd_C_apply (let
                                                                           x2039 = leftSupply x2040
                                                                           x2041 = rightSupply x2040
                                                                            in (seq x2039 (seq x2041 (let
                                                                                x2001 = leftSupply x2041
                                                                                x2037 = rightSupply x2041
                                                                                 in (seq x2001 (seq x2037 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2001 x3250 x3500) (let
                                                                                     x2036 = leftSupply x2037
                                                                                     x2038 = rightSupply x2037
                                                                                      in (seq x2036 (seq x2038 (let
                                                                                          x2027 = leftSupply x2038
                                                                                          x2034 = rightSupply x2038
                                                                                           in (seq x2027 (seq x2034 (Curry_Prelude.nd_C_apply (let
                                                                                               x2026 = leftSupply x2027
                                                                                               x2028 = rightSupply x2027
                                                                                                in (seq x2026 (seq x2028 (let
                                                                                                    x2002 = leftSupply x2028
                                                                                                    x2024 = rightSupply x2028
                                                                                                     in (seq x2002 (seq x2024 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (let
                                                                                                         x2023 = leftSupply x2024
                                                                                                         x2025 = rightSupply x2024
                                                                                                          in (seq x2023 (seq x2025 (let
                                                                                                              x2020 = leftSupply x2025
                                                                                                              x2022 = rightSupply x2025
                                                                                                               in (seq x2020 (seq x2022 (Curry_Prelude.nd_C_apply (let
                                                                                                                   x2019 = leftSupply x2020
                                                                                                                   x2021 = rightSupply x2020
                                                                                                                    in (seq x2019 (seq x2021 (let
                                                                                                                        x2003 = leftSupply x2021
                                                                                                                        x2017 = rightSupply x2021
                                                                                                                         in (seq x2003 (seq x2017 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2003 x3250 x3500) (let
                                                                                                                             x2016 = leftSupply x2017
                                                                                                                             x2018 = rightSupply x2017
                                                                                                                              in (seq x2016 (seq x2018 (let
                                                                                                                                  x2007 = leftSupply x2018
                                                                                                                                  x2014 = rightSupply x2018
                                                                                                                                   in (seq x2007 (seq x2014 (Curry_Prelude.nd_C_apply (let
                                                                                                                                       x2006 = leftSupply x2007
                                                                                                                                       x2008 = rightSupply x2007
                                                                                                                                        in (seq x2006 (seq x2008 (let
                                                                                                                                            x2004 = leftSupply x2008
                                                                                                                                            x2005 = rightSupply x2008
                                                                                                                                             in (seq x2004 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2004 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) Curry_Prelude.OP_List)) x2005 x3250 x3500) x2006 x3250 x3500))))))) (let
                                                                                                                                       x2013 = leftSupply x2014
                                                                                                                                       x2015 = rightSupply x2014
                                                                                                                                        in (seq x2013 (seq x2015 (let
                                                                                                                                            x2009 = leftSupply x2015
                                                                                                                                            x2012 = rightSupply x2015
                                                                                                                                             in (seq x2009 (seq x2012 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2009 x3250 x3500) (let
                                                                                                                                                 x2011 = leftSupply x2012
                                                                                                                                                 x2010 = rightSupply x2012
                                                                                                                                                  in (seq x2011 (seq x2010 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2010 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 0#) x3250 x3500) x2011 x3250 x3500)))) x2013 x3250 x3500))))))) x2016 x3250 x3500))))))) x2019 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) Curry_Prelude.OP_List)))) x2022 x3250 x3500) x2023 x3250 x3500))))))) x2026 x3250 x3500))))))) (let
                                                                                               x2033 = leftSupply x2034
                                                                                               x2035 = rightSupply x2034
                                                                                                in (seq x2033 (seq x2035 (let
                                                                                                    x2029 = leftSupply x2035
                                                                                                    x2032 = rightSupply x2035
                                                                                                     in (seq x2029 (seq x2032 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2029 x3250 x3500) (let
                                                                                                         x2031 = leftSupply x2032
                                                                                                         x2030 = rightSupply x2032
                                                                                                          in (seq x2031 (seq x2030 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2030 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 1#) x3250 x3500) x2031 x3250 x3500)))) x2033 x3250 x3500))))))) x2036 x3250 x3500))))))) x2039 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))) x2042 x3250 x3500) x2043 x3250 x3500))))))) x2046 x3250 x3500))))))) (let
                                                       x2053 = leftSupply x2054
                                                       x2055 = rightSupply x2054
                                                        in (seq x2053 (seq x2055 (let
                                                            x2049 = leftSupply x2055
                                                            x2052 = rightSupply x2055
                                                             in (seq x2049 (seq x2052 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2049 x3250 x3500) (let
                                                                 x2051 = leftSupply x2052
                                                                 x2050 = rightSupply x2052
                                                                  in (seq x2051 (seq x2050 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2050 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 2#) x3250 x3500) x2051 x3250 x3500)))) x2053 x3250 x3500))))))) x2056 x3250 x3500))))))
                                        x19 = d_OP__case_70 x16 x15 (d_OP_expDoc2_dot_name_dot_347 (Curry_AbstractCurry.C_CApply x15 x16) x3250 x3500) x3250 x3500
                                        x39 = Curry_Prelude.nd_C_maybe (Curry_Prelude.OP_Tuple2 Curry_Prelude.OP_List Curry_Prelude.OP_List) (wrapDX id Curry_Prelude.d_C_id) x19 x2059 x3250 x3500
                                        x40 = nd_OP__case_69 x3 (Curry_Maybe.d_C_isJust x3 x3250 x3500) x2060 x3250 x3500
                                        x41 = nd_OP_expDoc2_dot___hash_selFP2_hash_lbr x40 x2061 x3250 x3500
                                        x42 = nd_OP_expDoc2_dot___hash_selFP3_hash_rbr x40 x2062 x3250 x3500
                                        x43 = d_OP__case_68 x2 x39 (Curry_Prelude.d_C_lookup x39 x2 x3250 x3500) x3250 x3500
                                         in (let
                                             x2064 = leftSupply x2065
                                             x2063 = rightSupply x2065
                                              in (seq x2064 (seq x2063 (nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapDX id d_C_isTupleName) x19 x2063 x3250 x3500) x2064 x3250 x3500)))))))))))))))))))))
     (Curry_AbstractCurry.C_CDoExpr x45) -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2000 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2000 (seq x2012 (Curry_Prelude.nd_OP_dollar (nd_C_par x3 x2000 x3250 x3500) (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2004 = leftSupply x2013
                              x2009 = rightSupply x2013
                               in (seq x2004 (seq x2009 (Curry_Prelude.nd_C_apply (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List)) x2002 x3250 x3500) x2003 x3250 x3500))))))) (let
                                   x2008 = leftSupply x2009
                                   x2010 = rightSupply x2009
                                    in (seq x2008 (seq x2010 (let
                                        x2006 = leftSupply x2010
                                        x2007 = rightSupply x2010
                                         in (seq x2006 (seq x2007 (Curry_Prelude.nd_C_apply (nd_C_layout x2006 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_statementDoc x2 x4)) x45 x2007 x3250 x3500) x2008 x3250 x3500))))))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CListComp x46 x47) -> let
          x2029 = x3000
           in (seq x2029 (let
               x2028 = leftSupply x2029
               x2030 = rightSupply x2029
                in (seq x2028 (seq x2030 (let
                    x2000 = leftSupply x2030
                    x2026 = rightSupply x2030
                     in (seq x2000 (seq x2026 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_brackets x2000 x3250 x3500) (let
                         x2025 = leftSupply x2026
                         x2027 = rightSupply x2026
                          in (seq x2025 (seq x2027 (let
                              x2014 = leftSupply x2027
                              x2021 = rightSupply x2027
                               in (seq x2014 (seq x2021 (Curry_Prelude.nd_C_apply (let
                                   x2013 = leftSupply x2014
                                   x2015 = rightSupply x2014
                                    in (seq x2013 (seq x2015 (let
                                        x2001 = leftSupply x2015
                                        x2011 = rightSupply x2015
                                         in (seq x2001 (seq x2011 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                             x2010 = leftSupply x2011
                                             x2012 = rightSupply x2011
                                              in (seq x2010 (seq x2012 (let
                                                  x2007 = leftSupply x2012
                                                  x2009 = rightSupply x2012
                                                   in (seq x2007 (seq x2009 (Curry_Prelude.nd_C_apply (let
                                                       x2006 = leftSupply x2007
                                                       x2008 = rightSupply x2007
                                                        in (seq x2006 (seq x2008 (let
                                                            x2002 = leftSupply x2008
                                                            x2005 = rightSupply x2008
                                                             in (seq x2002 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (let
                                                                 x2004 = leftSupply x2005
                                                                 x2003 = rightSupply x2005
                                                                  in (seq x2004 (seq x2003 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2003 x3250 x3500) x2 Curry_Prelude.C_Nothing x4 x46 x2004 x3250 x3500)))) x2006 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '|'#) Curry_Prelude.OP_List) x2009 x3250 x3500) x2010 x3250 x3500))))))) x2013 x3250 x3500))))))) (let
                                   x2022 = leftSupply x2021
                                   x2023 = rightSupply x2021
                                    in (seq x2022 (seq x2023 (let
                                        x2020 = leftSupply x2022
                                        x2016 = rightSupply x2022
                                         in (seq x2020 (seq x2016 (let
                                             x2017 = leftSupply x2023
                                             x2024 = rightSupply x2023
                                              in (seq x2017 (seq x2024 (let
                                                  x2018 = leftSupply x2024
                                                  x2019 = rightSupply x2024
                                                   in (seq x2018 (seq x2019 (Curry_Pretty.nd_C_encloseSep (Curry_Pretty.nd_C_empty x2016 x3250 x3500) (Curry_Pretty.nd_C_empty x2017 x3250 x3500) (Curry_Pretty.nd_C_comma x2018 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_statementDoc x2 x4)) x47 x2019 x3250 x3500) x2020 x3250 x3500))))))))))))) x2025 x3250 x3500))))))) x2028 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_expDoc2 x1 x2 x3 x4 x1002 x3000 x3250 x3500) (nd_C_expDoc2 x1 x2 x3 x4 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_expDoc2 x1 x2 x3 x4 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_expDoc2 x1 x2 x3 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_expDoc2_dot_appPar_dot_347 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_expDoc2_dot_appPar_dot_347 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> Curry_Prelude.d_C_id
     (Curry_Prelude.C_Just x3) -> d_OP__case_67 x1 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_snd x3 x3250 x3500) (Curry_Prelude.C_Int 11#) x3250 x3500) x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_expDoc2_dot_appPar_dot_347 x1 x1002 x3250 x3500) (d_OP_expDoc2_dot_appPar_dot_347 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_expDoc2_dot_appPar_dot_347 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_expDoc2_dot_appPar_dot_347 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_expDoc2_dot_appPar_dot_347 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int) -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP_expDoc2_dot_appPar_dot_347 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_Nothing -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.C_Just x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_67 x1 x3 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_snd x3 x3250 x3500) (Curry_Prelude.C_Int 11#) x3250 x3500) x1 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_expDoc2_dot_appPar_dot_347 x1 x1002 x3000 x3250 x3500) (nd_OP_expDoc2_dot_appPar_dot_347 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_expDoc2_dot_appPar_dot_347 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_expDoc2_dot_appPar_dot_347 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_expDoc2_dot_args_dot_347 :: Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr
d_OP_expDoc2_dot_args_dot_347 x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CApply x2 x3) -> Curry_Prelude.d_OP_plus_plus (d_OP_expDoc2_dot_args_dot_347 x2 x3250 x3500) (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) x3250 x3500
     (Curry_AbstractCurry.C_CVar x4) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CLit x5) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CSymbol x6) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CLambda x7 x8) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CLetDecl x9 x10) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CDoExpr x11) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CListComp x12 x13) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CCase x14 x15) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CRecConstr x16) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CRecSelect x17 x18) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CRecUpdate x19 x20) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_expDoc2_dot_args_dot_347 x1002 x3250 x3500) (d_OP_expDoc2_dot_args_dot_347 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_expDoc2_dot_args_dot_347 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_expDoc2_dot_args_dot_347 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_expDoc2_dot_name_dot_347 :: Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_AbstractCurry.C_CExpr
d_OP_expDoc2_dot_name_dot_347 x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CApply x2 x3) -> d_OP_expDoc2_dot_name_dot_347 x2 x3250 x3500
     (Curry_AbstractCurry.C_CVar x4) -> x1
     (Curry_AbstractCurry.C_CLit x5) -> x1
     (Curry_AbstractCurry.C_CSymbol x6) -> x1
     (Curry_AbstractCurry.C_CLambda x7 x8) -> x1
     (Curry_AbstractCurry.C_CLetDecl x9 x10) -> x1
     (Curry_AbstractCurry.C_CDoExpr x11) -> x1
     (Curry_AbstractCurry.C_CListComp x12 x13) -> x1
     (Curry_AbstractCurry.C_CCase x14 x15) -> x1
     (Curry_AbstractCurry.C_CRecConstr x16) -> x1
     (Curry_AbstractCurry.C_CRecSelect x17 x18) -> x1
     (Curry_AbstractCurry.C_CRecUpdate x19 x20) -> x1
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_expDoc2_dot_name_dot_347 x1002 x3250 x3500) (d_OP_expDoc2_dot_name_dot_347 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_expDoc2_dot_name_dot_347 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_expDoc2_dot_name_dot_347 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_expDoc2_dot___hash_selFP2_hash_lbr :: Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_expDoc2_dot___hash_selFP2_hash_lbr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_expDoc2_dot___hash_selFP2_hash_lbr x1002 x3250 x3500) (d_OP_expDoc2_dot___hash_selFP2_hash_lbr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_expDoc2_dot___hash_selFP2_hash_lbr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_expDoc2_dot___hash_selFP2_hash_lbr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_expDoc2_dot___hash_selFP2_hash_lbr :: Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_expDoc2_dot___hash_selFP2_hash_lbr x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_expDoc2_dot___hash_selFP2_hash_lbr x1002 x3000 x3250 x3500) (nd_OP_expDoc2_dot___hash_selFP2_hash_lbr x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_expDoc2_dot___hash_selFP2_hash_lbr z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_expDoc2_dot___hash_selFP2_hash_lbr x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP_expDoc2_dot___hash_selFP3_hash_rbr :: Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP_expDoc2_dot___hash_selFP3_hash_rbr x1 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP_expDoc2_dot___hash_selFP3_hash_rbr x1002 x3250 x3500) (d_OP_expDoc2_dot___hash_selFP3_hash_rbr x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP_expDoc2_dot___hash_selFP3_hash_rbr z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP_expDoc2_dot___hash_selFP3_hash_rbr x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP_expDoc2_dot___hash_selFP3_hash_rbr :: Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP_expDoc2_dot___hash_selFP3_hash_rbr x1 x3000 x3250 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP_expDoc2_dot___hash_selFP3_hash_rbr x1002 x3000 x3250 x3500) (nd_OP_expDoc2_dot___hash_selFP3_hash_rbr x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP_expDoc2_dot___hash_selFP3_hash_rbr z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP_expDoc2_dot___hash_selFP3_hash_rbr x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_statementDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CStatement -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_statementDoc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CSExpr x4) -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (nd_C_expDoc Curry_Prelude.C_False x1 Curry_Prelude.C_Nothing x2 x4 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_AbstractCurry.C_CSPat x5 x6) -> let
          x2018 = x3000
           in (seq x2018 (let
               x2017 = leftSupply x2018
               x2015 = rightSupply x2018
                in (seq x2017 (seq x2015 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                    x2014 = leftSupply x2015
                    x2016 = rightSupply x2015
                     in (seq x2014 (seq x2016 (let
                         x2011 = leftSupply x2016
                         x2013 = rightSupply x2016
                          in (seq x2011 (seq x2013 (Curry_Prelude.nd_C_apply (let
                              x2010 = leftSupply x2011
                              x2012 = rightSupply x2011
                               in (seq x2010 (seq x2012 (let
                                   x2000 = leftSupply x2012
                                   x2008 = rightSupply x2012
                                    in (seq x2000 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                        x2007 = leftSupply x2008
                                        x2009 = rightSupply x2008
                                         in (seq x2007 (seq x2009 (let
                                             x2004 = leftSupply x2009
                                             x2006 = rightSupply x2009
                                              in (seq x2004 (seq x2006 (Curry_Prelude.nd_C_apply (let
                                                  x2003 = leftSupply x2004
                                                  x2005 = rightSupply x2004
                                                   in (seq x2003 (seq x2005 (let
                                                       x2001 = leftSupply x2005
                                                       x2002 = rightSupply x2005
                                                        in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (nd_C_patternDoc x2 x5 x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '<'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))) (nd_C_expDoc Curry_Prelude.C_False x1 Curry_Prelude.C_Nothing x2 x6 x2013 x3250 x3500) x2014 x3250 x3500))))))) x2017 x3250 x3500)))))
     (Curry_AbstractCurry.C_CSLet x7) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) Curry_Prelude.OP_List))) x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_localDeclsDoc x1 x2 x7 x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CStatement x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_statementDoc x1 x2 x1002 x3000 x3250 x3500) (nd_C_statementDoc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CStatement x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_statementDoc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CStatement x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_statementDoc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CStatement x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_branchDoc :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CBranchExpr -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_branchDoc x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CBranch x4 x5) -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2007 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2007 (seq x2012 (nd_C_def (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2003 = leftSupply x2008
                              x2005 = rightSupply x2008
                               in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                                   x2002 = leftSupply x2003
                                   x2004 = rightSupply x2003
                                    in (seq x2002 (seq x2004 (let
                                        x2000 = leftSupply x2004
                                        x2001 = rightSupply x2004
                                         in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (nd_C_patternDoc x2 x4 x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_arrow x2005 x3250 x3500) x2006 x3250 x3500))))))) Curry_Prelude.OP_List (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2009 = leftSupply x2013
                              x2010 = rightSupply x2013
                               in (seq x2009 (seq x2010 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2009 x3250 x3500) (nd_C_expDoc Curry_Prelude.C_False x1 Curry_Prelude.C_Nothing x2 x5 x2010 x3250 x3500) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_CBranchExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_branchDoc x1 x2 x1002 x3000 x3250 x3500) (nd_C_branchDoc x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CBranchExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_branchDoc x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CBranchExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_branchDoc x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CBranchExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_patternsDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_patternsDoc x1 x3250 x3500 = Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_align x3250 x3500) (Curry_Prelude.d_OP_dot (Curry_Pretty.d_C_fillSep x3250 x3500) (Curry_Prelude.d_C_map (d_C_patternDoc x1)) x3250 x3500) x3250 x3500

nd_C_patternsDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern) Curry_Pretty.C_Doc
nd_C_patternsDoc x1 x3000 x3250 x3500 = let
     x2005 = x3000
      in (seq x2005 (let
          x2004 = leftSupply x2005
          x2006 = rightSupply x2005
           in (seq x2004 (seq x2006 (let
               x2000 = leftSupply x2006
               x2003 = rightSupply x2006
                in (seq x2000 (seq x2003 (Curry_Prelude.nd_OP_dot (Curry_Pretty.nd_C_align x2000 x3250 x3500) (let
                    x2002 = leftSupply x2003
                    x2001 = rightSupply x2003
                     in (seq x2002 (seq x2001 (Curry_Prelude.nd_OP_dot (Curry_Pretty.nd_C_fillSep x2001 x3250 x3500) (wrapNX id (Curry_Prelude.nd_C_map (wrapNX id (nd_C_patternDoc x1)))) x2002 x3250 x3500)))) x2004 x3250 x3500))))))))

d_C_patternDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_C_patternDoc x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CPVar x3) -> d_C_tvarDoc x3 x3250 x3500
     (Curry_AbstractCurry.C_CPLit x4) -> d_C_litDoc x4 x3250 x3500
     (Curry_AbstractCurry.C_CPComb x5 x6) -> let
          x7 = Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_C_lbracket x3250 x3500) (Curry_Pretty.d_C_rbracket x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (Curry_Pretty.d_C_comma x3250 x3500) x3250 x3500)
          x8 = Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_C_lparen x3250 x3500) (Curry_Pretty.d_C_rparen x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (Curry_Pretty.d_C_comma x3250 x3500) x3250 x3500)
          x9 = d_C_toListPattern x2 x3250 x3500
          x10 = d_C_toStringPattern x2 x3250 x3500
          x11 = Curry_Maybe.d_C_fromJust x9 x3250 x3500
          x12 = Curry_Maybe.d_C_fromJust x10 x3250 x3500
           in (d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 (Curry_Maybe.d_C_isJust x10 x3250 x3500) x3250 x3500)
     (Curry_AbstractCurry.C_CPAs x13 x14) -> Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_OP_lt_gt (d_C_tvarDoc x13 x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) x3250 x3500) x3250 x3500) (d_C_patternDoc x1 x14 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.C_CPFuncComb x15 x16) -> d_OP__case_58 x16 x15 x1 (Curry_Prelude.d_C_null x16 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_patternDoc x1 x1002 x3250 x3500) (d_C_patternDoc x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_patternDoc x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_patternDoc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_patternDoc :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CPattern -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_C_patternDoc x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CPVar x3) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_tvarDoc x3 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CPLit x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_C_litDoc x4 x2000 x3250 x3500))
     (Curry_AbstractCurry.C_CPComb x5 x6) -> let
          x2019 = x3000
           in (seq x2019 (let
               x2007 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2007 (seq x2020 (let
                    x2016 = leftSupply x2020
                    x2018 = rightSupply x2020
                     in (seq x2016 (seq x2018 (let
                         x7 = let
                              x2000 = leftSupply x2007
                              x2008 = rightSupply x2007
                               in (seq x2000 (seq x2008 (let
                                   x2001 = leftSupply x2008
                                   x2005 = rightSupply x2008
                                    in (seq x2001 (seq x2005 (wrapNX id (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_lbracket x2000 x3250 x3500) (Curry_Pretty.nd_C_rbracket x2001 x3250 x3500) (let
                                        x2004 = leftSupply x2005
                                        x2006 = rightSupply x2005
                                         in (seq x2004 (seq x2006 (let
                                             x2002 = leftSupply x2006
                                             x2003 = rightSupply x2006
                                              in (seq x2002 (seq x2003 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2002 x3250 x3500) (Curry_Pretty.nd_C_comma x2003 x3250 x3500) x2004 x3250 x3500))))))))))))))
                         x8 = let
                              x2009 = leftSupply x2016
                              x2017 = rightSupply x2016
                               in (seq x2009 (seq x2017 (let
                                   x2010 = leftSupply x2017
                                   x2014 = rightSupply x2017
                                    in (seq x2010 (seq x2014 (wrapNX id (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_lparen x2009 x3250 x3500) (Curry_Pretty.nd_C_rparen x2010 x3250 x3500) (let
                                        x2013 = leftSupply x2014
                                        x2015 = rightSupply x2014
                                         in (seq x2013 (seq x2015 (let
                                             x2011 = leftSupply x2015
                                             x2012 = rightSupply x2015
                                              in (seq x2011 (seq x2012 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2011 x3250 x3500) (Curry_Pretty.nd_C_comma x2012 x3250 x3500) x2013 x3250 x3500))))))))))))))
                         x9 = d_C_toListPattern x2 x3250 x3500
                         x10 = d_C_toStringPattern x2 x3250 x3500
                         x11 = Curry_Maybe.d_C_fromJust x9 x3250 x3500
                         x12 = Curry_Maybe.d_C_fromJust x10 x3250 x3500
                          in (nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 (Curry_Maybe.d_C_isJust x10 x3250 x3500) x2018 x3250 x3500)))))))))
     (Curry_AbstractCurry.C_CPAs x13 x14) -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Pretty.nd_OP_lt_gt (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (nd_C_tvarDoc x13 x2000 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) x2001 x3250 x3500) x2002 x3250 x3500))))))) (nd_C_patternDoc x1 x14 x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_AbstractCurry.C_CPFuncComb x15 x16) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_58 x16 x15 x1 (Curry_Prelude.d_C_null x16 x3250 x3500) x2000 x3250 x3500))
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_patternDoc x1 x1002 x3000 x3250 x3500) (nd_C_patternDoc x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_patternDoc x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_patternDoc x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toListPattern :: Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_C_toListPattern x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_55 x3 x2 x3250 x3500
     (Curry_AbstractCurry.C_CPVar x47) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPLit x48) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPAs x49 x50) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPFuncComb x51 x52) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPLazy x53) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPRecord x54 x55) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toListPattern x1002 x3250 x3500) (d_C_toListPattern x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toListPattern z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toListPattern x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_toStringPattern :: Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_toStringPattern x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CPComb x2 x3) -> d_OP__case_28 x3 x2 x3250 x3500
     (Curry_AbstractCurry.C_CPVar x61) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPLit x62) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPAs x63 x64) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPFuncComb x65 x66) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPLazy x67) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPRecord x68 x69) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_toStringPattern x1002 x3250 x3500) (d_C_toStringPattern x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_toStringPattern z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_toStringPattern x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_trCurryProg :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> t0) -> Curry_AbstractCurry.C_CurryProg -> Cover -> ConstStore -> t0
d_C_trCurryProg x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x4 x3250 x3500) x5 x3250 x3500) x6 x3250 x3500) x7 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_trCurryProg x1 x1002 x3250 x3500) (d_C_trCurryProg x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_trCurryProg x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_trCurryProg x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_trCurryProg :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeDecl) (Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CFuncDecl) (Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl) t0)))) -> Curry_AbstractCurry.C_CurryProg -> IDSupply -> Cover -> ConstStore -> t0
nd_C_trCurryProg x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))) x6 x2005 x3250 x3500)))) x7 x2007 x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CurryProg x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_trCurryProg x1 x1002 x3000 x3250 x3500) (nd_C_trCurryProg x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CurryProg x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_trCurryProg x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CurryProg x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_trCurryProg x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CurryProg x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_trCType :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> t0) -> (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility -> Cover -> ConstStore -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> t0) -> Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> t0
d_C_trCType x1 x2 x3 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CType x4 x5 x6 x7) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x4 x3250 x3500) x5 x3250 x3500) x6 x3250 x3500) x7 x3250 x3500
     (Curry_AbstractCurry.C_CTypeSyn x8 x9 x10 x11) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x2 x8 x3250 x3500) x9 x3250 x3500) x10 x3250 x3500) x11 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_trCType x1 x2 x1002 x3250 x3500) (d_C_trCType x1 x2 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_trCType x1 x2 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_trCType x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_trCType :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func Curry_AbstractCurry.C_CVisibility (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl) t0))) -> Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func Curry_AbstractCurry.C_CVisibility (Func (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char))) (Func Curry_AbstractCurry.C_CTypeExpr t0))) -> Curry_AbstractCurry.C_CTypeDecl -> IDSupply -> Cover -> ConstStore -> t0
nd_C_trCType x1 x2 x3 x3000 x3250 x3500 = case x3 of
     (Curry_AbstractCurry.C_CType x4 x5 x6 x7) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x4 x2000 x3250 x3500) x5 x2001 x3250 x3500)))) x6 x2003 x3250 x3500)))) x7 x2005 x3250 x3500)))))
     (Curry_AbstractCurry.C_CTypeSyn x8 x9 x10 x11) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x2 x8 x2000 x3250 x3500) x9 x2001 x3250 x3500)))) x10 x2003 x3250 x3500)))) x11 x2005 x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CTypeDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_trCType x1 x2 x1002 x3000 x3250 x3500) (nd_C_trCType x1 x2 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_trCType x1 x2 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_trCType x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_typeCVisibility :: Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_C_typeCVisibility x3250 x3500 = d_C_trCType (acceptCs (acceptCs (acceptCs id)) d_OP_typeCVisibility_dot___hash_lambda19) (acceptCs (acceptCs (acceptCs id)) d_OP_typeCVisibility_dot___hash_lambda20)

nd_C_typeCVisibility :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CTypeDecl Curry_AbstractCurry.C_CVisibility
nd_C_typeCVisibility x3000 x3250 x3500 = wrapNX id (nd_C_trCType (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_typeCVisibility_dot___hash_lambda19)) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_typeCVisibility_dot___hash_lambda20)))

d_OP_typeCVisibility_dot___hash_lambda19 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_OP_typeCVisibility_dot___hash_lambda19 x1 x2 x3 x4 x3250 x3500 = x2

d_OP_typeCVisibility_dot___hash_lambda20 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_OP_typeCVisibility_dot___hash_lambda20 x1 x2 x3 x4 x3250 x3500 = x2

d_C_typeName :: Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_typeName x3250 x3500 = d_C_trCType (acceptCs (acceptCs (acceptCs id)) d_OP_typeName_dot___hash_lambda21) (acceptCs (acceptCs (acceptCs id)) d_OP_typeName_dot___hash_lambda22)

nd_C_typeName :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CTypeDecl (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_typeName x3000 x3250 x3500 = wrapNX id (nd_C_trCType (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_typeName_dot___hash_lambda21)) (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_typeName_dot___hash_lambda22)))

d_OP_typeName_dot___hash_lambda21 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_typeName_dot___hash_lambda21 x1 x2 x3 x4 x3250 x3500 = x1

d_OP_typeName_dot___hash_lambda22 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_typeName_dot___hash_lambda22 x1 x2 x3 x4 x3250 x3500 = x1

d_C_trCFunc :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility -> Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_AbstractCurry.C_CRules -> Cover -> ConstStore -> t0) -> Curry_AbstractCurry.C_CFuncDecl -> Cover -> ConstStore -> t0
d_C_trCFunc x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CFunc x3 x4 x5 x6 x7) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x4 x3250 x3500) x5 x3250 x3500) x6 x3250 x3500) x7 x3250 x3500
     (Curry_AbstractCurry.C_CmtFunc x8 x9 x10 x11 x12 x13) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x9 x3250 x3500) x10 x3250 x3500) x11 x3250 x3500) x12 x3250 x3500) x13 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_trCFunc x1 x1002 x3250 x3500) (d_C_trCFunc x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_trCFunc x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_trCFunc x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_trCFunc :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func Curry_Prelude.C_Int (Func Curry_AbstractCurry.C_CVisibility (Func Curry_AbstractCurry.C_CTypeExpr (Func Curry_AbstractCurry.C_CRules t0)))) -> Curry_AbstractCurry.C_CFuncDecl -> IDSupply -> Cover -> ConstStore -> t0
nd_C_trCFunc x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CFunc x3 x4 x5 x6 x7) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))) x6 x2005 x3250 x3500)))) x7 x2007 x3250 x3500)))))
     (Curry_AbstractCurry.C_CmtFunc x8 x9 x10 x11 x12 x13) -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2006 = rightSupply x2008
                in (seq x2007 (seq x2006 (Curry_Prelude.nd_C_apply (let
                    x2005 = leftSupply x2006
                    x2004 = rightSupply x2006
                     in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                         x2003 = leftSupply x2004
                         x2002 = rightSupply x2004
                          in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x9 x2000 x3250 x3500) x10 x2001 x3250 x3500)))) x11 x2003 x3250 x3500)))) x12 x2005 x3250 x3500)))) x13 x2007 x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CFuncDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_trCFunc x1 x1002 x3000 x3250 x3500) (nd_C_trCFunc x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CFuncDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_trCFunc x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CFuncDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_trCFunc x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CFuncDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_funcName :: Cover -> ConstStore -> Curry_AbstractCurry.C_CFuncDecl -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_C_funcName x3250 x3500 = d_C_trCFunc (acceptCs (acceptCs (acceptCs (acceptCs id))) d_OP_funcName_dot___hash_lambda23)

nd_C_funcName :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CFuncDecl (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
nd_C_funcName x3000 x3250 x3500 = wrapNX id (nd_C_trCFunc (wrapDX (wrapDX (wrapDX (wrapDX (wrapDX id)))) (acceptCs (acceptCs (acceptCs (acceptCs id))) d_OP_funcName_dot___hash_lambda23)))

d_OP_funcName_dot___hash_lambda23 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CVisibility -> Curry_AbstractCurry.C_CTypeExpr -> Curry_AbstractCurry.C_CRules -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP_funcName_dot___hash_lambda23 x1 x2 x3 x4 x5 x3250 x3500 = x1

d_C_funcCVisibility :: Cover -> ConstStore -> Curry_AbstractCurry.C_CFuncDecl -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_C_funcCVisibility x3250 x3500 = d_C_trCFunc (acceptCs (acceptCs (acceptCs (acceptCs id))) d_OP_funcCVisibility_dot___hash_lambda24)

nd_C_funcCVisibility :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CFuncDecl Curry_AbstractCurry.C_CVisibility
nd_C_funcCVisibility x3000 x3250 x3500 = wrapNX id (nd_C_trCFunc (wrapDX (wrapDX (wrapDX (wrapDX (wrapDX id)))) (acceptCs (acceptCs (acceptCs (acceptCs id))) d_OP_funcCVisibility_dot___hash_lambda24)))

d_OP_funcCVisibility_dot___hash_lambda24 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CVisibility -> Curry_AbstractCurry.C_CTypeExpr -> Curry_AbstractCurry.C_CRules -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_OP_funcCVisibility_dot___hash_lambda24 x1 x2 x3 x4 x5 x3250 x3500 = x3

d_C_trCCons :: Curry_Prelude.Curry t0 => (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Int -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> t0) -> Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> t0
d_C_trCCons x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCons x3 x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply x1 x3 x3250 x3500) x4 x3250 x3500) x5 x3250 x3500) x6 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_trCCons x1 x1002 x3250 x3500) (d_C_trCCons x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_trCCons x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_trCCons x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_C_trCCons :: Curry_Prelude.Curry t0 => Func (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Func Curry_Prelude.C_Int (Func Curry_AbstractCurry.C_CVisibility (Func (Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr) t0))) -> Curry_AbstractCurry.C_CConsDecl -> IDSupply -> Cover -> ConstStore -> t0
nd_C_trCCons x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_CCons x3 x4 x5 x6) -> let
          x2006 = x3000
           in (seq x2006 (let
               x2005 = leftSupply x2006
               x2004 = rightSupply x2006
                in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_apply (let
                    x2003 = leftSupply x2004
                    x2002 = rightSupply x2004
                     in (seq x2003 (seq x2002 (Curry_Prelude.nd_C_apply (let
                         x2001 = leftSupply x2002
                         x2000 = rightSupply x2002
                          in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Prelude.nd_C_apply x1 x3 x2000 x3250 x3500) x4 x2001 x3250 x3500)))) x5 x2003 x3250 x3500)))) x6 x2005 x3250 x3500)))))
     (Curry_AbstractCurry.Choice_C_CConsDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_C_trCCons x1 x1002 x3000 x3250 x3500) (nd_C_trCCons x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CConsDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_C_trCCons x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CConsDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_C_trCCons x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CConsDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_consCVisibility :: Cover -> ConstStore -> Curry_AbstractCurry.C_CConsDecl -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_C_consCVisibility x3250 x3500 = d_C_trCCons (acceptCs (acceptCs (acceptCs id)) d_OP_consCVisibility_dot___hash_lambda25)

nd_C_consCVisibility :: IDSupply -> Cover -> ConstStore -> Func Curry_AbstractCurry.C_CConsDecl Curry_AbstractCurry.C_CVisibility
nd_C_consCVisibility x3000 x3250 x3500 = wrapNX id (nd_C_trCCons (wrapDX (wrapDX (wrapDX (wrapDX id))) (acceptCs (acceptCs (acceptCs id)) d_OP_consCVisibility_dot___hash_lambda25)))

d_OP_consCVisibility_dot___hash_lambda25 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CVisibility -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_AbstractCurry.C_CVisibility
d_OP_consCVisibility_dot___hash_lambda25 x1 x2 x3 x4 x3250 x3500 = x3

d_C_argTypes :: Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr
d_C_argTypes x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CTCons x3 x4) -> Curry_Prelude.OP_List
     (Curry_AbstractCurry.C_CFuncType x5 x6) -> Curry_Prelude.OP_Cons x5 (d_C_argTypes x6 x3250 x3500)
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_argTypes x1002 x3250 x3500) (d_C_argTypes x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_argTypes z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_argTypes x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_C_resultType :: Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_AbstractCurry.C_CTypeExpr
d_C_resultType x1 x3250 x3500 = case x1 of
     (Curry_AbstractCurry.C_CTVar x2) -> Curry_AbstractCurry.C_CTVar x2
     (Curry_AbstractCurry.C_CTCons x3 x4) -> Curry_AbstractCurry.C_CTCons x3 x4
     (Curry_AbstractCurry.C_CFuncType x5 x6) -> d_C_resultType x6 x3250 x3500
     (Curry_AbstractCurry.Choice_C_CTypeExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_C_resultType x1002 x3250 x3500) (d_C_resultType x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CTypeExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_C_resultType z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CTypeExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_C_resultType x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CTypeExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_28 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_28 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_27 x5 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_28 x3 x1002 x3250 x3500) (d_OP__case_28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_27 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_27 x5 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = x6
           in (d_OP__case_26 x8 x7 x5 x3 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_27 x5 x3 x1002 x3250 x3500) (d_OP__case_27 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_27 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_27 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_26 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_26 x8 x7 x5 x3 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_25 x5 x3 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_26 x8 x7 x5 x3 x1002 x3250 x3500) (d_OP__case_26 x8 x7 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_26 x8 x7 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_26 x8 x7 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_25 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_25 x5 x3 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_24 x11 x10 x5 x3 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_25 x5 x3 x1002 x3250 x3500) (d_OP__case_25 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_25 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_25 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_24 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_24 x11 x10 x5 x3 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_23 x5 x3 x10 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_24 x11 x10 x5 x3 x1002 x3250 x3500) (d_OP__case_24 x11 x10 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_24 x11 x10 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_24 x11 x10 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_23 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_23 x5 x3 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_22 x14 x13 x5 x3 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_23 x5 x3 x1002 x3250 x3500) (d_OP__case_23 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_23 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_23 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_22 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_22 x14 x13 x5 x3 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_21 x5 x3 x13 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_22 x14 x13 x5 x3 x1002 x3250 x3500) (d_OP__case_22 x14 x13 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_22 x14 x13 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_22 x14 x13 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_21 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_21 x5 x3 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x17 = x15
           in (d_OP__case_20 x17 x16 x5 x3 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_21 x5 x3 x1002 x3250 x3500) (d_OP__case_21 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_21 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_21 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_20 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_20 x17 x16 x5 x3 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> d_OP__case_19 x5 x3 x16 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_20 x17 x16 x5 x3 x1002 x3250 x3500) (d_OP__case_20 x17 x16 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_20 x17 x16 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_20 x17 x16 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_19 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_19 x5 x3 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_18 x20 x19 x5 x3 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_19 x5 x3 x1002 x3250 x3500) (d_OP__case_19 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_19 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_19 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_18 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_18 x20 x19 x5 x3 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_17 x5 x3 x19 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_18 x20 x19 x5 x3 x1002 x3250 x3500) (d_OP__case_18 x20 x19 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_18 x20 x19 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_18 x20 x19 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_17 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_17 x5 x3 x19 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_16 x23 x22 x5 x3 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_17 x5 x3 x1002 x3250 x3500) (d_OP__case_17 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_17 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_17 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_16 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_16 x23 x22 x5 x3 x24 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_15 x5 x3 x22 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_16 x23 x22 x5 x3 x1002 x3250 x3500) (d_OP__case_16 x23 x22 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_16 x23 x22 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_16 x23 x22 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_15 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_15 x5 x3 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_14 x26 x25 x5 x3 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_15 x5 x3 x1002 x3250 x3500) (d_OP__case_15 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_15 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_15 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_14 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_14 x26 x25 x5 x3 x27 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_13 x5 x3 x25 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_14 x26 x25 x5 x3 x1002 x3250 x3500) (d_OP__case_14 x26 x25 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_14 x26 x25 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_14 x26 x25 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_13 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_13 x5 x3 x25 x3250 x3500 = case x25 of
     Curry_Prelude.OP_List -> d_OP__case_12 x3 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x59 x60) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_13 x5 x3 x1002 x3250 x3500) (d_OP__case_13 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_13 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_13 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_12 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_12 x3 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_11 x29 x28 x3 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_12 x3 x1002 x3250 x3500) (d_OP__case_12 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_12 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_12 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_11 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_11 x29 x28 x3 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_10 x3 x28 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_6 x29 x28 x3 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char ':'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_11 x29 x28 x3 x1002 x3250 x3500) (d_OP__case_11 x29 x28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_11 x29 x28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_11 x29 x28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_6 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_6 x29 x28 x3 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_5 x3 x28 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_6 x29 x28 x3 x1002 x3250 x3500) (d_OP__case_6 x29 x28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_6 x29 x28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_6 x29 x28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_5 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_5 x3 x28 x3250 x3500 = case x28 of
     Curry_Prelude.OP_List -> d_OP__case_4 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x57 x58) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_5 x3 x1002 x3250 x3500) (d_OP__case_5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_4 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x37 x38) -> d_OP__case_3 x38 x37 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_4 x1002 x3250 x3500) (d_OP__case_4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_3 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_3 x38 x37 x3250 x3500 = case x37 of
     (Curry_AbstractCurry.C_CPLit x39) -> d_OP__case_2 x38 x39 x3250 x3500
     (Curry_AbstractCurry.C_CPVar x47) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPComb x48 x49) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPAs x50 x51) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPFuncComb x52 x53) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPLazy x54) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CPRecord x55 x56) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CPattern x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_3 x38 x1002 x3250 x3500) (d_OP__case_3 x38 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CPattern x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_3 x38 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CPattern x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_3 x38 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CPattern x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_2 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CLiteral -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_2 x38 x39 x3250 x3500 = case x39 of
     (Curry_AbstractCurry.C_CCharc x40) -> d_OP__case_1 x40 x38 x3250 x3500
     (Curry_AbstractCurry.C_CIntc x45) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CFloatc x46) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_2 x38 x1002 x3250 x3500) (d_OP__case_2 x38 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_2 x38 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_2 x38 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_1 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_1 x40 x38 x3250 x3500 = case x38 of
     (Curry_Prelude.OP_Cons x41 x42) -> d_OP__case_0 x40 x41 x42 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_1 x40 x1002 x3250 x3500) (d_OP__case_1 x40 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_1 x40 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_1 x40 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_0 :: Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_0 x40 x41 x42 x3250 x3500 = case x42 of
     Curry_Prelude.OP_List -> Curry_Maybe.d_OP_gt_gt_minus (d_C_toStringPattern x41 x3250 x3500) (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) (Curry_Prelude.d_OP_plus_plus (d_C_quoteChar x40 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x43 x44) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_0 x40 x41 x1002 x3250 x3500) (d_OP__case_0 x40 x41 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_0 x40 x41 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_0 x40 x41 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_10 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_10 x3 x28 x3250 x3500 = case x28 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x32 = x30
           in (d_OP__case_9 x32 x31 x3 (Curry_Prelude.d_OP_eq_eq x32 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_10 x3 x1002 x3250 x3500) (d_OP__case_10 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_10 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_10 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_9 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_9 x32 x31 x3 x33 x3250 x3500 = case x33 of
     Curry_Prelude.C_True -> d_OP__case_8 x3 x31 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_9 x32 x31 x3 x1002 x3250 x3500) (d_OP__case_9 x32 x31 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_9 x32 x31 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_9 x32 x31 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_8 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_8 x3 x31 x3250 x3500 = case x31 of
     Curry_Prelude.OP_List -> d_OP__case_7 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x35 x36) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_8 x3 x1002 x3250 x3500) (d_OP__case_8 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_8 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_8 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_7 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_7 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x33 x34) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_7 x1002 x3250 x3500) (d_OP__case_7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_55 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_55 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_54 x5 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_55 x3 x1002 x3250 x3500) (d_OP__case_55 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_55 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_55 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_54 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_54 x5 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = x6
           in (d_OP__case_53 x8 x7 x5 x3 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_54 x5 x3 x1002 x3250 x3500) (d_OP__case_54 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_54 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_54 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_53 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_53 x8 x7 x5 x3 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_52 x5 x3 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_53 x8 x7 x5 x3 x1002 x3250 x3500) (d_OP__case_53 x8 x7 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_53 x8 x7 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_53 x8 x7 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_52 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_52 x5 x3 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_51 x11 x10 x5 x3 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_52 x5 x3 x1002 x3250 x3500) (d_OP__case_52 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_52 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_52 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_51 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_51 x11 x10 x5 x3 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_50 x5 x3 x10 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_51 x11 x10 x5 x3 x1002 x3250 x3500) (d_OP__case_51 x11 x10 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_51 x11 x10 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_51 x11 x10 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_50 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_50 x5 x3 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_49 x14 x13 x5 x3 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_50 x5 x3 x1002 x3250 x3500) (d_OP__case_50 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_50 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_50 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_49 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_49 x14 x13 x5 x3 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_48 x5 x3 x13 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_49 x14 x13 x5 x3 x1002 x3250 x3500) (d_OP__case_49 x14 x13 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_49 x14 x13 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_49 x14 x13 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_48 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_48 x5 x3 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x17 = x15
           in (d_OP__case_47 x17 x16 x5 x3 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_48 x5 x3 x1002 x3250 x3500) (d_OP__case_48 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_48 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_48 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_47 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_47 x17 x16 x5 x3 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> d_OP__case_46 x5 x3 x16 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_47 x17 x16 x5 x3 x1002 x3250 x3500) (d_OP__case_47 x17 x16 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_47 x17 x16 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_47 x17 x16 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_46 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_46 x5 x3 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_45 x20 x19 x5 x3 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_46 x5 x3 x1002 x3250 x3500) (d_OP__case_46 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_46 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_46 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_45 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_45 x20 x19 x5 x3 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_44 x5 x3 x19 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_45 x20 x19 x5 x3 x1002 x3250 x3500) (d_OP__case_45 x20 x19 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_45 x20 x19 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_45 x20 x19 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_44 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_44 x5 x3 x19 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_43 x23 x22 x5 x3 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_44 x5 x3 x1002 x3250 x3500) (d_OP__case_44 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_44 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_44 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_43 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_43 x23 x22 x5 x3 x24 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_42 x5 x3 x22 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_43 x23 x22 x5 x3 x1002 x3250 x3500) (d_OP__case_43 x23 x22 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_43 x23 x22 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_43 x23 x22 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_42 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_42 x5 x3 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_41 x26 x25 x5 x3 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_42 x5 x3 x1002 x3250 x3500) (d_OP__case_42 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_42 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_42 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_41 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_41 x26 x25 x5 x3 x27 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_40 x5 x3 x25 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_41 x26 x25 x5 x3 x1002 x3250 x3500) (d_OP__case_41 x26 x25 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_41 x26 x25 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_41 x26 x25 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_40 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_40 x5 x3 x25 x3250 x3500 = case x25 of
     Curry_Prelude.OP_List -> d_OP__case_39 x3 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x45 x46) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_40 x5 x3 x1002 x3250 x3500) (d_OP__case_40 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_40 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_40 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_39 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_39 x3 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_38 x29 x28 x3 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_39 x3 x1002 x3250 x3500) (d_OP__case_39 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_39 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_39 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_38 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_38 x29 x28 x3 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_37 x3 x28 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_33 x29 x28 x3 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char ':'#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_38 x29 x28 x3 x1002 x3250 x3500) (d_OP__case_38 x29 x28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_38 x29 x28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_38 x29 x28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_33 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_33 x29 x28 x3 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_32 x3 x28 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_33 x29 x28 x3 x1002 x3250 x3500) (d_OP__case_33 x29 x28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_33 x29 x28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_33 x29 x28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_32 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_32 x3 x28 x3250 x3500 = case x28 of
     Curry_Prelude.OP_List -> d_OP__case_31 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x43 x44) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_32 x3 x1002 x3250 x3500) (d_OP__case_32 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_32 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_32 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_31 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_31 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x37 x38) -> d_OP__case_30 x37 x38 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_31 x1002 x3250 x3500) (d_OP__case_31 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_31 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_31 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_30 :: Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_30 x37 x38 x3250 x3500 = case x38 of
     (Curry_Prelude.OP_Cons x39 x40) -> d_OP__case_29 x37 x39 x40 x3250 x3500
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_30 x37 x1002 x3250 x3500) (d_OP__case_30 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_30 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_30 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_29 :: Curry_AbstractCurry.C_CPattern -> Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_29 x37 x39 x40 x3250 x3500 = case x40 of
     Curry_Prelude.OP_List -> Curry_Maybe.d_OP_gt_gt_minus (d_C_toListPattern x39 x3250 x3500) (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) (acceptCs id (Curry_Prelude.OP_Cons x37)) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x41 x42) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_29 x37 x39 x1002 x3250 x3500) (d_OP__case_29 x37 x39 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_29 x37 x39 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_29 x37 x39 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_37 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_37 x3 x28 x3250 x3500 = case x28 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x32 = x30
           in (d_OP__case_36 x32 x31 x3 (Curry_Prelude.d_OP_eq_eq x32 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_37 x3 x1002 x3250 x3500) (d_OP__case_37 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_37 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_37 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_36 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_36 x32 x31 x3 x33 x3250 x3500 = case x33 of
     Curry_Prelude.C_True -> d_OP__case_35 x3 x31 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_36 x32 x31 x3 x1002 x3250 x3500) (d_OP__case_36 x32 x31 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_36 x32 x31 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_36 x32 x31 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_35 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_35 x3 x31 x3250 x3500 = case x31 of
     Curry_Prelude.OP_List -> d_OP__case_34 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x35 x36) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_35 x3 x1002 x3250 x3500) (d_OP__case_35 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_35 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_35 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_34 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern)
d_OP__case_34 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x33 x34) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_34 x1002 x3250 x3500) (d_OP__case_34 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_34 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_34 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_58 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_58 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> d_C_qname x1 x15 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_57 x16 x15 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isInfixName x15 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x16 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_58 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_58 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_58 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_58 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_58 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_58 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_qname x1 x15 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_57 x16 x15 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isInfixName x15 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x16 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_58 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_58 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_58 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_58 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_57 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_57 x16 x15 x1 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_OP_lt_gt (d_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x16 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.d_C_snd x15 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x16 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_56 x16 x1 x15 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_57 x16 x15 x1 x1002 x3250 x3500) (d_OP__case_57 x16 x15 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_57 x16 x15 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_57 x16 x15 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_57 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_57 x16 x15 x1 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2000 = leftSupply x2012
                    x2008 = rightSupply x2012
                     in (seq x2000 (seq x2008 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (let
                         x2007 = leftSupply x2008
                         x2009 = rightSupply x2008
                          in (seq x2007 (seq x2009 (let
                              x2004 = leftSupply x2009
                              x2006 = rightSupply x2009
                               in (seq x2004 (seq x2006 (Curry_Pretty.nd_OP_lt_gt (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Pretty.nd_OP_lt_gt (nd_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x16 (Curry_Prelude.C_Int 0#) x3250 x3500) x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.d_C_snd x15 x3250 x3500) x2002 x3250 x3500) x2003 x3250 x3500))))))) (nd_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x16 (Curry_Prelude.C_Int 1#) x3250 x3500) x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_56 x16 x1 x15 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_57 x16 x15 x1 x1002 x3000 x3250 x3500) (nd_OP__case_57 x16 x15 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_57 x16 x15 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_57 x16 x15 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_56 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_56 x16 x1 x15 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (d_C_qname x1 x15 x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_hsep x3250 x3500) (Curry_Prelude.d_C_map (d_C_patternDoc x1) x16 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_56 x16 x1 x15 x1002 x3250 x3500) (d_OP__case_56 x16 x1 x15 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_56 x16 x1 x15 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_56 x16 x1 x15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_56 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_56 x16 x1 x15 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2000 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2000 (seq x2012 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2004 = leftSupply x2013
                              x2009 = rightSupply x2013
                               in (seq x2004 (seq x2009 (Curry_Prelude.nd_C_apply (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (nd_C_qname x1 x15 x2002 x3250 x3500) x2003 x3250 x3500))))))) (let
                                   x2008 = leftSupply x2009
                                   x2010 = rightSupply x2009
                                    in (seq x2008 (seq x2010 (let
                                        x2006 = leftSupply x2010
                                        x2007 = rightSupply x2010
                                         in (seq x2006 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_hsep x2006 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_patternDoc x1)) x16 x2007 x3250 x3500) x2008 x3250 x3500))))))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_56 x16 x1 x15 x1002 x3000 x3250 x3500) (nd_OP__case_56 x16 x1 x15 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_56 x16 x1 x15 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_56 x16 x1 x15 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_65 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> d_OP__case_64 x12 (Curry_Prelude.d_C_null x12 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 (Curry_Maybe.d_C_isJust x9 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1002 x3250 x3500) (d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_65 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x13 x3000 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_64 x12 (Curry_Prelude.d_C_null x12 x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 (Curry_Maybe.d_C_isJust x9 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1002 x3000 x3250 x3500) (nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_65 x10 x9 x6 x5 x1 x8 x11 x7 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_63 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply x7 (Curry_Prelude.d_C_map (d_C_patternDoc x1) x11 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_62 x6 x5 x1 x8 (Curry_Prelude.d_C_null x6 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1002 x3250 x3500) (d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_63 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x12 x3000 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x7 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_patternDoc x1)) x11 x2000 x3250 x3500) x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_62 x6 x5 x1 x8 (Curry_Prelude.d_C_null x6 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1002 x3000 x3250 x3500) (nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_63 x9 x6 x5 x1 x8 x11 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_62 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_62 x6 x5 x1 x8 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_C_qname x1 x5 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_61 x5 x6 x1 x8 (d_C_isTupleName x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_62 x6 x5 x1 x8 x1002 x3250 x3500) (d_OP__case_62 x6 x5 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_62 x6 x5 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_62 x6 x5 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_62 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_62 x6 x5 x1 x8 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_qname x1 x5 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_61 x5 x6 x1 x8 (d_C_isTupleName x5 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_62 x6 x5 x1 x8 x1002 x3000 x3250 x3500) (nd_OP__case_62 x6 x5 x1 x8 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_62 x6 x5 x1 x8 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_62 x6 x5 x1 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_61 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> (Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_61 x5 x6 x1 x8 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply x8 (Curry_Prelude.d_C_map (d_C_patternDoc x1) x6 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_60 x6 x5 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isInfixName x5 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x6 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_61 x5 x6 x1 x8 x1002 x3250 x3500) (d_OP__case_61 x5 x6 x1 x8 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_61 x5 x6 x1 x8 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_61 x5 x6 x1 x8 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_61 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Func (Curry_Prelude.OP_List Curry_Pretty.C_Doc) Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_61 x5 x6 x1 x8 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply x8 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_patternDoc x1)) x6 x2000 x3250 x3500) x2001 x3250 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_60 x6 x5 x1 (Curry_Prelude.d_OP_ampersand_ampersand (d_C_isInfixName x5 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x6 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_61 x5 x6 x1 x8 x1002 x3000 x3250 x3500) (nd_OP__case_61 x5 x6 x1 x8 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_61 x5 x6 x1 x8 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_61 x5 x6 x1 x8 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_60 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_60 x6 x5 x1 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (d_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.d_C_snd x5 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_59 x6 x1 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_60 x6 x5 x1 x1002 x3250 x3500) (d_OP__case_60 x6 x5 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_60 x6 x5 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_60 x6 x5 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_60 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_60 x6 x5 x1 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2019 = x3000
           in (seq x2019 (let
               x2018 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2018 (seq x2020 (let
                    x2000 = leftSupply x2020
                    x2016 = rightSupply x2020
                     in (seq x2000 (seq x2016 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (let
                         x2015 = leftSupply x2016
                         x2017 = rightSupply x2016
                          in (seq x2015 (seq x2017 (let
                              x2012 = leftSupply x2017
                              x2014 = rightSupply x2017
                               in (seq x2012 (seq x2014 (Curry_Prelude.nd_C_apply (let
                                   x2011 = leftSupply x2012
                                   x2013 = rightSupply x2012
                                    in (seq x2011 (seq x2013 (let
                                        x2001 = leftSupply x2013
                                        x2009 = rightSupply x2013
                                         in (seq x2001 (seq x2009 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                             x2008 = leftSupply x2009
                                             x2010 = rightSupply x2009
                                              in (seq x2008 (seq x2010 (let
                                                  x2005 = leftSupply x2010
                                                  x2007 = rightSupply x2010
                                                   in (seq x2005 (seq x2007 (Curry_Prelude.nd_C_apply (let
                                                       x2004 = leftSupply x2005
                                                       x2006 = rightSupply x2005
                                                        in (seq x2004 (seq x2006 (let
                                                            x2002 = leftSupply x2006
                                                            x2003 = rightSupply x2006
                                                             in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (nd_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 0#) x3250 x3500) x2003 x3250 x3500) x2004 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.d_C_snd x5 x3250 x3500) x2007 x3250 x3500) x2008 x3250 x3500))))))) x2011 x3250 x3500))))))) (nd_C_patternDoc x1 (Curry_Prelude.d_OP_bang_bang x6 (Curry_Prelude.C_Int 1#) x3250 x3500) x2014 x3250 x3500) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_59 x6 x1 x5 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_60 x6 x5 x1 x1002 x3000 x3250 x3500) (nd_OP__case_60 x6 x5 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_60 x6 x5 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_60 x6 x5 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_59 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_59 x6 x1 x5 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (d_C_qname x1 x5 x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_group (Curry_Pretty.d_C_hang (Curry_Prelude.C_Int 0#) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_vsep x3250 x3500) (Curry_Prelude.d_C_map (d_C_patternDoc x1) x6 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_59 x6 x1 x5 x1002 x3250 x3500) (d_OP__case_59 x6 x1 x5 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_59 x6 x1 x5 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_59 x6 x1 x5 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_59 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_59 x6 x1 x5 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2019 = x3000
           in (seq x2019 (let
               x2018 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2018 (seq x2020 (let
                    x2000 = leftSupply x2020
                    x2016 = rightSupply x2020
                     in (seq x2000 (seq x2016 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (let
                         x2015 = leftSupply x2016
                         x2017 = rightSupply x2016
                          in (seq x2015 (seq x2017 (let
                              x2004 = leftSupply x2017
                              x2014 = rightSupply x2017
                               in (seq x2004 (seq x2014 (Curry_Prelude.nd_C_apply (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (nd_C_qname x1 x5 x2002 x3250 x3500) x2003 x3250 x3500))))))) (let
                                   x2013 = leftSupply x2014
                                   x2012 = rightSupply x2014
                                    in (seq x2013 (seq x2012 (Curry_Pretty.nd_C_group (let
                                        x2011 = leftSupply x2012
                                        x2009 = rightSupply x2012
                                         in (seq x2011 (seq x2009 (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 0#) (let
                                             x2008 = leftSupply x2009
                                             x2010 = rightSupply x2009
                                              in (seq x2008 (seq x2010 (let
                                                  x2006 = leftSupply x2010
                                                  x2007 = rightSupply x2010
                                                   in (seq x2006 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vsep x2006 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_patternDoc x1)) x6 x2007 x3250 x3500) x2008 x3250 x3500))))))) x2011 x3250 x3500)))) x2013 x3250 x3500)))) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_59 x6 x1 x5 x1002 x3000 x3250 x3500) (nd_OP__case_59 x6 x1 x5 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_59 x6 x1 x5 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_59 x6 x1 x5 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_64 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_64 x12 x13 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_dquotes x3250 x3500) (Curry_Pretty.d_C_text x12 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_64 x12 x1002 x3250 x3500) (d_OP__case_64 x12 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_64 x12 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_64 x12 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_64 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_64 x12 x13 x3000 x3250 x3500 = case x13 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_dquotes x2000 x3250 x3500) (Curry_Pretty.nd_C_text x12 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_64 x12 x1002 x3000 x3250 x3500) (nd_OP__case_64 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_64 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_64 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_67 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_67 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_parens x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_66 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_67 x1 x3 x1002 x3250 x3500) (d_OP__case_67 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_67 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_67 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_67 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Bool -> Curry_Prelude.OP_Tuple2 t0 Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_67 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_parens x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_66 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_67 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_67 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_67 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_67 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_66 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_66 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_id
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_66 x1002 x3250 x3500) (d_OP__case_66 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_66 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_66 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_66 :: Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_66 x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> wrapDX id Curry_Prelude.d_C_id
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_66 x1002 x3000 x3250 x3500) (nd_OP__case_66 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_66 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_66 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_68 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int
d_OP__case_68 x2 x39 x45 x3250 x3500 = case x45 of
     (Curry_Prelude.C_Just x44) -> x44
     Curry_Prelude.C_Nothing -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CInfixlOp (Curry_Prelude.C_Int 9#)
     (Curry_Prelude.Choice_C_Maybe x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_68 x2 x39 x1002 x3250 x3500) (d_OP__case_68 x2 x39 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Maybe x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_68 x2 x39 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Maybe x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_68 x2 x39 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Maybe x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_69 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc
d_OP__case_69 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Curry_Pretty.d_C_lparen x3250 x3500) (Curry_Pretty.d_C_rparen x3250 x3500)
     Curry_Prelude.C_False -> Curry_Prelude.OP_Tuple2 (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_69 x3 x1002 x3250 x3500) (d_OP__case_69 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_69 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_69 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_69 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_Tuple2 Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_69 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Pretty.nd_C_lparen x2000 x3250 x3500) (Curry_Pretty.nd_C_rparen x2001 x3250 x3500))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2000 = leftSupply x2002
               x2001 = rightSupply x2002
                in (seq x2000 (seq x2001 (Curry_Prelude.OP_Tuple2 (Curry_Pretty.nd_C_empty x2000 x3250 x3500) (Curry_Pretty.nd_C_empty x2001 x3250 x3500))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_69 x3 x1002 x3000 x3250 x3500) (nd_OP__case_69 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_69 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_69 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_70 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char))
d_OP__case_70 x16 x15 x39 x3250 x3500 = case x39 of
     (Curry_AbstractCurry.C_CSymbol x20) -> Curry_Prelude.C_Just x20
     (Curry_AbstractCurry.C_CVar x21) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x22) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CApply x23 x24) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x25 x26) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x27 x28) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x29) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x30 x31) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x32 x33) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x34) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x35 x36) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x37 x38) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_70 x16 x15 x1002 x3250 x3500) (d_OP__case_70 x16 x15 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_70 x16 x15 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_70 x16 x15 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_76 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x44 x3000 x3250 x3500 = case x44 of
     Curry_Prelude.C_True -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_tupled x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2001 x3250 x3500) x2 Curry_Prelude.C_Nothing x4)) x17 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapDX id d_C_isInfixName) x19 x2000 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x17 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1002 x3000 x3250 x3500) (nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_76 x19 x17 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_75 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x44 x3000 x3250 x3500 = case x44 of
     Curry_Prelude.C_True -> let
          x2023 = x3000
           in (seq x2023 (let
               x2022 = leftSupply x2023
               x2024 = rightSupply x2023
                in (seq x2022 (seq x2024 (let
                    x2000 = leftSupply x2024
                    x2020 = rightSupply x2024
                     in (seq x2000 (seq x2020 (Curry_Pretty.nd_OP_lt_gt (nd_C_showPrecs (Curry_Prelude.d_C_snd x39 x3250 x3500) (Curry_Prelude.OP_Tuple3 x1 x43 x3) x2000 x3250 x3500) (let
                         x2019 = leftSupply x2020
                         x2021 = rightSupply x2020
                          in (seq x2019 (seq x2021 (let
                              x2001 = leftSupply x2021
                              x2017 = rightSupply x2021
                               in (seq x2001 (seq x2017 (Curry_Prelude.nd_OP_dollar (Curry_Pretty.nd_C_align x2001 x3250 x3500) (let
                                   x2016 = leftSupply x2017
                                   x2018 = rightSupply x2017
                                    in (seq x2016 (seq x2018 (let
                                        x2002 = leftSupply x2018
                                        x2015 = rightSupply x2018
                                         in (seq x2002 (seq x2015 (nd_C_precFillEncloseSep x1 x43 x3 x41 x42 (Curry_Pretty.nd_C_empty x2002 x3250 x3500) (let
                                             x2003 = leftSupply x2015
                                             x2014 = rightSupply x2015
                                              in (seq x2003 (seq x2014 (Curry_Prelude.OP_Cons (nd_C_expDoc Curry_Prelude.C_True x2 (Curry_Prelude.C_Just x43) x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 0#) x3250 x3500) x2003 x3250 x3500) (let
                                                  x2007 = leftSupply x2014
                                                  x2012 = rightSupply x2014
                                                   in (seq x2007 (seq x2012 (Curry_Prelude.OP_Cons (let
                                                       x2006 = leftSupply x2007
                                                       x2008 = rightSupply x2007
                                                        in (seq x2006 (seq x2008 (let
                                                            x2004 = leftSupply x2008
                                                            x2005 = rightSupply x2008
                                                             in (seq x2004 (seq x2005 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2004 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.d_C_snd x39 x3250 x3500) x2005 x3250 x3500) x2006 x3250 x3500))))))) (Curry_Prelude.OP_Cons (let
                                                       x2011 = leftSupply x2012
                                                       x2013 = rightSupply x2012
                                                        in (seq x2011 (seq x2013 (let
                                                            x2009 = leftSupply x2013
                                                            x2010 = rightSupply x2013
                                                             in (seq x2009 (seq x2010 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2009 x3250 x3500) (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just x43) x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 1#) x3250 x3500) x2010 x3250 x3500) x2011 x3250 x3500))))))) Curry_Prelude.OP_List))))))))) x2016 x3250 x3500))))))) x2019 x3250 x3500))))))) x2022 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapDX id d_C_isInfixName) x19 x2000 x3250 x3500) (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_length x17 x3250 x3500) (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1002 x3000 x3250 x3500) (nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_75 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x42 x41 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_74 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x44 x3000 x3250 x3500 = case x44 of
     Curry_Prelude.C_True -> let
          x2029 = x3000
           in (seq x2029 (let
               x2028 = leftSupply x2029
               x2030 = rightSupply x2029
                in (seq x2028 (seq x2030 (let
                    x2000 = leftSupply x2030
                    x2026 = rightSupply x2030
                     in (seq x2000 (seq x2026 (Curry_Prelude.nd_OP_dollar (nd_OP_expDoc2_dot_appPar_dot_347 Curry_Prelude.C_True x3 x2000 x3250 x3500) (let
                         x2025 = leftSupply x2026
                         x2027 = rightSupply x2026
                          in (seq x2025 (seq x2027 (let
                              x2018 = leftSupply x2027
                              x2024 = rightSupply x2027
                               in (seq x2018 (seq x2024 (nd_C_app (let
                                   x2019 = leftSupply x2018
                                   x2020 = rightSupply x2018
                                    in (seq x2019 (seq x2020 (let
                                        x2017 = leftSupply x2019
                                        x2001 = rightSupply x2019
                                         in (seq x2017 (seq x2001 (let
                                             x2002 = leftSupply x2020
                                             x2021 = rightSupply x2020
                                              in (seq x2002 (seq x2021 (let
                                                  x2003 = leftSupply x2021
                                                  x2016 = rightSupply x2021
                                                   in (seq x2003 (seq x2016 (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_lparen x2001 x3250 x3500) (Curry_Pretty.nd_C_rparen x2002 x3250 x3500) (Curry_Pretty.nd_C_empty x2003 x3250 x3500) (let
                                                       x2004 = leftSupply x2016
                                                       x2015 = rightSupply x2016
                                                        in (seq x2004 (seq x2015 (Curry_Prelude.OP_Cons (nd_C_expDoc Curry_Prelude.C_True x2 (Curry_Prelude.C_Just x43) x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 0#) x3250 x3500) x2004 x3250 x3500) (let
                                                            x2008 = leftSupply x2015
                                                            x2013 = rightSupply x2015
                                                             in (seq x2008 (seq x2013 (Curry_Prelude.OP_Cons (let
                                                                 x2007 = leftSupply x2008
                                                                 x2009 = rightSupply x2008
                                                                  in (seq x2007 (seq x2009 (let
                                                                      x2005 = leftSupply x2009
                                                                      x2006 = rightSupply x2009
                                                                       in (seq x2005 (seq x2006 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2005 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.d_C_snd x39 x3250 x3500) x2006 x3250 x3500) x2007 x3250 x3500))))))) (Curry_Prelude.OP_Cons (let
                                                                 x2012 = leftSupply x2013
                                                                 x2014 = rightSupply x2013
                                                                  in (seq x2012 (seq x2014 (let
                                                                      x2010 = leftSupply x2014
                                                                      x2011 = rightSupply x2014
                                                                       in (seq x2010 (seq x2011 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2010 x3250 x3500) (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just x43) x4 (Curry_Prelude.d_OP_bang_bang x17 (Curry_Prelude.C_Int 1#) x3250 x3500) x2011 x3250 x3500) x2012 x3250 x3500))))))) Curry_Prelude.OP_List))))))))) x2017 x3250 x3500))))))))))))) (let
                                   x2023 = leftSupply x2024
                                   x2022 = rightSupply x2024
                                    in (seq x2023 (seq x2022 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_unknown x2022 x3250 x3500) (Curry_Prelude.C_Int 11#))) x4)) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 2#) x17 x3250 x3500) x2023 x3250 x3500)))) x2025 x3250 x3500))))))) x2028 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))) x19 x2000 x3250 x3500) (Curry_Prelude.d_OP_eq_eq (Curry_Prelude.d_C_length x17 x3250 x3500) (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x1002 x3000 x3250 x3500) (nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_74 x17 x19 x4 x2 x16 x15 x3 x18 x43 x39 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_73 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 x20 x3000 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_OP_dollar (nd_C_par x3 x2000 x3250 x3500) (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) x18 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.nd_C_maybe Curry_Prelude.C_False (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapDX id) (acceptCs id Curry_Prelude.d_OP_eq_eq)) (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'P'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 't'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '_'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 's'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))))))))))))) x19 x2000 x3250 x3500) (Curry_Prelude.d_OP_gt (Curry_Prelude.d_C_length x17 x3250 x3500) (Curry_Prelude.C_Int 3#) x3250 x3500) x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 x1002 x3000 x3250 x3500) (nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_73 x17 x19 x4 x2 x16 x15 x3 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_72 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 x20 x3000 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x2013 = x3000
           in (seq x2013 (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2000 = leftSupply x2014
                    x2011 = rightSupply x2014
                     in (seq x2000 (seq x2011 (Curry_Prelude.nd_OP_dollar (nd_OP_expDoc2_dot_appPar_dot_347 Curry_Prelude.C_True x3 x2000 x3250 x3500) (let
                         x2010 = leftSupply x2011
                         x2008 = rightSupply x2011
                          in (seq x2010 (seq x2008 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 1#))) (let
                              x2007 = leftSupply x2008
                              x2009 = rightSupply x2008
                               in (seq x2007 (seq x2009 (let
                                   x2003 = leftSupply x2009
                                   x2006 = rightSupply x2009
                                    in (seq x2003 (seq x2006 (nd_C_app (let
                                        x2002 = leftSupply x2003
                                        x2001 = rightSupply x2003
                                         in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_parens x2001 x3250 x3500) x18 x2002 x3250 x3500)))) (let
                                        x2005 = leftSupply x2006
                                        x2004 = rightSupply x2006
                                         in (seq x2005 (seq x2004 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_unknown x2004 x3250 x3500) (Curry_Prelude.C_Int 11#))) x4)) (Curry_Prelude.d_C_drop (Curry_Prelude.C_Int 3#) x17 x3250 x3500) x2005 x3250 x3500)))) x2007 x3250 x3500))))))) x2010 x3250 x3500)))) x2012 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_71 x17 x4 x2 x16 x15 x3 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 x1002 x3000 x3250 x3500) (nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_72 x17 x19 x4 x2 x16 x15 x3 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_71 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_71 x17 x4 x2 x16 x15 x3 x18 x3000 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2000 = leftSupply x2016
                    x2012 = rightSupply x2016
                     in (seq x2000 (seq x2012 (Curry_Pretty.nd_OP_lt_gt (nd_C_showPrecs (d_OP_expDoc2_dot_name_dot_347 (Curry_AbstractCurry.C_CApply x15 x16) x3250 x3500) x3 x2000 x3250 x3500) (let
                         x2011 = leftSupply x2012
                         x2013 = rightSupply x2012
                          in (seq x2011 (seq x2013 (let
                              x2001 = leftSupply x2013
                              x2009 = rightSupply x2013
                               in (seq x2001 (seq x2009 (Curry_Prelude.nd_OP_dollar (nd_OP_expDoc2_dot_appPar_dot_347 (Curry_Prelude.d_C_not (Curry_Prelude.d_C_null x17 x3250 x3500) x3250 x3500) x3 x2001 x3250 x3500) (let
                                   x2008 = leftSupply x2009
                                   x2010 = rightSupply x2009
                                    in (seq x2008 (seq x2010 (let
                                        x2004 = leftSupply x2010
                                        x2007 = rightSupply x2010
                                         in (seq x2004 (seq x2007 (nd_C_app (let
                                             x2003 = leftSupply x2004
                                             x2002 = rightSupply x2004
                                              in (seq x2003 (seq x2002 (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_unknown x2002 x3250 x3500) (Curry_Prelude.C_Int 11#))) x4 (d_OP_expDoc2_dot_name_dot_347 (Curry_AbstractCurry.C_CApply x15 x16) x3250 x3500) x2003 x3250 x3500)))) (let
                                             x2006 = leftSupply x2007
                                             x2005 = rightSupply x2007
                                              in (seq x2006 (seq x2005 (Curry_Prelude.nd_C_map (wrapNX id (nd_C_expDoc Curry_Prelude.C_False x2 (Curry_Prelude.C_Just (Curry_Prelude.OP_Tuple2 (Curry_Prelude.nd_C_unknown x2005 x3250 x3500) (Curry_Prelude.C_Int 11#))) x4)) x17 x2006 x3250 x3500)))) x2008 x3250 x3500))))))) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_71 x17 x4 x2 x16 x15 x3 x1002 x3000 x3250 x3500) (nd_OP__case_71 x17 x4 x2 x16 x15 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_71 x17 x4 x2 x16 x15 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_71 x17 x4 x2 x16 x15 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_99 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_99 x37 x36 x3250 x3500 = case x36 of
     (Curry_AbstractCurry.C_CApply x38 x39) -> d_OP__case_98 x39 x37 x38 x3250 x3500
     (Curry_AbstractCurry.C_CVar x111) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x112) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CSymbol x113) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x114 x115) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x116 x117) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x118) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x119 x120) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x121 x122) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x123) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x124 x125) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x126 x127) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_99 x37 x1002 x3250 x3500) (d_OP__case_99 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_99 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_99 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_98 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_98 x39 x37 x38 x3250 x3500 = case x38 of
     (Curry_AbstractCurry.C_CSymbol x40) -> d_OP__case_97 x39 x37 x40 x3250 x3500
     (Curry_AbstractCurry.C_CVar x93) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x94) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CApply x95 x96) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x97 x98) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x99 x100) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x101) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x102 x103) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x104 x105) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x106) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x107 x108) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x109 x110) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_98 x39 x37 x1002 x3250 x3500) (d_OP__case_98 x39 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_98 x39 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_98 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_97 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_97 x39 x37 x40 x3250 x3500 = case x40 of
     (Curry_Prelude.OP_Tuple2 x41 x42) -> d_OP__case_96 x42 x39 x37 x41 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_97 x39 x37 x1002 x3250 x3500) (d_OP__case_97 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_97 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_97 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_96 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_96 x42 x39 x37 x41 x3250 x3500 = case x41 of
     (Curry_Prelude.OP_Cons x43 x44) -> let
          x45 = x43
           in (d_OP__case_95 x45 x44 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x45 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_96 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_96 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_96 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_96 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_95 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_95 x45 x44 x42 x39 x37 x46 x3250 x3500 = case x46 of
     Curry_Prelude.C_True -> d_OP__case_94 x42 x39 x37 x44 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_95 x45 x44 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_95 x45 x44 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_95 x45 x44 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_95 x45 x44 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_94 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_94 x42 x39 x37 x44 x3250 x3500 = case x44 of
     (Curry_Prelude.OP_Cons x46 x47) -> let
          x48 = x46
           in (d_OP__case_93 x48 x47 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x48 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_94 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_94 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_94 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_94 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_93 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_93 x48 x47 x42 x39 x37 x49 x3250 x3500 = case x49 of
     Curry_Prelude.C_True -> d_OP__case_92 x42 x39 x37 x47 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_93 x48 x47 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_93 x48 x47 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_93 x48 x47 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_93 x48 x47 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_92 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_92 x42 x39 x37 x47 x3250 x3500 = case x47 of
     (Curry_Prelude.OP_Cons x49 x50) -> let
          x51 = x49
           in (d_OP__case_91 x51 x50 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x51 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_92 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_92 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_92 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_92 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_91 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_91 x51 x50 x42 x39 x37 x52 x3250 x3500 = case x52 of
     Curry_Prelude.C_True -> d_OP__case_90 x42 x39 x37 x50 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_91 x51 x50 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_91 x51 x50 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_91 x51 x50 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_91 x51 x50 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_90 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_90 x42 x39 x37 x50 x3250 x3500 = case x50 of
     (Curry_Prelude.OP_Cons x52 x53) -> let
          x54 = x52
           in (d_OP__case_89 x54 x53 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x54 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_90 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_90 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_90 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_90 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_89 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_89 x54 x53 x42 x39 x37 x55 x3250 x3500 = case x55 of
     Curry_Prelude.C_True -> d_OP__case_88 x42 x39 x37 x53 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_89 x54 x53 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_89 x54 x53 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_89 x54 x53 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_89 x54 x53 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_88 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_88 x42 x39 x37 x53 x3250 x3500 = case x53 of
     (Curry_Prelude.OP_Cons x55 x56) -> let
          x57 = x55
           in (d_OP__case_87 x57 x56 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x57 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_88 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_88 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_88 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_88 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_87 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_87 x57 x56 x42 x39 x37 x58 x3250 x3500 = case x58 of
     Curry_Prelude.C_True -> d_OP__case_86 x42 x39 x37 x56 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_87 x57 x56 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_87 x57 x56 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_87 x57 x56 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_87 x57 x56 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_86 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_86 x42 x39 x37 x56 x3250 x3500 = case x56 of
     (Curry_Prelude.OP_Cons x58 x59) -> let
          x60 = x58
           in (d_OP__case_85 x60 x59 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x60 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_86 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_86 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_86 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_86 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_85 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_85 x60 x59 x42 x39 x37 x61 x3250 x3500 = case x61 of
     Curry_Prelude.C_True -> d_OP__case_84 x42 x39 x37 x59 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_85 x60 x59 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_85 x60 x59 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_85 x60 x59 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_85 x60 x59 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_84 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_84 x42 x39 x37 x59 x3250 x3500 = case x59 of
     (Curry_Prelude.OP_Cons x61 x62) -> let
          x63 = x61
           in (d_OP__case_83 x63 x62 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x63 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_84 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_84 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_84 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_84 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_83 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_83 x63 x62 x42 x39 x37 x64 x3250 x3500 = case x64 of
     Curry_Prelude.C_True -> d_OP__case_82 x42 x39 x37 x62 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_83 x63 x62 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_83 x63 x62 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_83 x63 x62 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_83 x63 x62 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_82 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_82 x42 x39 x37 x62 x3250 x3500 = case x62 of
     Curry_Prelude.OP_List -> d_OP__case_81 x39 x37 x42 x3250 x3500
     (Curry_Prelude.OP_Cons x91 x92) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_82 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_82 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_82 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_82 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_81 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_81 x39 x37 x42 x3250 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x64 x65) -> let
          x66 = x64
           in (d_OP__case_80 x66 x65 x39 x37 (Curry_Prelude.d_OP_eq_eq x66 (Curry_Prelude.C_Char ':'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_81 x39 x37 x1002 x3250 x3500) (d_OP__case_81 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_81 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_81 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_80 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_80 x66 x65 x39 x37 x67 x3250 x3500 = case x67 of
     Curry_Prelude.C_True -> d_OP__case_79 x39 x37 x65 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_80 x66 x65 x39 x37 x1002 x3250 x3500) (d_OP__case_80 x66 x65 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_80 x66 x65 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_80 x66 x65 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_79 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_79 x39 x37 x65 x3250 x3500 = case x65 of
     Curry_Prelude.OP_List -> d_OP__case_78 x37 x39 x3250 x3500
     (Curry_Prelude.OP_Cons x89 x90) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_79 x39 x37 x1002 x3250 x3500) (d_OP__case_79 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_79 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_79 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_78 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_78 x37 x39 x3250 x3500 = case x39 of
     (Curry_AbstractCurry.C_CLit x67) -> d_OP__case_77 x37 x67 x3250 x3500
     (Curry_AbstractCurry.C_CVar x71) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CSymbol x72) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CApply x73 x74) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x75 x76) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x77 x78) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x79) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x80 x81) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x82 x83) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x84) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x85 x86) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x87 x88) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_78 x37 x1002 x3250 x3500) (d_OP__case_78 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_78 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_78 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_77 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CLiteral -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_77 x37 x67 x3250 x3500 = case x67 of
     (Curry_AbstractCurry.C_CCharc x68) -> Curry_Maybe.d_OP_gt_gt_minus (d_C_toString x37 x3250 x3500) (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) (Curry_Prelude.d_OP_plus_plus (d_C_quoteChar x68 x3250 x3500)) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.C_CIntc x69) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CFloatc x70) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CLiteral x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_77 x37 x1002 x3250 x3500) (d_OP__case_77 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CLiteral x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_77 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CLiteral x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_77 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CLiteral x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_120 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_120 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_119 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_120 x1002 x3250 x3500) (d_OP__case_120 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_120 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_120 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_119 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_119 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_118 x7 x6 x4 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_119 x4 x1002 x3250 x3500) (d_OP__case_119 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_119 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_119 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_118 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_118 x7 x6 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_117 x4 x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_118 x7 x6 x4 x1002 x3250 x3500) (d_OP__case_118 x7 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_118 x7 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_118 x7 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_117 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_117 x4 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_116 x10 x9 x4 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_117 x4 x1002 x3250 x3500) (d_OP__case_117 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_117 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_117 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_116 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_116 x10 x9 x4 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_115 x4 x9 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_116 x10 x9 x4 x1002 x3250 x3500) (d_OP__case_116 x10 x9 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_116 x10 x9 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_116 x10 x9 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_115 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_115 x4 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_114 x13 x12 x4 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_115 x4 x1002 x3250 x3500) (d_OP__case_115 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_115 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_115 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_114 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_114 x13 x12 x4 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_113 x4 x12 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_114 x13 x12 x4 x1002 x3250 x3500) (d_OP__case_114 x13 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_114 x13 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_114 x13 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_113 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_113 x4 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x16 = x14
           in (d_OP__case_112 x16 x15 x4 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_113 x4 x1002 x3250 x3500) (d_OP__case_113 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_113 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_113 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_112 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_112 x16 x15 x4 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> d_OP__case_111 x4 x15 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_112 x16 x15 x4 x1002 x3250 x3500) (d_OP__case_112 x16 x15 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_112 x16 x15 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_112 x16 x15 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_111 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_111 x4 x15 x3250 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_110 x19 x18 x4 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_111 x4 x1002 x3250 x3500) (d_OP__case_111 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_111 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_111 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_110 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_110 x19 x18 x4 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_109 x4 x18 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_110 x19 x18 x4 x1002 x3250 x3500) (d_OP__case_110 x19 x18 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_110 x19 x18 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_110 x19 x18 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_109 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_109 x4 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x22 = x20
           in (d_OP__case_108 x22 x21 x4 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_109 x4 x1002 x3250 x3500) (d_OP__case_109 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_109 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_109 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_108 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_108 x22 x21 x4 x23 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> d_OP__case_107 x4 x21 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_108 x22 x21 x4 x1002 x3250 x3500) (d_OP__case_108 x22 x21 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_108 x22 x21 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_108 x22 x21 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_107 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_107 x4 x21 x3250 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x25 = x23
           in (d_OP__case_106 x25 x24 x4 (Curry_Prelude.d_OP_eq_eq x25 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_107 x4 x1002 x3250 x3500) (d_OP__case_107 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_107 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_107 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_106 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_106 x25 x24 x4 x26 x3250 x3500 = case x26 of
     Curry_Prelude.C_True -> d_OP__case_105 x4 x24 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_106 x25 x24 x4 x1002 x3250 x3500) (d_OP__case_106 x25 x24 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_106 x25 x24 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_106 x25 x24 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_105 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_105 x4 x24 x3250 x3500 = case x24 of
     Curry_Prelude.OP_List -> d_OP__case_104 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x34 x35) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_105 x4 x1002 x3250 x3500) (d_OP__case_105 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_105 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_105 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_104 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_104 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x26 x27) -> let
          x28 = x26
           in (d_OP__case_103 x28 x27 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_104 x1002 x3250 x3500) (d_OP__case_104 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_104 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_104 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_103 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_103 x28 x27 x29 x3250 x3500 = case x29 of
     Curry_Prelude.C_True -> d_OP__case_102 x27 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_103 x28 x27 x1002 x3250 x3500) (d_OP__case_103 x28 x27 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_103 x28 x27 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_103 x28 x27 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_102 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_102 x27 x3250 x3500 = case x27 of
     (Curry_Prelude.OP_Cons x29 x30) -> let
          x31 = x29
           in (d_OP__case_101 x31 x30 (Curry_Prelude.d_OP_eq_eq x31 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_102 x1002 x3250 x3500) (d_OP__case_102 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_102 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_102 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_101 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_101 x31 x30 x32 x3250 x3500 = case x32 of
     Curry_Prelude.C_True -> d_OP__case_100 x30 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_101 x31 x30 x1002 x3250 x3500) (d_OP__case_101 x31 x30 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_101 x31 x30 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_101 x31 x30 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_100 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_Prelude.C_Char)
d_OP__case_100 x30 x3250 x3500 = case x30 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x32 x33) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_100 x1002 x3250 x3500) (d_OP__case_100 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_100 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_100 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_141 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_141 x37 x36 x3250 x3500 = case x36 of
     (Curry_AbstractCurry.C_CApply x38 x39) -> d_OP__case_140 x39 x37 x38 x3250 x3500
     (Curry_AbstractCurry.C_CVar x89) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x90) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CSymbol x91) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x92 x93) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x94 x95) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x96) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x97 x98) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x99 x100) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x101) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x102 x103) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x104 x105) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_141 x37 x1002 x3250 x3500) (d_OP__case_141 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_141 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_141 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_140 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_140 x39 x37 x38 x3250 x3500 = case x38 of
     (Curry_AbstractCurry.C_CSymbol x40) -> d_OP__case_139 x39 x37 x40 x3250 x3500
     (Curry_AbstractCurry.C_CVar x71) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLit x72) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CApply x73 x74) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLambda x75 x76) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CLetDecl x77 x78) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CDoExpr x79) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CListComp x80 x81) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CCase x82 x83) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecConstr x84) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecSelect x85 x86) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.C_CRecUpdate x87 x88) -> Curry_Prelude.C_Nothing
     (Curry_AbstractCurry.Choice_C_CExpr x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_140 x39 x37 x1002 x3250 x3500) (d_OP__case_140 x39 x37 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_CExpr x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_140 x39 x37 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_CExpr x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_140 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_CExpr x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_139 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_139 x39 x37 x40 x3250 x3500 = case x40 of
     (Curry_Prelude.OP_Tuple2 x41 x42) -> d_OP__case_138 x42 x39 x37 x41 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_139 x39 x37 x1002 x3250 x3500) (d_OP__case_139 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_139 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_139 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_138 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_138 x42 x39 x37 x41 x3250 x3500 = case x41 of
     (Curry_Prelude.OP_Cons x43 x44) -> let
          x45 = x43
           in (d_OP__case_137 x45 x44 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x45 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_138 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_138 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_138 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_138 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_137 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_137 x45 x44 x42 x39 x37 x46 x3250 x3500 = case x46 of
     Curry_Prelude.C_True -> d_OP__case_136 x42 x39 x37 x44 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_137 x45 x44 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_137 x45 x44 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_137 x45 x44 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_137 x45 x44 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_136 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_136 x42 x39 x37 x44 x3250 x3500 = case x44 of
     (Curry_Prelude.OP_Cons x46 x47) -> let
          x48 = x46
           in (d_OP__case_135 x48 x47 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x48 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_136 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_136 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_136 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_136 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_135 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_135 x48 x47 x42 x39 x37 x49 x3250 x3500 = case x49 of
     Curry_Prelude.C_True -> d_OP__case_134 x42 x39 x37 x47 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_135 x48 x47 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_135 x48 x47 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_135 x48 x47 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_135 x48 x47 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_134 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_134 x42 x39 x37 x47 x3250 x3500 = case x47 of
     (Curry_Prelude.OP_Cons x49 x50) -> let
          x51 = x49
           in (d_OP__case_133 x51 x50 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x51 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_134 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_134 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_134 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_134 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_133 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_133 x51 x50 x42 x39 x37 x52 x3250 x3500 = case x52 of
     Curry_Prelude.C_True -> d_OP__case_132 x42 x39 x37 x50 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_133 x51 x50 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_133 x51 x50 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_133 x51 x50 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_133 x51 x50 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_132 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_132 x42 x39 x37 x50 x3250 x3500 = case x50 of
     (Curry_Prelude.OP_Cons x52 x53) -> let
          x54 = x52
           in (d_OP__case_131 x54 x53 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x54 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_132 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_132 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_132 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_132 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_131 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_131 x54 x53 x42 x39 x37 x55 x3250 x3500 = case x55 of
     Curry_Prelude.C_True -> d_OP__case_130 x42 x39 x37 x53 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_131 x54 x53 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_131 x54 x53 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_131 x54 x53 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_131 x54 x53 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_130 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_130 x42 x39 x37 x53 x3250 x3500 = case x53 of
     (Curry_Prelude.OP_Cons x55 x56) -> let
          x57 = x55
           in (d_OP__case_129 x57 x56 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x57 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_130 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_130 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_130 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_130 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_129 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_129 x57 x56 x42 x39 x37 x58 x3250 x3500 = case x58 of
     Curry_Prelude.C_True -> d_OP__case_128 x42 x39 x37 x56 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_129 x57 x56 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_129 x57 x56 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_129 x57 x56 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_129 x57 x56 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_128 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_128 x42 x39 x37 x56 x3250 x3500 = case x56 of
     (Curry_Prelude.OP_Cons x58 x59) -> let
          x60 = x58
           in (d_OP__case_127 x60 x59 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x60 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_128 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_128 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_128 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_128 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_127 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_127 x60 x59 x42 x39 x37 x61 x3250 x3500 = case x61 of
     Curry_Prelude.C_True -> d_OP__case_126 x42 x39 x37 x59 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_127 x60 x59 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_127 x60 x59 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_127 x60 x59 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_127 x60 x59 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_126 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_126 x42 x39 x37 x59 x3250 x3500 = case x59 of
     (Curry_Prelude.OP_Cons x61 x62) -> let
          x63 = x61
           in (d_OP__case_125 x63 x62 x42 x39 x37 (Curry_Prelude.d_OP_eq_eq x63 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_126 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_126 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_126 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_126 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_125 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_125 x63 x62 x42 x39 x37 x64 x3250 x3500 = case x64 of
     Curry_Prelude.C_True -> d_OP__case_124 x42 x39 x37 x62 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_125 x63 x62 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_125 x63 x62 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_125 x63 x62 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_125 x63 x62 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_124 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_124 x42 x39 x37 x62 x3250 x3500 = case x62 of
     Curry_Prelude.OP_List -> d_OP__case_123 x39 x37 x42 x3250 x3500
     (Curry_Prelude.OP_Cons x69 x70) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_124 x42 x39 x37 x1002 x3250 x3500) (d_OP__case_124 x42 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_124 x42 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_124 x42 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_123 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_123 x39 x37 x42 x3250 x3500 = case x42 of
     (Curry_Prelude.OP_Cons x64 x65) -> let
          x66 = x64
           in (d_OP__case_122 x66 x65 x39 x37 (Curry_Prelude.d_OP_eq_eq x66 (Curry_Prelude.C_Char ':'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_123 x39 x37 x1002 x3250 x3500) (d_OP__case_123 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_123 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_123 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_122 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_122 x66 x65 x39 x37 x67 x3250 x3500 = case x67 of
     Curry_Prelude.C_True -> d_OP__case_121 x39 x37 x65 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_122 x66 x65 x39 x37 x1002 x3250 x3500) (d_OP__case_122 x66 x65 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_122 x66 x65 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_122 x66 x65 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_121 :: Curry_AbstractCurry.C_CExpr -> Curry_AbstractCurry.C_CExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_121 x39 x37 x65 x3250 x3500 = case x65 of
     Curry_Prelude.OP_List -> Curry_Maybe.d_OP_gt_gt_minus (d_C_toList x37 x3250 x3500) (Curry_Prelude.d_OP_dot (acceptCs id Curry_Prelude.C_Just) (acceptCs id (Curry_Prelude.OP_Cons x39)) x3250 x3500) x3250 x3500
     (Curry_Prelude.OP_Cons x67 x68) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_121 x39 x37 x1002 x3250 x3500) (d_OP__case_121 x39 x37 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_121 x39 x37 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_121 x39 x37 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_162 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_162 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__case_161 x4 x3 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_162 x1002 x3250 x3500) (d_OP__case_162 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_162 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_162 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_161 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_161 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x7 = x5
           in (d_OP__case_160 x7 x6 x4 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_161 x4 x1002 x3250 x3500) (d_OP__case_161 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_161 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_161 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_160 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_160 x7 x6 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> d_OP__case_159 x4 x6 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_160 x7 x6 x4 x1002 x3250 x3500) (d_OP__case_160 x7 x6 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_160 x7 x6 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_160 x7 x6 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_159 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_159 x4 x6 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x10 = x8
           in (d_OP__case_158 x10 x9 x4 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_159 x4 x1002 x3250 x3500) (d_OP__case_159 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_159 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_159 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_158 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_158 x10 x9 x4 x11 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> d_OP__case_157 x4 x9 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_158 x10 x9 x4 x1002 x3250 x3500) (d_OP__case_158 x10 x9 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_158 x10 x9 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_158 x10 x9 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_157 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_157 x4 x9 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x13 = x11
           in (d_OP__case_156 x13 x12 x4 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_157 x4 x1002 x3250 x3500) (d_OP__case_157 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_157 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_157 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_156 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_156 x13 x12 x4 x14 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> d_OP__case_155 x4 x12 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_156 x13 x12 x4 x1002 x3250 x3500) (d_OP__case_156 x13 x12 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_156 x13 x12 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_156 x13 x12 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_155 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_155 x4 x12 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x16 = x14
           in (d_OP__case_154 x16 x15 x4 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_155 x4 x1002 x3250 x3500) (d_OP__case_155 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_155 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_155 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_154 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_154 x16 x15 x4 x17 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> d_OP__case_153 x4 x15 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_154 x16 x15 x4 x1002 x3250 x3500) (d_OP__case_154 x16 x15 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_154 x16 x15 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_154 x16 x15 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_153 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_153 x4 x15 x3250 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x19 = x17
           in (d_OP__case_152 x19 x18 x4 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_153 x4 x1002 x3250 x3500) (d_OP__case_153 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_153 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_153 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_152 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_152 x19 x18 x4 x20 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> d_OP__case_151 x4 x18 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_152 x19 x18 x4 x1002 x3250 x3500) (d_OP__case_152 x19 x18 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_152 x19 x18 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_152 x19 x18 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_151 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_151 x4 x18 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x22 = x20
           in (d_OP__case_150 x22 x21 x4 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_151 x4 x1002 x3250 x3500) (d_OP__case_151 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_151 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_151 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_150 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_150 x22 x21 x4 x23 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> d_OP__case_149 x4 x21 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_150 x22 x21 x4 x1002 x3250 x3500) (d_OP__case_150 x22 x21 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_150 x22 x21 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_150 x22 x21 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_149 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_149 x4 x21 x3250 x3500 = case x21 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x25 = x23
           in (d_OP__case_148 x25 x24 x4 (Curry_Prelude.d_OP_eq_eq x25 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_149 x4 x1002 x3250 x3500) (d_OP__case_149 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_149 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_149 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_148 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_148 x25 x24 x4 x26 x3250 x3500 = case x26 of
     Curry_Prelude.C_True -> d_OP__case_147 x4 x24 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_148 x25 x24 x4 x1002 x3250 x3500) (d_OP__case_148 x25 x24 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_148 x25 x24 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_148 x25 x24 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_147 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_147 x4 x24 x3250 x3500 = case x24 of
     Curry_Prelude.OP_List -> d_OP__case_146 x4 x3250 x3500
     (Curry_Prelude.OP_Cons x34 x35) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_147 x4 x1002 x3250 x3500) (d_OP__case_147 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_147 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_147 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_146 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_146 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x26 x27) -> let
          x28 = x26
           in (d_OP__case_145 x28 x27 (Curry_Prelude.d_OP_eq_eq x28 (Curry_Prelude.C_Char '['#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_146 x1002 x3250 x3500) (d_OP__case_146 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_146 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_146 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_145 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_145 x28 x27 x29 x3250 x3500 = case x29 of
     Curry_Prelude.C_True -> d_OP__case_144 x27 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_145 x28 x27 x1002 x3250 x3500) (d_OP__case_145 x28 x27 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_145 x28 x27 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_145 x28 x27 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_144 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_144 x27 x3250 x3500 = case x27 of
     (Curry_Prelude.OP_Cons x29 x30) -> let
          x31 = x29
           in (d_OP__case_143 x31 x30 (Curry_Prelude.d_OP_eq_eq x31 (Curry_Prelude.C_Char ']'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_144 x1002 x3250 x3500) (d_OP__case_144 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_144 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_144 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_143 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_143 x31 x30 x32 x3250 x3500 = case x32 of
     Curry_Prelude.C_True -> d_OP__case_142 x30 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_143 x31 x30 x1002 x3250 x3500) (d_OP__case_143 x31 x30 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_143 x31 x30 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_143 x31 x30 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_142 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Maybe (Curry_Prelude.OP_List Curry_AbstractCurry.C_CExpr)
d_OP__case_142 x30 x3250 x3500 = case x30 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_Just Curry_Prelude.OP_List
     (Curry_Prelude.OP_Cons x32 x33) -> Curry_Prelude.C_Nothing
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_142 x1002 x3250 x3500) (d_OP__case_142 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_142 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_142 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_163 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_163 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_dquotes x3250 x3500) (Curry_Pretty.d_C_text x1 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_163 x1 x1002 x3250 x3500) (d_OP__case_163 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_163 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_163 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_163 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_163 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_dquotes x2000 x3250 x3500) (Curry_Pretty.nd_C_text x1 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_163 x1 x1002 x3000 x3250 x3500) (nd_OP__case_163 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_163 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_163 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_164 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CLocalDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_164 x7 x2 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2007 = leftSupply x2012
                    x2009 = rightSupply x2012
                     in (seq x2007 (seq x2009 (Curry_Prelude.nd_C_apply (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2004 = rightSupply x2008
                               in (seq x2000 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_line x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x2002 x3250 x3500) x2003 x3250 x3500))))))) x2006 x3250 x3500))))))) (nd_C_localDeclsDoc x1 x2 x7 x2009 x3250 x3500) x2010 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_164 x7 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_164 x7 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_164 x7 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_164 x7 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_165 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_165 x5 x3 x8 x2 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (d_C_patternDoc x2 (Curry_Prelude.d_OP_bang_bang x5 (Curry_Prelude.C_Int 0#) x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.d_C_snd x3 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) (d_C_patternDoc x2 (Curry_Prelude.d_OP_bang_bang x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_OP_lt_gt (d_C_qname x2 x3 x3250 x3500) x8 x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_165 x5 x3 x8 x2 x1002 x3250 x3500) (d_OP__case_165 x5 x3 x8 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_165 x5 x3 x8 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_165 x5 x3 x8 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_165 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_165 x5 x3 x8 x2 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2011 = leftSupply x2016
                    x2013 = rightSupply x2016
                     in (seq x2011 (seq x2013 (Curry_Prelude.nd_C_apply (let
                         x2010 = leftSupply x2011
                         x2012 = rightSupply x2011
                          in (seq x2010 (seq x2012 (let
                              x2000 = leftSupply x2012
                              x2008 = rightSupply x2012
                               in (seq x2000 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2007 = leftSupply x2008
                                   x2009 = rightSupply x2008
                                    in (seq x2007 (seq x2009 (let
                                        x2004 = leftSupply x2009
                                        x2006 = rightSupply x2009
                                         in (seq x2004 (seq x2006 (Curry_Prelude.nd_C_apply (let
                                             x2003 = leftSupply x2004
                                             x2005 = rightSupply x2004
                                              in (seq x2003 (seq x2005 (let
                                                  x2001 = leftSupply x2005
                                                  x2002 = rightSupply x2005
                                                   in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (nd_C_patternDoc x2 (Curry_Prelude.d_OP_bang_bang x5 (Curry_Prelude.C_Int 0#) x3250 x3500) x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.d_C_snd x3 x3250 x3500) x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))) (nd_C_patternDoc x2 (Curry_Prelude.d_OP_bang_bang x5 (Curry_Prelude.C_Int 1#) x3250 x3500) x2013 x3250 x3500) x2014 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (Curry_Pretty.nd_OP_lt_gt (nd_C_qname x2 x3 x2000 x3250 x3500) x8 x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_165 x5 x3 x8 x2 x1002 x3000 x3250 x3500) (nd_OP__case_165 x5 x3 x8 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_165 x5 x3 x8 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_165 x5 x3 x8 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_166 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_166 x5 x2 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (Curry_Prelude.d_C_apply (d_C_patternsDoc x2 x3250 x3500) x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_166 x5 x2 x1002 x3250 x3500) (d_OP__case_166 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_166 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_166 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_166 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CPattern -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_166 x5 x2 x6 x3000 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2005 = x3000
           in (seq x2005 (let
               x2004 = leftSupply x2005
               x2006 = rightSupply x2005
                in (seq x2004 (seq x2006 (let
                    x2000 = leftSupply x2006
                    x2003 = rightSupply x2006
                     in (seq x2000 (seq x2003 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2000 x3250 x3500) (let
                         x2002 = leftSupply x2003
                         x2001 = rightSupply x2003
                          in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (nd_C_patternsDoc x2 x2001 x3250 x3500) x5 x2002 x3250 x3500)))) x2004 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_166 x5 x2 x1002 x3000 x3250 x3500) (nd_OP__case_166 x5 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_166 x5 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_166 x5 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_168 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr) -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_168 x6 x10 x1 x2 x9 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2028 = x3000
           in (seq x2028 (let
               x2027 = leftSupply x2028
               x2026 = rightSupply x2028
                in (seq x2027 (seq x2026 (Curry_Prelude.nd_OP_dollar (wrapNX id (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 2#))) (let
                    x2025 = leftSupply x2026
                    x2024 = rightSupply x2026
                     in (seq x2025 (seq x2024 (Curry_Pretty.nd_OP_lt_gt (let
                         x2023 = leftSupply x2024
                         x2022 = rightSupply x2024
                          in (seq x2023 (seq x2022 (Curry_Prelude.nd_OP_dollar (wrapNX id Curry_Pretty.nd_C_group) (let
                              x2021 = leftSupply x2022
                              x2019 = rightSupply x2022
                               in (seq x2021 (seq x2019 (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 2#) (let
                                   x2018 = leftSupply x2019
                                   x2020 = rightSupply x2019
                                    in (seq x2018 (seq x2020 (let
                                        x2009 = leftSupply x2020
                                        x2016 = rightSupply x2020
                                         in (seq x2009 (seq x2016 (Curry_Prelude.nd_C_apply (let
                                             x2008 = leftSupply x2009
                                             x2010 = rightSupply x2009
                                              in (seq x2008 (seq x2010 (let
                                                  x2000 = leftSupply x2010
                                                  x2006 = rightSupply x2010
                                                   in (seq x2000 (seq x2006 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) (let
                                                       x2005 = leftSupply x2006
                                                       x2007 = rightSupply x2006
                                                        in (seq x2005 (seq x2007 (let
                                                            x2003 = leftSupply x2007
                                                            x2004 = rightSupply x2007
                                                             in (seq x2003 (seq x2004 (Curry_Prelude.nd_C_apply (let
                                                                 x2002 = leftSupply x2003
                                                                 x2001 = rightSupply x2003
                                                                  in (seq x2002 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) x9 x2002 x3250 x3500)))) (Curry_Pretty.nd_C_equals x2004 x3250 x3500) x2005 x3250 x3500))))))) x2008 x3250 x3500))))))) (let
                                             x2015 = leftSupply x2016
                                             x2017 = rightSupply x2016
                                              in (seq x2015 (seq x2017 (let
                                                  x2011 = leftSupply x2017
                                                  x2014 = rightSupply x2017
                                                   in (seq x2011 (seq x2014 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2011 x3250 x3500) (let
                                                       x2013 = leftSupply x2014
                                                       x2012 = rightSupply x2014
                                                        in (seq x2013 (seq x2012 (nd_C_expDoc (Curry_Prelude.nd_C_unknown x2012 x3250 x3500) x1 Curry_Prelude.C_Nothing x2 (Curry_Prelude.d_C_snd (Curry_Prelude.d_C_head x6 x3250 x3500) x3250 x3500) x2013 x3250 x3500)))) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))) x2021 x3250 x3500)))) x2023 x3250 x3500)))) x10 x2025 x3250 x3500)))) x2027 x3250 x3500)))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_167 x10 x6 x1 x2 x9 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_168 x6 x10 x1 x2 x9 x1002 x3000 x3250 x3500) (nd_OP__case_168 x6 x10 x1 x2 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_168 x6 x10 x1 x2 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_168 x6 x10 x1 x2 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_167 :: Curry_Pretty.C_Doc -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CExpr Curry_AbstractCurry.C_CExpr) -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_167 x10 x6 x1 x2 x9 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2020 = x3000
           in (seq x2020 (let
               x2019 = leftSupply x2020
               x2018 = rightSupply x2020
                in (seq x2019 (seq x2018 (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 2#) (let
                    x2017 = leftSupply x2018
                    x2016 = rightSupply x2018
                     in (seq x2017 (seq x2016 (Curry_Pretty.nd_OP_lt_gt (let
                         x2015 = leftSupply x2016
                         x2013 = rightSupply x2016
                          in (seq x2015 (seq x2013 (Curry_Pretty.nd_C_hang (Curry_Prelude.C_Int 2#) (let
                              x2012 = leftSupply x2013
                              x2014 = rightSupply x2013
                               in (seq x2012 (seq x2014 (let
                                   x2002 = leftSupply x2014
                                   x2010 = rightSupply x2014
                                    in (seq x2002 (seq x2010 (Curry_Prelude.nd_C_apply (let
                                        x2001 = leftSupply x2002
                                        x2000 = rightSupply x2002
                                         in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) x9 x2001 x3250 x3500)))) (let
                                        x2009 = leftSupply x2010
                                        x2011 = rightSupply x2010
                                         in (seq x2009 (seq x2011 (let
                                             x2003 = leftSupply x2011
                                             x2007 = rightSupply x2011
                                              in (seq x2003 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2003 x3250 x3500) (let
                                                  x2006 = leftSupply x2007
                                                  x2008 = rightSupply x2007
                                                   in (seq x2006 (seq x2008 (let
                                                       x2004 = leftSupply x2008
                                                       x2005 = rightSupply x2008
                                                        in (seq x2004 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_vsep x2004 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_OP_ruleDoc_dot_choiceDoc_dot_285 x2 x1)) x6 x2005 x3250 x3500) x2006 x3250 x3500))))))) x2009 x3250 x3500))))))) x2012 x3250 x3500))))))) x2015 x3250 x3500)))) x10 x2017 x3250 x3500)))) x2019 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_167 x10 x6 x1 x2 x9 x1002 x3000 x3250 x3500) (nd_OP__case_167 x10 x6 x1 x2 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_167 x10 x6 x1 x2 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_167 x10 x6 x1 x2 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_169 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CLocalDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char)) (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_169 x7 x2 x1 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2007 = leftSupply x2012
                    x2009 = rightSupply x2012
                     in (seq x2007 (seq x2009 (Curry_Prelude.nd_C_apply (let
                         x2006 = leftSupply x2007
                         x2008 = rightSupply x2007
                          in (seq x2006 (seq x2008 (let
                              x2000 = leftSupply x2008
                              x2004 = rightSupply x2008
                               in (seq x2000 (seq x2004 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_line x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x2002 x3250 x3500) x2003 x3250 x3500))))))) x2006 x3250 x3500))))))) (nd_C_localDeclsDoc x1 x2 x7 x2009 x3250 x3500) x2010 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_169 x7 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_169 x7 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_169 x7 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_169 x7 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_170 :: Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_170 x7 x4 x2 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_dollar_gt x3250 x3500) (d_C_funcTypeDeclDoc x2 x4 x7 x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_170 x7 x4 x2 x1002 x3250 x3500) (d_OP__case_170 x7 x4 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_170 x7 x4 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_170 x7 x4 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_170 :: Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_170 x7 x4 x2 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Prelude.nd_C_apply (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) (nd_C_funcTypeDeclDoc x2 x4 x7 x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Pretty.nd_C_empty x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_170 x7 x4 x2 x1002 x3000 x3250 x3500) (nd_OP__case_170 x7 x4 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_170 x7 x4 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_170 x7 x4 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_171 :: Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_171 x7 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_C_empty x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_171 x7 x1002 x3250 x3500) (d_OP__case_171 x7 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_171 x7 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_171 x7 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_171 :: Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_171 x7 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_171 x7 x1002 x3000 x3250 x3500) (nd_OP__case_171 x7 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_171 x7 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_171 x7 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_203 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_203 x3 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x4 x5) -> d_OP__case_202 x5 x3 x4 x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_203 x3 x1002 x3250 x3500) (d_OP__case_203 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_203 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_203 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_202 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_202 x5 x3 x4 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x6 x7) -> let
          x8 = x6
           in (d_OP__case_201 x8 x7 x5 x3 (Curry_Prelude.d_OP_eq_eq x8 (Curry_Prelude.C_Char 'P'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_202 x5 x3 x1002 x3250 x3500) (d_OP__case_202 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_202 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_202 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_201 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_201 x8 x7 x5 x3 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> d_OP__case_200 x5 x3 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_201 x8 x7 x5 x3 x1002 x3250 x3500) (d_OP__case_201 x8 x7 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_201 x8 x7 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_201 x8 x7 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_200 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_200 x5 x3 x7 x3250 x3500 = case x7 of
     (Curry_Prelude.OP_Cons x9 x10) -> let
          x11 = x9
           in (d_OP__case_199 x11 x10 x5 x3 (Curry_Prelude.d_OP_eq_eq x11 (Curry_Prelude.C_Char 'r'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_200 x5 x3 x1002 x3250 x3500) (d_OP__case_200 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_200 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_200 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_199 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_199 x11 x10 x5 x3 x12 x3250 x3500 = case x12 of
     Curry_Prelude.C_True -> d_OP__case_198 x5 x3 x10 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_199 x11 x10 x5 x3 x1002 x3250 x3500) (d_OP__case_199 x11 x10 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_199 x11 x10 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_199 x11 x10 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_198 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_198 x5 x3 x10 x3250 x3500 = case x10 of
     (Curry_Prelude.OP_Cons x12 x13) -> let
          x14 = x12
           in (d_OP__case_197 x14 x13 x5 x3 (Curry_Prelude.d_OP_eq_eq x14 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_198 x5 x3 x1002 x3250 x3500) (d_OP__case_198 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_198 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_198 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_197 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_197 x14 x13 x5 x3 x15 x3250 x3500 = case x15 of
     Curry_Prelude.C_True -> d_OP__case_196 x5 x3 x13 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_197 x14 x13 x5 x3 x1002 x3250 x3500) (d_OP__case_197 x14 x13 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_197 x14 x13 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_197 x14 x13 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_196 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_196 x5 x3 x13 x3250 x3500 = case x13 of
     (Curry_Prelude.OP_Cons x15 x16) -> let
          x17 = x15
           in (d_OP__case_195 x17 x16 x5 x3 (Curry_Prelude.d_OP_eq_eq x17 (Curry_Prelude.C_Char 'l'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_196 x5 x3 x1002 x3250 x3500) (d_OP__case_196 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_196 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_196 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_195 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_195 x17 x16 x5 x3 x18 x3250 x3500 = case x18 of
     Curry_Prelude.C_True -> d_OP__case_194 x5 x3 x16 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_195 x17 x16 x5 x3 x1002 x3250 x3500) (d_OP__case_195 x17 x16 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_195 x17 x16 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_195 x17 x16 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_194 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_194 x5 x3 x16 x3250 x3500 = case x16 of
     (Curry_Prelude.OP_Cons x18 x19) -> let
          x20 = x18
           in (d_OP__case_193 x20 x19 x5 x3 (Curry_Prelude.d_OP_eq_eq x20 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_194 x5 x3 x1002 x3250 x3500) (d_OP__case_194 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_194 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_194 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_193 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_193 x20 x19 x5 x3 x21 x3250 x3500 = case x21 of
     Curry_Prelude.C_True -> d_OP__case_192 x5 x3 x19 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_193 x20 x19 x5 x3 x1002 x3250 x3500) (d_OP__case_193 x20 x19 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_193 x20 x19 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_193 x20 x19 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_192 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_192 x5 x3 x19 x3250 x3500 = case x19 of
     (Curry_Prelude.OP_Cons x21 x22) -> let
          x23 = x21
           in (d_OP__case_191 x23 x22 x5 x3 (Curry_Prelude.d_OP_eq_eq x23 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_192 x5 x3 x1002 x3250 x3500) (d_OP__case_192 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_192 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_192 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_191 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_191 x23 x22 x5 x3 x24 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> d_OP__case_190 x5 x3 x22 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_191 x23 x22 x5 x3 x1002 x3250 x3500) (d_OP__case_191 x23 x22 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_191 x23 x22 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_191 x23 x22 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_190 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_190 x5 x3 x22 x3250 x3500 = case x22 of
     (Curry_Prelude.OP_Cons x24 x25) -> let
          x26 = x24
           in (d_OP__case_189 x26 x25 x5 x3 (Curry_Prelude.d_OP_eq_eq x26 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_190 x5 x3 x1002 x3250 x3500) (d_OP__case_190 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_190 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_190 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_189 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_189 x26 x25 x5 x3 x27 x3250 x3500 = case x27 of
     Curry_Prelude.C_True -> d_OP__case_188 x5 x3 x25 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_189 x26 x25 x5 x3 x1002 x3250 x3500) (d_OP__case_189 x26 x25 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_189 x26 x25 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_189 x26 x25 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_188 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_188 x5 x3 x25 x3250 x3500 = case x25 of
     Curry_Prelude.OP_List -> d_OP__case_187 x3 x5 x3250 x3500
     (Curry_Prelude.OP_Cons x52 x53) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_188 x5 x3 x1002 x3250 x3500) (d_OP__case_188 x5 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_188 x5 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_188 x5 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_187 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_187 x3 x5 x3250 x3500 = case x5 of
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x29 = x27
           in (d_OP__case_186 x29 x28 x3 (Curry_Prelude.d_OP_eq_eq x29 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_187 x3 x1002 x3250 x3500) (d_OP__case_187 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_187 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_187 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_186 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_186 x29 x28 x3 x30 x3250 x3500 = case x30 of
     Curry_Prelude.C_True -> d_OP__case_185 x3 x28 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_186 x29 x28 x3 x1002 x3250 x3500) (d_OP__case_186 x29 x28 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_186 x29 x28 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_186 x29 x28 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_185 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_185 x3 x28 x3250 x3500 = case x28 of
     (Curry_Prelude.OP_Cons x30 x31) -> let
          x32 = x30
           in (d_OP__case_184 x32 x31 x3 (Curry_Prelude.d_OP_eq_eq x32 (Curry_Prelude.C_Char 'n'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_185 x3 x1002 x3250 x3500) (d_OP__case_185 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_185 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_185 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_184 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_184 x32 x31 x3 x33 x3250 x3500 = case x33 of
     Curry_Prelude.C_True -> d_OP__case_183 x3 x31 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_184 x32 x31 x3 x1002 x3250 x3500) (d_OP__case_184 x32 x31 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_184 x32 x31 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_184 x32 x31 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_183 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_183 x3 x31 x3250 x3500 = case x31 of
     (Curry_Prelude.OP_Cons x33 x34) -> let
          x35 = x33
           in (d_OP__case_182 x35 x34 x3 (Curry_Prelude.d_OP_eq_eq x35 (Curry_Prelude.C_Char 't'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_183 x3 x1002 x3250 x3500) (d_OP__case_183 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_183 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_183 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_182 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_182 x35 x34 x3 x36 x3250 x3500 = case x36 of
     Curry_Prelude.C_True -> d_OP__case_181 x3 x34 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_182 x35 x34 x3 x1002 x3250 x3500) (d_OP__case_182 x35 x34 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_182 x35 x34 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_182 x35 x34 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_181 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_181 x3 x34 x3250 x3500 = case x34 of
     (Curry_Prelude.OP_Cons x36 x37) -> let
          x38 = x36
           in (d_OP__case_180 x38 x37 x3 (Curry_Prelude.d_OP_eq_eq x38 (Curry_Prelude.C_Char 'y'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_181 x3 x1002 x3250 x3500) (d_OP__case_181 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_181 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_181 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_180 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_180 x38 x37 x3 x39 x3250 x3500 = case x39 of
     Curry_Prelude.C_True -> d_OP__case_179 x3 x37 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_180 x38 x37 x3 x1002 x3250 x3500) (d_OP__case_180 x38 x37 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_180 x38 x37 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_180 x38 x37 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_179 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_179 x3 x37 x3250 x3500 = case x37 of
     (Curry_Prelude.OP_Cons x39 x40) -> let
          x41 = x39
           in (d_OP__case_178 x41 x40 x3 (Curry_Prelude.d_OP_eq_eq x41 (Curry_Prelude.C_Char 'p'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_179 x3 x1002 x3250 x3500) (d_OP__case_179 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_179 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_179 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_178 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_178 x41 x40 x3 x42 x3250 x3500 = case x42 of
     Curry_Prelude.C_True -> d_OP__case_177 x3 x40 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_178 x41 x40 x3 x1002 x3250 x3500) (d_OP__case_178 x41 x40 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_178 x41 x40 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_178 x41 x40 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_177 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_177 x3 x40 x3250 x3500 = case x40 of
     (Curry_Prelude.OP_Cons x42 x43) -> let
          x44 = x42
           in (d_OP__case_176 x44 x43 x3 (Curry_Prelude.d_OP_eq_eq x44 (Curry_Prelude.C_Char 'e'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_177 x3 x1002 x3250 x3500) (d_OP__case_177 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_177 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_177 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_176 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_176 x44 x43 x3 x45 x3250 x3500 = case x45 of
     Curry_Prelude.C_True -> d_OP__case_175 x3 x43 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_176 x44 x43 x3 x1002 x3250 x3500) (d_OP__case_176 x44 x43 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_176 x44 x43 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_176 x44 x43 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_175 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_175 x3 x43 x3250 x3500 = case x43 of
     (Curry_Prelude.OP_Cons x45 x46) -> let
          x47 = x45
           in (d_OP__case_174 x47 x46 x3 (Curry_Prelude.d_OP_eq_eq x47 (Curry_Prelude.C_Char 'd'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_175 x3 x1002 x3250 x3500) (d_OP__case_175 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_175 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_175 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_174 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_174 x47 x46 x3 x48 x3250 x3500 = case x48 of
     Curry_Prelude.C_True -> d_OP__case_173 x3 x46 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_174 x47 x46 x3 x1002 x3250 x3500) (d_OP__case_174 x47 x46 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_174 x47 x46 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_174 x47 x46 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_173 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_173 x3 x46 x3250 x3500 = case x46 of
     Curry_Prelude.OP_List -> d_OP__case_172 x3 x3250 x3500
     (Curry_Prelude.OP_Cons x50 x51) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_173 x3 x1002 x3250 x3500) (d_OP__case_173 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_173 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_173 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_172 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_172 x3 x3250 x3500 = case x3 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_True
     (Curry_Prelude.OP_Cons x48 x49) -> Curry_Prelude.C_False
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_172 x1002 x3250 x3500) (d_OP__case_172 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_172 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_172 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_204 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_204 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_parens x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_204 x2 x1002 x3250 x3500) (d_OP__case_204 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_204 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_204 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_204 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_204 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_parens x2000 x3250 x3500))
     Curry_Prelude.C_False -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_204 x2 x1002 x3000 x3250 x3500) (nd_OP__case_204 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_204 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_204 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_209 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_209 x6 x5 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> d_C_qname x1 x5 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_208 x5 x6 x1 x2 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_209 x6 x5 x1 x2 x1002 x3250 x3500) (d_OP__case_209 x6 x5 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_209 x6 x5 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_209 x6 x5 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_209 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_209 x6 x5 x1 x2 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_qname x1 x5 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_208 x5 x6 x1 x2 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.OP_Tuple2 (d_C_prelude x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '['#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ']'#) Curry_Prelude.OP_List))) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_209 x6 x5 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_209 x6 x5 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_209 x6 x5 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_209 x6 x5 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_208 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_208 x5 x6 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_brackets x3250 x3500) (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) (Curry_Prelude.d_C_head x6 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_207 x5 x6 x1 x2 (d_C_isTupleName x5 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_208 x5 x6 x1 x2 x1002 x3250 x3500) (d_OP__case_208 x5 x6 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_208 x5 x6 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_208 x5 x6 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_208 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_208 x5 x6 x1 x2 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_brackets x2000 x3250 x3500) (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#) (Curry_Prelude.d_C_head x6 x3250 x3500) x2001 x3250 x3500) x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_207 x5 x6 x1 x2 (d_C_isTupleName x5 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_208 x5 x6 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_208 x5 x6 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_208 x5 x6 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_208 x5 x6 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_207 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_207 x5 x6 x1 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_tupled x3250 x3500) (Curry_Prelude.d_C_map (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#)) x6 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_206 x6 x1 x5 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_207 x5 x6 x1 x2 x1002 x3250 x3500) (d_OP__case_207 x5 x6 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_207 x5 x6 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_207 x5 x6 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_207 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_207 x5 x6 x1 x2 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_tupled x2000 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 0#))) x6 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_206 x6 x1 x5 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_207 x5 x6 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_207 x5 x6 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_207 x5 x6 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_207 x5 x6 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_206 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_206 x6 x1 x5 x2 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (d_OP__case_205 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 2#) x3250 x3500) x3250 x3500) (d_C_app (d_C_qname x1 x5 x3250 x3500) (Curry_Prelude.d_C_map (d_C_typeExprDoc x1 (Curry_Prelude.C_Int 2#)) x6 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_206 x6 x1 x5 x2 x1002 x3250 x3500) (d_OP__case_206 x6 x1 x5 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_206 x6 x1 x5 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_206 x6 x1 x5 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_206 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CTypeExpr -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_206 x6 x1 x5 x2 x7 x3000 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2000 = leftSupply x2008
                    x2004 = rightSupply x2008
                     in (seq x2000 (seq x2004 (Curry_Prelude.nd_OP_dollar (nd_OP__case_205 x2 (Curry_Prelude.d_OP_eq_eq x2 (Curry_Prelude.C_Int 2#) x3250 x3500) x2000 x3250 x3500) (let
                         x2003 = leftSupply x2004
                         x2005 = rightSupply x2004
                          in (seq x2003 (seq x2005 (let
                              x2001 = leftSupply x2005
                              x2002 = rightSupply x2005
                               in (seq x2001 (seq x2002 (nd_C_app (nd_C_qname x1 x5 x2001 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id (nd_C_typeExprDoc x1 (Curry_Prelude.C_Int 2#))) x6 x2002 x3250 x3500) x2003 x3250 x3500))))))) x2006 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_206 x6 x1 x5 x2 x1002 x3000 x3250 x3500) (nd_OP__case_206 x6 x1 x5 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_206 x6 x1 x5 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_206 x6 x1 x5 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_205 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_205 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_parens x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_205 x2 x1002 x3250 x3500) (d_OP__case_205 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_205 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_205 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_205 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_205 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_parens x2000 x3250 x3500))
     Curry_Prelude.C_False -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_205 x2 x1002 x3000 x3250 x3500) (nd_OP__case_205 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_205 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_205 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_211 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_211 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_210 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_211 x2 x1 x1002 x3250 x3500) (d_OP__case_211 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_211 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_211 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_211 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_211 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_210 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_211 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_211 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_211 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_211 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_210 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_210 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar (Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_equals x3250 x3500) (Curry_Pretty.d_C_space x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_OP_lt_gt (d_C_bar x3250 x3500) (Curry_Pretty.d_C_space x3250 x3500) x3250 x3500)) (Curry_Prelude.d_C_map (Curry_Prelude.d_OP_dot (Curry_Prelude.d_C_flip (acceptCs id Curry_Pretty.d_OP_lt_gt) (Curry_Pretty.d_C_space x3250 x3500)) (d_C_consDeclDoc x1) x3250 x3500) x2 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_210 x2 x1 x1002 x3250 x3500) (d_OP__case_210 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_210 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_210 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_210 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_210 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2019 = x3000
           in (seq x2019 (let
               x2018 = leftSupply x2019
               x2020 = rightSupply x2019
                in (seq x2018 (seq x2020 (let
                    x2011 = leftSupply x2020
                    x2017 = rightSupply x2020
                     in (seq x2011 (seq x2017 (Curry_Prelude.nd_OP_dollar (let
                         x2003 = leftSupply x2011
                         x2012 = rightSupply x2011
                          in (seq x2003 (seq x2012 (let
                              x2005 = leftSupply x2012
                              x2009 = rightSupply x2012
                               in (seq x2005 (seq x2009 (wrapNX id (Curry_Pretty.nd_C_fillEncloseSep (let
                                   x2002 = leftSupply x2003
                                   x2004 = rightSupply x2003
                                    in (seq x2002 (seq x2004 (let
                                        x2000 = leftSupply x2004
                                        x2001 = rightSupply x2004
                                         in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_equals x2000 x3250 x3500) (Curry_Pretty.nd_C_space x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Pretty.nd_C_empty x2005 x3250 x3500) (let
                                   x2008 = leftSupply x2009
                                   x2010 = rightSupply x2009
                                    in (seq x2008 (seq x2010 (let
                                        x2006 = leftSupply x2010
                                        x2007 = rightSupply x2010
                                         in (seq x2006 (seq x2007 (Curry_Pretty.nd_OP_lt_gt (nd_C_bar x2006 x3250 x3500) (Curry_Pretty.nd_C_space x2007 x3250 x3500) x2008 x3250 x3500))))))))))))))) (let
                         x2016 = leftSupply x2017
                         x2015 = rightSupply x2017
                          in (seq x2016 (seq x2015 (Curry_Prelude.nd_C_map (let
                              x2014 = leftSupply x2015
                              x2013 = rightSupply x2015
                               in (seq x2014 (seq x2013 (Curry_Prelude.nd_OP_dot (wrapNX id (Curry_Prelude.nd_C_flip (wrapDX (wrapNX id) (acceptCs id Curry_Pretty.nd_OP_lt_gt)) (Curry_Pretty.nd_C_space x2013 x3250 x3500))) (wrapNX id (nd_C_consDeclDoc x1)) x2014 x3250 x3500)))) x2 x2016 x3250 x3500)))) x2018 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_210 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_210 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_210 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_210 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_213 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_213 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x7 = d_OP__case_212 x2 x6 (d_C_isInfixName x2 x3250 x3500) x3250 x3500
           in (Curry_Pretty.d_C_text x7 x3250 x3500)
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_213 x1002 x3250 x3500) (d_OP__case_213 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_213 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_213 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_213 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_213 x2 x3000 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Tuple2 x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (let
               x7 = d_OP__case_212 x2 x6 (d_C_isInfixName x2 x3250 x3500) x3250 x3500
                in (Curry_Pretty.nd_C_text x7 x2000 x3250 x3500)))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_213 x1002 x3000 x3250 x3500) (nd_OP__case_213 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_213 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_213 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_212 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_212 x2 x6 x7 x3250 x3500 = case x7 of
     Curry_Prelude.C_True -> x6
     Curry_Prelude.C_False -> Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) (Curry_Prelude.d_OP_plus_plus x6 (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '`'#) Curry_Prelude.OP_List) x3250 x3500)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_212 x2 x6 x1002 x3250 x3500) (d_OP__case_212 x2 x6 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_212 x2 x6 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_212 x2 x6 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_214 :: Curry_Prelude.C_Int -> Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_214 x5 x4 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_COp x6 x7 x8) -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_eq_eq x4 x7 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x5 x8 x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_214 x5 x4 x1002 x3250 x3500) (d_OP__case_214 x5 x4 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_214 x5 x4 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_214 x5 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_215 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Curry_AbstractCurry.C_COpDecl -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_215 x1 x2 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_COp x4 x5 x6) -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) x3250 x3500) (d_OP_opLineDoc_dot_fixDoc_dot_185 x5 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_int x6 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_align x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_hsep x3250 x3500) (Curry_Pretty.d_C_punctuate (Curry_Pretty.d_C_comma x3250 x3500) (Curry_Prelude.d_C_map d_C_opDoc x1 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_215 x1 x1002 x3250 x3500) (d_OP__case_215 x1 x1003 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_215 x1 z x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_215 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_215 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_COpDecl -> Curry_AbstractCurry.C_COpDecl -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_215 x1 x2 x3000 x3250 x3500 = case x2 of
     (Curry_AbstractCurry.C_COp x4 x5 x6) -> let
          x2031 = x3000
           in (seq x2031 (let
               x2030 = leftSupply x2031
               x2032 = rightSupply x2031
                in (seq x2030 (seq x2032 (let
                    x2015 = leftSupply x2032
                    x2028 = rightSupply x2032
                     in (seq x2015 (seq x2028 (Curry_Prelude.nd_C_apply (let
                         x2014 = leftSupply x2015
                         x2016 = rightSupply x2015
                          in (seq x2014 (seq x2016 (let
                              x2000 = leftSupply x2016
                              x2012 = rightSupply x2016
                               in (seq x2000 (seq x2012 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2011 = leftSupply x2012
                                   x2013 = rightSupply x2012
                                    in (seq x2011 (seq x2013 (let
                                        x2008 = leftSupply x2013
                                        x2010 = rightSupply x2013
                                         in (seq x2008 (seq x2010 (Curry_Prelude.nd_C_apply (let
                                             x2007 = leftSupply x2008
                                             x2009 = rightSupply x2008
                                              in (seq x2007 (seq x2009 (let
                                                  x2001 = leftSupply x2009
                                                  x2005 = rightSupply x2009
                                                   in (seq x2001 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                                       x2004 = leftSupply x2005
                                                       x2006 = rightSupply x2005
                                                        in (seq x2004 (seq x2006 (let
                                                            x2002 = leftSupply x2006
                                                            x2003 = rightSupply x2006
                                                             in (seq x2002 (seq x2003 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'n'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'f'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'i'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List))))) x2002 x3250 x3500) (nd_OP_opLineDoc_dot_fixDoc_dot_185 x5 x2003 x3250 x3500) x2004 x3250 x3500))))))) x2007 x3250 x3500))))))) (Curry_Pretty.nd_C_int x6 x2010 x3250 x3500) x2011 x3250 x3500))))))) x2014 x3250 x3500))))))) (let
                         x2027 = leftSupply x2028
                         x2029 = rightSupply x2028
                          in (seq x2027 (seq x2029 (let
                              x2017 = leftSupply x2029
                              x2025 = rightSupply x2029
                               in (seq x2017 (seq x2025 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2017 x3250 x3500) (let
                                   x2024 = leftSupply x2025
                                   x2026 = rightSupply x2025
                                    in (seq x2024 (seq x2026 (let
                                        x2018 = leftSupply x2026
                                        x2022 = rightSupply x2026
                                         in (seq x2018 (seq x2022 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_hsep x2018 x3250 x3500) (let
                                             x2021 = leftSupply x2022
                                             x2023 = rightSupply x2022
                                              in (seq x2021 (seq x2023 (let
                                                  x2019 = leftSupply x2023
                                                  x2020 = rightSupply x2023
                                                   in (seq x2019 (seq x2020 (Curry_Pretty.nd_C_punctuate (Curry_Pretty.nd_C_comma x2019 x3250 x3500) (Curry_Prelude.nd_C_map (wrapNX id nd_C_opDoc) x1 x2020 x3250 x3500) x2021 x3250 x3500))))))) x2024 x3250 x3500))))))) x2027 x3250 x3500))))))) x2030 x3250 x3500))))))))
     (Curry_AbstractCurry.Choice_C_COpDecl x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_215 x1 x1002 x3000 x3250 x3500) (nd_OP__case_215 x1 x1003 x3000 x3250 x3500)
     (Curry_AbstractCurry.Choices_C_COpDecl x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_215 x1 z x3000 x3250 x3500) x1002
     (Curry_AbstractCurry.Guard_C_COpDecl x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_215 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_AbstractCurry.Fail_C_COpDecl x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_216 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_216 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_216 x3 x1002 x3250 x3500) (d_OP__case_216 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_216 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_216 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_216 :: Curry_Prelude.OP_List Curry_AbstractCurry.C_CConsDecl -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_216 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '('#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '.'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char ')'#) Curry_Prelude.OP_List)))) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_216 x3 x1002 x3000 x3250 x3500) (nd_OP__case_216 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_216 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_216 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_218 :: Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_218 x2 x1 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text x1 x3250 x3500) x3250 x3500) x3250 x3500) (d_C_exportsDoc x3 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_217 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_218 x2 x1 x3 x1002 x3250 x3500) (d_OP__case_218 x2 x1 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_218 x2 x1 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_218 x2 x1 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_218 :: Curry_AbstractCurry.C_CurryProg -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_218 x2 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2023 = x3000
           in (seq x2023 (let
               x2022 = leftSupply x2023
               x2024 = rightSupply x2023
                in (seq x2022 (seq x2024 (let
                    x2019 = leftSupply x2024
                    x2021 = rightSupply x2024
                     in (seq x2019 (seq x2021 (Curry_Prelude.nd_C_apply (let
                         x2018 = leftSupply x2019
                         x2020 = rightSupply x2019
                          in (seq x2018 (seq x2020 (let
                              x2000 = leftSupply x2020
                              x2016 = rightSupply x2020
                               in (seq x2000 (seq x2016 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2015 = leftSupply x2016
                                   x2017 = rightSupply x2016
                                    in (seq x2015 (seq x2017 (let
                                        x2012 = leftSupply x2017
                                        x2014 = rightSupply x2017
                                         in (seq x2012 (seq x2014 (Curry_Prelude.nd_C_apply (let
                                             x2011 = leftSupply x2012
                                             x2013 = rightSupply x2012
                                              in (seq x2011 (seq x2013 (let
                                                  x2001 = leftSupply x2013
                                                  x2009 = rightSupply x2013
                                                   in (seq x2001 (seq x2009 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (let
                                                       x2008 = leftSupply x2009
                                                       x2010 = rightSupply x2009
                                                        in (seq x2008 (seq x2010 (let
                                                            x2005 = leftSupply x2010
                                                            x2007 = rightSupply x2010
                                                             in (seq x2005 (seq x2007 (Curry_Prelude.nd_C_apply (let
                                                                 x2004 = leftSupply x2005
                                                                 x2006 = rightSupply x2005
                                                                  in (seq x2004 (seq x2006 (let
                                                                      x2002 = leftSupply x2006
                                                                      x2003 = rightSupply x2006
                                                                       in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2002 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x2003 x3250 x3500) x2004 x3250 x3500))))))) (Curry_Pretty.nd_C_text x1 x2007 x3250 x3500) x2008 x3250 x3500))))))) x2011 x3250 x3500))))))) (nd_C_exportsDoc x3 x2014 x3250 x3500) x2015 x3250 x3500))))))) x2018 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x2021 x3250 x3500) x2022 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_217 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_218 x2 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_218 x2 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_218 x2 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_218 x2 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_217 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_217 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_plus_gt x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text x1 x3250 x3500) x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_217 x1 x1002 x3250 x3500) (d_OP__case_217 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_217 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_217 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_217 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_217 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2015 = x3000
           in (seq x2015 (let
               x2014 = leftSupply x2015
               x2016 = rightSupply x2015
                in (seq x2014 (seq x2016 (let
                    x2011 = leftSupply x2016
                    x2013 = rightSupply x2016
                     in (seq x2011 (seq x2013 (Curry_Prelude.nd_C_apply (let
                         x2010 = leftSupply x2011
                         x2012 = rightSupply x2011
                          in (seq x2010 (seq x2012 (let
                              x2000 = leftSupply x2012
                              x2008 = rightSupply x2012
                               in (seq x2000 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2000 x3250 x3500) (let
                                   x2007 = leftSupply x2008
                                   x2009 = rightSupply x2008
                                    in (seq x2007 (seq x2009 (let
                                        x2004 = leftSupply x2009
                                        x2006 = rightSupply x2009
                                         in (seq x2004 (seq x2006 (Curry_Prelude.nd_C_apply (let
                                             x2003 = leftSupply x2004
                                             x2005 = rightSupply x2004
                                              in (seq x2003 (seq x2005 (let
                                                  x2001 = leftSupply x2005
                                                  x2002 = rightSupply x2005
                                                   in (seq x2001 (seq x2002 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_plus_gt x2001 x3250 x3500) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'm'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'd'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'u'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List)))))) x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Pretty.nd_C_text x1 x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))) (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'w'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'h'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'r'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) Curry_Prelude.OP_List))))) x2013 x3250 x3500) x2014 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_217 x1 x1002 x3000 x3250 x3500) (nd_OP__case_217 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_217 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_217 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_221 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_221 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text x3 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_220 x2 (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 25#) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_221 x3 x2 x1002 x3250 x3500) (d_OP__case_221 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_221 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_221 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_221 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_221 x3 x2 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_220 x2 (Curry_Prelude.d_OP_gt x2 (Curry_Prelude.C_Int 25#) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_221 x3 x2 x1002 x3000 x3250 x3500) (nd_OP__case_221 x3 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_221 x3 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_221 x3 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_220 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_220 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_219 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_220 x2 x1002 x3250 x3500) (d_OP__case_220 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_220 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_220 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_220 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_220 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'x'#) Curry_Prelude.OP_List) (Curry_Prelude.d_C_show x2 x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_219 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_220 x2 x1002 x3000 x3250 x3500) (nd_OP__case_220 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_220 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_220 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_219 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_219 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_219 x2 x1002 x3250 x3500) (d_OP__case_219 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_219 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_219 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_219 :: Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_219 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.OP_Cons (Curry_Prelude.d_C_chr (Curry_Prelude.d_OP_plus (Curry_Prelude.C_Int 97#) x2 x3250 x3500) x3250 x3500) Curry_Prelude.OP_List) x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_219 x2 x1002 x3000 x3250 x3500) (nd_OP__case_219 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_219 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_219 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_223 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_223 x2 x3250 x3500 = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          x5 = x3
           in (d_OP__case_222 x5 x2 x4 (Curry_Prelude.d_OP_eq_eq x5 (Curry_Prelude.C_Char '_'#) x3250 x3500) x3250 x3500)
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_223 x1002 x3250 x3500) (d_OP__case_223 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_223 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_223 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_222 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.OP_List Curry_Prelude.C_Char
d_OP__case_222 x5 x2 x4 x6 x3250 x3500 = case x6 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> x2
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_222 x5 x2 x4 x1002 x3250 x3500) (d_OP__case_222 x5 x2 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_222 x5 x2 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_222 x5 x2 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_228 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_228 x2 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text x4 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_227 x2 x3 x1 x4 (d_C_isInfixName x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_228 x2 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_228 x2 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_228 x2 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_228 x2 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_228 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_228 x2 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text x4 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_227 x2 x3 x1 x4 (d_C_isInfixName x2 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_228 x2 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_228 x2 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_228 x2 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_228 x2 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_227 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_227 x2 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP__case_226 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 x1 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_not (d_C_qualifiedNames x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_225 x3 x1 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_227 x2 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_227 x2 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_227 x2 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_227 x2 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_227 :: Curry_Prelude.OP_Tuple2 (Curry_Prelude.OP_List Curry_Prelude.C_Char) (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_227 x2 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_226 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 x1 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_not (d_C_qualifiedNames x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_225 x3 x1 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_227 x2 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_227 x2 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_227 x2 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_227 x2 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_225 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_225 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> d_OP__case_224 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 x1 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_not (d_C_qualifiedNames x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_225 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_225 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_225 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_225 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_225 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_225 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_224 x3 x1 x4 (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 x1 x3250 x3500) (Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x3 (d_C_prelude x3250 x3500) x3250 x3500) (Curry_Prelude.d_C_not (d_C_qualifiedNames x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_225 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_225 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_225 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_225 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_224 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_224 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (d_C_correctName x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_text x3 x3250 x3500) (Curry_Pretty.d_C_dot x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text x4 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_224 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_224 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_224 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_224 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_224 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_224 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (d_C_correctName x4 x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2007 = x3000
           in (seq x2007 (let
               x2006 = leftSupply x2007
               x2008 = rightSupply x2007
                in (seq x2006 (seq x2008 (let
                    x2003 = leftSupply x2008
                    x2005 = rightSupply x2008
                     in (seq x2003 (seq x2005 (Curry_Pretty.nd_OP_lt_gt (let
                         x2002 = leftSupply x2003
                         x2004 = rightSupply x2003
                          in (seq x2002 (seq x2004 (let
                              x2000 = leftSupply x2004
                              x2001 = rightSupply x2004
                               in (seq x2000 (seq x2001 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_text x3 x2000 x3250 x3500) (Curry_Pretty.nd_C_dot x2001 x3250 x3500) x2002 x3250 x3500))))))) (Curry_Pretty.nd_C_text x4 x2005 x3250 x3500) x2006 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_224 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_224 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_224 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_224 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_226 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_226 x3 x1 x4 x5 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Pretty.d_C_text x4 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (Curry_Pretty.d_C_parens x3250 x3500) (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_text x3 x3250 x3500) (Curry_Pretty.d_C_dot x3250 x3500) x3250 x3500) (Curry_Pretty.d_C_text x4 x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_226 x3 x1 x4 x1002 x3250 x3500) (d_OP__case_226 x3 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_226 x3 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_226 x3 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_226 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_226 x3 x1 x4 x5 x3000 x3250 x3500 = case x5 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (Curry_Pretty.nd_C_text x4 x2001 x3250 x3500) x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2011 = x3000
           in (seq x2011 (let
               x2010 = leftSupply x2011
               x2012 = rightSupply x2011
                in (seq x2010 (seq x2012 (let
                    x2000 = leftSupply x2012
                    x2008 = rightSupply x2012
                     in (seq x2000 (seq x2008 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_parens x2000 x3250 x3500) (let
                         x2007 = leftSupply x2008
                         x2009 = rightSupply x2008
                          in (seq x2007 (seq x2009 (let
                              x2004 = leftSupply x2009
                              x2006 = rightSupply x2009
                               in (seq x2004 (seq x2006 (Curry_Pretty.nd_OP_lt_gt (let
                                   x2003 = leftSupply x2004
                                   x2005 = rightSupply x2004
                                    in (seq x2003 (seq x2005 (let
                                        x2001 = leftSupply x2005
                                        x2002 = rightSupply x2005
                                         in (seq x2001 (seq x2002 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_text x3 x2001 x3250 x3500) (Curry_Pretty.nd_C_dot x2002 x3250 x3500) x2003 x3250 x3500))))))) (Curry_Pretty.nd_C_text x4 x2006 x3250 x3500) x2007 x3250 x3500))))))) x2010 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_226 x3 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_226 x3 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_226 x3 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_226 x3 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_236 :: Curry_Prelude.Curry t0 => t0 -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_236 x6 x5 x1 x4 x3 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__case_235 x8 x6 x5 x7 x1 x4 (Curry_Prelude.d_OP_gt x6 x8 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_236 x6 x5 x1 x4 x1002 x3250 x3500) (d_OP__case_236 x6 x5 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_236 x6 x5 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_236 x6 x5 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_236 :: Curry_Prelude.Curry t0 => t0 -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity t0 -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_236 x6 x5 x1 x4 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_235 x8 x6 x5 x7 x1 x4 (Curry_Prelude.d_OP_gt x6 x8 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_236 x6 x5 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_236 x6 x5 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_236 x6 x5 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_236 x6 x5 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_Tuple2 x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_235 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_235 x8 x6 x5 x7 x1 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_234 x8 x6 x5 x7 x1 x4 (Curry_Prelude.d_OP_lt x6 x8 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_235 x8 x6 x5 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_235 x8 x6 x5 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_235 x8 x6 x5 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_235 x8 x6 x5 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_235 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_235 x8 x6 x5 x7 x1 x4 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_234 x8 x6 x5 x7 x1 x4 (Curry_Prelude.d_OP_lt x6 x8 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_235 x8 x6 x5 x7 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_235 x8 x6 x5 x7 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_235 x8 x6 x5 x7 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_235 x8 x6 x5 x7 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_234 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_234 x8 x6 x5 x7 x1 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_233 x5 x7 x1 x4 (Curry_Prelude.d_OP_eq_eq x5 Curry_AbstractCurry.C_CInfixOp x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_234 x8 x6 x5 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_234 x8 x6 x5 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_234 x8 x6 x5 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_234 x8 x6 x5 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_234 :: Curry_Prelude.Curry t0 => t0 -> t0 -> Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_234 x8 x6 x5 x7 x1 x4 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_233 x5 x7 x1 x4 (Curry_Prelude.d_OP_eq_eq x5 Curry_AbstractCurry.C_CInfixOp x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_234 x8 x6 x5 x7 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_234 x8 x6 x5 x7 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_234 x8 x6 x5 x7 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_234 x8 x6 x5 x7 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_233 :: Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_233 x5 x7 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_232 x7 x5 x1 x4 (Curry_Prelude.d_OP_slash_eq x5 x7 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_233 x5 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_233 x5 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_233 x5 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_233 x5 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_233 :: Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_233 x5 x7 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_232 x7 x5 x1 x4 (Curry_Prelude.d_OP_slash_eq x5 x7 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_233 x5 x7 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_233 x5 x7 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_233 x5 x7 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_233 x5 x7 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_232 :: Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_232 x7 x5 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_231 x7 x1 x4 (Curry_Prelude.d_OP_ampersand_ampersand x1 (Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractCurry.C_CInfixrOp x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_232 x7 x5 x1 x4 x1002 x3250 x3500) (d_OP__case_232 x7 x5 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_232 x7 x5 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_232 x7 x5 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_232 :: Curry_AbstractCurry.C_CFixity -> Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_232 x7 x5 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_231 x7 x1 x4 (Curry_Prelude.d_OP_ampersand_ampersand x1 (Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractCurry.C_CInfixrOp x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_232 x7 x5 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_232 x7 x5 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_232 x7 x5 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_232 x7 x5 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_231 :: Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_231 x7 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_230 x7 x1 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not x1 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractCurry.C_CInfixlOp x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_231 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_231 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_231 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_231 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_231 :: Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_231 x7 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_230 x7 x1 x4 (Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_C_not x1 x3250 x3500) (Curry_Prelude.d_OP_eq_eq x7 Curry_AbstractCurry.C_CInfixlOp x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_231 x7 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_231 x7 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_231 x7 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_231 x7 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_230 :: Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_230 x7 x1 x4 x8 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> d_OP__case_229 Curry_Prelude.C_True x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_230 x7 x1 x4 x1002 x3250 x3500) (d_OP__case_230 x7 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_230 x7 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_230 x7 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_230 :: Curry_AbstractCurry.C_CFixity -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_230 x7 x1 x4 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> x4
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_229 Curry_Prelude.C_True x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_230 x7 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_230 x7 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_230 x7 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_230 x7 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_229 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_229 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_229 x1002 x3250 x3500) (d_OP__case_229 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_229 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_229 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_229 :: Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_229 x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_229 x1002 x3000 x3250 x3500) (nd_OP__case_229 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_229 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_229 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_238 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) x6 x7 x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1002 x3250 x3500) (d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_238 :: Curry_Prelude.C_Maybe (Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int) -> Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_empty x2000 x3250 x3500) (Curry_Pretty.nd_C_empty x2001 x3250 x3500) x6 x7 x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_238 x3 x7 x6 x5 x8 x2 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_237 :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x9 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_fillEncloseSep (d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x8 x4 x3250 x3500) (d_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x8 x5 x3250 x3500) x6 x7 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1002 x3250 x3500) (d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_237 :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.OP_Tuple2 Curry_AbstractCurry.C_CFixity Curry_Prelude.C_Int -> Curry_Prelude.C_Bool -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x9 x3000 x3250 x3500 = case x9 of
     Curry_Prelude.C_True -> let
          x2003 = x3000
           in (seq x2003 (let
               x2002 = leftSupply x2003
               x2004 = rightSupply x2003
                in (seq x2002 (seq x2004 (let
                    x2000 = leftSupply x2004
                    x2001 = rightSupply x2004
                     in (seq x2000 (seq x2001 (Curry_Pretty.nd_C_fillEncloseSep (nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x8 x4 x2000 x3250 x3500) (nd_OP_precFillEncloseSep_dot_pre_dot_107 x1 x2 x8 x5 x2001 x3250 x3500) x6 x7 x2002 x3250 x3500))))))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1002 x3000 x3250 x3500) (nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_237 x7 x6 x5 x8 x2 x1 x4 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_239 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_239 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_parens x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_239 x1 x1002 x3250 x3500) (d_OP__case_239 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_239 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_239 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_239 :: Curry_Prelude.Curry t0 => Curry_Prelude.C_Maybe t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Func Curry_Pretty.C_Doc Curry_Pretty.C_Doc
nd_OP__case_239 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_parens x2000 x3250 x3500))
     Curry_Prelude.C_False -> wrapDX id Curry_Prelude.d_C_id
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_239 x1 x1002 x3000 x3250 x3500) (nd_OP__case_239 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_239 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_239 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_240 :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_240 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> Curry_Prelude.d_C_apply (d_C_block x3250 x3500) (Curry_Pretty.d_C_fillEncloseSep (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_C_empty x3250 x3500) (Curry_Pretty.d_C_space x3250 x3500) (Curry_Prelude.OP_Cons x1 x2) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_240 x2 x1 x1002 x3250 x3500) (d_OP__case_240 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_240 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_240 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_240 :: Curry_Prelude.OP_List Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_240 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2009 = x3000
           in (seq x2009 (let
               x2008 = leftSupply x2009
               x2010 = rightSupply x2009
                in (seq x2008 (seq x2010 (let
                    x2000 = leftSupply x2010
                    x2005 = rightSupply x2010
                     in (seq x2000 (seq x2005 (Curry_Prelude.nd_C_apply (nd_C_block x2000 x3250 x3500) (let
                         x2006 = leftSupply x2005
                         x2007 = rightSupply x2005
                          in (seq x2006 (seq x2007 (let
                              x2004 = leftSupply x2006
                              x2001 = rightSupply x2006
                               in (seq x2004 (seq x2001 (let
                                   x2002 = leftSupply x2007
                                   x2003 = rightSupply x2007
                                    in (seq x2002 (seq x2003 (Curry_Pretty.nd_C_fillEncloseSep (Curry_Pretty.nd_C_empty x2001 x3250 x3500) (Curry_Pretty.nd_C_empty x2002 x3250 x3500) (Curry_Pretty.nd_C_space x2003 x3250 x3500) (Curry_Prelude.OP_Cons x1 x2) x2004 x3250 x3500)))))))))) x2008 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_240 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_240 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_240 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_240 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_241 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_241 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Pretty.d_OP_lt_gt (Curry_Pretty.d_C_space x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_align x3250 x3500) (Curry_Prelude.d_C_apply (Curry_Pretty.d_C_fillSep x3250 x3500) (Curry_Prelude.d_C_map (d_C_varDoc x3250 x3500) x2 x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_241 x2 x1002 x3250 x3500) (d_OP__case_241 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_241 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_241 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_241 :: Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 Curry_Prelude.C_Int (Curry_Prelude.OP_List Curry_Prelude.C_Char)) -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_241 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2013 = x3000
           in (seq x2013 (let
               x2012 = leftSupply x2013
               x2014 = rightSupply x2013
                in (seq x2012 (seq x2014 (let
                    x2000 = leftSupply x2014
                    x2010 = rightSupply x2014
                     in (seq x2000 (seq x2010 (Curry_Pretty.nd_OP_lt_gt (Curry_Pretty.nd_C_space x2000 x3250 x3500) (let
                         x2009 = leftSupply x2010
                         x2011 = rightSupply x2010
                          in (seq x2009 (seq x2011 (let
                              x2001 = leftSupply x2011
                              x2007 = rightSupply x2011
                               in (seq x2001 (seq x2007 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_align x2001 x3250 x3500) (let
                                   x2006 = leftSupply x2007
                                   x2008 = rightSupply x2007
                                    in (seq x2006 (seq x2008 (let
                                        x2002 = leftSupply x2008
                                        x2005 = rightSupply x2008
                                         in (seq x2002 (seq x2005 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_C_fillSep x2002 x3250 x3500) (let
                                             x2004 = leftSupply x2005
                                             x2003 = rightSupply x2005
                                              in (seq x2004 (seq x2003 (Curry_Prelude.nd_C_map (nd_C_varDoc x2003 x3250 x3500) x2 x2004 x3250 x3500)))) x2006 x3250 x3500))))))) x2009 x3250 x3500))))))) x2012 x3250 x3500))))))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_241 x2 x1002 x3000 x3250 x3500) (nd_OP__case_241 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_241 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_241 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_244 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_244 x1 x2 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> d_OP__case_243 x2 x1 (Curry_Pretty.d_C_isEmpty x2 x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_244 x1 x2 x1002 x3250 x3500) (d_OP__case_244 x1 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_244 x1 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_244 x1 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_244 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_244 x1 x2 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x2
     Curry_Prelude.C_False -> let
          x2002 = x3000
           in (seq x2002 (let
               x2001 = leftSupply x2002
               x2000 = rightSupply x2002
                in (seq x2001 (seq x2000 (nd_OP__case_243 x2 x1 (Curry_Pretty.nd_C_isEmpty x2 x2000 x3250 x3500) x2001 x3250 x3500)))))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_244 x1 x2 x1002 x3000 x3250 x3500) (nd_OP__case_244 x1 x2 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_244 x1 x2 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_244 x1 x2 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_243 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_243 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> d_OP__case_242 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_243 x2 x1 x1002 x3250 x3500) (d_OP__case_243 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_243 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_243 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_243 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_243 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> x1
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_242 x2 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_243 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_243 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_243 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_243 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_242 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_242 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_OP_lt_gt (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_apply (Curry_Pretty.d_OP_lt_dollar_gt x3250 x3500) x1 x3250 x3500) (Curry_Pretty.d_C_line x3250 x3500) x3250 x3500) x2 x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_242 x2 x1 x1002 x3250 x3500) (d_OP__case_242 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_242 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_242 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_242 :: Curry_Pretty.C_Doc -> Curry_Pretty.C_Doc -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_242 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2008 = x3000
           in (seq x2008 (let
               x2007 = leftSupply x2008
               x2005 = rightSupply x2008
                in (seq x2007 (seq x2005 (Curry_Pretty.nd_OP_lt_gt (let
                    x2004 = leftSupply x2005
                    x2006 = rightSupply x2005
                     in (seq x2004 (seq x2006 (let
                         x2002 = leftSupply x2006
                         x2003 = rightSupply x2006
                          in (seq x2002 (seq x2003 (Curry_Prelude.nd_C_apply (let
                              x2001 = leftSupply x2002
                              x2000 = rightSupply x2002
                               in (seq x2001 (seq x2000 (Curry_Prelude.nd_C_apply (Curry_Pretty.nd_OP_lt_dollar_gt x2000 x3250 x3500) x1 x2001 x3250 x3500)))) (Curry_Pretty.nd_C_line x2003 x3250 x3500) x2004 x3250 x3500))))))) x2 x2007 x3250 x3500)))))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_242 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_242 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_242 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_242 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_287 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_287 x4 x1 x3 x3000 x3250 x3500 = case x3 of
     (Curry_Prelude.OP_Cons x5 x6) -> let
          x2000 = x3000
           in (seq x2000 (let
               x7 = x5
                in (nd_OP__case_286 x7 x4 x1 x3 x6 (Curry_Prelude.d_OP_eq_eq x7 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_246 x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_287 x4 x1 x1002 x3000 x3250 x3500) (nd_OP__case_287 x4 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_287 x4 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_287 x4 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_246 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_246 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_245 x1 (d_OP_main_dot_checkMod_dot_66 Curry_Prelude.OP_List x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x53 x54) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_246 x1 x1002 x3000 x3250 x3500) (nd_OP__case_246 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_246 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_246 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_245 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_245 x1 x2 x3000 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg Curry_Prelude.OP_List x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_245 x1 x1002 x3000 x3250 x3500) (nd_OP__case_245 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_245 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_245 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_286 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_286 x7 x4 x1 x3 x6 x8 x3000 x3250 x3500 = case x8 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_285 x4 x1 x3 x6 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_248 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_286 x7 x4 x1 x3 x6 x1002 x3000 x3250 x3500) (nd_OP__case_286 x7 x4 x1 x3 x6 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_286 x7 x4 x1 x3 x6 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_286 x7 x4 x1 x3 x6 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_248 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_248 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_247 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x51 x52) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_248 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_248 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_248 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_248 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_247 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_247 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_247 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_247 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_247 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_247 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_285 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_285 x4 x1 x3 x6 x3000 x3250 x3500 = case x6 of
     (Curry_Prelude.OP_Cons x8 x9) -> let
          x2000 = x3000
           in (seq x2000 (let
               x10 = x8
                in (nd_OP__case_284 x10 x4 x1 x3 x9 (Curry_Prelude.d_OP_eq_eq x10 (Curry_Prelude.C_Char '-'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_250 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_285 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_285 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_285 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_285 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_250 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_250 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_249 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x49 x50) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_250 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_250 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_250 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_250 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_249 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_249 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_249 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_249 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_249 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_249 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_284 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_284 x10 x4 x1 x3 x9 x11 x3000 x3250 x3500 = case x11 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_283 x4 x1 x3 x9 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_252 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_284 x10 x4 x1 x3 x9 x1002 x3000 x3250 x3500) (nd_OP__case_284 x10 x4 x1 x3 x9 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_284 x10 x4 x1 x3 x9 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_284 x10 x4 x1 x3 x9 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_252 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_252 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_251 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x47 x48) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_252 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_252 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_252 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_252 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_251 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_251 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_251 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_251 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_251 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_251 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_283 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_283 x4 x1 x3 x9 x3000 x3250 x3500 = case x9 of
     (Curry_Prelude.OP_Cons x11 x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = x11
                in (nd_OP__case_282 x13 x4 x1 x3 x12 (Curry_Prelude.d_OP_eq_eq x13 (Curry_Prelude.C_Char 'u'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_254 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_283 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_283 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_283 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_283 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_254 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_254 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_253 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x45 x46) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_254 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_254 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_254 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_254 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_253 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_253 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_253 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_253 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_253 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_253 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_282 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_282 x13 x4 x1 x3 x12 x14 x3000 x3250 x3500 = case x14 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_281 x4 x1 x3 x12 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_256 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_282 x13 x4 x1 x3 x12 x1002 x3000 x3250 x3500) (nd_OP__case_282 x13 x4 x1 x3 x12 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_282 x13 x4 x1 x3 x12 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_282 x13 x4 x1 x3 x12 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_256 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_256 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_255 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x43 x44) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_256 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_256 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_256 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_256 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_255 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_255 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_255 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_255 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_255 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_255 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_281 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_281 x4 x1 x3 x12 x3000 x3250 x3500 = case x12 of
     (Curry_Prelude.OP_Cons x14 x15) -> let
          x2000 = x3000
           in (seq x2000 (let
               x16 = x14
                in (nd_OP__case_280 x16 x4 x1 x3 x15 (Curry_Prelude.d_OP_eq_eq x16 (Curry_Prelude.C_Char 'a'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_258 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_281 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_281 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_281 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_281 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_258 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_258 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_257 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x41 x42) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_258 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_258 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_258 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_258 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_257 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_257 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_257 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_257 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_257 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_257 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_280 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_280 x16 x4 x1 x3 x15 x17 x3000 x3250 x3500 = case x17 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_279 x4 x1 x3 x15 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_260 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_280 x16 x4 x1 x3 x15 x1002 x3000 x3250 x3500) (nd_OP__case_280 x16 x4 x1 x3 x15 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_280 x16 x4 x1 x3 x15 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_280 x16 x4 x1 x3 x15 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_260 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_260 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_259 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x39 x40) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_260 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_260 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_260 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_260 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_259 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_259 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_259 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_259 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_259 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_259 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_279 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_279 x4 x1 x3 x15 x3000 x3250 x3500 = case x15 of
     (Curry_Prelude.OP_Cons x17 x18) -> let
          x2000 = x3000
           in (seq x2000 (let
               x19 = x17
                in (nd_OP__case_278 x19 x4 x1 x3 x18 (Curry_Prelude.d_OP_eq_eq x19 (Curry_Prelude.C_Char 'c'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_262 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_279 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_279 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_279 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_279 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_262 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_262 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_261 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x37 x38) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_262 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_262 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_262 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_262 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_261 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_261 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_261 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_261 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_261 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_261 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_278 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_278 x19 x4 x1 x3 x18 x20 x3000 x3250 x3500 = case x20 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_277 x4 x1 x3 x18 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_264 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_278 x19 x4 x1 x3 x18 x1002 x3000 x3250 x3500) (nd_OP__case_278 x19 x4 x1 x3 x18 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_278 x19 x4 x1 x3 x18 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_278 x19 x4 x1 x3 x18 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_264 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_264 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_263 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x35 x36) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_264 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_264 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_264 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_264 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_263 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_263 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_263 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_263 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_263 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_263 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_277 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_277 x4 x1 x3 x18 x3000 x3250 x3500 = case x18 of
     (Curry_Prelude.OP_Cons x20 x21) -> let
          x2000 = x3000
           in (seq x2000 (let
               x22 = x20
                in (nd_OP__case_276 x22 x4 x1 x3 x21 (Curry_Prelude.d_OP_eq_eq x22 (Curry_Prelude.C_Char 'y'#) x3250 x3500) x2000 x3250 x3500)))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_266 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_277 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_277 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_277 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_277 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_266 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_266 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_265 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x33 x34) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_266 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_266 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_266 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_266 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_265 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_265 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_265 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_265 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_265 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_265 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_276 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_276 x22 x4 x1 x3 x21 x23 x3000 x3250 x3500 = case x23 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_275 x4 x1 x3 x21 x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_268 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_276 x22 x4 x1 x3 x21 x1002 x3000 x3250 x3500) (nd_OP__case_276 x22 x4 x1 x3 x21 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_276 x22 x4 x1 x3 x21 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_276 x22 x4 x1 x3 x21 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_268 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_268 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_267 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x31 x32) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_268 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_268 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_268 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_268 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_267 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_267 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_267 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_267 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_267 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_267 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_275 :: Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_275 x4 x1 x3 x21 x3000 x3250 x3500 = case x21 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_274 x3 x1 x4 x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x27 x28) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_270 x1 x3 x4 x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_275 x4 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_275 x4 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_275 x4 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_275 x4 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_270 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_270 x1 x3 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_269 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x29 x30) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_270 x1 x3 x1002 x3000 x3250 x3500) (nd_OP__case_270 x1 x3 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_270 x1 x3 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_270 x1 x3 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_269 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_269 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_269 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_269 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_269 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_269 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_274 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_274 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     (Curry_Prelude.OP_Cons x23 x24) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_273 x1 x23 x24 x2000 x3250 x3500))
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_271 x3 x1 (d_OP_main_dot_checkMod_dot_66 x3 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_274 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_274 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_274 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_274 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_271 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_271 x3 x1 x4 x3000 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printCProg x3 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_271 x3 x1 x1002 x3000 x3250 x3500) (nd_OP__case_271 x3 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_271 x3 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_271 x3 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_273 :: Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.OP_List (Curry_Prelude.OP_List Curry_Prelude.C_Char) -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_273 x1 x23 x24 x3000 x3250 x3500 = case x24 of
     Curry_Prelude.OP_List -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_272 x23 x1 (d_OP_main_dot_checkMod_dot_66 x23 x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.OP_Cons x25 x26) -> x1
     (Curry_Prelude.Choice_OP_List x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_273 x1 x23 x1002 x3000 x3250 x3500) (nd_OP__case_273 x1 x23 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_OP_List x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_273 x1 x23 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_OP_List x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_273 x1 x23 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_OP_List x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_272 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
nd_OP__case_272 x23 x1 x24 x3000 x3250 x3500 = case x24 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_C_printUCProg x23 x2000 x3250 x3500))
     Curry_Prelude.C_False -> x1
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_272 x23 x1 x1002 x3000 x3250 x3500) (nd_OP__case_272 x23 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_272 x23 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_272 x23 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_289 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_289 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.C_False
     Curry_Prelude.C_False -> d_OP__case_288 x1 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_289 x1 x1002 x3250 x3500) (d_OP__case_289 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_289 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_289 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_288 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_288 x1 x2 x3250 x3500 = case x2 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_dollar d_OP_main_dot_checkMod'_dot_66 (Curry_Prelude.d_C_apply (Curry_Prelude.d_C_reverse x3250 x3500) x1 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_288 x1 x1002 x3250 x3500) (d_OP__case_288 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_288 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_288 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_291 :: Curry_Prelude.C_Char -> Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_291 x2 x3 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.C_True
     Curry_Prelude.C_False -> d_OP__case_290 x3 x2 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_291 x2 x3 x1002 x3250 x3500) (d_OP__case_291 x2 x3 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_291 x2 x3 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_291 x2 x3 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_290 :: Curry_Prelude.OP_List Curry_Prelude.C_Char -> Curry_Prelude.C_Char -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_OP__case_290 x3 x2 x4 x3250 x3500 = case x4 of
     Curry_Prelude.C_True -> Curry_Prelude.d_OP_ampersand_ampersand (Curry_Prelude.d_OP_slash_eq x2 (Curry_Prelude.C_Char '.'#) x3250 x3500) (d_OP_main_dot_checkMod'_dot_66 x3 x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_290 x3 x2 x1002 x3250 x3500) (d_OP__case_290 x3 x2 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_290 x3 x2 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_290 x3 x2 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_293 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t1 -> t0 -> Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_293 x2 x1 x3 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500
     Curry_Prelude.C_False -> d_OP__case_292 (Curry_Prelude.d_C_otherwise x3250 x3500) x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_293 x2 x1 x1002 x3250 x3500) (d_OP__case_293 x2 x1 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_293 x2 x1 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_293 x2 x1 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_293 :: (Curry_Prelude.Curry t1,Curry_Prelude.Curry t0) => t1 -> t0 -> Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_293 x2 x1 x3 x3000 x3250 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_text (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '{'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) Curry_Prelude.OP_List)) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x1 x3250 x3500) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '@'#) Curry_Prelude.OP_List) (Curry_Prelude.d_OP_plus_plus (Curry_Prelude.d_C_show x2 x3250 x3500) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '-'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char '}'#) Curry_Prelude.OP_List)) x3250 x3500) x3250 x3500) x3250 x3500) x3250 x3500) x2000 x3250 x3500))
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__case_292 (Curry_Prelude.d_C_otherwise x3250 x3500) x2000 x3250 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_293 x2 x1 x1002 x3000 x3250 x3500) (nd_OP__case_293 x2 x1 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_293 x2 x1 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_293 x2 x1 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

d_OP__case_292 :: Curry_Prelude.C_Bool -> Cover -> ConstStore -> Curry_Pretty.C_Doc
d_OP__case_292 x1 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> Curry_Pretty.d_C_empty x3250 x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (d_OP__case_292 x1002 x3250 x3500) (d_OP__case_292 x1003 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> d_OP__case_292 z x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((d_OP__case_292 x1002 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo

nd_OP__case_292 :: Curry_Prelude.C_Bool -> IDSupply -> Cover -> ConstStore -> Curry_Pretty.C_Doc
nd_OP__case_292 x1 x3000 x3250 x3500 = case x1 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (Curry_Pretty.nd_C_empty x2000 x3250 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3250 x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002 x1003) -> narrow x1000 x1001 (nd_OP__case_292 x1002 x3000 x3250 x3500) (nd_OP__case_292 x1003 x3000 x3250 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001 x1002) -> narrows x3500 x1000 x1001 (\z -> nd_OP__case_292 z x3000 x3250 x3500) x1002
     (Curry_Prelude.Guard_C_Bool x1000 x1001 x1002) -> guardCons x1000 x1001 ((nd_OP__case_292 x1002 x3000 x3250) $! (addCs x1001 x3500))
     (Curry_Prelude.Fail_C_Bool x1000 x1001) -> failCons x1000 x1001
     _ -> failCons x3250 defFailInfo
